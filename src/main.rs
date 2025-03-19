// TODO:
// * Backspace at an indented line start should unindent by 1.
// * ...same for Shift+Tab.
// * Backspace undo grouping.
// * Goto line
// * When focus changes, all scrollareas in the path should scroll the item into view
// * Windows should have a yedit titlebar
// * yori colors
// * grid layout
// * Checking if the focus-down path is identical to focus-up, so that drags stay in their path and clicks only click.
// * Output diffing / compression
// * BUG: When word-wrap is enabled, insert a tab into the last word of the first of a wrapped line.
//   The entire word will wrap with the tab in the middle, as if the tab is not whitespace.
// * COMMIT_MESSAGE, --rulers, .editorconfig
// --------------------------------------------------
// * This would allow us to skip computing the stats.visual_lines,
//   because we could simply scroll by the number of logical lines.
// * Word wrapping could not be part of ucd, but rather get computed
//   lazily by the render function. This trivializes everything else.
// * Replace
// * Multi-Cursor
// * Scrolling by dragging the track/thumb
// * For the focus path we can use the tree depth to O(1) check if the path contains the focus.

#![allow(
    dead_code,
    clippy::needless_if,
    clippy::uninit_assumed_init,
    clippy::missing_transmute_annotations
)]

use buffer::RcTextBuffer;
use helpers::{COORD_TYPE_SAFE_MAX, DisplayablePathBuf, Point};
use input::{kbmod, vk};

use crate::framebuffer::IndexedColor;
use crate::helpers::{Rect, Size};
use crate::loc::{LocId, loc};
use crate::tui::*;
use crate::vt::Token;
use std::fs::File;
use std::mem;
use std::path::{Path, PathBuf};
use std::{cmp, process};

#[cfg(feature = "debug-latency")]
use std::fmt::Write;

mod apperr;
mod buffer;
mod framebuffer;
mod fuzzy;
mod helpers;
mod icu;
mod input;
mod loc;
mod memchr;
mod sys;
mod trust_me_bro;
mod tui;
mod ucd;
mod ucd_gen;
mod utf8;
mod vt;

struct RestoreModes;

impl Drop for RestoreModes {
    fn drop(&mut self) {
        // Same as in the beginning but in the reverse order.
        // It also includes DECSCUSR 0 to reset the cursor style and DECTCEM to show the cursor.
        sys::write_stdout("\x1b[?1002;1006;2004l\x1b[?1049l\x1b[0 q\x1b[?25h");
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StateSearch {
    Hidden,
    Focus,
    Shown,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StateSave {
    None,
    Save,
    SaveAs,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum StateEncodingChange {
    None,
    Convert,
    Reopen,
}

struct State {
    path: Option<PathBuf>,
    buffer: RcTextBuffer,

    // A ring buffer of the last 10 errors.
    error_log: [String; 10],
    error_log_index: usize,
    error_log_count: usize,

    wants_save: StateSave,
    save_dir: DisplayablePathBuf,
    save_entries: Option<Vec<DisplayablePathBuf>>,
    save_filename: String, // This could be PathBuf, if `tui` would expose its TextBuffer for editline.

    wants_search: StateSearch,
    search_needle: String,
    search_options: buffer::SearchOptions,

    wants_encoding_change: StateEncodingChange,
    wants_about: bool,
    wants_exit: bool,
    exit: bool,
}

impl State {
    fn new() -> apperr::Result<Self> {
        let path = std::env::args_os()
            .nth(1)
            .and_then(|p| if p == "-" { None } else { Some(p) })
            .map(PathBuf::from);
        let save_filename = path
            .as_ref()
            .and_then(|p| p.file_name())
            .unwrap_or_default()
            .to_string_lossy()
            .into_owned();

        let mut buffer = RcTextBuffer::new(false)?;
        buffer.set_margin_enabled(true);
        buffer.set_ruler(if save_filename == "COMMIT_EDITMSG" {
            Some(72)
        } else {
            None
        });

        Ok(Self {
            path,
            buffer,

            error_log: [const { String::new() }; 10],
            error_log_index: 0,
            error_log_count: 0,

            wants_save: StateSave::None,
            save_dir: DisplayablePathBuf::new(std::env::current_dir()?),
            save_entries: None,
            save_filename,

            wants_search: StateSearch::Hidden,
            search_needle: String::new(),
            search_options: buffer::SearchOptions::default(),

            wants_encoding_change: StateEncodingChange::None,
            wants_about: false,
            wants_exit: false,
            exit: false,
        })
    }
}

impl State {
    fn update_path(&mut self, path: Option<PathBuf>) {
        self.path = path;
    }
}

fn main() -> process::ExitCode {
    if cfg!(debug_assertions) {
        let hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            drop(RestoreModes);
            sys::deinit();
            hook(info);
        }));
    }

    let code = match run() {
        Ok(()) => process::ExitCode::SUCCESS,
        Err(err) => {
            let mut msg = err.message();
            msg.push_str("\r\n");
            sys::write_stdout(&msg);
            process::ExitCode::FAILURE
        }
    };
    sys::deinit();
    code
}

fn run() -> apperr::Result<()> {
    sys::init()?;

    let mut state = State::new()?;
    let mut vt_parser = vt::Parser::new();
    let mut input_parser = input::Parser::new();
    let mut tui = Tui::new();

    if let Some(mut file) = sys::open_stdin_if_redirected() {
        state.buffer.read_file(&mut file, None)?;
        state.buffer.mark_as_dirty();
    } else if let Some(path) = &state.path {
        match file_open(path) {
            Ok(mut file) => {
                state.buffer.read_file(&mut file, None)?;
            }
            Err(apperr::APP_FILE_NOT_FOUND) => {}
            Err(err) => return Err(err),
        }
    }

    query_color_palette(&mut tui, &mut vt_parser);

    // 1049: Alternative Screen Buffer
    //   I put the ASB switch in the beginning, just in case the terminal performs
    //   some additional state tracking beyond the modes we enable/disable.
    // 1002: Cell Motion Mouse Tracking
    // 1006: SGR Mouse Mode
    // 2004: Bracketed Paste Mode
    let _restore_modes = RestoreModes;
    sys::write_stdout("\x1b[?1049h\x1b[?1002;1006;2004h");
    sys::inject_window_size_into_stdin();

    loop {
        let Some(input) = sys::read_stdin(vt_parser.read_timeout()) else {
            break;
        };

        #[cfg(feature = "debug-latency")]
        let time_beg = std::time::Instant::now();
        #[cfg(feature = "debug-latency")]
        let mut passes = 0usize;

        // TODO: lifetime
        let vt_iter = vt_parser.parse(trust_me_bro::this_lifetime_change_is_totally_safe(&input));
        let mut input_iter = input_parser.parse(vt_iter);

        // Process all input.
        while let Some(ui_input) = input_iter.next() {
            let mut ctx = tui.create_context(Some(ui_input));
            draw(&mut ctx, &mut state);

            #[cfg(feature = "debug-latency")]
            {
                passes += 1;
            }
        }

        // Continue rendering until the layout has settled.
        // This can take >1 frame, if the input focus is tossed between different controls.
        while tui.needs_settling() {
            let mut ctx = tui.create_context(None);
            draw(&mut ctx, &mut state);

            #[cfg(feature = "debug-layout")]
            state.buffer.debug_replace_everything(&tui.debug_layout());

            #[cfg(feature = "debug-latency")]
            {
                passes += 1;
            }
        }

        if state.exit {
            break;
        }

        #[cfg(feature = "debug-latency")]
        {
            let mut output = tui.render();

            // Print the number of passes and latency in the top right corner.
            let time_end = std::time::Instant::now();
            let status = time_end - time_beg;
            let status = format!("{}x {:.3}μs", passes, status.as_nanos() as f64 / 1000.0);

            // "μs" is 3 bytes and 2 columns.
            let cols = status.len() as i32 - 3 + 2;
            let x = tui.size().width - cols;

            // To avoid moving the cursor, push and pop it onto the VT cursor stack.
            _ = write!(output, "\x1b7\x1b[1;{}H{}\x1b8", x + 1, status);

            sys::write_stdout(&output);
        }
        #[cfg(not(feature = "debug-latency"))]
        {
            let output = tui.render();
            sys::write_stdout(&output);
        }
    }

    Ok(())
}

fn draw(ctx: &mut Context, state: &mut State) {
    draw_menubar(ctx, state);
    if state.wants_search != StateSearch::Hidden {
        draw_search(ctx, state);
    }
    draw_editor(ctx, state);
    draw_statusbar(ctx, state);

    if state.wants_save != StateSave::None
        && (state.wants_save == StateSave::SaveAs || state.path.is_none())
    {
        draw_dialog_saveas(ctx, state);
    }

    if state.wants_save == StateSave::Save {
        draw_handle_save(state, None);
    }

    if state.wants_encoding_change != StateEncodingChange::None {
        draw_dialog_encoding_change(ctx, state);
    }

    // If the user presses "Save" on the exit dialog we'll possible show a SaveAs dialog.
    // The exit dialog should then get hidden.
    if state.wants_exit && state.wants_save == StateSave::None {
        draw_handle_wants_exit(ctx, state);
    }

    if state.wants_about {
        draw_dialog_about(ctx, state);
    }

    if state.error_log_count != 0 {
        draw_error_log(ctx, state);
    }

    // Shortcuts that are not handled as part of the textarea.
    if ctx.consume_shortcut(kbmod::CTRL | vk::S) {
        state.wants_save = StateSave::Save;
    }
    if ctx.consume_shortcut(kbmod::CTRL_SHIFT | vk::S) {
        state.wants_save = StateSave::SaveAs;
    }
    if ctx.consume_shortcut(kbmod::CTRL | vk::Q) {
        state.wants_exit = true;
    }
    if ctx.consume_shortcut(kbmod::CTRL | vk::F) {
        state.wants_search = StateSearch::Focus;
    }
}

fn draw_menubar(ctx: &mut Context, state: &mut State) {
    ctx.menubar_begin();
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::BrightBlue));
    {
        if ctx.menubar_menu_begin(loc(LocId::File), 'F') {
            draw_menu_file(ctx, state);
        }
        if ctx.menubar_menu_begin(loc(LocId::Edit), 'E') {
            draw_menu_edit(ctx, state);
        }
        if ctx.menubar_menu_begin(loc(LocId::View), 'V') {
            draw_menu_view(ctx, state);
        }
        if ctx.menubar_menu_begin(loc(LocId::Help), 'H') {
            draw_menu_help(ctx, state);
        }
    }
    ctx.menubar_end();
}

fn draw_menu_file(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_item(loc(LocId::FileSave), 'S', kbmod::CTRL | vk::S) {
        state.wants_save = StateSave::Save;
    }
    if ctx.menubar_menu_item(loc(LocId::FileSaveAs), 'A', vk::NULL) {
        state.wants_save = StateSave::SaveAs;
    }
    if ctx.menubar_menu_item(loc(LocId::FileExit), 'X', kbmod::CTRL | vk::Q) {
        state.wants_exit = true;
    }
    ctx.menubar_menu_end();
}

fn draw_menu_edit(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_item(loc(LocId::EditUndo), 'U', kbmod::CTRL | vk::Z) {
        state.buffer.undo();
    }
    if ctx.menubar_menu_item(loc(LocId::EditRedo), 'R', kbmod::CTRL | vk::Y) {
        state.buffer.redo();
    }
    if ctx.menubar_menu_item(loc(LocId::EditCut), 'T', kbmod::CTRL | vk::X) {
        ctx.set_clipboard(state.buffer.extract_selection(true));
    }
    if ctx.menubar_menu_item(loc(LocId::EditCopy), 'C', kbmod::CTRL | vk::C) {
        ctx.set_clipboard(state.buffer.extract_selection(false));
    }
    if ctx.menubar_menu_item(loc(LocId::EditPaste), 'P', kbmod::CTRL | vk::V) {
        state.buffer.write(ctx.get_clipboard());
    }
    if ctx.menubar_menu_item(loc(LocId::EditFind), 'F', kbmod::CTRL | vk::F) {
        state.wants_search = StateSearch::Focus;
    }
    ctx.menubar_menu_end();
}

fn draw_menu_view(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_item(loc(LocId::ViewWordWrap), 'W', kbmod::ALT | vk::Z) {
        state.buffer.toggle_word_wrap();
    }
    ctx.menubar_menu_end();
}

fn draw_menu_help(ctx: &mut Context, state: &mut State) {
    if ctx.menubar_menu_item(loc(LocId::HelpAbout), 'A', vk::NULL) {
        state.wants_about = true;
    }
    ctx.menubar_menu_end();
}

fn draw_search(ctx: &mut Context, state: &mut State) {
    ctx.block_begin("search");
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::White));
    {
        if ctx.contains_focus() && ctx.consume_shortcut(vk::ESCAPE) {
            state.wants_search = StateSearch::Hidden;
        }

        ctx.table_begin("needle");
        ctx.table_set_cell_gap(Size {
            width: 1,
            height: 0,
        });
        {
            ctx.table_next_row();

            ctx.label("label", Overflow::Clip, loc(LocId::SearchLabel));

            ctx.editline("input", &mut state.search_needle);
            ctx.attr_intrinsic_size(Size {
                width: COORD_TYPE_SAFE_MAX,
                height: 1,
            });
            if state.wants_search == StateSearch::Focus {
                state.wants_search = StateSearch::Shown;
                ctx.steal_focus();
            }
            if ctx.is_focused() && ctx.consume_shortcut(vk::RETURN) {
                if let Err(err) = state
                    .buffer
                    .find_and_select(&state.search_needle, state.search_options)
                {
                    error_log_add(state, err);
                }
            }
        }
        ctx.table_end();

        ctx.table_begin("options");
        ctx.table_set_cell_gap(Size {
            width: 2,
            height: 0,
        });
        {
            ctx.table_next_row();

            if ctx.button("close", Overflow::Clip, "Close") {
                state.wants_search = StateSearch::Hidden;
            }

            ctx.checkbox(
                "match-case",
                Overflow::Clip,
                loc(LocId::SearchMatchCase),
                &mut state.search_options.match_case,
            );
            ctx.checkbox(
                "whole-word",
                Overflow::Clip,
                loc(LocId::SearchWholeWord),
                &mut state.search_options.whole_word,
            );
            ctx.checkbox(
                "use-regex",
                Overflow::Clip,
                loc(LocId::SearchUseRegex),
                &mut state.search_options.use_regex,
            );
        }
        ctx.table_end();
    }
    ctx.block_end();
}

fn draw_editor(ctx: &mut Context, state: &mut State) {
    let size = ctx.size();
    // TODO: The layout code should be able to just figure out the height on its own.
    let mut height_reduction = 2;
    if state.wants_search != StateSearch::Hidden {
        height_reduction += 2;
    }
    ctx.textarea("textarea", state.buffer.clone());
    ctx.inherit_focus();
    ctx.attr_intrinsic_size(Size {
        width: 0,
        height: size.height - height_reduction,
    });
}

fn draw_statusbar(ctx: &mut Context, state: &mut State) {
    ctx.table_begin("statusbar");
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::BrightBlue));
    ctx.table_set_cell_gap(Size {
        width: 2,
        height: 0,
    });
    ctx.attr_padding(Rect::two(0, 1));
    {
        ctx.table_next_row();

        if ctx.button(
            "newline",
            Overflow::Clip,
            if state.buffer.is_crlf() { "CRLF" } else { "LF" },
        ) {
            let is_crlf = state.buffer.is_crlf();
            state.buffer.normalize_newlines(!is_crlf);
            ctx.toss_focus_up();
        }

        ctx.button("encoding", Overflow::Clip, state.buffer.encoding());
        if ctx.contains_focus() {
            if state.path.is_some() {
                ctx.block_begin("encoding-picker");
                ctx.attr_float(FloatSpec {
                    anchor: Anchor::Last,
                    gravity_x: 0.0,
                    gravity_y: 1.0,
                    offset_x: 0,
                    offset_y: 0,
                });
                ctx.attr_background_rgba(ctx.indexed(IndexedColor::White));
                ctx.attr_border();
                ctx.attr_padding(Rect::two(0, 1));
                {
                    if ctx.button("reopen", Overflow::Clip, loc(LocId::EncodingReopen)) {
                        state.wants_encoding_change = StateEncodingChange::Reopen;
                    }
                    ctx.focus_on_first_present();

                    if ctx.button("convert", Overflow::Clip, loc(LocId::EncodingConvert)) {
                        state.wants_encoding_change = StateEncodingChange::Convert;
                    }
                }
                ctx.block_end();
            } else {
                // Can't reopen a file that doesn't exist.
                state.wants_encoding_change = StateEncodingChange::Convert;
            }
        }

        ctx.button(
            "indentation",
            Overflow::Clip,
            &format!(
                "{}:{}",
                loc(if state.buffer.indent_with_tabs() {
                    LocId::IndentationTabs
                } else {
                    LocId::IndentationSpaces
                }),
                state.buffer.tab_size(),
            ),
        );
        if ctx.contains_focus() {
            ctx.table_begin("indentation-picker");
            ctx.attr_float(FloatSpec {
                anchor: Anchor::Last,
                gravity_x: 0.0,
                gravity_y: 1.0,
                offset_x: 0,
                offset_y: 0,
            });
            ctx.attr_background_rgba(ctx.indexed(IndexedColor::White));
            ctx.attr_border();
            ctx.attr_padding(Rect::two(0, 1));
            ctx.table_set_cell_gap(Size {
                width: 1,
                height: 0,
            });
            {
                ctx.table_next_row();

                ctx.block_begin("indentation-type");
                {
                    if ctx.button("tabs", Overflow::Clip, loc(LocId::IndentationTabs)) {
                        state.buffer.set_indent_with_tabs(true);
                    }
                    if state.buffer.indent_with_tabs() {
                        ctx.attr_background_rgba(ctx.indexed(IndexedColor::Blue));
                    }
                    ctx.attr_padding(Rect::two(0, 2));
                    ctx.focus_on_first_present();

                    if ctx.button("spaces", Overflow::Clip, loc(LocId::IndentationSpaces)) {
                        state.buffer.set_indent_with_tabs(false);
                    }
                    if !state.buffer.indent_with_tabs() {
                        ctx.attr_background_rgba(ctx.indexed(IndexedColor::Blue));
                    }
                    ctx.attr_padding(Rect::two(0, 2));
                }
                ctx.block_end();

                ctx.block_begin("indentation-width");
                {
                    for width in 1u8..=8 {
                        let ch = [b'0' + width];
                        let label = unsafe { std::str::from_utf8_unchecked(&ch) };

                        ctx.next_block_id_mixin(width as u64);
                        if ctx.button("width", Overflow::Clip, label) {
                            state.buffer.set_tab_size(width as i32);
                        }
                        if state.buffer.tab_size() == width as i32 {
                            ctx.attr_background_rgba(ctx.indexed(IndexedColor::Blue));
                        }
                        ctx.attr_padding(Rect::two(0, 2));
                    }
                }
                ctx.block_end();
            }
            ctx.table_end();
        }

        ctx.label(
            "location",
            Overflow::Clip,
            &format!(
                "{}:{}",
                state.buffer.get_cursor_logical_pos().y + 1,
                state.buffer.get_cursor_logical_pos().x + 1
            ),
        );

        #[cfg(any(feature = "debug-layout", feature = "debug-latency"))]
        ctx.label(
            "stats",
            Overflow::Clip,
            &format!(
                "{}/{}",
                state.buffer.get_logical_line_count(),
                state.buffer.get_visual_line_count(),
            ),
        );

        if state.buffer.is_overtype() && ctx.button("overtype", Overflow::Clip, "OVR") {
            state.buffer.set_overtype(false);
        }
    }
    ctx.table_end();
}

fn draw_dialog_saveas(ctx: &mut Context, state: &mut State) {
    if state.wants_save == StateSave::Save && state.path.is_none() {
        state.wants_save = StateSave::SaveAs;
    }

    let width = (ctx.size().width - 20).max(10);
    let height = (ctx.size().height - 10).max(10);

    ctx.modal_begin("saveas", loc(LocId::SaveAsDialogTitle));
    ctx.attr_intrinsic_size(Size { width, height });
    {
        ctx.label("path", Overflow::TruncateMiddle, state.save_dir.as_str());

        {
            if state.save_entries.is_none() {
                draw_refresh_save_entries(state);
            }

            let files = state.save_entries.as_ref().unwrap();
            let mut change_dir = None;

            ctx.scrollarea_begin(
                "directory",
                Size {
                    width: 0,
                    // -1 for the label (top)
                    // -1 for the label (bottom)
                    // -1 for the editline (bottom)
                    height: height - 3,
                },
            );
            ctx.next_block_id_mixin(state.save_dir.as_str().len() as u64);
            ctx.list_begin("files");
            for (i, entry) in files.iter().enumerate() {
                if ctx.list_item(Overflow::TruncateMiddle, entry.as_str()) {
                    let str = entry.as_str();
                    if str.ends_with('/') || str == ".." {
                        change_dir = Some(entry);
                    } else if state.save_filename != str {
                        state.save_filename = str.to_string();
                    } else {
                        // Treat clicking twice on an item as confirmation to save it.
                        // TODO: This feels a bit weird if the user clicks on a `save_filename`-named item,
                        // because it skips the double-click confirmation.
                        state.wants_save = StateSave::Save;
                    }
                }
                if i == 0 {
                    ctx.focus_on_first_present();
                }
            }
            ctx.list_end();
            ctx.scrollarea_end();

            if let Some(entry) = change_dir {
                let mut path = mem::take(&mut state.save_dir).take();

                if entry.as_str() == ".." {
                    path.pop();
                } else {
                    // `entry` is a directory name with trailing "/",
                    // but we don't want the "/" in the path (it would look ugly).
                    let entry_str = entry.as_str();
                    path.push(&entry_str[..entry_str.len() - 1]);
                }

                state.save_dir = DisplayablePathBuf::new(path);
                state.save_entries = None;
                ctx.scrollarea_scroll_to(Point { x: 0, y: 0 });
            }
        }

        ctx.label(
            "filename-label",
            Overflow::Clip,
            &loc(LocId::SaveAsDialogFilenameLabel),
        );

        ctx.editline("filename", &mut state.save_filename);
        ctx.focus_on_first_present();
        ctx.inherit_focus();
        if ctx.is_focused() && ctx.consume_shortcut(vk::RETURN) {
            state.wants_save = StateSave::Save;
        }

        if state.wants_save == StateSave::Save && !state.save_filename.is_empty() {
            let path = Some(state.save_dir.as_path().join(&state.save_filename));
            // Only update the path if the save was successful.
            if draw_handle_save(state, path.as_ref()) {
                state.path = path;
            }
        }
    }
    if ctx.modal_end() {
        state.wants_save = StateSave::None;
    }
}

fn draw_refresh_save_entries(state: &mut State) {
    let dir = state.save_dir.as_path();
    let mut files = Vec::new();

    if dir.parent().is_some() {
        files.push(DisplayablePathBuf::from(".."));
    }

    if let Ok(iter) = std::fs::read_dir(dir) {
        for entry in iter.flatten() {
            if let Ok(metadata) = entry.metadata() {
                let mut name = entry.file_name();
                if metadata.is_dir() {
                    name.push("/");
                }
                files.push(DisplayablePathBuf::from(name));
            }
        }
    }

    // Sort directories first, then by name, case-insensitive.
    files[1..].sort_by(|a, b| {
        let a = a.as_bytes();
        let b = b.as_bytes();

        let a_is_dir = a.last() == Some(&b'/');
        let b_is_dir = b.last() == Some(&b'/');

        match b_is_dir.cmp(&a_is_dir) {
            cmp::Ordering::Equal => icu::compare_strings(a, b),
            other => other,
        }
    });

    state.save_entries = Some(files);
}

fn draw_handle_save(state: &mut State, path: Option<&PathBuf>) -> bool {
    if let Some(path) = path.or(state.path.as_ref()) {
        if let Err(err) = state.buffer.write_file(path) {
            error_log_add(state, err);
            return false;
        }
    }
    state.wants_save = StateSave::None;
    true
}

fn draw_dialog_encoding_change(ctx: &mut Context, state: &mut State) {
    let reopen = state.wants_encoding_change == StateEncodingChange::Reopen;
    let width = (ctx.size().width - 20).max(10);
    let height = (ctx.size().height - 10).max(10);

    ctx.modal_begin(
        "encode",
        if reopen {
            loc(LocId::EncodingReopen)
        } else {
            loc(LocId::EncodingConvert)
        },
    );
    {
        ctx.scrollarea_begin("scrollarea", Size { width, height });
        {
            let encodings = icu::get_available_encodings();

            ctx.list_begin("encodings");
            for encoding in encodings {
                if ctx.list_item(Overflow::Clip, encoding.as_str()) {
                    state.wants_encoding_change = StateEncodingChange::None;
                    if reopen && state.path.is_some() {
                        if state.buffer.is_dirty() {
                            if let Some(path) = &state.path {
                                if let Err(err) = state.buffer.write_file(path) {
                                    error_log_add(state, err);
                                }
                            }
                        }

                        if let Some(path) = &mut state.path {
                            if let Err(err) = file_open(path).and_then(|mut file| {
                                state.buffer.read_file(&mut file, Some(encoding.as_str()))
                            }) {
                                error_log_add(state, err);
                            }
                        }
                    } else {
                        state.buffer.set_encoding(encoding.as_str());
                    }
                }
            }
            ctx.list_end();
        }
        ctx.scrollarea_end();
    }
    if ctx.modal_end() {
        state.wants_encoding_change = StateEncodingChange::None;
    }
}

fn draw_handle_wants_exit(ctx: &mut Context, state: &mut State) {
    if !state.buffer.is_dirty() {
        state.exit = true;
        return;
    }

    ctx.modal_begin("unsaved-changes", loc(LocId::UnsavedChangesDialogTitle));
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
    {
        ctx.label(
            "description",
            Overflow::Clip,
            loc(LocId::UnsavedChangesDialogDescription),
        );
        ctx.attr_padding(Rect::three(1, 2, 1));

        ctx.table_begin("choices");
        ctx.attr_padding(Rect::three(0, 2, 1));
        ctx.attr_alignment(Alignment::Center);
        ctx.table_set_cell_gap(Size {
            width: 2,
            height: 0,
        });
        {
            ctx.table_next_row();

            if ctx.button("yes", Overflow::Clip, loc(LocId::UnsavedChangesDialogYes)) {
                state.wants_save = StateSave::Save;
            }
            if ctx.button("no", Overflow::Clip, loc(LocId::UnsavedChangesDialogNo)) {
                state.exit = true;
            }
            if ctx.button(
                "cancel",
                Overflow::Clip,
                loc(LocId::UnsavedChangesDialogCancel),
            ) {
                state.wants_exit = false;
            }
            ctx.focus_on_first_present();
        }
        ctx.table_end();
    }

    if ctx.modal_end() {
        state.wants_exit = false;
    }
}

fn draw_dialog_about(ctx: &mut Context, state: &mut State) {
    ctx.modal_begin("about", loc(LocId::AboutDialogTitle));
    {
        ctx.block_begin("content");
        ctx.attr_padding(Rect::three(1, 2, 1));
        {
            ctx.label(
                "description",
                Overflow::TruncateTail,
                loc(LocId::AboutDialogDescription),
            );
            ctx.attr_alignment(Alignment::Center);
            ctx.label(
                "version",
                Overflow::TruncateHead,
                &format!(
                    "{}{}",
                    loc(LocId::AboutDialogVersion),
                    env!("CARGO_PKG_VERSION")
                ),
            );
            ctx.attr_alignment(Alignment::Center);
            ctx.label(
                "copyright",
                Overflow::TruncateTail,
                "Copyright (c) Microsoft Corp 2025",
            );
            ctx.attr_alignment(Alignment::Center);
        }
        ctx.block_end();
    }
    if ctx.modal_end() {
        state.wants_about = false;
    }
}

fn draw_error_log(ctx: &mut Context, state: &mut State) {
    ctx.modal_begin("errors", "Error");
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::Red));
    {
        let off = state.error_log_index + state.error_log.len() - state.error_log_count;
        for i in 0..state.error_log_count {
            let idx = (off + i) % state.error_log.len();
            let msg = &state.error_log[idx][..];
            if !msg.is_empty() {
                ctx.next_block_id_mixin(i as u64);
                ctx.label("error", Overflow::TruncateTail, msg);
                ctx.attr_padding(Rect::three(if i == 0 { 1 } else { 0 }, 2, 1));
            }
        }

        if ctx.button("ok", Overflow::Clip, "Ok") {
            state.error_log_count = 0;
        }
        ctx.attr_padding(Rect::three(1, 2, 1));
        ctx.focus_on_first_present();
    }
    if ctx.modal_end() {
        state.error_log_count = 0;
    }
}

fn error_log_add(state: &mut State, err: apperr::Error) {
    let msg = err.message();
    if !msg.is_empty() {
        state.error_log[state.error_log_index] = msg;
        state.error_log_index = (state.error_log_index + 1) % state.error_log.len();
        state.error_log_count = cmp::min(state.error_log_count + 1, state.error_log.len());
    }
}

fn file_open(path: &Path) -> apperr::Result<File> {
    File::open(path).map_err(apperr::Error::from)
}

fn query_color_palette(tui: &mut Tui, vt_parser: &mut vt::Parser) {
    let mut indexed_colors = framebuffer::DEFAULT_THEME;

    sys::write_stdout(concat!(
        // OSC 4 color table requests for indices 0 through 15 (base colors).
        "\x1b]4;0;?;1;?;2;?;3;?;4;?;5;?;6;?;7;?\x07",
        "\x1b]4;8;?;9;?;10;?;11;?;12;?;13;?;14;?;15;?\x07",
        // OSC 10 and 11 queries for the current foreground and background colors.
        "\x1b]10;?\x07\x1b]11;?\x07",
        // CSI c reports the terminal capabilities.
        // It also helps us to detect the end of the responses, because not all
        // terminals support the OSC queries, but all of them support CSI c.
        "\x1b[c",
    ));

    let mut done = false;
    let mut osc_buffer = String::new();

    while !done {
        let Some(input) = sys::read_stdin(vt_parser.read_timeout()) else {
            break;
        };

        let mut vt_stream = vt_parser.parse(&input);
        while let Some(token) = vt_stream.next() {
            match token {
                Token::Csi(state) if state.final_byte == 'c' => done = true,
                Token::Osc { mut data, partial } => {
                    if partial {
                        osc_buffer.push_str(data);
                        continue;
                    }
                    if !osc_buffer.is_empty() {
                        osc_buffer.push_str(data);
                        data = &osc_buffer;
                    }

                    let mut splits = data.split_terminator(';');

                    let color = match splits.next().unwrap_or("") {
                        // The response is `4;<color>;rgb:<r>/<g>/<b>`.
                        "4" => match splits.next().unwrap_or("").parse::<usize>() {
                            Ok(val) if val < 16 => &mut indexed_colors[val],
                            _ => continue,
                        },
                        // The response is `10;rgb:<r>/<g>/<b>`.
                        "10" => &mut indexed_colors[IndexedColor::DefaultForeground as usize],
                        // The response is `11;rgb:<r>/<g>/<b>`.
                        "11" => &mut indexed_colors[IndexedColor::DefaultBackground as usize],
                        _ => continue,
                    };

                    let color_param = splits.next().unwrap_or("");
                    if !color_param.starts_with("rgb:") {
                        continue;
                    }

                    let mut iter = color_param[4..].split_terminator('/');
                    let rgb_parts = [(); 3].map(|_| iter.next().unwrap_or("0"));
                    let mut rgb = 0;

                    for part in rgb_parts {
                        if part.len() == 2 || part.len() == 4 {
                            let Ok(mut val) = usize::from_str_radix(part, 16) else {
                                continue;
                            };
                            if part.len() == 4 {
                                val = (val * 0xff + 0x80) / 0xffff;
                            }
                            rgb = (rgb >> 8) | ((val as u32) << 16);
                        }
                    }

                    *color = rgb | 0xff000000;
                    osc_buffer.clear();
                }
                _ => {}
            }
        }
    }

    tui.setup_indexed_colors(indexed_colors);
}
