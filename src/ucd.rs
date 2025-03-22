use crate::helpers::{CoordType, Point};
use crate::memchr::{memchr2, memrchr2};
use crate::ucd_gen::*;
use crate::utf8::Utf8Chars;
use std::cmp::Ordering;

pub trait Document {
    fn read_backward(&self, off: usize) -> &[u8];
    fn read_forward(&self, off: usize) -> &[u8];
}

impl Document for &[u8] {
    fn read_backward(&self, off: usize) -> &[u8] {
        let s = *self;
        &s[..off.min(s.len())]
    }

    fn read_forward(&self, off: usize) -> &[u8] {
        let s = *self;
        &s[off.min(s.len())..]
    }
}

#[derive(Clone, Copy, Default)]
pub struct UcdCursor {
    /// Offset in bytes within the buffer.
    pub offset: usize,
    /// Position in the buffer in lines (.y) and grapheme clusters (.x).
    /// Line wrapping has NO influence on this.
    pub logical_pos: Point,
    /// Position in the buffer in laid out rows (.y) and columns (.x).
    /// Line wrapping has an influence on this.
    pub visual_pos: Point,
    /// Horizontal position in visual columns.
    /// Line wrapping has NO influence on this and if word wrap is disabled,
    /// it's identical to `visual_pos.x`. This is useful for calculating tab widths.
    pub column: CoordType,
}

pub struct WrapOpportunity {
    absolute_offset: usize,
    offset_next_cluster: usize,
    props_next_cluster: usize,
    logical_pos_x: CoordType,
}

pub struct MeasurementConfig<'doc> {
    buffer: &'doc dyn Document,
    tab_size: CoordType,
    word_wrap_column: CoordType,
    cursor: UcdCursor,
}

impl<'doc> MeasurementConfig<'doc> {
    pub fn new(buffer: &'doc dyn Document) -> Self {
        Self {
            buffer,
            tab_size: 8,
            word_wrap_column: CoordType::MAX,
            cursor: UcdCursor::default(),
        }
    }

    pub fn with_tab_size(mut self, tab_size: CoordType) -> Self {
        self.tab_size = tab_size;
        self
    }

    pub fn with_word_wrap_column(mut self, word_wrap_column: CoordType) -> Self {
        self.word_wrap_column = word_wrap_column;
        self
    }

    pub fn with_cursor(mut self, cursor: UcdCursor) -> Self {
        self.cursor = cursor;
        self
    }

    pub fn goto_offset(&mut self, offset: usize) -> UcdCursor {
        self.cursor = Self::measure_forward(
            self.tab_size,
            self.word_wrap_column,
            offset,
            Point::MAX,
            Point::MAX,
            self.cursor,
            self.buffer,
        );
        self.cursor
    }

    pub fn goto_logical(&mut self, logical_target: Point) -> UcdCursor {
        self.cursor = Self::measure_forward(
            self.tab_size,
            self.word_wrap_column,
            usize::MAX,
            logical_target,
            Point::MAX,
            self.cursor,
            self.buffer,
        );
        self.cursor
    }

    pub fn goto_visual(&mut self, visual_target: Point) -> UcdCursor {
        self.cursor = Self::measure_forward(
            self.tab_size,
            self.word_wrap_column,
            usize::MAX,
            Point::MAX,
            visual_target,
            self.cursor,
            self.buffer,
        );
        self.cursor
    }

    pub fn cursor(&self) -> UcdCursor {
        self.cursor
    }

    fn measure_forward(
        tab_size: CoordType,
        word_wrap_column: CoordType,
        offset_target: usize,
        logical_target: Point,
        visual_target: Point,
        cursor: UcdCursor,
        buffer: &dyn Document,
    ) -> UcdCursor {
        if cursor.logical_pos >= logical_target || cursor.visual_pos >= visual_target {
            return cursor;
        }

        let mut wrap: Option<WrapOpportunity> = None;
        let mut hit: Option<UcdCursor> = None;
        let mut absolute_offset = cursor.offset;
        let mut logical_pos_x = cursor.logical_pos.x;
        let mut logical_pos_y = cursor.logical_pos.y;
        let mut visual_pos_x = cursor.visual_pos.x;
        let mut visual_pos_y = cursor.visual_pos.y;
        let mut column = cursor.column;
        let (mut offset_target_x, mut logical_target_x, mut visual_target_x) = Self::recalc_target(
            offset_target,
            logical_target,
            visual_target,
            logical_pos_y,
            visual_pos_y,
        );

        'outer: loop {
            let chunk = buffer.read_forward(absolute_offset);
            let chunk_beg = absolute_offset;
            let chunk_end = absolute_offset + chunk.len();
            let mut it = Utf8Chars::new(chunk, 0);
            let Some(mut ch) = it.next() else {
                break;
            };

            let mut props_next_cluster = ucd_grapheme_cluster_lookup(ch);

            loop {
                if absolute_offset >= chunk_end {
                    break;
                }
                if absolute_offset >= offset_target_x
                    || logical_pos_x >= logical_target_x
                    || visual_pos_x >= visual_target_x
                {
                    if wrap.is_none() {
                        break 'outer;
                    }

                    hit = Some(UcdCursor {
                        offset: absolute_offset,
                        logical_pos: Point {
                            x: logical_pos_x,
                            y: logical_pos_y,
                        },
                        visual_pos: Point {
                            x: visual_pos_x,
                            y: visual_pos_y,
                        },
                        column,
                    });
                    // Prevent hits on the same line until we encounter a line wrap or explicit newline.
                    offset_target_x = usize::MAX;
                    logical_target_x = CoordType::MAX;
                    visual_target_x = CoordType::MAX;
                }

                let props_current_cluster = props_next_cluster;
                let is_tab = ch == '\t';
                let mut offset_next_cluster;
                let mut width = 0;
                let mut state = 0;

                // Figure out the length and width of the rest of the grapheme cluster.
                loop {
                    offset_next_cluster = it.offset();
                    width += ucd_grapheme_cluster_character_width(props_next_cluster) as CoordType;

                    let Some(ch_next) = it.next() else {
                        break;
                    };

                    ch = ch_next;
                    let props_trail = ucd_grapheme_cluster_lookup(ch);
                    state = ucd_grapheme_cluster_joins(state, props_next_cluster, props_trail);
                    props_next_cluster = props_trail;

                    if ucd_grapheme_cluster_joins_done(state) {
                        break;
                    }
                }

                let offset_next_cluster = chunk_beg + offset_next_cluster;

                if is_tab {
                    // Tabs require special handling because they can have a variable width.
                    width = tab_size - (column % tab_size);
                } else {
                    width = width.min(2);
                }

                // Hard wrap: Both the logical and visual position advance by one line.
                if ucd_grapheme_cluster_is_newline(props_current_cluster) {
                    // Don't cross the newline if the target is on this line.
                    // E.g. if the callers asks for column 100 on a 10 column line,
                    // we'll return with the cursor set to column 10.
                    if logical_pos_y >= logical_target.y || visual_pos_y >= visual_target.y {
                        break 'outer;
                    }

                    logical_pos_x = 0;
                    logical_pos_y += 1;
                    visual_pos_x = 0;
                    visual_pos_y += 1;
                    column = 0;
                    // We moved the logical/visual pos past the newline,
                    // so we also need to move the offset past it.
                    absolute_offset = offset_next_cluster;
                    (offset_target_x, logical_target_x, visual_target_x) = Self::recalc_target(
                        offset_target,
                        logical_target,
                        visual_target,
                        logical_pos_y,
                        visual_pos_y,
                    );
                    continue;
                }

                // Line/word-wrap handling.
                if word_wrap_column != CoordType::MAX && visual_pos_x + width > word_wrap_column {
                    // Reset to the last break opportunity, if there was any.
                    if let Some(ref w) = wrap {
                        absolute_offset = w.absolute_offset;
                        it.seek(w.offset_next_cluster);
                        props_next_cluster = w.props_next_cluster;
                        logical_pos_x = w.logical_pos_x;
                    }

                    // Wrap!
                    visual_pos_x = 0;
                    visual_pos_y += 1;
                    (offset_target_x, logical_target_x, visual_target_x) = Self::recalc_target(
                        offset_target,
                        logical_target,
                        visual_target,
                        logical_pos_y,
                        visual_pos_y,
                    );
                    wrap = None;
                    hit = None;

                    if absolute_offset < chunk_beg {
                        // We've had to reset to a point before this chunk,
                        // so we have to re-read the previous contents.
                        break;
                    }

                    continue;
                }

                // Avoid advancing past the visual target, because `width` can be greater than 1.
                if visual_pos_x + width > visual_target_x {
                    if word_wrap_column == CoordType::MAX || wrap.is_none() {
                        break 'outer;
                    }

                    hit = Some(UcdCursor {
                        offset: absolute_offset,
                        logical_pos: Point {
                            x: logical_pos_x,
                            y: logical_pos_y,
                        },
                        visual_pos: Point {
                            x: visual_pos_x,
                            y: visual_pos_y,
                        },
                        column,
                    });
                    // Prevent hits on the same line until we encounter a line wrap or explicit newline.
                    offset_target_x = usize::MAX;
                    logical_target_x = CoordType::MAX;
                    visual_target_x = CoordType::MAX;
                }

                absolute_offset = offset_next_cluster;
                logical_pos_x += 1;
                visual_pos_x += width;
                column += width;

                if word_wrap_column != CoordType::MAX
                    && !ucd_line_break_joins(props_current_cluster, props_next_cluster)
                {
                    if hit.is_some() {
                        break 'outer;
                    }
                    wrap = Some(WrapOpportunity {
                        absolute_offset,
                        offset_next_cluster: it.offset(),
                        props_next_cluster,
                        logical_pos_x,
                    });
                }
            }
        }

        if visual_pos_x >= word_wrap_column {
            visual_pos_x = 0;
            visual_pos_y += 1;
        }

        if let Some(c) = hit {
            return c;
        }

        UcdCursor {
            offset: absolute_offset,
            logical_pos: Point {
                x: logical_pos_x,
                y: logical_pos_y,
            },
            visual_pos: Point {
                x: visual_pos_x,
                y: visual_pos_y,
            },
            column,
        }
    }

    #[inline]
    fn recalc_target(
        offset_target: usize,
        logical_target: Point,
        visual_target: Point,
        logical_pos_y: CoordType,
        visual_pos_y: CoordType,
    ) -> (usize, CoordType, CoordType) {
        (
            offset_target,
            Self::target_column(logical_target, logical_pos_y),
            Self::target_column(visual_target, visual_pos_y),
        )
    }

    #[inline]
    fn target_column(target: Point, y: CoordType) -> CoordType {
        match y.cmp(&target.y) {
            Ordering::Less => CoordType::MAX,
            Ordering::Equal => target.x,
            Ordering::Greater => 0,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum CharClass {
    Whitespace,
    Newline,
    Separator,
    Word,
}

const fn construct_classifier(seperators: &[u8]) -> [CharClass; 256] {
    let mut classifier = [CharClass::Word; 256];

    classifier[b' ' as usize] = CharClass::Whitespace;
    classifier[b'\t' as usize] = CharClass::Whitespace;
    classifier[b'\n' as usize] = CharClass::Newline;
    classifier[b'\r' as usize] = CharClass::Newline;

    let mut i = 0;
    let len = seperators.len();
    while i < len {
        let ch = seperators[i];
        assert!(ch < 128, "Only ASCII separators are supported.");
        classifier[ch as usize] = CharClass::Separator;
        i += 1;
    }

    classifier
}

const WORD_CLASSIFIER: [CharClass; 256] =
    construct_classifier(br#"`~!@#$%^&*()-=+[{]}\|;:'",.<>/?"#);

/// Finds the next word boundary given a document cursor offset.
/// Returns the offset of the next word boundary.
pub fn word_forward(doc: &dyn Document, offset: usize) -> usize {
    word_navigation(WordForward {
        doc,
        offset,
        chunk: &[],
        chunk_off: 0,
    })
}

/// The backward version of `word_forward`.
pub fn word_backward(doc: &dyn Document, offset: usize) -> usize {
    word_navigation(WordBackward {
        doc,
        offset,
        chunk: &[],
        chunk_off: 0,
    })
}

/// Word navigation implementation. Matches the behavior of VS Code.
fn word_navigation<T: WordNavigation>(mut nav: T) -> usize {
    // First, fill `self.chunk` with at least 1 grapheme.
    nav.read();

    // Skip one newline, if any.
    nav.skip_newline();

    // Skip any whitespace.
    nav.skip_class(CharClass::Whitespace);

    // Skip one word or seperator and take note of the class.
    let class = nav.peek(CharClass::Whitespace);
    if matches!(class, CharClass::Separator | CharClass::Word) {
        nav.next();

        let off = nav.offset();

        // Continue skipping the same class.
        nav.skip_class(class);

        // If the class was a separator and we only moved one character,
        // continue skipping characters of the word class.
        if off == nav.offset() && class == CharClass::Separator {
            nav.skip_class(CharClass::Word);
        }
    }

    nav.offset()
}

trait WordNavigation {
    fn read(&mut self);
    fn skip_newline(&mut self);
    fn skip_class(&mut self, class: CharClass);
    fn peek(&self, default: CharClass) -> CharClass;
    fn next(&mut self);
    fn offset(&self) -> usize;
}

struct WordForward<'a> {
    doc: &'a dyn Document,
    offset: usize,
    chunk: &'a [u8],
    chunk_off: usize,
}

impl WordNavigation for WordForward<'_> {
    fn read(&mut self) {
        self.chunk = self.doc.read_forward(self.offset);
        self.chunk_off = 0;
    }

    fn skip_newline(&mut self) {
        // We can rely on the fact that the document does not split graphemes across chunks.
        // = If there's a newline it's wholly contained in this chunk.
        if self.chunk_off < self.chunk.len() && self.chunk[self.chunk_off] == b'\r' {
            self.chunk_off += 1;
        }
        if self.chunk_off < self.chunk.len() && self.chunk[self.chunk_off] == b'\n' {
            self.chunk_off += 1;
        }
    }

    fn skip_class(&mut self, class: CharClass) {
        while !self.chunk.is_empty() {
            while self.chunk_off < self.chunk.len() {
                if WORD_CLASSIFIER[self.chunk[self.chunk_off] as usize] != class {
                    return;
                }
                self.chunk_off += 1;
            }

            self.offset += self.chunk.len();
            self.chunk = self.doc.read_forward(self.offset);
            self.chunk_off = 0;
        }
    }

    fn peek(&self, default: CharClass) -> CharClass {
        if self.chunk_off < self.chunk.len() {
            WORD_CLASSIFIER[self.chunk[self.chunk_off] as usize]
        } else {
            default
        }
    }

    fn next(&mut self) {
        self.chunk_off += 1;
    }

    fn offset(&self) -> usize {
        self.offset + self.chunk_off
    }
}

struct WordBackward<'a> {
    doc: &'a dyn Document,
    offset: usize,
    chunk: &'a [u8],
    chunk_off: usize,
}

impl WordNavigation for WordBackward<'_> {
    fn read(&mut self) {
        self.chunk = self.doc.read_backward(self.offset);
        self.chunk_off = self.chunk.len();
    }

    fn skip_newline(&mut self) {
        // We can rely on the fact that the document does not split graphemes across chunks.
        // = If there's a newline it's wholly contained in this chunk.
        if self.chunk_off > 0 && self.chunk[self.chunk_off - 1] == b'\r' {
            self.chunk_off -= 1;
        }
        if self.chunk_off > 0 && self.chunk[self.chunk_off - 1] == b'\n' {
            self.chunk_off -= 1;
        }
    }

    fn skip_class(&mut self, class: CharClass) {
        while !self.chunk.is_empty() {
            while self.chunk_off > 0 {
                if WORD_CLASSIFIER[self.chunk[self.chunk_off - 1] as usize] != class {
                    return;
                }
                self.chunk_off -= 1;
            }

            self.offset -= self.chunk.len();
            self.chunk = self.doc.read_backward(self.offset);
            self.chunk_off = self.chunk.len();
        }
    }

    fn peek(&self, default: CharClass) -> CharClass {
        if self.chunk_off > 0 {
            WORD_CLASSIFIER[self.chunk[self.chunk_off - 1] as usize]
        } else {
            default
        }
    }

    fn next(&mut self) {
        self.chunk_off -= 1;
    }

    fn offset(&self) -> usize {
        self.offset - self.chunk.len() + self.chunk_off
    }
}

// TODO: This code could be optimized by replacing memchr with manual line counting.
// If `line_stop` is very far away, we could accumulate newline counts horizontally
// in a AVX2 register (= 32 u8 slots). Then, every 256 bytes we compute the horizontal
// sum via `_mm256_sad_epu8` yielding us the newline count in the last block.
pub fn newlines_forward(
    text: &[u8],
    mut offset: usize,
    mut line: CoordType,
    line_stop: CoordType,
) -> (usize, CoordType) {
    // Leaving the cursor at the beginning of the current line when the limit
    // is 0 makes this function behave identical to ucd_newlines_backward.
    if line >= line_stop {
        return newlines_backward(text, offset, line, line_stop);
    }

    let len = text.len();
    offset = offset.min(len);

    loop {
        offset = memchr2(b'\n', b'\n', text, offset);
        if offset >= len {
            break;
        }

        offset += 1;
        line += 1;
        if line >= line_stop {
            break;
        }
    }

    (offset, line)
}

// Seeks to the start of the given line.
// No matter what parameters are given, it only returns an offset at the start of a line.
// Put differently, even if `line == line_stop`, it'll seek backward to the line start.
pub fn newlines_backward(
    text: &[u8],
    mut offset: usize,
    mut line: CoordType,
    line_stop: CoordType,
) -> (usize, CoordType) {
    offset = offset.min(text.len());

    loop {
        offset = match memrchr2(b'\n', b'\n', text, offset) {
            Some(i) => i,
            None => return (0, line),
        };
        if line <= line_stop {
            // +1: Past the newline, at the start of the current line.
            return (offset + 1, line);
        }
        line -= 1;
    }
}

pub fn strip_newline(mut text: &[u8]) -> &[u8] {
    // Rust generates surprisingly tight assembly for this.
    if text.last() == Some(&b'\n') {
        text = &text[..text.len() - 1];
    }
    if text.last() == Some(&b'\r') {
        text = &text[..text.len() - 1];
    }
    text
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_measure_forward_newline_start() {
        let cursor =
            MeasurementConfig::new(&"foo\nbar".as_bytes()).goto_visual(Point { x: 0, y: 1 });
        assert_eq!(cursor.offset, 4);
        assert_eq!(cursor.logical_pos, Point { x: 0, y: 1 });
        assert_eq!(cursor.visual_pos, Point { x: 0, y: 1 });
    }

    #[test]
    fn test_measure_forward_clipped_wide_char() {
        let cursor = MeasurementConfig::new(&"a😶‍🌫️b".as_bytes()).goto_visual(Point { x: 2, y: 0 });
        assert_eq!(cursor.offset, 1);
        assert_eq!(cursor.logical_pos, Point { x: 1, y: 0 });
        assert_eq!(cursor.visual_pos, Point { x: 1, y: 0 });
    }

    #[test]
    fn test_measure_forward_word_wrap() {
        //   |foo␣  |
        //   |bar␣  |
        //   |baz   |
        let text = "foo bar \nbaz".as_bytes();

        let cursor = MeasurementConfig::new(&text)
            .with_word_wrap_column(6)
            .goto_logical(Point { x: 5, y: 0 });
        assert_eq!(cursor.offset, 5);
        assert_eq!(cursor.logical_pos, Point { x: 5, y: 0 });
        assert_eq!(cursor.visual_pos, Point { x: 1, y: 1 });

        let mut cfg = MeasurementConfig::new(&text).with_word_wrap_column(6);
        let cursor = cfg.goto_visual(Point { x: 5, y: 0 });
        assert_eq!(cursor.offset, 4);
        assert_eq!(cursor.logical_pos, Point { x: 4, y: 0 });
        assert_eq!(cursor.visual_pos, Point { x: 0, y: 1 });

        let cursor = cfg.goto_visual(Point { x: 0, y: 1 });
        assert_eq!(cursor.offset, 4);
        assert_eq!(cursor.logical_pos, Point { x: 4, y: 0 });
        assert_eq!(cursor.visual_pos, Point { x: 0, y: 1 });

        let cursor = cfg.goto_visual(Point { x: 100, y: 1 });
        assert_eq!(cursor.offset, 8);
        assert_eq!(cursor.logical_pos, Point { x: 8, y: 0 });
        assert_eq!(cursor.visual_pos, Point { x: 4, y: 1 });

        let cursor = cfg.goto_visual(Point { x: 0, y: 2 });
        assert_eq!(cursor.offset, 9);
        assert_eq!(cursor.logical_pos, Point { x: 0, y: 1 });
        assert_eq!(cursor.visual_pos, Point { x: 0, y: 2 });

        let cursor = cfg.goto_visual(Point { x: 100, y: 2 });
        assert_eq!(cursor.offset, 12);
        assert_eq!(cursor.logical_pos, Point { x: 3, y: 1 });
        assert_eq!(cursor.visual_pos, Point { x: 3, y: 2 });
    }
}
