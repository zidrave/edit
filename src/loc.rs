use crate::sys;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LocId {
    Ctrl,
    Alt,
    Shift,

    // File menu
    File,
    FileSave,
    FileSaveAs,
    FileExit,

    // Edit menu
    Edit,
    EditUndo,
    EditRedo,
    EditCut,
    EditCopy,
    EditPaste,
    EditFind,
    EditReplace,

    // View menu
    View,
    ViewWordWrap,

    // Help menu
    Help,
    HelpAbout,

    // Exit dialog
    UnsavedChangesDialogTitle,
    UnsavedChangesDialogDescription,
    UnsavedChangesDialogYes,
    UnsavedChangesDialogNo,
    UnsavedChangesDialogCancel,

    // About dialog
    AboutDialogTitle,
    AboutDialogDescription,
    AboutDialogVersion,

    SearchLabel,
    SearchClose,
    SearchMatchCase,
    SearchWholeWord,
    SearchUseRegex,

    EncodingReopen,
    EncodingConvert,

    IndentationTabs,
    IndentationSpaces,

    SaveAsDialogTitle,
    SaveAsDialogPathLabel,
    SaveAsDialogNameLabel,

    Count,
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq)]
enum LangId {
    // Base language. It's always the first one.
    en,

    // Other languages. Sorted alphabetically.
    de,
    es,
    fr,
    it,
    ja,
    ko,
    pt_br,
    ru,
    zh_hans,
    zh_hant,

    Count,
}

#[rustfmt::skip]
const S_LANG_LUT: [[&str; LangId::Count as usize]; LocId::Count as usize] = [
    // Ctrl
    [
        /* en      */ "Ctrl",
        /* de      */ "Strg",
        /* es      */ "Ctrl",
        /* fr      */ "Ctrl",
        /* it      */ "Ctrl",
        /* ja      */ "Ctrl",
        /* ko      */ "Ctrl",
        /* pt_br   */ "Ctrl",
        /* ru      */ "Ctrl",
        /* zh_hans */ "Ctrl",
        /* zh_hant */ "Ctrl",
    ],
    // Alt
    [
        /* en      */ "Alt",
        /* de      */ "Alt",
        /* es      */ "Alt",
        /* fr      */ "Alt",
        /* it      */ "Alt",
        /* ja      */ "Alt",
        /* ko      */ "Alt",
        /* pt_br   */ "Alt",
        /* ru      */ "Alt",
        /* zh_hans */ "Alt",
        /* zh_hant */ "Alt",
    ],
    // Shift
    [
        /* en      */ "Shift",
        /* de      */ "Umschalt",
        /* es      */ "Mayús",
        /* fr      */ "Maj",
        /* it      */ "Maiusc",
        /* ja      */ "Shift",
        /* ko      */ "Shift",
        /* pt_br   */ "Shift",
        /* ru      */ "Shift",
        /* zh_hans */ "Shift",
        /* zh_hant */ "Shift",
    ],

    // File
    [
        /* en      */ "File",
        /* de      */ "Datei",
        /* es      */ "Archivo",
        /* fr      */ "Fichier",
        /* it      */ "File",
        /* ja      */ "ファイル",
        /* ko      */ "파일",
        /* pt_br   */ "Arquivo",
        /* ru      */ "Файл",
        /* zh_hans */ "文件",
        /* zh_hant */ "檔案",
    ],
    // FileSave
    [
        /* en      */ "Save",
        /* de      */ "Speichern",
        /* es      */ "Guardar",
        /* fr      */ "Enregistrer",
        /* it      */ "Salva",
        /* ja      */ "保存",
        /* ko      */ "저장",
        /* pt_br   */ "Salvar",
        /* ru      */ "Сохранить",
        /* zh_hans */ "保存",
        /* zh_hant */ "儲存",
    ],
    // FileSaveAs
    // NOTE: Exact same translation as SaveAsDialogTitle, and both should be kept in sync.
    [
        /* en      */ "Save As…",
        /* de      */ "Speichern unter…",
        /* es      */ "Guardar como…",
        /* fr      */ "Enregistrer sous…",
        /* it      */ "Salva come…",
        /* ja      */ "名前を付けて保存…",
        /* ko      */ "다른 이름으로 저장…",
        /* pt_br   */ "Salvar como…",
        /* ru      */ "Сохранить как…",
        /* zh_hans */ "另存为…",
        /* zh_hant */ "另存新檔…",
    ],
    // FileExit
    [
        /* en      */ "Exit",
        /* de      */ "Beenden",
        /* es      */ "Salir",
        /* fr      */ "Quitter",
        /* it      */ "Esci",
        /* ja      */ "終了",
        /* ko      */ "종료",
        /* pt_br   */ "Sair",
        /* ru      */ "Выход",
        /* zh_hans */ "退出",
        /* zh_hant */ "退出",
    ],

    // Edit
    [
        /* en      */ "Edit",
        /* de      */ "Bearbeiten",
        /* es      */ "Editar",
        /* fr      */ "Éditer",
        /* it      */ "Modifica",
        /* ja      */ "編集",
        /* ko      */ "편집",
        /* pt_br   */ "Editar",
        /* ru      */ "Правка",
        /* zh_hans */ "编辑",
        /* zh_hant */ "編輯",
    ],
    // EditUndo
    [
        /* en      */ "Undo",
        /* de      */ "Rückgängig",
        /* es      */ "Deshacer",
        /* fr      */ "Annuler",
        /* it      */ "Annulla",
        /* ja      */ "元に戻す",
        /* ko      */ "실행 취소",
        /* pt_br   */ "Desfazer",
        /* ru      */ "Отменить",
        /* zh_hans */ "撤销",
        /* zh_hant */ "復原",
    ],
    // EditRedo
    [
        /* en      */ "Redo",
        /* de      */ "Wiederholen",
        /* es      */ "Rehacer",
        /* fr      */ "Rétablir",
        /* it      */ "Ripeti",
        /* ja      */ "やり直し",
        /* ko      */ "다시 실행",
        /* pt_br   */ "Refazer",
        /* ru      */ "Повторить",
        /* zh_hans */ "重做",
        /* zh_hant */ "重做",
    ],
    // EditCut
    [
        /* en      */ "Cut",
        /* de      */ "Ausschneiden",
        /* es      */ "Cortar",
        /* fr      */ "Couper",
        /* it      */ "Taglia",
        /* ja      */ "切り取り",
        /* ko      */ "잘라내기",
        /* pt_br   */ "Cortar",
        /* ru      */ "Вырезать",
        /* zh_hans */ "剪切",
        /* zh_hant */ "剪下",
    ],
    // EditCopy
    [
        /* en      */ "Copy",
        /* de      */ "Kopieren",
        /* es      */ "Copiar",
        /* fr      */ "Copier",
        /* it      */ "Copia",
        /* ja      */ "コピー",
        /* ko      */ "복사",
        /* pt_br   */ "Copiar",
        /* ru      */ "Копировать",
        /* zh_hans */ "复制",
        /* zh_hant */ "複製",
    ],
    // EditPaste
    [
        /* en      */ "Paste",
        /* de      */ "Einfügen",
        /* es      */ "Pegar",
        /* fr      */ "Coller",
        /* it      */ "Incolla",
        /* ja      */ "貼り付け",
        /* ko      */ "붙여넣기",
        /* pt_br   */ "Colar",
        /* ru      */ "Вставить",
        /* zh_hans */ "粘贴",
        /* zh_hant */ "貼上",
    ],
    // EditFind
    [
        /* en      */ "Find",
        /* de      */ "Suchen",
        /* es      */ "Buscar",
        /* fr      */ "Rechercher",
        /* it      */ "Trova",
        /* ja      */ "検索",
        /* ko      */ "찾기",
        /* pt_br   */ "Encontrar",
        /* ru      */ "Найти",
        /* zh_hans */ "查找",
        /* zh_hant */ "尋找",
    ],
    // EditReplace
    [
        /* en      */ "Replace",
        /* de      */ "Ersetzen",
        /* es      */ "Reemplazar",
        /* fr      */ "Remplacer",
        /* it      */ "Sostituisci",
        /* ja      */ "置換",
        /* ko      */ "바꾸기",
        /* pt_br   */ "Substituir",
        /* ru      */ "Заменить",
        /* zh_hans */ "替换",
        /* zh_hant */ "取代",
    ],

    // View
    [
        /* en      */ "View",
        /* de      */ "Ansicht",
        /* es      */ "Ver",
        /* fr      */ "Affichage",
        /* it      */ "Visualizza",
        /* ja      */ "表示",
        /* ko      */ "보기",
        /* pt_br   */ "Exibir",
        /* ru      */ "Вид",
        /* zh_hans */ "视图",
        /* zh_hant */ "檢視",
    ],
    // ViewWordWrap
    [
        /* en      */ "Word Wrap",
        /* de      */ "Zeilenumbruch",
        /* es      */ "Ajuste de línea",
        /* fr      */ "Retour à la ligne",
        /* it      */ "A capo automatico",
        /* ja      */ "折り返し",
        /* ko      */ "자동 줄 바꿈",
        /* pt_br   */ "Quebra de linha",
        /* ru      */ "Перенос слов",
        /* zh_hans */ "自动换行",
        /* zh_hant */ "自動換行",
    ],

    // Help
    [
        /* en      */ "Help",
        /* de      */ "Hilfe",
        /* es      */ "Ayuda",
        /* fr      */ "Aide",
        /* it      */ "Aiuto",
        /* ja      */ "ヘルプ",
        /* ko      */ "도움말",
        /* pt_br   */ "Ajuda",
        /* ru      */ "Помощь",
        /* zh_hans */ "帮助",
        /* zh_hant */ "幫助",
    ],
    // HelpAbout
    [
        /* en      */ "About",
        /* de      */ "Über",
        /* es      */ "Acerca de",
        /* fr      */ "À propos",
        /* it      */ "Informazioni",
        /* ja      */ "情報",
        /* ko      */ "정보",
        /* pt_br   */ "Sobre",
        /* ru      */ "О программе",
        /* zh_hans */ "关于",
        /* zh_hant */ "關於",
    ],

    // UnsavedChangesDialogTitle
    [
        /* en      */ "Unsaved Changes",
        /* de      */ "Ungespeicherte Änderungen",
        /* es      */ "Cambios sin guardar",
        /* fr      */ "Modifications non enregistrées",
        /* it      */ "Modifiche non salvate",
        /* ja      */ "未保存の変更",
        /* ko      */ "저장되지 않은 변경 사항",
        /* pt_br   */ "Alterações não salvas",
        /* ru      */ "Несохраненные изменения",
        /* zh_hans */ "未保存的更改",
        /* zh_hant */ "未儲存的變更",
    ],
    // UnsavedChangesDialogDescription
    [
        /* en      */ "Do you want to save the changes you made?",
        /* de      */ "Möchten Sie die vorgenommenen Änderungen speichern?",
        /* es      */ "¿Desea guardar los cambios realizados?",
        /* fr      */ "Voulez-vous enregistrer les modifications apportées?",
        /* it      */ "Vuoi salvare le modifiche apportate?",
        /* ja      */ "変更内容を保存しますか？",
        /* ko      */ "변경한 내용을 저장하시겠습니까?",
        /* pt_br   */ "Deseja salvar as alterações feitas?",
        /* ru      */ "Вы хотите сохранить внесённые изменения?",
        /* zh_hans */ "您要保存所做的更改吗？",
        /* zh_hant */ "您要保存所做的變更嗎？",
    ],
    // UnsavedChangesDialogYes
    [
        /* en      */ "Save",
        /* de      */ "Speichern",
        /* es      */ "Guardar",
        /* fr      */ "Enregistrer",
        /* it      */ "Salva",
        /* ja      */ "保存",
        /* ko      */ "저장",
        /* pt_br   */ "Salvar",
        /* ru      */ "Сохранить",
        /* zh_hans */ "保存",
        /* zh_hant */ "儲存",
    ],
    // UnsavedChangesDialogNo
    [
        /* en      */ "Don't Save",
        /* de      */ "Nicht speichern",
        /* es      */ "No guardar",
        /* fr      */ "Ne pas enregistrer",
        /* it      */ "Non salvare",
        /* ja      */ "保存しない",
        /* ko      */ "저장 안 함",
        /* pt_br   */ "Não salvar",
        /* ru      */ "Не сохранять",
        /* zh_hans */ "不保存",
        /* zh_hant */ "不儲存",
    ],
    // UnsavedChangesDialogCancel
    [
        /* en      */ "Cancel",
        /* de      */ "Abbrechen",
        /* es      */ "Cancelar",
        /* fr      */ "Annuler",
        /* it      */ "Annulla",
        /* ja      */ "キャンセル",
        /* ko      */ "취소",
        /* pt_br   */ "Cancelar",
        /* ru      */ "Отмена",
        /* zh_hans */ "取消",
        /* zh_hant */ "取消",
    ],

    // AboutDialogTitle
    [
        /* en      */ "About",
        /* de      */ "Über",
        /* es      */ "Acerca de",
        /* fr      */ "À propos",
        /* it      */ "Informazioni",
        /* ja      */ "情報",
        /* ko      */ "정보",
        /* pt_br   */ "Sobre",
        /* ru      */ "О программе",
        /* zh_hans */ "关于",
        /* zh_hant */ "關於",
    ],
    // AboutDialogDescription
    [
        /* en      */ "Grug's favorite editor",
        /* de      */ "Grugs Lieblingseditor",
        /* es      */ "El editor favorito de Grug",
        /* fr      */ "L'éditeur préféré de Grug",
        /* it      */ "L'editor preferito di Grug",
        /* ja      */ "Grugのお気に入りエディタ",
        /* ko      */ "Grug이 가장 좋아하는 편집기",
        /* pt_br   */ "O editor favorito do Grug",
        /* ru      */ "Любимый редактор Груга",
        /* zh_hans */ "Grug最喜欢的编辑器",
        /* zh_hant */ "Grug最喜歡的編輯器",
    ],
    // AboutDialogVersion
    [
        /* en      */ "Version: ",
        /* de      */ "Version: ",
        /* es      */ "Versión: ",
        /* fr      */ "Version : ",
        /* it      */ "Versione: ",
        /* ja      */ "バージョン: ",
        /* ko      */ "버전: ",
        /* pt_br   */ "Versão: ",
        /* ru      */ "Версия: ",
        /* zh_hans */ "版本:",
        /* zh_hant */ "版本:",
    ],

    // SearchLabel
    [
        /* en      */ "Find:",
        /* de      */ "Suchen:",
        /* es      */ "Buscar:",
        /* fr      */ "Rechercher:",
        /* it      */ "Trova:",
        /* ja      */ "検索:",
        /* ko      */ "찾기:",
        /* pt_br   */ "Encontrar:",
        /* ru      */ "Найти:",
        /* zh_hans */ "查找:",
        /* zh_hant */ "尋找:",
    ],
    // SearchClose
    [
        /* en      */ "Close",
        /* de      */ "Schließen",
        /* es      */ "Cerrar",
        /* fr      */ "Fermer",
        /* it      */ "Chiudi",
        /* ja      */ "閉じる",
        /* ko      */ "닫기",
        /* pt_br   */ "Fechar",
        /* ru      */ "Закрыть",
        /* zh_hans */ "关闭",
        /* zh_hant */ "關閉",
    ],
    // SearchMatchCase
    [
        /* en      */ "Match Case",
        /* de      */ "Groß/Klein",
        /* es      */ "May/Min",
        /* fr      */ "Casse",
        /* it      */ "Maius/minus",
        /* ja      */ "大/小文字",
        /* ko      */ "대소문자",
        /* pt_br   */ "Maius/minus",
        /* ru      */ "Регистр",
        /* zh_hans */ "区分大小写",
        /* zh_hant */ "區分大小寫",
    ],
    // SearchWholeWord
    [
        /* en      */ "Whole Word",
        /* de      */ "Ganzes Wort",
        /* es      */ "Palabra",
        /* fr      */ "Mot entier",
        /* it      */ "Parola",
        /* ja      */ "単語単位",
        /* ko      */ "전체 단어",
        /* pt_br   */ "Palavra",
        /* ru      */ "Слово",
        /* zh_hans */ "全字匹配",
        /* zh_hant */ "全字匹配",
    ],
    // SearchUseRegex
    [
        /* en      */ "Use Regex",
        /* de      */ "RegEx",
        /* es      */ "RegEx",
        /* fr      */ "RegEx",
        /* it      */ "RegEx",
        /* ja      */ "正規表現",
        /* ko      */ "정규식",
        /* pt_br   */ "RegEx",
        /* ru      */ "RegEx",
        /* zh_hans */ "正则",
        /* zh_hant */ "正則",
    ],

    // EncodingReopen
    [
        /* en      */ "Reopen with encoding",
        /* de      */ "Mit Kodierung erneut öffnen",
        /* es      */ "Reabrir con codificación",
        /* fr      */ "Rouvrir avec un encodage différent",
        /* it      */ "Riapri con codifica",
        /* ja      */ "エンコーディングで再度開く",
        /* ko      */ "인코딩으로 다시 열기",
        /* pt_br   */ "Reabrir com codificação",
        /* ru      */ "Открыть снова с кодировкой",
        /* zh_hans */ "使用编码重新打开",
        /* zh_hant */ "使用編碼重新打開",
    ],
    // EncodingConvert
    [
        /* en      */ "Convert to encoding",
        /* de      */ "In Kodierung konvertieren",
        /* es      */ "Convertir a otra codificación",
        /* fr      */ "Convertir en encodage",
        /* it      */ "Converti in codifica",
        /* ja      */ "エンコーディングに変換",
        /* ko      */ "인코딩으로 변환",
        /* pt_br   */ "Converter para codificação",
        /* ru      */ "Преобразовать в кодировку",
        /* zh_hans */ "转换为编码",
        /* zh_hant */ "轉換為編碼",
    ],

    // IndentationTabs
    [
        /* en      */ "Tabs",
        /* de      */ "Tabs",
        /* es      */ "Tabulaciones",
        /* fr      */ "Tabulations",
        /* it      */ "Tabulazioni",
        /* ja      */ "タブ",
        /* ko      */ "탭",
        /* pt_br   */ "Tabulações",
        /* ru      */ "Табы",
        /* zh_hans */ "制表符",
        /* zh_hant */ "製表符",
    ],
    // IndentationSpaces
    [
        /* en      */ "Spaces",
        /* de      */ "Leerzeichen",
        /* es      */ "Espacios",
        /* fr      */ "Espaces",
        /* it      */ "Spazi",
        /* ja      */ "スペース",
        /* ko      */ "공백",
        /* pt_br   */ "Espaços",
        /* ru      */ "Пробелы",
        /* zh_hans */ "空格",
        /* zh_hant */ "空格",
    ],

    // SaveAsDialogTitle
    // NOTE: Exact same translation as FileSaveAs, and both should be kept in sync.
    [
        /* en      */ "Save As…",
        /* de      */ "Speichern unter…",
        /* es      */ "Guardar como…",
        /* fr      */ "Enregistrer sous…",
        /* it      */ "Salva come…",
        /* ja      */ "名前を付けて保存…",
        /* ko      */ "다른 이름으로 저장…",
        /* pt_br   */ "Salvar como…",
        /* ru      */ "Сохранить как…",
        /* zh_hans */ "另存为…",
        /* zh_hant */ "另存新檔…",
    ],
    // SaveAsDialogPathLabel
    [
        /* en      */ "Folder:",
        /* de      */ "Ordner:",
        /* es      */ "Carpeta:",
        /* fr      */ "Dossier:",
        /* it      */ "Cartella:",
        /* ja      */ "フォルダ:",
        /* ko      */ "폴더:",
        /* pt_br   */ "Pasta:",
        /* ru      */ "Папка:",
        /* zh_hans */ "文件夹:",
        /* zh_hant */ "資料夾:",
    ],
    // SaveAsDialogNameLabel
    [
        /* en      */ "File name:",
        /* de      */ "Dateiname:",
        /* es      */ "Nombre de archivo:",
        /* fr      */ "Nom de fichier:",
        /* it      */ "Nome del file:",
        /* ja      */ "ファイル名:",
        /* ko      */ "파일 이름:",
        /* pt_br   */ "Nome do arquivo:",
        /* ru      */ "Имя файла:",
        /* zh_hans */ "文件名:",
        /* zh_hant */ "檔案名稱:",
    ],
];

static mut S_LANG: LangId = LangId::en;

pub fn init() {
    let langs = sys::preferred_languages();
    let mut lang = LangId::en;

    for l in langs {
        lang = match l.as_str() {
            "en" => LangId::en,
            "de" => LangId::de,
            "es" => LangId::es,
            "fr" => LangId::fr,
            "it" => LangId::it,
            "ja" => LangId::ja,
            "ko" => LangId::ko,
            "pt-br" => LangId::pt_br,
            "ru" => LangId::ru,
            "zh-hant" => LangId::zh_hant,
            "zh" => LangId::zh_hans,
            _ => continue,
        };
        break;
    }

    unsafe {
        S_LANG = lang;
    }
}

pub fn loc(id: LocId) -> &'static str {
    S_LANG_LUT[id as usize][unsafe { S_LANG as usize }]
}
