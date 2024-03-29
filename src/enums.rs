#[cfg(test)]
mod tests;

use heapless::Vec;

///The following are the implemented ANSI escape sequences. More to be added.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AnsiSequence {
    Escape,
    CursorPos(u32, u32),
    CursorUp(u32),
    CursorDown(u32),
    CursorForward(u32),
    CursorBackward(u32),
    CursorSave,
    CursorRestore,
    EraseDisplay,
    EraseLine,
    SetGraphicsMode(Vec<u8, 16>),
    SetMode(u8),
    ResetMode(u8),
    HideCursor,
    ShowCursor,
    CursorToApp,
    SetNewLineMode,
    SetCol132,
    SetSmoothScroll,
    SetReverseVideo,
    SetOriginRelative,
    SetAutoWrap,
    SetAutoRepeat,
    SetInterlacing,
    SetLineFeedMode,
    SetCursorKeyToCursor,
    SetVT52,
    SetCol80,
    SetJumpScrolling,
    SetNormalVideo,
    SetOriginAbsolute,
    ResetAutoWrap,
    ResetAutoRepeat,
    ResetInterlacing,
    SetAlternateKeypad,
    SetNumericKeypad,
    SetUKG0,
    SetUKG1,
    SetUSG0,
    SetUSG1,
    SetG0SpecialChars,
    SetG1SpecialChars,
    SetG0AlternateChar,
    SetG1AlternateChar,
    SetG0AltAndSpecialGraph,
    SetG1AltAndSpecialGraph,
    SetSingleShift2,
    SetSingleShift3,
    SetTopAndBottom(u32, u32),
}

use core::fmt::{Display, Formatter, Result as DisplayResult};
impl Display for AnsiSequence {
    fn fmt(&self, formatter: &mut Formatter) -> DisplayResult {
        #[allow(clippy::enum_glob_use)]
        use AnsiSequence::*;

        write!(formatter, "\u{1b}")?;

        match self {
            Escape => write!(formatter, "\u{1b}"),
            CursorPos(line, col) => write!(formatter, "[{line};{col}H"),
            CursorUp(amt) => write!(formatter, "[{amt}A"),
            CursorDown(amt) => write!(formatter, "[{amt}B"),
            CursorForward(amt) => write!(formatter, "[{amt}C"),
            CursorBackward(amt) => write!(formatter, "[{amt}D"),
            CursorSave => write!(formatter, "[s"),
            CursorRestore => write!(formatter, "[u"),
            EraseDisplay => write!(formatter, "[2J"),
            EraseLine => write!(formatter, "[K"),
            SetGraphicsMode(vec) => match vec.len() {
                0 => write!(formatter, "[m"),
                1 => write!(formatter, "[{}m", vec[0]),
                2 => write!(formatter, "[{};{}m", vec[0], vec[1]),
                3 => write!(formatter, "[{};{};{}m", vec[0], vec[1], vec[2]),
                5 => write!(
                    formatter,
                    "[{};{};{};{};{}m",
                    vec[0], vec[1], vec[2], vec[3], vec[4]
                ),
                _ => unreachable!(),
            },
            SetMode(mode) => write!(formatter, "[={mode}h"),
            ResetMode(mode) => write!(formatter, "[={mode}l"),
            ShowCursor => write!(formatter, "[?25h"),
            HideCursor => write!(formatter, "[?25l"),
            CursorToApp => write!(formatter, "[?1h"),
            SetNewLineMode => write!(formatter, "[20h"),
            SetCol132 => write!(formatter, "[?3h"),
            SetSmoothScroll => write!(formatter, "[?4h"),
            SetReverseVideo => write!(formatter, "[?5h"),
            SetOriginRelative => write!(formatter, "[?6h"),
            SetAutoWrap => write!(formatter, "[?7h"),
            SetAutoRepeat => write!(formatter, "[?8h"),
            SetInterlacing => write!(formatter, "[?9h"),
            SetLineFeedMode => write!(formatter, "[20l"),
            SetCursorKeyToCursor => write!(formatter, "[?1l"),
            SetVT52 => write!(formatter, "[?2l"),
            SetCol80 => write!(formatter, "[?3l"),
            SetJumpScrolling => write!(formatter, "[?4l"),
            SetNormalVideo => write!(formatter, "[?5l"),
            SetOriginAbsolute => write!(formatter, "[?6l"),
            ResetAutoWrap => write!(formatter, "[?7l"),
            ResetAutoRepeat => write!(formatter, "[?8l"),
            ResetInterlacing => write!(formatter, "[?9l"),
            SetAlternateKeypad => write!(formatter, "="),
            SetNumericKeypad => write!(formatter, ">"),
            SetUKG0 => write!(formatter, "(A"),
            SetUKG1 => write!(formatter, ")A"),
            SetUSG0 => write!(formatter, "(B"),
            SetUSG1 => write!(formatter, ")B"),
            SetG0SpecialChars => write!(formatter, "(0"),
            SetG1SpecialChars => write!(formatter, ")0"),
            SetG0AlternateChar => write!(formatter, "(1"),
            SetG1AlternateChar => write!(formatter, ")1"),
            SetG0AltAndSpecialGraph => write!(formatter, "(2"),
            SetG1AltAndSpecialGraph => write!(formatter, ")2"),
            SetSingleShift2 => write!(formatter, "N"),
            SetSingleShift3 => write!(formatter, "O"),
            SetTopAndBottom(x, y) => write!(formatter, "{x};{y}r"),
        }
    }
}

///This is what is outputted by the parsing iterator.
///Each block contains either straight-up text, or simply
///an ANSI escape sequence.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Output<'a> {
    TextBlock(&'a str),
    Escape(AnsiSequence),
}

impl<'a> Display for Output<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> DisplayResult {
        use Output::{Escape, TextBlock};
        match self {
            Escape(seq) => write!(formatter, "{seq}"),
            TextBlock(txt) => write!(formatter, "{txt}"),
        }
    }
}
