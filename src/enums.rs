#[cfg(test)]
mod tests;

///The following are the implemented ANSI escape sequences. More to be added.
#[derive(Debug, PartialEq)]
pub enum AnsiSequence {
    CursorPos(u32, u32),
    CursorUp(u32),
    CursorDown(u32),
    CursorForward(u32),
    CursorBackward(u32),
    CursorSave,
    CursorRestore,
    EraseDisplay,
    EraseLine,
    SetGraphicsMode(Vec<u32>),
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
}

use std::fmt::Display;
impl Display for AnsiSequence {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "\u{1b}[")?;
        
        use AnsiSequence::*;
        match self {
            CursorPos(line, col) 
                => write!(formatter, "{};{}H", line, col),
            CursorUp(amt)
                => write!(formatter, "{}A", amt),
            CursorDown(amt)
                => write!(formatter, "{}B", amt),
            CursorForward(amt)
                => write!(formatter, "{}C", amt),
            CursorBackward(amt)
                => write!(formatter, "{}D", amt),
            CursorSave
                => write!(formatter, "s"),
            CursorRestore
                => write!(formatter, "u"),
            EraseDisplay
                => write!(formatter, "2J"),
            EraseLine
                => write!(formatter, "K"),
            SetGraphicsMode(vec)
                => {
                    match vec.len() {
                        1 => write!(formatter, "{}m", vec[0]),
                        2 => write!(formatter, "{};{}m", vec[0], vec[1]),
                        3 => write!(formatter, "{};{};{}m", vec[0], vec[1], vec[2]),
                        _ => unreachable!()
                    }
                },
            SetMode(mode)
                => write!(formatter, "={}h", mode),
            ResetMode(mode)
                => write!(formatter, "={}l", mode),
            ShowCursor
                => write!(formatter, "?25h"),
            HideCursor
                => write!(formatter, "?25l"),
            CursorToApp
                => write!(formatter, "?1h"),
            SetNewLineMode
                => write!(formatter, "20h"),
            SetCol132
                => write!(formatter, "?3h"),
            SetSmoothScroll
                => write!(formatter, "?4h"),
            SetReverseVideo
                => write!(formatter, "?5h"),
            SetOriginRelative
                => write!(formatter, "?6h"),
            SetAutoWrap
                => write!(formatter, "?7h"),
            SetAutoRepeat
                => write!(formatter, "?8h"),
            SetInterlacing
                => write!(formatter, "?9h"),
            SetLineFeedMode
                => write!(formatter, "20l"),
            SetCursorKeyToCursor
                => write!(formatter, "?1l")
        }
    }
}

///This is what is outputted by the parsing iterator.
///Each block contains either straight-up text, or simply
///an ANSI escape sequence.
#[derive(Debug, PartialEq)]
pub enum Output<'a> {
    TextBlock(&'a str),
    Escape(AnsiSequence)
}
