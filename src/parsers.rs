#[cfg(test)]
mod tests;

use crate::AnsiSequence;

pub use heapless::Vec;
use nom::{bytes::streaming::tag, error::{Error, ErrorKind}, IResult};

/*
macro_rules! expr_res {
    ($i:expr, $e:expr) => {{
        match $e {
            Ok(output) => Ok(($i, output)),
            // TODO: what should be the error for a failed Vec::from_slice?
            // ErrorKind::ExprRes has been removed in rust-nom@7.0.0
            Err(_) => Err(nom::Err::Error(error_position!(
                $i,
                nom::error::ErrorKind::TooLarge
            ))),
        }
    }};
}
*/

fn err_hack(i: &str) -> nom::Err<Error<&str>> {
    nom::Err::Error(Error::<&str>{ input: i, code: ErrorKind::Fix })
}

// Previous macro body
/*
named!(
    $sig<&str, AnsiSequence>,
    do_parse!(
        tag!($tag) >>
        ($ret)
    )
);
*/

macro_rules! tag_parser {
    ($sig:ident, $tag:expr, $ret:expr) => {
        fn $sig(i: &str) -> IResult<&str, AnsiSequence> {
            let (i, _) = tag($tag)(i)?;
            Ok((i, $ret))
        }
    };
}

fn bracket(i: &str) -> IResult<&str, &str> {
    tag("[")(i)
}

fn semicolon(i: &str) -> IResult<&str, &str> {
    tag(";")(i)
}

/*
fn parse_int(i: &str) -> IResult<&str, u32> {
    nom::combinator::map_res(nom::character::streaming::digit0, |s: &str| {
        s.parse::<u32>()
    })(i)
}
*/

// so, technically, this can match any FromStr, but since we match digit0, it has to be an int
// this removes the need to use .into() and bubble up types
fn parse_int<T: core::str::FromStr>() -> impl Fn(&str) -> IResult<&str, T> {
    |i| {
        nom::combinator::map_res(nom::character::streaming::digit0, |s: &str| s.parse::<T>())(i)
    }
}

/*
named!(
    parse_def_cursor_int<&str, u32>,
    map!(
        nom::character::streaming::digit0,
        |s: &str| s.parse::<u32>().unwrap_or(1)
    )
);
*/

// TODO kind of ugly, would prefer to pass in the default so we could use it for
// all escapes with defaults (not just those that default to 1).
fn parse_def_cursor_int(i: &str) -> IResult<&str, u32> {
    nom::combinator::map(nom::character::streaming::digit0, |s: &str| {
        s.parse::<u32>().unwrap_or(1)
    })(i)
}

/*
named!(
    cursor_pos<&str, AnsiSequence>,
    do_parse!(
        tag!("[")               >>
        x: parse_def_cursor_int >>
        opt!(tag!(";"))         >>
        y: parse_def_cursor_int >>
        alt!(
            tag!("H") |
            tag!("f")
        )               >>
        (AnsiSequence::CursorPos(x, y))
    )
);
*/

fn cursor_pos(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, x, _, y, _)) = nom::sequence::tuple((
        bracket,
        parse_def_cursor_int,
        nom::combinator::opt(semicolon),
        parse_def_cursor_int,
        nom::branch::alt((tag("H"), tag("f"))),
    ))(i)?;
    Ok((i, AnsiSequence::CursorPos(x, y)))
}

/*
named!(
    escape<&str, AnsiSequence>,
    do_parse!(
        tag!("\u{1b}") >>
        (AnsiSequence::Escape)
    )
);
*/

fn escape(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, _) = tag("\u{1b}")(i)?;
    Ok((i, AnsiSequence::Escape))
}

/*
named!(
    cursor_up<&str, AnsiSequence>,
    do_parse!(
        tag!("[")                >>
        am: parse_def_cursor_int >>
        tag!("A")                >>
        (AnsiSequence::CursorUp(am))
    )
);
*/

fn cursor_up(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, am, _)) = nom::sequence::tuple((bracket, parse_def_cursor_int, tag("A")))(i)?;
    Ok((i, (AnsiSequence::CursorUp(am))))
}

/*
named!(
    cursor_down<&str, AnsiSequence>,
    do_parse!(
        tag!("[")                >>
        am: parse_def_cursor_int >>
        tag!("B")                >>
        (AnsiSequence::CursorDown(am))
    )
);
*/

fn cursor_down(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, am, _)) = nom::sequence::tuple((bracket, parse_def_cursor_int, tag("B")))(i)?;
    Ok((i, (AnsiSequence::CursorDown(am))))
}

/*
named!(
    cursor_forward<&str, AnsiSequence>,
    do_parse!(
        tag!("[")                >>
        am: parse_def_cursor_int >>
        tag!("C")                >>
        (AnsiSequence::CursorForward(am))
    )
);
*/

fn cursor_forward(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, am, _)) = nom::sequence::tuple((bracket, parse_def_cursor_int, tag("C")))(i)?;
    Ok((i, (AnsiSequence::CursorForward(am))))
}

/*
named!(
    cursor_backward<&str, AnsiSequence>,
    do_parse!(
        tag!("[")                >>
        am: parse_def_cursor_int >>
        tag!("D")                >>
        (AnsiSequence::CursorBackward(am))
    )
);
*/

fn cursor_backward(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, am, _)) = nom::sequence::tuple((bracket, parse_def_cursor_int, tag("D")))(i)?;
    Ok((i, (AnsiSequence::CursorBackward(am))))
}

/*
named!(
    graphics_mode1<&str, AnsiSequence>,
    do_parse!(
        tag!("[")       >>
        val: parse_int >>
        tag!("m")      >>
        val: expr_res!(val.try_into()) >>
        conv: expr_res!(Vec::from_slice(&[val])) >>
        (AnsiSequence::SetGraphicsMode(conv))
    )
);
*/

/*
fn graphics_mode1(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, val, _)) = nom::sequence::tuple((bracket, parse_int::<u8>(), tag("m")))(i)?;
    let v = Vec::from_slice(&[val]).map_err(|_| err_hack(i))?;
    Ok((i, AnsiSequence::SetGraphicsMode(v)))
}
*/

/*
named!(
    graphics_mode2<&str, AnsiSequence>,
    do_parse!(
        tag!("[")       >>
        val1: parse_int >>
        tag!(";")       >>
        val2: parse_int >>
        tag!("m")       >>
        val1: expr_res!(val1.try_into()) >>
        val2: expr_res!(val2.try_into()) >>
        conv: expr_res!(Vec::from_slice(&[
            val1,
            val2,
        ])) >>
        (AnsiSequence::SetGraphicsMode(conv))
    )
);
*/

/*
fn graphics_mode2(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, val1, _, val2, _)) = nom::sequence::tuple((
        bracket,
        parse_int::<u8>(),
        semicolon,
        parse_int::<u8>(),
        tag("m"),
    ))(i)?;
    let val1 = u8::try_from(val1).map_err(|_| err_hack(i))?;
    let val2 = u8::try_from(val2).map_err(|_| err_hack(i))?;
    let v = Vec::from_slice(&[val1, val2]).map_err(|_| err_hack(i))?;
    Ok((i, AnsiSequence::SetGraphicsMode(v)))
}
*/

/*
named!(
    graphics_mode3<&str, AnsiSequence>,
    do_parse!(
        tag!("[")       >>
        val1: parse_int >>
        tag!(";")       >>
        val2: parse_int >>
        tag!(";")       >>
        val3: parse_int >>
        tag!("m")       >>
        val1: expr_res!(val1.try_into()) >>
        val2: expr_res!(val2.try_into()) >>
        val3: expr_res!(val3.try_into()) >>
        conv: expr_res!(Vec::from_slice(&[
            val1,
            val2,
            val3,
        ])) >>
        (AnsiSequence::SetGraphicsMode(conv))
    )
);
*/

/*
fn graphics_mode3(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, val1, _, val2, _, val3, _)) = nom::sequence::tuple((
        bracket,
        parse_int::<u8>(),
        semicolon,
        parse_int::<u8>(),
        semicolon,
        parse_int::<u8>(),
        tag("m"),
    ))(i)?;
    let val1 = u8::try_from(val1).map_err(|_| err_hack(i))?;
    let val2 = u8::try_from(val2).map_err(|_| err_hack(i))?;
    let val3 = u8::try_from(val3).map_err(|_| err_hack(i))?;
    let v = Vec::from_slice(&[val1, val2, val3]).map_err(|_| err_hack(i))?;
    Ok((i, AnsiSequence::SetGraphicsMode(v)))
}
*/

/*
named!(
    graphics_mode4<&str, AnsiSequence>,
    do_parse!(
        tag!("[m") >>
        (AnsiSequence::SetGraphicsMode(Vec::new()))
    )
);
*/

/*
// Previous versions of this function (see above) discarded all values because there is currently
// no valid use for them. However, actually including the data -- which can be useful, eventually
// doesn't break tests so this should be fine.
fn graphics_mode4(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, val1, _, val2, _, val3, _, val4, _)) = nom::sequence::tuple((
        bracket,
        parse_int::<u8>(),
        semicolon,
        parse_int::<u8>(),
        semicolon,
        parse_int::<u8>(),
        semicolon,
        parse_int::<u8>(),
        tag("m"),
    ))(i)?;
    let val1 = u8::try_from(val1).map_err(|_| err_hack(i))?;
    let val2 = u8::try_from(val2).map_err(|_| err_hack(i))?;
    let val3 = u8::try_from(val3).map_err(|_| err_hack(i))?;
    let val4 = u8::try_from(val4).map_err(|_| err_hack(i))?;
    let v = Vec::from_slice(&[val1, val2, val3, val4]).map_err(|_| err_hack(i))?;
    Ok((i, AnsiSequence::SetGraphicsMode(v)))
}
*/

/*
named!(
    graphics_mode5<&str, AnsiSequence>,
    do_parse!(
        tag!("[")       >>
        val1: parse_int >>
        tag!(";")       >>
        val2: parse_int >>
        tag!(";")       >>
        val3: parse_int >>
        tag!(";")       >>
        val4: parse_int >>
        tag!(";")       >>
        val5: parse_int >>
        tag!("m")       >>
        val1: expr_res!(val1.try_into()) >>
        val2: expr_res!(val2.try_into()) >>
        val3: expr_res!(val3.try_into()) >>
        val4: expr_res!(val4.try_into()) >>
        val5: expr_res!(val5.try_into()) >>
        conv: expr_res!(Vec::from_slice(&[
            val1,
            val2,
            val3,
            val4,
            val5,
        ])) >>
        (AnsiSequence::SetGraphicsMode(conv))
    )
);
*/

/*
fn graphics_mode5(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, val1, _, val2, _, val3, _, val4, _, val5, _)) = nom::sequence::tuple((
        bracket,
        parse_int::<u8>(),
        semicolon,
        parse_int::<u8>(),
        semicolon,
        parse_int::<u8>(),
        semicolon,
        parse_int::<u8>(),
        semicolon,
        parse_int::<u8>(),
        tag("m"),
    ))(i)?;
    let val1 = u8::try_from(val1).map_err(|_| err_hack(i))?;
    let val2 = u8::try_from(val2).map_err(|_| err_hack(i))?;
    let val3 = u8::try_from(val3).map_err(|_| err_hack(i))?;
    let val4 = u8::try_from(val4).map_err(|_| err_hack(i))?;
    let val5 = u8::try_from(val5).map_err(|_| err_hack(i))?;
    let v = Vec::from_slice(&[val1, val2, val3, val4, val5]).map_err(|_| err_hack(i))?;
    Ok((i, AnsiSequence::SetGraphicsMode(v)))
}
*/

/*
named!(
    graphics_mode<&str, AnsiSequence>,
    alt!(
          graphics_mode1
        | graphics_mode2
        | graphics_mode3
        | graphics_mode4
        | graphics_mode5
    )
);
*/

fn graphics_mode(input_: &str) -> IResult<&str, AnsiSequence> {
    let mut input = &input_[..];
    (input, _) = bracket(input)?;
    let mut v = Vec::<u8, 16>::new();
    for _ in 1..=16 {
        // set this to false if a number is found but a semicolon isn't matched
        let should_continue: bool;
        (input, should_continue) = match parse_int::<u8>()(input) {
            // TODO: non existent values should be marked to 0
            // this happens rarely enough in practice that it's not worth fixing
            // no match, return
            //
            // Note: This breaks here and does *not* progress `input`
            Err(_) => break,
            Ok((input, val)) => {
                v.push(val).expect("1..=16 loop overflowed");
                match semicolon(input) {
                    Err(_) => (input, false),
                    Ok((input, _)) => (input, true)
                }
            }
        };
        if !should_continue {
            break
        }
    };
    (input, _) = tag("m")(input)?;
    Ok((input, AnsiSequence::SetGraphicsMode(v)))
}

/*
named!(
    set_mode<&str, AnsiSequence>,
    do_parse!(
        tag!("[=")                       >>
        mode: parse_int                  >>
        conv: expr_res!(mode.try_into()) >>
        tag!("h")                        >>
        (AnsiSequence::SetMode(conv))
    )
);
*/

fn set_mode(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, _, mode, _)) = nom::sequence::tuple((
        bracket,
        // minor incorrect optimization, but it's fine
        tag("="),
        parse_int::<u8>(),
        tag("h"),
    ))(i)?;

    Ok((i, AnsiSequence::SetMode(mode)))
}

/*
named!(
    reset_mode<&str, AnsiSequence>,
    do_parse!(
        tag!("[=")                       >>
        mode: parse_int_                  >>
        conv: expr_res!(mode.try_into()) >>
        tag!("l")                        >>
        (AnsiSequence::ResetMode(conv))
    )
);
*/

fn reset_mode(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, _, mode, _)) = nom::sequence::tuple((
        bracket,
        // minor incorrect optimization, but it's fine
        tag("="),
        parse_int::<u8>(),
        tag("l"),
    ))(i)?;

    Ok((i, AnsiSequence::ResetMode(mode)))
}

/*
named!(
    set_top_and_bottom<&str, AnsiSequence>,
    do_parse!(
        tag!("[")    >>
        x: parse_int_ >>
        tag!(";")    >>
        y: parse_int_ >>
        tag!("r")    >>
        (AnsiSequence::SetTopAndBottom(x, y))
    )
);
*/

fn set_top_and_bottom(i: &str) -> IResult<&str, AnsiSequence> {
    let (i, (_, x, _, y, _)) = nom::sequence::tuple((
        bracket,
        parse_int::<u32>(),
        semicolon,
        parse_int::<u32>(),
        tag("r"),
    ))(i)?;

    Ok((i, AnsiSequence::SetTopAndBottom(x, y)))
}

tag_parser!(cursor_save, "[s", AnsiSequence::CursorSave);
tag_parser!(cursor_restore, "[u", AnsiSequence::CursorRestore);
tag_parser!(erase_display, "[2J", AnsiSequence::EraseDisplay);
tag_parser!(erase_line, "[K", AnsiSequence::EraseLine);
tag_parser!(hide_cursor, "[?25l", AnsiSequence::HideCursor);
tag_parser!(show_cursor, "[?25h", AnsiSequence::ShowCursor);
tag_parser!(cursor_to_app, "[?1h", AnsiSequence::CursorToApp);
tag_parser!(set_new_line_mode, "[20h", AnsiSequence::SetNewLineMode);
tag_parser!(set_col_132, "[?3h", AnsiSequence::SetCol132);
tag_parser!(set_smooth_scroll, "[?4h", AnsiSequence::SetSmoothScroll);
tag_parser!(set_reverse_video, "[?5h", AnsiSequence::SetReverseVideo);
tag_parser!(set_origin_rel, "[?6h", AnsiSequence::SetOriginRelative);
tag_parser!(set_auto_wrap, "[?7h", AnsiSequence::SetAutoWrap);
tag_parser!(set_auto_repeat, "[?8h", AnsiSequence::SetAutoRepeat);
tag_parser!(set_interlacing, "[?9h", AnsiSequence::SetInterlacing);
tag_parser!(set_linefeed, "[20l", AnsiSequence::SetLineFeedMode);
tag_parser!(set_cursorkey, "[?1l", AnsiSequence::SetCursorKeyToCursor);
tag_parser!(set_vt52, "[?2l", AnsiSequence::SetVT52);
tag_parser!(set_col80, "[?3l", AnsiSequence::SetCol80);
tag_parser!(set_jump_scroll, "[?4l", AnsiSequence::SetJumpScrolling);
tag_parser!(set_normal_video, "[?5l", AnsiSequence::SetNormalVideo);
tag_parser!(set_origin_abs, "[?6l", AnsiSequence::SetOriginAbsolute);
tag_parser!(reset_auto_wrap, "[?7l", AnsiSequence::ResetAutoWrap);
tag_parser!(reset_auto_repeat, "[?8l", AnsiSequence::ResetAutoRepeat);
tag_parser!(reset_interlacing, "[?9l", AnsiSequence::ResetInterlacing);

tag_parser!(set_alternate_keypad, "=", AnsiSequence::SetAlternateKeypad);
tag_parser!(set_numeric_keypad, ">", AnsiSequence::SetNumericKeypad);
tag_parser!(set_uk_g0, "(A", AnsiSequence::SetUKG0);
tag_parser!(set_uk_g1, ")A", AnsiSequence::SetUKG1);
tag_parser!(set_us_g0, "(B", AnsiSequence::SetUSG0);
tag_parser!(set_us_g1, ")B", AnsiSequence::SetUSG1);
tag_parser!(set_g0_special, "(0", AnsiSequence::SetG0SpecialChars);
tag_parser!(set_g1_special, ")0", AnsiSequence::SetG1SpecialChars);
tag_parser!(set_g0_alternate, "(1", AnsiSequence::SetG0AlternateChar);
tag_parser!(set_g1_alternate, ")1", AnsiSequence::SetG1AlternateChar);
tag_parser!(set_g0_graph, "(2", AnsiSequence::SetG0AltAndSpecialGraph);
tag_parser!(set_g1_graph, ")2", AnsiSequence::SetG1AltAndSpecialGraph);
tag_parser!(set_single_shift2, "N", AnsiSequence::SetSingleShift2);
tag_parser!(set_single_shift3, "O", AnsiSequence::SetSingleShift3);

/*
named!(
    combined<&str, AnsiSequence>,
    alt!(
          escape
        | cursor_pos
        | cursor_up
        | cursor_down
        | cursor_forward
        | cursor_backward
        | cursor_save
        | cursor_restore
        | erase_display
        | erase_line
        | graphics_mode
        | set_mode
        | reset_mode
        | hide_cursor
        | show_cursor
        | cursor_to_app
        | set_new_line_mode
        | set_col_132
        | set_smooth_scroll
        | set_reverse_video
        | set_origin_rel
        | set_auto_wrap
        | set_auto_repeat
        | set_interlacing
        | set_linefeed
        | set_cursorkey
        | set_vt52
        | set_col80
        | set_jump_scroll
        | set_normal_video
        | set_origin_abs
        | reset_auto_wrap
        | reset_auto_repeat
        | reset_interlacing
        | set_top_and_bottom
        | set_alternate_keypad
        | set_numeric_keypad
        | set_uk_g0
        | set_uk_g1
        | set_us_g0
        | set_us_g1
        | set_g0_special
        | set_g1_special
        | set_g0_alternate
        | set_g1_alternate
        | set_g0_graph
        | set_g1_graph
        | set_single_shift2
        | set_single_shift3
    )
);
*/

static PARSERS: [for<'r> fn(&'r str) -> Result<(&'r str, AnsiSequence), nom::Err<Error<&'r str>>>; 49] = [
    escape,
    cursor_pos,
    cursor_up,
    cursor_down,
    cursor_forward,
    cursor_backward,
    cursor_save,
    cursor_restore,
    erase_display,
    erase_line,
    graphics_mode,
    set_mode,
    reset_mode,
    hide_cursor,
    show_cursor,
    cursor_to_app,
    set_new_line_mode,
    set_col_132,
    set_smooth_scroll,
    set_reverse_video,
    set_origin_rel,
    set_auto_wrap,
    set_auto_repeat,
    set_interlacing,
    set_linefeed,
    set_cursorkey,
    set_vt52,
    set_col80,
    set_jump_scroll,
    set_normal_video,
    set_origin_abs,
    reset_auto_wrap,
    reset_auto_repeat,
    reset_interlacing,
    set_top_and_bottom,
    set_alternate_keypad,
    set_numeric_keypad,
    set_uk_g0,
    set_uk_g1,
    set_us_g0,
    set_us_g1,
    set_g0_special,
    set_g1_special,
    set_g0_alternate,
    set_g1_alternate,
    set_g0_graph,
    set_g1_graph,
    set_single_shift2,
    set_single_shift3,
];

fn combined(i: &str) -> IResult<&str, AnsiSequence> {
    // Note: This seems like it's poorly optimized but it actually runs pretty quickly compared to
    // attempts at making optimized versions
    for item in &PARSERS {
        if let Ok(result) = item(i) {
            return Ok(result);
        }
    }
    Err(err_hack(i))
}

/*
named!(
    pub parse_escape<&str, AnsiSequence>,
    do_parse!(
        tag!("\u{1b}")    >>
        seq: combined     >>
        (seq)
    )
);
*/

pub fn parse_escape(i: &str) -> IResult<&str, AnsiSequence> {
    nom::sequence::preceded(tag("\u{1b}"), combined)(i)
}
