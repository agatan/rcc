#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone, Copy)]
pub enum Location {
    At(usize),
    Span(usize, usize),
}

pub fn describe_location(
    f: &mut std::fmt::Formatter,
    source: &str,
    location: Location,
) -> std::fmt::Result {
    let start = match location {
        Location::At(s) => s,
        Location::Span(s, _) => s,
    };
    if source.len() <= start {
        write!(f, " caused at end of input")
    } else {
        let line_number = source[..start].lines().count();
        let current_line_start = source[..start].rfind('\n').unwrap_or(0);
        let current_line_end = source[start..]
            .find(|c| c == '\r' || c == '\n')
            .map(|i| i + start)
            .unwrap_or(source.len());
        let current_line = &source[current_line_start..current_line_end];
        let column = start - current_line_start + 1;
        writeln!(f, " caused at {}:{}:", line_number, column)?;
        writeln!(f, "  | {}", current_line)?;
        match location {
            Location::At(_) => writeln!(f, "  . {:>width$}", "^", width = column),
            Location::Span(_, end) => writeln!(
                f,
                "  . {:>width$}{:~>trailing$}",
                "^",
                "",
                width = column,
                trailing = std::cmp::min(current_line_end, end) - column
            ),
        }
    }
}
