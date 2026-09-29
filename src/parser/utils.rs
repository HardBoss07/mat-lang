use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace0;
use winnow::token::literal;

pub fn skip_ws_and_comments(input: &mut &str) -> ModalResult<()> {
    loop {
        let _ = multispace0.parse_next(input)?;
        if input.starts_with("//") {
            if let Some(pos) = input.find('\n') {
                *input = &input[pos + 1..];
            } else {
                *input = "";
            }
        } else if input.starts_with("/*") {
            *input = &input[2..];
            let mut depth = 1;

            while depth > 0 && !input.is_empty() {
                if input.starts_with("/*") {
                    depth += 1;
                    *input = &input[2..];
                } else if input.starts_with("*/") {
                    depth -= 1;
                    *input = &input[2..];
                } else {
                    let ch_len = input.chars().next().map_or(1, |c| c.len_utf8());
                    *input = &input[ch_len..];
                }
            }

            // Unterminated block comment
            if depth > 0 {
                return Err(winnow::error::ErrMode::Backtrack(
                    winnow::error::ContextError::default(),
                ));
            }
        } else {
            break;
        }
    }
    Ok(())
}

pub fn ws<'a, F, O>(mut inner: F) -> impl FnMut(&mut &'a str) -> ModalResult<O>
where
    F: FnMut(&mut &'a str) -> ModalResult<O>,
{
    move |input: &mut &'a str| {
        let _ = skip_ws_and_comments(input)?;
        inner.parse_next(input)
    }
}

pub fn keyword<'a>(kw: &'static str) -> impl FnMut(&mut &'a str) -> ModalResult<&'a str> {
    move |input: &mut &'a str| {
        let checkpoint = *input;
        let _ = skip_ws_and_comments(input)?;
        if !input.starts_with(kw) {
            *input = checkpoint;
            return Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ));
        }
        let after = &input[kw.len()..];
        if after
            .chars()
            .next()
            .map_or(false, |c| c.is_alphanumeric() || c == '_')
        {
            *input = checkpoint;
            return Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ));
        }
        let matched = literal(kw).parse_next(input)?;
        Ok(matched)
    }
}

pub fn symbol<'a>(sym: &'static str) -> impl FnMut(&mut &'a str) -> ModalResult<&'a str> {
    move |input: &mut &'a str| {
        let checkpoint = *input;
        let _ = skip_ws_and_comments(input)?;
        if !input.starts_with(sym) {
            *input = checkpoint;
            return Err(winnow::error::ErrMode::Backtrack(
                winnow::error::ContextError::default(),
            ));
        }
        let matched = literal(sym).parse_next(input)?;
        Ok(matched)
    }
}
