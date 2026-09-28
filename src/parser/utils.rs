use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace0;
use winnow::token::literal;

pub fn ws<'a, F, O>(mut inner: F) -> impl FnMut(&mut &'a str) -> ModalResult<O>
where
    F: FnMut(&mut &'a str) -> ModalResult<O>,
{
    move |input: &mut &'a str| {
        let _ = multispace0.parse_next(input)?;
        inner.parse_next(input)
    }
}

pub fn keyword<'a>(kw: &'static str) -> impl FnMut(&mut &'a str) -> ModalResult<&'a str> {
    move |input: &mut &'a str| {
        let checkpoint = *input;
        let _ = multispace0.parse_next(input)?;
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
        let _ = multispace0.parse_next(input)?;
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
