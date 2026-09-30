use std::cell::Cell;
use winnow::ModalResult;
use winnow::Parser;
use winnow::ascii::multispace0;
use winnow::token::literal;

use crate::ast::Span;

thread_local! {
    static ROOT_PTR: Cell<usize> = const { Cell::new(0) };
}

pub fn set_root_source(source: &str) {
    ROOT_PTR.with(|p| p.set(source.as_ptr() as usize));
}

pub fn get_span(slice: &str) -> Span {
    ROOT_PTR.with(|p| {
        let root = p.get();
        if root == 0 || (slice.as_ptr() as usize) < root {
            Span::new(0, slice.len())
        } else {
            let start = slice.as_ptr() as usize - root;
            Span::new(start, start + slice.len())
        }
    })
}

pub fn get_span_between(start_slice: &str, end_slice: &str) -> Span {
    ROOT_PTR.with(|p| {
        let root = p.get();
        if root == 0 || (start_slice.as_ptr() as usize) < root {
            let len = start_slice.len().saturating_sub(end_slice.len());
            Span::new(0, len)
        } else {
            let start = start_slice.as_ptr() as usize - root;
            let end = end_slice.as_ptr() as usize - root;
            Span::new(start, end)
        }
    })
}

pub fn get_span_between_ptrs(start_offset: usize, end_slice: &str) -> Span {
    ROOT_PTR.with(|p| {
        let root = p.get();
        if root == 0 || (end_slice.as_ptr() as usize) < root {
            Span::new(start_offset, start_offset)
        } else {
            let end = end_slice.as_ptr() as usize - root;
            Span::new(start_offset, end)
        }
    })
}

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
