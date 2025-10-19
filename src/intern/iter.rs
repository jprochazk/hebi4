use core::{iter, slice};

use super::{Interner, Span};

/// An iterator over all interned strings from an [`Interner`].
#[derive(Debug, Clone)]
pub struct Iter<'a> {
    pub(super) spans: slice::Iter<'a, Span>,
    pub(super) span_to_string: &'a str,
}

impl<'a, Id, S> IntoIterator for &'a Interner<Id, S> {
    type Item = &'a str;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            spans: self.symbol_to_span.iter(),
            span_to_string: &self.span_to_string,
        }
    }
}

// The same impls as iter::Map
impl<'a> Iterator for Iter<'a> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<&'a str> {
        let span = self.spans.next()?;
        Some(unsafe { index_unchecked!(self.span_to_string, span.range()) })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.spans.size_hint()
    }
}

impl<'a> DoubleEndedIterator for Iter<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a str> {
        let span = self.spans.next_back()?;
        Some(unsafe { index_unchecked!(self.span_to_string, span.range()) })
    }
}

impl ExactSizeIterator for Iter<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.spans.len()
    }
}

impl iter::FusedIterator for Iter<'_> {}
