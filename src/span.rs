use std::ops::Deref;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    #[inline]
    pub fn start(self) -> usize {
        self.start as usize
    }

    #[inline]
    pub fn end(self) -> usize {
        self.end as usize
    }

    #[inline]
    pub fn empty() -> Span {
        Span { start: 0, end: 0 }
    }

    #[inline]
    pub fn is_empty(self) -> bool {
        self.start == self.end
    }

    #[inline]
    pub fn to(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }
}

impl From<std::ops::Range<usize>> for Span {
    #[inline]
    fn from(value: std::ops::Range<usize>) -> Self {
        Span {
            start: value.start as u32,
            end: value.end as u32,
        }
    }
}

impl From<std::ops::Range<u32>> for Span {
    #[inline]
    fn from(value: std::ops::Range<u32>) -> Self {
        Span {
            start: value.start,
            end: value.end,
        }
    }
}

impl From<Span> for std::ops::Range<usize> {
    #[inline]
    fn from(value: Span) -> Self {
        value.start as usize..value.end as usize
    }
}

impl<T: std::ops::Index<std::ops::Range<usize>>> std::ops::Index<Span> for [T] {
    type Output = <[T] as std::ops::Index<std::ops::Range<usize>>>::Output;

    #[inline]
    fn index(&self, index: Span) -> &Self::Output {
        self.index(std::ops::Range::from(index))
    }
}

impl std::ops::Index<Span> for str {
    type Output = <str as std::ops::Index<std::ops::Range<usize>>>::Output;

    #[inline]
    fn index(&self, index: Span) -> &Self::Output {
        self.index(std::ops::Range::from(index))
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Spanned<T> {
    inner: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    #[inline]
    pub fn new(inner: T, span: impl Into<Span>) -> Self {
        Self {
            inner,
            span: span.into(),
        }
    }

    #[inline]
    pub fn empty(inner: T) -> Self {
        Self::new(inner, Span::empty())
    }

    #[inline]
    pub fn into_inner(self) -> T {
        self.inner
    }

    #[inline]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            inner: f(self.inner),
            span: self.span,
        }
    }

    #[inline]
    pub fn map_into<U: From<T>>(self) -> Spanned<U> {
        Spanned {
            inner: self.inner.into(),
            span: self.span,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
