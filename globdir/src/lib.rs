use proc_macro2::{Delimiter, Group, Punct, Spacing};
use proc_macro2::{Literal, Span, TokenStream, TokenTree, token_stream::IntoIter};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::str::FromStr as _;

#[proc_macro]
pub fn globdir(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    match try_globdir(input.clone()) {
        Ok(token_stream) => token_stream.into(),
        Err(err) => {
            let mut input = input;
            input.extend(err.to_token_stream());
            input.into()
        }
    }
}

fn invalid_arguments() -> Error {
    Error {
        message: "invalid arguments, expected: (filename, fn)".into(),
        span: Span::call_site(),
    }
}

fn try_globdir(input: TokenStream) -> Result<TokenStream> {
    let mut input = input.into_iter();
    let pattern = parse_str_literal(&mut input)?;
    expect_comma(&mut input)?;
    // just assume the remainder is a callable thing.
    let fn_thing = input.collect::<TokenStream>();

    let mut fn_thing_wrapper = TokenStream::new();
    fn_thing_wrapper.extend(TokenStream::from_str("const _: fn(&'static str) = ").unwrap());
    fn_thing_wrapper.extend([TokenTree::Group(Group::new(
        Delimiter::Brace,
        fn_thing.clone(),
    ))]);
    fn_thing_wrapper.extend([TokenTree::Punct(Punct::new(';', Spacing::Alone))]);

    let mut output = String::new();

    let crate_root = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let resolved_pattern = format!("{crate_root}/{pattern}");

    for path in
        glob::glob(&resolved_pattern).map_err(|err| error(err.to_string(), Span::call_site()))?
    {
        let path = path.map_err(|err| error(err.to_string(), Span::call_site()))?;
        write!(
            &mut output,
            "#[test] fn {stem}() {{ let __callable = ({fn_thing}); __callable({path:?}); }}",
            stem = path.file_stem().unwrap().to_str().unwrap(),
            path = path.to_str().unwrap(),
        )
        .unwrap();
    }

    fn_thing_wrapper.extend(
        TokenStream::from_str(&output).map_err(|err| error(err.to_string(), Span::call_site()))?,
    );
    Ok(fn_thing_wrapper)
}

fn expect_comma(input: &mut IntoIter) -> Result<()> {
    let tt = input.next().ok_or_else(invalid_arguments)?;

    let punct = match tt {
        TokenTree::Punct(punct) => punct,
        _ => return Err(invalid_arguments()),
    };

    if punct.as_char() != ',' {
        return Err(error("expected comma".into(), punct.span()));
    }

    Ok(())
}

fn parse_str_literal(input: &mut IntoIter) -> Result<String> {
    let tt = input.next().ok_or_else(invalid_arguments)?;

    let lit = match tt {
        TokenTree::Literal(lit) => lit,
        _ => return Err(invalid_arguments()),
    };

    let span = lit.span();
    let lit = parse_literal(&lit).map_err(|err| error(err.to_string(), span))?;
    String::from_utf8(lit).map_err(|err| error(err.to_string(), span))
}

struct Error {
    message: String,
    span: Span,
}

fn error(message: String, span: Span) -> Error {
    Error { message, span }
}

impl Error {
    fn to_token_stream(self) -> TokenStream {
        let token_stream =
            TokenStream::from_str(&format!("compile_error!(r##\"{}\"##)", self.message)).unwrap();

        let span = Span::call_site().located_at(self.span);
        token_stream
            .into_iter()
            .map(|mut tt| {
                tt.set_span(span.clone());
                tt
            })
            .collect::<TokenStream>()
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

macro_rules! unexpected_content {
    () => {
        "expected a string literal"
    };
}

fn parse_literal(literal: &Literal) -> Result<Vec<u8>, &'static str> {
    let s = literal.to_string();
    let s = s.as_bytes();
    match s[0] {
        b'"' => Ok(parse_cooked_content(&s)),
        b'r' => Ok(parse_raw_content(&s[1..])),
        b'b' => match s[1] {
            b'"' => Ok(parse_cooked_content(&s[1..])),
            b'r' => Ok(parse_raw_content(&s[2..])),
            _ => Err(unexpected_content!()),
        },
        _ => Err(unexpected_content!()),
    }
}

fn all_pounds(bytes: &[u8]) -> bool {
    bytes.iter().all(|b| *b == b'#')
}

/// Parses raw string / bytes content after `r` prefix.
fn parse_raw_content(s: &[u8]) -> Vec<u8> {
    let q_start = s.iter().position(|b| *b == b'"').unwrap();
    let q_end = s.iter().rposition(|b| *b == b'"').unwrap();
    assert!(all_pounds(&s[0..q_start]));
    assert!(all_pounds(&s[q_end + 1..q_end + q_start + 1]));
    Vec::from(&s[q_start + 1..q_end])
}

/// Parses the cooked string / bytes content within quotes.
fn parse_cooked_content(mut s: &[u8]) -> Vec<u8> {
    s = &s[1..s.iter().rposition(|b| *b == b'"').unwrap()];
    let mut result = Vec::new();
    while !s.is_empty() {
        match s[0] {
            b'\\' => {}
            b'\r' => {
                assert_eq!(s[1], b'\n');
                result.push(b'\n');
                s = &s[2..];
                continue;
            }
            b => {
                result.push(b);
                s = &s[1..];
                continue;
            }
        }
        let b = s[1];
        s = &s[2..];
        match b {
            b'x' => {
                let (b, rest) = backslash_x(&s);
                result.push(b);
                s = rest;
            }
            b'u' => {
                let (c, rest) = backslash_u(&s);
                result.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes());
                s = rest;
            }
            b'n' => result.push(b'\n'),
            b'r' => result.push(b'\r'),
            b't' => result.push(b'\t'),
            b'\\' => result.push(b'\\'),
            b'0' => result.push(b'\0'),
            b'\'' => result.push(b'\''),
            b'"' => result.push(b'"'),
            b'\r' | b'\n' => {
                let next = s.iter().position(|b| {
                    let ch = char::from_u32(u32::from(*b)).unwrap();
                    !ch.is_whitespace()
                });
                match next {
                    Some(pos) => s = &s[pos..],
                    None => s = b"",
                }
            }
            b => panic!("unexpected byte {:?} after \\", b),
        }
    }
    result
}

fn backslash_x(s: &[u8]) -> (u8, &[u8]) {
    let ch = hex_to_u8(s[0]) * 0x10 + hex_to_u8(s[1]);
    (ch, &s[2..])
}

fn hex_to_u8(b: u8) -> u8 {
    match b {
        b'0'..=b'9' => b - b'0',
        b'a'..=b'f' => b - b'a' + 10,
        b'A'..=b'F' => b - b'A' + 10,
        _ => unreachable!("unexpected non-hex character {:?} after \\x", b),
    }
}

fn backslash_u(s: &[u8]) -> (char, &[u8]) {
    assert_eq!(s[0], b'{');
    let end = s[1..].iter().position(|b| *b == b'}').unwrap();
    let mut ch = 0;
    for b in &s[1..=end] {
        ch *= 0x10;
        ch += u32::from(hex_to_u8(*b));
    }
    (char::from_u32(ch).unwrap(), &s[end + 2..])
}
