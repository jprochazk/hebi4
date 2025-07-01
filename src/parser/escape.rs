use std::borrow::Cow;

#[inline]
pub fn unescape(s: &str) -> Result<Cow<'_, str>, InvalidEscape> {
    let result = if s.chars().any(|c| c == '\\') {
        actually_unescape(s).map(Cow::Owned)
    } else {
        Ok(Cow::Borrowed(s))
    };

    result.map_err(|i| {
        // count the number of utf8 bytes
        let pos = s.chars().take(i).map(|c| c.len_utf8()).sum();
        InvalidEscape { pos }
    })
}

pub struct InvalidEscape {
    pub pos: usize,
}

// In case of error returns the character index of the invalid escape
fn actually_unescape(s: &str) -> Result<String, usize> {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().enumerate();
    while let Some((i, prefix)) = chars.next() {
        if prefix == '\\' {
            if let Some((_, ch)) = chars.next() {
                let escape = match ch {
                    'a' => Some('\u{07}'),
                    'b' => Some('\u{08}'),
                    'v' => Some('\u{0B}'),
                    'f' => Some('\u{0C}'),
                    'n' => Some('\n'),
                    'r' => Some('\r'),
                    't' => Some('\t'),
                    '\'' => Some('\''),
                    '"' => Some('"'),
                    '\\' => Some('\\'),
                    'e' | 'E' => Some('\u{1B}'),
                    'x' => Some(parse_hex_code(&mut chars).ok_or(i)?),
                    'u' => Some(parse_unicode(&mut chars).ok_or(i)?),
                    _ => None,
                };
                match escape {
                    Some(esc) => {
                        out.push(esc);
                    }
                    None => return Err(i),
                }
            }
        } else {
            out.push(prefix);
        }
    }
    Ok(out)
}

fn parse_hex_code<I>(chars: &mut I) -> Option<char>
where
    I: Iterator<Item = (usize, char)>,
{
    let digits = [
        u8::try_from(chars.next()?.1).ok()?,
        u8::try_from(chars.next()?.1).ok()?,
    ];
    let digits = std::str::from_utf8(&digits[..]).ok()?;
    let c = u32::from_str_radix(digits, 16).ok()?;
    char::from_u32(c)
}

// Adapted from https://docs.rs/snailquote/0.3.0/x86_64-pc-windows-msvc/src/snailquote/lib.rs.html.
fn parse_unicode<I>(chars: &mut I) -> Option<char>
where
    I: Iterator<Item = (usize, char)>,
{
    match chars.next() {
        Some((_, '{')) => {}
        _ => {
            return None;
        }
    }

    let unicode_seq: String = chars
        .take_while(|(_, c)| *c != '}')
        .map(|(_, c)| c)
        .collect();

    u32::from_str_radix(&unicode_seq, 16)
        .ok()
        .and_then(char::from_u32)
}
