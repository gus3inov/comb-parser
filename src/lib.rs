#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

#[allow(dead_code)]
fn build_parser(expected: &'static str) -> impl Fn(&str) -> ParseResult<String> {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], next.to_string())),
        _ => Err(input),
    }
}

#[test]
fn literal_parser() {
    let parse_joe = build_parser("Hello, Joe!");

    assert_eq!(
        Ok(("", "Hello, Joe!".to_string())),
        parse_joe("Hello, Joe!")
    );

    assert_eq!(
        Ok((" Hello, Mike!", "Hello, Joe!".to_string())),
        parse_joe("Hello, Joe! Hello, Mike!")
    );

    assert_eq!(Err("Hello, Mike!"), parse_joe("Hello, Mike!"));
}

fn parse_identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(&input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();

    Ok((&input[next_index..], matched))
}

#[test]
fn identifier_parser() {
    assert_eq!(
        Ok(("", "i-am-an-identifier".to_string())),
        parse_identifier("i-am-an-identifier")
    );

    assert_eq!(
        Ok((" entirely an identifier", "not".to_string())),
        parse_identifier("not entirely an identifier")
    );

    assert_eq!(
        Err("!not at all an identifier"),
        parse_identifier("!not at all an identifier")
    );
}

fn pair<P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Fn(&str) -> ParseResult<(R1, R2)>
where
    P1: Fn(&str) -> ParseResult<R1>,
    P2: Fn(&str) -> ParseResult<R2>,
{
    move |input| match parser1(input) {
        Ok((next_input, result1)) => match parser2(next_input) {
            Ok((final_input, result2)) => Ok((final_input, (result1, result2))),
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}

#[test]
fn pair_combinator() {
    let parse_open_tag = build_parser("<");
    let tag_opener = pair(parse_open_tag, parse_identifier);

    assert_eq!(
        Ok((" />", ("<".to_string(), "my-element".to_string()))),
        tag_opener("<my-element />")
    );

    assert_eq!(Err(" oops />"), tag_opener(" oops />"));

    assert_eq!(Err("!oops"), tag_opener("<!oops"));
}

fn map<P, F, A, B>(parser: P, map_fn: F) -> impl Fn(&str) -> ParseResult<B>
where
    P: Fn(&str) -> ParseResult<A>,
    F: Fn(A) -> B,
{
    move |input| parser(input).map(|(next_input, result)| (next_input, map_fn(result)))
}
