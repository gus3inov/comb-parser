use std::ops::RangeFrom;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

#[allow(dead_code)]
fn build_parser<'a>(expected: &'static str) -> impl Parser<'a, String> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], next.to_string())),
        _ => Err(input),
    }
}

#[test]
fn literal_parser() {
    let parse_joe = build_parser("Hello, Joe!");

    assert_eq!(
        Ok(("", "Hello, Joe!".to_string())),
        parse_joe.parse("Hello, Joe!")
    );

    assert_eq!(
        Ok((" Hello, Mike!", "Hello, Joe!".to_string())),
        parse_joe.parse("Hello, Joe! Hello, Mike!")
    );

    assert_eq!(Err("Hello, Mike!"), parse_joe.parse("Hello, Mike!"));
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

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(move |(final_input, result2)| (final_input, (result1, result2)))
        })
    }
}

#[test]
fn pair_combinator() {
    let parse_open_tag = build_parser("<");
    let tag_opener = right(parse_open_tag, parse_identifier);

    assert_eq!(
        Ok((" />", "my-element".to_string())),
        tag_opener.parse("<my-element />")
    );

    assert_eq!(Err(" oops />"), tag_opener.parse(" oops />"));

    assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    range(parser, 1..)
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    range(parser, 0..)
}

fn range<'a, 'b, P, A>(parser: P, range_bound: RangeFrom<usize>) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |input: &'a str| {
        let mut result = Vec::new();

        match input.get(*range_bound) {
            Some(next) => {
                if let Ok((next_input, first_item)) = parser.parse(next) {
                    input = next_input;
                    result.push(first_item);
                } else {
                    return Err(input);
                }
            }
            _ => return Err(input),
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

#[test]
fn one_or_more_combinator() {
    let parser = one_or_more(build_parser("ha"));

    assert_eq!(
        Ok((
            "",
            vec!["ha".to_string(), "ha".to_string(), "ha".to_string()]
        )),
        parser.parse("hahaha")
    );
    assert_eq!(Err("ahah"), parser.parse("ahah"));
    assert_eq!(Err(""), parser.parse(""));
}

#[test]
fn zero_or_more_combinator() {
    let parser = zero_or_more(build_parser("ha"));
    assert_eq!(
        Ok((
            "",
            vec!["ha".to_string(), "ha".to_string(), "ha".to_string()]
        )),
        parser.parse("hahaha")
    );
    assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
    assert_eq!(Ok(("", vec![])), parser.parse(""));
}
