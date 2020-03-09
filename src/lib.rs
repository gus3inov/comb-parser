use std::ops::Range;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

enum List<A> {
    Cons(A, Box<List<A>>),
    Nil,
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, pref_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pref_fn))
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

fn range<'a, P, A>(parser: P, range_bound: Range<usize>) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    let box_rb = Box::new(range_bound);
    move |mut input: &'a str| {
        let mut result = Vec::new();
        let rc_rb = box_rb.clone();

        for _ in *rc_rb {
            if let Ok((next_input, first_item)) = parser.parse(input) {
                input = next_input;
                result.push(first_item);
            } else {
                return Err(input);
            }
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    range(parser, 0..1)
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

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    range(parser, 0..0)
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

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }

        Err(input)
    }
}

#[test]
fn predicate_combinator() {
    let parser = pred(any_char, |c| *c == 'o');

    assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
    assert_eq!(Err("lol"), parser.parse("lol"));
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        build_parser("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            build_parser("\""),
        ),
    )
    .map(|chars| chars.into_iter().collect())
}

#[test]
fn quoted_string_parser() {
    assert_eq!(
        Ok(("", "Hello, Joe!".to_string())),
        quoted_string().parse("\"Hello, Joe!\"")
    )
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(parse_identifier, right(build_parser("="), quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

#[test]
fn attribute_parser() {
    assert_eq!(
        Ok((
            "",
            vec![
                ("one".to_string(), "1".to_string()),
                ("two".to_string(), "2".to_string())
            ]
        )),
        attributes().parse(" one=\"1\" two=\"2\"")
    );
}

fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(build_parser("<"), pair(parse_identifier, attributes()))
}

fn single_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), build_parser("/>")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

#[test]
fn single_element_parser() {
    assert_eq!(
        Ok((
            "",
            Element {
                name: "div".to_string(),
                attributes: vec![("class".to_string(), "float".to_string())],
                children: vec![]
            }
        )),
        single_element().parse("<div class=\"float\"/>")
    );
}
