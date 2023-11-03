use std::{collections::HashMap, fmt::Debug};

use aoc_lib::Solver;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alphanumeric1, char, space1, u32},
    combinator::{map, rest},
    error::ParseError,
    sequence::{pair, preceded, separated_pair},
    IResult, Parser,
};

impl Solver for Day7 {
    fn day() -> u8 {
        7
    }
    fn year() -> u16 {
        2022
    }

    type OutputPart1 = u32;
    type OutputPart2 = u32;

    fn solution_part1(input: aoc_lib::Input) -> Option<Self::OutputPart1> {
        let builder = FSBuilder::default().interpret(&mut input.lines.into_iter());
        let xs: u32 = builder
            .fs
            .iter()
            .filter(|(_, s)| *s <= &100_000)
            .map(|x| x.1)
            .sum();

        Some(xs)
    }

    fn solution_part2(input: aoc_lib::Input) -> Option<Self::OutputPart2> {
        let builder = FSBuilder::default().interpret(&mut input.lines.into_iter());
        let k: Vec<String> = vec![];
        let total = builder.fs.get(&k).unwrap_or(&0);
        let min_needed = 30_000_000 - (70_000_000 - total);
        let mut xs = builder.fs.iter().map(|x| x.1).collect::<Vec<_>>();
        xs.sort();
        xs.iter().find(|&&x| x >= &min_needed).copied().copied()
    }
}

pub struct Day7 {}

enum CDTo {
    Root,
    Parent,
    Dir(String),
}

enum Op {
    CD(CDTo),
    LS,
}
enum LSOut {
    Dir(String),
    File(u32, String),
}

enum ParseResult {
    Op(Op),
    LSOut(LSOut),
}
#[derive(Debug, Clone, Default)]
struct FSBuilder {
    current_path: Vec<String>,
    fs: HashMap<Vec<String>, u32>,
}

fn subsequence<T: Copy>(xs: Vec<T>) -> Vec<Vec<T>> {
    let count = xs.len();
    let mut ret: Vec<Vec<T>> = vec![];
    for i in (0..=count).rev() {
        ret.push(xs.iter().copied().take(i).collect());
    }
    ret
}

impl FSBuilder {
    fn interpret(mut self, lines: &mut dyn Iterator<Item = String>) -> Self {
        while let Some(line) = lines.next() {
            let (_, parse_result) =
                alt((cmd_parser(), ls_parser(), dir_parser(), file_parser()))(&line)
                    .expect("could not parse line");
            match parse_result {
                ParseResult::Op(op) => match op {
                    Op::CD(to) => match to {
                        CDTo::Root => self.current_path = vec![],
                        CDTo::Parent => {
                            self.current_path.pop();
                        }
                        CDTo::Dir(d) => {
                            self.current_path.push(d);
                        }
                    },
                    Op::LS => (),
                },
                ParseResult::LSOut(ls_out) => match ls_out {
                    LSOut::Dir(_) => {}
                    LSOut::File(size, _) => {
                        let y = self.current_path.iter().map(|x| x.as_str()).collect();
                        let ms = subsequence(y);
                        let ms = ms.iter().map(|xs| (xs, size)).collect::<Vec<_>>();
                        let new_m: HashMap<Vec<String>, u32> =
                            ms.iter().fold(HashMap::new(), |mut acc, (xs, sz)| {
                                acc.insert(xs.iter().map(|x| x.to_string()).collect(), *sz);
                                acc
                            });

                        new_m.iter().for_each(|(k, v)| {
                            self.fs
                                .entry(k.clone())
                                .and_modify(|e| *e += v)
                                .or_insert(*v);
                        })
                    }
                },
            }
        }
        self
    }
}

// only parse the cd commands and ignore the rest
fn user_cmd_init_parser<'a, O, E, F>(p: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    F: Parser<&'a str, O, E>,
{
    preceded(pair(char('$'), space1), p)
}

fn cmd_parser<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, ParseResult> {
    user_cmd_init_parser(preceded(
        pair(tag("cd"), space1),
        map(alt((alphanumeric1, tag(".."), tag("/"))), |x| match x {
            ".." => ParseResult::Op(Op::CD(CDTo::Parent)),
            "/" => ParseResult::Op(Op::CD(CDTo::Root)),
            _ => ParseResult::Op(Op::CD(CDTo::Dir(x.to_string()))),
        }),
    ))
}

fn ls_parser<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, ParseResult> {
    user_cmd_init_parser(map(tag("ls"), |_| ParseResult::Op(Op::LS)))
}

fn dir_parser<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, ParseResult> {
    preceded(
        pair(tag("dir"), space1),
        map(alphanumeric1, |x: &str| {
            ParseResult::LSOut(LSOut::Dir(x.to_string()))
        }),
    )
}

fn file_parser<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, ParseResult> {
    map(separated_pair(u32, space1, rest), |(a, s): (u32, &str)| {
        ParseResult::LSOut(LSOut::File(a, s.to_string()))
    })
}
