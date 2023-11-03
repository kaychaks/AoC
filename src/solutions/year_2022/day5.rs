use aoc_lib::{Input, Solver};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, char, space1, u8},
    combinator::{eof, map},
    multi::{count, many0},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult,
};

#[derive(Debug, Clone)]
struct Crate(char);
#[derive(Debug)]
struct Stack(Vec<Crate>);

type Count = u8;
type Source = u8;
type Destination = u8;
#[derive(Debug)]
struct MoveCommand((Count, Source, Destination));
#[derive(Debug)]
pub struct Day5 {
    stacks: Vec<Stack>,
    move_commands: Vec<MoveCommand>,
}

fn transpose(v: Vec<Stack>) -> Vec<Stack> {
    assert!(!v.is_empty());
    let len = v[0].0.len();
    let mut iters = v.into_iter().map(|n| n.0.into_iter()).collect::<Vec<_>>();
    (0..len)
        .map(|_| {
            iters
                .iter_mut()
                .map(|n| n.next().unwrap())
                .filter(|x| x.0 != ' ')
                .collect::<Vec<_>>()
        })
        .map(Stack)
        .collect()
}

impl Day5 {
    fn output(&self) -> String {
        self.stacks
            .iter()
            .map(|x| x.0.last().unwrap().0)
            .into_iter()
            .collect()
    }

    fn new(input: Input) -> Self {
        let (stack_strs, move_stmts): (Vec<&String>, Vec<&String>) =
            input.lines.iter().partition(|x| !x.starts_with("move"));

        let mut stack_strs = stack_strs
            .iter()
            .filter(|x| !x.is_empty())
            .collect::<Vec<_>>();
        stack_strs.reverse();

        let stacks = stack_strs
            .split_first()
            .map(|x| x.1)
            .expect("could not get the stack elements string")
            .iter()
            .map(|a| {
                Stack(
                    Day5::stack_element_parser(a)
                        .expect("could not parse stack elements")
                        .1,
                )
            })
            .collect::<Vec<_>>();

        let move_commands = move_stmts
            .iter()
            .map(|s| {
                Day5::move_cmd_parser(s)
                    .expect("could not parse move statements")
                    .1
            })
            .collect();

        Day5 {
            stacks: transpose(stacks),
            move_commands,
        }
    }

    fn stack_element_parser(input: &str) -> IResult<&str, Vec<Crate>> {
        many0(terminated(
            map(
                alt((
                    delimited(char('['), anychar, char(']')),
                    map(count(char(' '), 3), |_x| ' '),
                )),
                Crate,
            ),
            alt((char(' '), map(eof, |_| ' '))),
        ))(input)
    }

    fn move_cmd_parser(input: &str) -> IResult<&str, MoveCommand> {
        map(
            pair(
                delimited(
                    terminated(tag("move"), space1),
                    u8,
                    preceded(space1, terminated(tag("from"), space1)),
                ),
                separated_pair(u8, preceded(space1, terminated(tag("to"), space1)), u8),
            ),
            |(count, (source, destination))| MoveCommand((count, source, destination)),
        )(input)
    }

    fn interpret_move_commands(&mut self, maintain_order: bool) -> &mut Self {
        self.move_commands.iter().for_each(|m| {
            let MoveCommand((count, source, destination)) = *m;

            let mut els = vec![];
            for _ in 0..count {
                let x = self.stacks[usize::from(source - 1)]
                    .0
                    .pop()
                    .expect("could not pop");
                els.push(x)
            }
            if maintain_order {
                els.reverse();
            }
            self.stacks[usize::from(destination - 1)].0.append(&mut els);
        });
        self
    }
}

impl Solver for Day5 {
    type OutputPart1 = String;
    type OutputPart2 = String;

    fn day() -> u8 {
        5
    }

    fn solution_part1(input: aoc_lib::Input) -> Option<Self::OutputPart1> {
        Some(Day5::new(input).interpret_move_commands(false).output())
    }

    fn solution_part2(input: aoc_lib::Input) -> Option<Self::OutputPart2> {
        Some(Day5::new(input).interpret_move_commands(true).output())
    }
}
