use aoc_lib::{Input, Solver};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, multispace1, space1, u128, u8},
    combinator::{map, map_opt},
    multi::separated_list1,
    sequence::{pair, preceded, separated_pair},
    IResult,
};

#[derive(Debug, Default, Clone)]
pub struct Day11 {
    monkey_holdings: Vec<Monkey>,
}

impl Day11 {
    fn monkey_business(&self) -> Option<u128> {
        let mut xs = self
            .monkey_holdings
            .iter()
            .map(|m| m.inspected_items)
            .collect::<Vec<_>>();
        xs.sort_by(|a, b| b.cmp(a));
        if xs.len() > 2 {
            Some(xs[0] as u128 * xs[1] as u128)
        } else {
            None
        }
    }
}

impl Solver for Day11 {
    fn day() -> u8 {
        11
    }
    type OutputPart1 = u128;
    type OutputPart2 = u128;

    fn solution_part1(input: aoc_lib::Input) -> Option<Self::OutputPart1> {
        let mut d = Day11::load_notes(input);
        for _ in 1..=20 {
            d.play_round(false, 1);
        }

        d.monkey_business()
    }

    fn solution_part2(input: aoc_lib::Input) -> Option<Self::OutputPart2> {
        let mut d = Day11::load_notes(input);
        let common_divisor = d
            .monkey_holdings
            .iter()
            .map(|x| x.test.divisible_by as u32)
            .product::<u32>();
        for _ in 1..=10000 {
            d.play_round(true, common_divisor);
        }

        d.monkey_business()
    }
}

fn monkey_id_parser<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, usize> {
    preceded(pair(tag("Monkey"), space1), map(u8, |x| x as usize))
}

fn items_parser<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, Vec<Item>> {
    preceded(
        pair(multispace1, pair(tag("Starting items:"), space1)),
        separated_list1(tag(", "), map(u128, |x| Item { worry_level: x })),
    )
}

fn operation_parser<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, Operation> {
    preceded(
        pair(space1, tag("Operation: new = old ")),
        map_opt(
            separated_pair(alt((tag("*"), tag("+"))), space1, alt((tag("old"), digit1))),
            |(c, v)| match c {
                "*" => {
                    if v == "old" {
                        Some(Operation::Pow(2))
                    } else {
                        Some(Operation::Multiply(v.parse().unwrap_or(1)))
                    }
                }
                "+" => {
                    if v == "old" {
                        Some(Operation::Multiply(2))
                    } else {
                        Some(Operation::Plus(v.parse().unwrap_or(0)))
                    }
                }
                _ => None,
            },
        ),
    )
}

fn test_div_parser<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, u8> {
    preceded(pair(space1, tag("Test: divisible by ")), u8)
}

fn test_true_parser<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, usize> {
    preceded(
        pair(space1, tag("If true: throw to monkey ")),
        map(u8, |x| x as usize),
    )
}

fn test_false_parser<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, usize> {
    preceded(
        pair(space1, tag("If false: throw to monkey ")),
        map(u8, |x| x as usize),
    )
}

fn monkey_parser(is: [String; 6]) -> Monkey {
    Monkey {
        id: monkey_id_parser()(&is[0])
            .expect("could not parse monkey id")
            .1,
        operation: operation_parser()(&is[2])
            .expect("could not parse operation")
            .1,
        test: Test {
            divisible_by: test_div_parser()(&is[3])
                .expect("could not parse divisible by")
                .1,
            if_true: test_true_parser()(&is[4])
                .expect("could not parse if true")
                .1,
            if_false: test_false_parser()(&is[5])
                .expect("could not parse if false")
                .1,
        },
        inspected_items: 0,
        current_holdings: items_parser()(&is[1])
            .expect("could not parse starting items")
            .1,
    }
}

impl Day11 {
    fn load_notes(input: Input) -> Self {
        let monkey_holdings = input
            .lines
            .split(|x| x.is_empty())
            .map(|ys| {
                let mut slice: [String; 6] = Default::default();
                ys.iter().enumerate().for_each(|(i, s)| {
                    slice[i] = s.clone();
                });
                slice
            })
            .map(monkey_parser)
            .collect::<Vec<_>>();
        Day11 { monkey_holdings }
    }

    fn play_round(&mut self, more_worried: bool, common_div: u32) -> &mut Self {
        self.monkey_holdings = self.monkey_holdings.iter().enumerate().fold(
            self.monkey_holdings.clone(),
            |mut acc, (index, _)| {
                let mut monkey = acc[index].clone();

                monkey.current_holdings.clone().into_iter().for_each(|x| {
                    monkey.inspected_items += 1;

                    let mut wl = match monkey.operation {
                        Operation::Plus(v) => x.worry_level.checked_add(v as u128).unwrap_or(0),
                        Operation::Multiply(v) => x.worry_level.checked_mul(v as u128).unwrap_or(1),
                        Operation::Pow(v) => x.worry_level.checked_pow(v as u32).unwrap_or(1),
                    };
                    if !more_worried {
                        wl /= 3;
                    } else {
                        wl = wl.checked_rem(common_div as u128).unwrap_or(wl);
                    }
                    if wl
                        .checked_rem(monkey.test.divisible_by as u128)
                        .expect("div by 0")
                        == 0
                    {
                        acc[monkey.test.if_true]
                            .current_holdings
                            .push(Item { worry_level: wl });
                    } else {
                        acc[monkey.test.if_false]
                            .current_holdings
                            .push(Item { worry_level: wl });
                    }
                });

                monkey.current_holdings = vec![];
                acc[index] = monkey;
                acc
            },
        );
        self
    }
}

#[derive(Debug, Clone)]
struct Monkey {
    id: usize,
    operation: Operation,
    test: Test,
    inspected_items: u64,
    current_holdings: Vec<Item>,
}

#[derive(Debug, Clone, Copy)]
enum Operation {
    Plus(u8),
    Multiply(u8),
    Pow(u8),
}

#[derive(Debug, Clone, Copy)]
struct Test {
    divisible_by: u8,
    if_true: usize,
    if_false: usize,
}

#[derive(Debug, Clone, Copy)]
struct Item {
    worry_level: u128,
}
