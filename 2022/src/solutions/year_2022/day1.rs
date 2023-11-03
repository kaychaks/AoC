use aoc_lib::{Input, Solver};

#[derive(Debug)]
pub(crate) struct Day1 {}

impl Day1 {
    fn get_sorted(input: Input) -> Vec<u32> {
        let mut x = input.lines.iter().fold(vec![0], |mut acc, x| {
            if x.is_empty() {
                acc.push(0_u32);
                acc
            } else {
                let l = acc.len();
                acc[l - 1] += x.parse::<u32>().unwrap_or(0);
                acc
            }
        });
        x.sort_by(|a, b| b.cmp(a));
        x
    }
}

impl Solver for Day1 {
    type OutputPart1 = u32;
    type OutputPart2 = u32;
    fn day() -> u8 {
        1
    }
    fn solution_part1(input: Input) -> Option<Self::OutputPart1> {
        let x = Day1::get_sorted(input);
        x.first().copied()
    }
    fn solution_part2(input: Input) -> Option<Self::OutputPart2> {
        let x = Day1::get_sorted(input);
        x.split_at(3).0.iter().copied().reduce(|a, b| a + b)
    }
}
