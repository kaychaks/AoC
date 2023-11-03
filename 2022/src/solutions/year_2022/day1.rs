use aoc_lib::{Day, Input, Solver};

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
    type Error = String;
    fn day() -> aoc_lib::Day {
        Day::try_from(1).expect("invalid day")
    }
    fn solution_part1(input: Input) -> Result<Self::OutputPart1, Self::Error> {
        let x = Day1::get_sorted(input);
        x.first()
            .copied()
            .ok_or_else(|| "no solution found".to_string())
    }
    fn solution_part2(input: Input) -> Result<Self::OutputPart2, Self::Error> {
        let x = Day1::get_sorted(input);
        x.split_at(3)
            .0
            .iter()
            .copied()
            .reduce(|a, b| a + b)
            .ok_or_else(|| "no solution found".to_string())
    }
}
