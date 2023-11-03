use std::fmt::Debug;

use crate::{Day, Input};

pub trait Solver {
    type OutputPart1: Debug;
    type OutputPart2: Debug;
    fn day() -> u8;
    fn year() -> u16;
    fn input() -> Input {
        let day = Day::try_from((Self::day(), Self::year())).expect("could not parse day");
        Input::new(day)
    }
    fn solution_part1(input: Input) -> Option<Self::OutputPart1>;
    fn solution_part2(input: Input) -> Option<Self::OutputPart2>;
    fn solve_part1() -> Result<Self::OutputPart1, String> {
        Self::solution_part1(Self::input()).ok_or_else(|| "could not solve".to_string())
    }
    fn solve_part2() -> Result<Self::OutputPart2, String> {
        Self::solution_part2(Self::input()).ok_or_else(|| "could not solve".to_string())
    }
}
