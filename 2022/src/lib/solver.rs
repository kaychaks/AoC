use std::fmt::Debug;

use crate::{Day, Input, Part};

pub trait Solver {
    type OutputPart1: Debug;
    type OutputPart2: Debug;
    type Error;
    fn day() -> Day;
    fn input(p: Part) -> Input {
        Input::new(Self::day(), p)
    }
    fn solution_part1(input: Input) -> Result<Self::OutputPart1, Self::Error>;
    fn solution_part2(input: Input) -> Result<Self::OutputPart2, Self::Error>;
    fn solve_part1() -> Result<Self::OutputPart1, Self::Error> {
        Self::solution_part1(Self::input(Part::Part1))
    }
    fn solve_part2() -> Result<Self::OutputPart2, Self::Error> {
        Self::solution_part2(Self::input(Part::Part2))
    }
}
