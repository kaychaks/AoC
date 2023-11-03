use std::{collections::HashSet, ops::Range};

use aoc_lib::{Input, Solver};

pub struct Day4 {}

impl Day4 {
    fn range(x: &str) -> HashSet<usize> {
        x.split_once('-')
            .map(|(s, e)| {
                Range {
                    start: s.parse::<usize>().unwrap(),
                    end: e.parse::<usize>().unwrap() + 1,
                }
                .into_iter()
                .collect::<HashSet<_>>()
            })
            .unwrap_or_default()
    }

    fn is_contained((a, b): (&str, &str)) -> bool {
        let ra = Day4::range(a);
        let rb = Day4::range(b);
        ra.is_subset(&rb) || ra.is_superset(&rb)
    }

    fn has_overlap((a, b): (&str, &str)) -> bool {
        let ra = Day4::range(a);
        let rb = Day4::range(b);
        ra.intersection(&rb).count() > 0
    }

    fn common_sol<T>(input: Input, check: T) -> usize
    where
        T: Copy + Fn((&str, &str)) -> bool,
    {
        input
            .lines
            .iter()
            .map(|x| x.split_once(',').map(check).unwrap_or_default())
            .filter(|x| *x)
            .count()
    }
}

impl Solver for Day4 {
    type OutputPart1 = usize;
    type OutputPart2 = usize;
    fn year() -> u16 {
        2022
    }

    fn day() -> u8 {
        4
    }

    fn solution_part1(input: aoc_lib::Input) -> Option<Self::OutputPart1> {
        Some(Day4::common_sol(input, Day4::is_contained))
    }

    fn solution_part2(input: aoc_lib::Input) -> Option<Self::OutputPart2> {
        Some(Day4::common_sol(input, Day4::has_overlap))
    }
}
