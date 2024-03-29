use std::collections::HashSet;

use aoc_lib::Solver;

pub struct Day3 {
    priorities: Vec<char>,
}

impl Default for Day3 {
    fn default() -> Self {
        let mut y = Vec::from_iter('a'..='z');
        y.append(&mut Vec::from_iter('A'..='Z'));
        Day3 { priorities: y }
    }
}

impl Day3 {
    fn priority(&self, z: &char) -> Option<usize> {
        self.priorities.iter().position(|v| v == z).map(|x| x + 1)
    }

    fn common_char(&self, strs: Vec<&str>) -> HashSet<char> {
        strs.into_iter()
            .map(|x| x.chars().collect::<HashSet<_>>())
            .reduce(|a, b| a.intersection(&b).copied().collect::<HashSet<_>>())
            .unwrap_or_default()
    }
}

impl Solver for Day3 {
    type OutputPart1 = usize;
    type OutputPart2 = usize;
    fn year() -> u16 {
        2022
    }

    fn day() -> u8 {
        3
    }

    fn solution_part2(input: aoc_lib::Input) -> Option<Self::OutputPart2> {
        let day3 = Day3::default();
        input
            .lines
            .chunks_exact(3)
            .flat_map(|xs| day3.common_char(xs.iter().map(|x| x.as_str()).collect::<Vec<&str>>()))
            .flat_map(|x| day3.priority(&x))
            .reduce(|a, b| a + b)
    }

    fn solution_part1(input: aoc_lib::Input) -> Option<Self::OutputPart1> {
        let day3 = Day3::default();
        input
            .lines
            .iter()
            .flat_map(|x| {
                let split = x.split_at(x.len() / 2);
                day3.common_char(vec![split.0, split.1])
            })
            .flat_map(|z| day3.priority(&z))
            .reduce(|a, b| a + b)
    }
}
