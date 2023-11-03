use std::collections::{HashSet, VecDeque};

use aoc_lib::{Day, Input, Solver};

pub struct Day6 {
    seq_checker: VecDeque<char>,
    chars_processed: u16,
    distinct_char_count: usize,
}

impl Day6 {
    fn new(count: usize) -> Self {
        Day6 {
            seq_checker: VecDeque::with_capacity(count),
            chars_processed: 0,
            distinct_char_count: count,
        }
    }
    fn update_chars_processed(&mut self) -> &mut Self {
        self.chars_processed += 1;
        self
    }

    fn check_seq_marker(&mut self) -> Option<&mut Self> {
        let s: HashSet<&char> = HashSet::from_iter(self.seq_checker.iter());
        let ll = s.len();
        if ll == self.distinct_char_count {
            Some(self)
        } else {
            None
        }
    }

    fn marker_detected(&mut self, c: char) -> Option<&mut Self> {
        let len = self.seq_checker.len();
        match len {
            x if x < self.distinct_char_count => {
                self.seq_checker.push_back(c);
                self.update_chars_processed();
                self.check_seq_marker()
            }
            _ => {
                self.seq_checker.pop_front();
                self.seq_checker.push_back(c);
                self.update_chars_processed();
                self.check_seq_marker()
            }
        }
    }

    fn common_sol(&mut self, input: Input) -> &mut Self {
        let line = input.lines.first().expect("first line is imp");
        let mut xs: Vec<char> = line.chars().rev().collect();
        let mut c = xs.pop().expect("char from the stream");
        while self.marker_detected(c).is_none() {
            c = xs.pop().expect("char from the stream");
        }
        self
    }
}

impl Solver for Day6 {
    fn solution_part1(input: Input) -> Result<Self::OutputPart1, Self::Error> {
        Ok(Day6::new(4).common_sol(input).chars_processed)
    }
    fn solution_part2(input: aoc_lib::Input) -> Result<Self::OutputPart2, Self::Error> {
        Ok(Day6::new(14).common_sol(input).chars_processed)
    }
    fn day() -> aoc_lib::Day {
        Day::try_from(6).expect("could not parse day")
    }
    type Error = String;
    type OutputPart1 = u16;
    type OutputPart2 = u16;
}
