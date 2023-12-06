use std::{collections::HashSet, str::Chars};

use aoc_lib::{create_solution, Input, Solver};

#[derive(Debug, Clone)]
pub struct Day3 {
  engine_schematic: EngineSchematic,
  part_numbers: Vec<u64>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
struct Point {
  x: usize,
  y: usize,
}

impl Point {
  fn neighbors(&self) -> HashSet<Self> {
    HashSet::from([
      Point {
        x: self.x.saturating_sub(1),
        y: self.y.saturating_sub(1),
      },
      Point {
        x: self.x.saturating_sub(1),
        y: self.y,
      },
      Point {
        x: self.x.saturating_sub(1),
        y: self.y + 1,
      },
      Point {
        x: self.x,
        y: self.y.saturating_sub(1),
      },
      Point {
        x: self.x,
        y: self.y + 1,
      },
      Point {
        x: self.x + 1,
        y: self.y.saturating_sub(1),
      },
      Point {
        x: self.x + 1,
        y: self.y,
      },
      Point {
        x: self.x + 1,
        y: self.y + 1,
      },
    ])
  }
}

#[derive(Debug, Clone)]
struct Digit {
  digit: char,
  point: Point,
}

#[derive(Debug, Clone)]
struct Number {
  number: u32,
  points: HashSet<Point>,
}

impl Number {
  fn is_adjacent_to_special_char(
    &self,
    special_char_indices: HashSet<Point>,
  ) -> bool {
    self.points.iter().any(|point| {
      point
        .neighbors()
        .intersection(&special_char_indices)
        .count()
        > 0
    })
  }
}

#[derive(Debug, Clone)]
struct SpecialChar {
  char: char,
  point: Point,
}

impl SpecialChar {
  fn gear_parts(&self, numbers: Vec<Number>) -> Option<[Number; 2]> {
    if self.char == '*' {
      let neighbors = self.point.neighbors();
      // check if neighbors contains two numbers
      let xs = numbers
        .iter()
        .filter(|n| n.points.intersection(&neighbors).count() > 0)
        .enumerate()
        .take_while(|(i, _)| i <= &2)
        .map(|x| x.1)
        .collect::<Vec<_>>();
      if xs.len() == 2 {
        Some([xs[0].clone(), xs[1].clone()])
      } else {
        None
      }
    } else {
      None
    }
  }
}

#[derive(Debug, Clone)]
struct EngineSchematic {
  special_chars: Vec<SpecialChar>,
  digits: Vec<Digit>,
  numbers: Vec<Number>,

  special_char_indices: HashSet<Point>,
}

impl FromIterator<Vec<char>> for EngineSchematic {
  fn from_iter<T: IntoIterator<Item = Vec<char>>>(iter: T) -> Self {
    let mut special_chars = Vec::new();
    let mut digits = Vec::new();
    let mut numbers = Vec::new();
    let mut current_number_string = String::new();
    let mut current_number = Number {
      number: 0,
      points: HashSet::new(),
    };

    for (x, row) in iter.into_iter().enumerate() {
      for (y, c) in row.into_iter().enumerate() {
        if c.is_digit(10) {
          digits.push(Digit {
            digit: c,
            point: Point { x, y },
          });
          current_number.points.insert(Point { x, y });
          current_number_string.push(c);
        } else {
          if !current_number_string.is_empty() {
            numbers.push(Number {
              number: current_number_string.parse::<u32>().unwrap(),
              points: current_number.points,
            });
            current_number = Number {
              number: 0,
              points: HashSet::new(),
            };
            current_number_string.clear();
          }

          if c != '.' {
            special_chars.push(SpecialChar {
              char: c,
              point: Point { x, y },
            });
          }
        }
      }
      if current_number_string.len() > 0 {
        numbers.push(Number {
          number: current_number_string.parse::<u32>().unwrap(),
          points: current_number.points,
        });
        current_number = Number {
          number: 0,
          points: HashSet::new(),
        };
        current_number_string.clear();
      }
    }
    Self {
      special_chars: special_chars.clone(),
      digits,
      numbers,
      special_char_indices: special_chars
        .iter()
        .map(|sc| sc.point.clone())
        .collect::<HashSet<Point>>(),
    }
  }
}

impl Day3 {
  fn part_numbers(&self) -> Vec<u32> {
    self
      .engine_schematic
      .numbers
      .iter()
      .filter(|n| {
        n.is_adjacent_to_special_char(
          self.engine_schematic.special_char_indices.clone(),
        )
      })
      .map(|n| n.number)
      .collect::<Vec<u32>>()
  }

  fn gear_ratios(&self) -> Vec<u64> {
    self
      .engine_schematic
      .special_chars
      .iter()
      .map(|sc| {
        sc.gear_parts(self.engine_schematic.numbers.clone())
          .map(|x| x[0].number as u64 * x[1].number as u64)
          .unwrap_or_default()
      })
      .collect::<Vec<_>>()
  }

  fn load_engine_schematic(input: Input) -> Self {
    Day3 {
      engine_schematic: input
        .lines
        .into_iter()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<EngineSchematic>(),
      part_numbers: Vec::new(),
    }
  }
}

create_solution!(
  out_1 => u64;
  out_2 => u64;
  year => 2023;
  day => 3;
  sol_1 => |input: Input| {
    let day3 = Day3::load_engine_schematic(input);
    Some(day3.part_numbers().iter().map(|x| *x as u64).sum())
  };
  sol_2 => |input: Input| {
    let day3 = Day3::load_engine_schematic(input);
    Some(day3.gear_ratios().iter().sum())
  };
);
