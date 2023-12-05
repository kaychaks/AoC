use std::{collections::HashMap, str::Chars};

use aoc_lib::{create_solution, Input, Solver};

#[derive(Debug)]
pub struct Day1 {}

impl Day1 {
  fn calculate_calibration(input: Input) -> u64 {
    input.lines.into_iter().fold(0, |acc, line| {
      let mut chars = line.chars();
      let first_digit = chars.find(|c| c.is_digit(10)).unwrap_or_else(|| '0');
      let last_digit = chars
        .rfind(|c| c.is_digit(10))
        .unwrap_or_else(|| first_digit);
      acc
        + format!("{}{}", first_digit, last_digit)
          .parse::<u64>()
          .unwrap_or(0)
    })
  }

  fn is_word_digit(str: String) -> Option<u8> {
    let m: HashMap<String, u8> = HashMap::from_iter([
      ("one".to_string(), 1),
      ("two".to_string(), 2),
      ("three".to_string(), 3),
      ("four".to_string(), 4),
      ("five".to_string(), 5),
      ("six".to_string(), 6),
      ("seven".to_string(), 7),
      ("eight".to_string(), 8),
      ("nine".to_string(), 9),
    ]);
    m.keys()
      .find(|k| str.contains(*k))
      .and_then(|k| m.get(k).copied())
  }

  fn get_first_digit(chars: &mut Chars) -> (String, String) {
    let mut digit = "".to_string();
    (
      chars
        .skip_while(|c| {
          if c.is_digit(10) {
            digit = c.to_string();
            false
          } else if let Some(d) = Day1::is_word_digit(format!("{}{}", digit, c))
          {
            digit = d.to_string();
            false
          } else {
            digit.push(*c);
            true
          }
        })
        .collect(),
      digit,
    )
  }

  fn get_last_digit(chars: &mut Chars, first_digit: String) -> String {
    let mut digits = vec![first_digit];
    let mut curr_digit_word = "".to_string();

    chars.for_each(|c| {
      if c.is_digit(10) {
        curr_digit_word = "".to_string();
        digits.push(c.to_string());
      } else if let Some(d) =
        Day1::is_word_digit(format!("{}{}", curr_digit_word, c))
      {
        curr_digit_word = c.to_string();
        digits.push(d.to_string());
      } else {
        curr_digit_word.push(c);
      }
    });

    digits.last().unwrap().to_owned()
  }

  fn calculate_calibration_with_words(input: Input) -> u64 {
    input.lines.into_iter().fold(0, |acc, line| {
      let (rest, first_digit) = Day1::get_first_digit(&mut line.chars());
      let last_digit =
        Day1::get_last_digit(&mut rest.chars(), first_digit.clone());

      acc
        + format!("{}{}", first_digit, last_digit)
          .parse::<u64>()
          .unwrap_or(0)
    })
  }
}

create_solution!(
  out_1 => u64;
  out_2 => u64;
  year => 2023;
  day => 1;
  sol_1 => |input: Input| {
    Some(Day1::calculate_calibration(input))
  };
  sol_2 => |input: Input| {
    Some(Day1::calculate_calibration_with_words(input))
  };
);
