use std::vec;

use aoc_lib::{create_solution, Input, Solver};

pub struct Day9 {}

create_solution!(
  out_1 => i64;
  out_2 => i64;
  year => 2023;
  day => 9;
  sol_1 => |input: Input| {
    let report = load_report(input);
    let s = report.into_iter().map(|xs| next_value(xs)).sum();
    Some(s)
  };
  sol_2 => |input: Input| {
    let report = load_report(input);
    let s = report.into_iter().map(|xs| next_value(xs.into_iter().rev().collect())).sum();
    Some(s)
  };
);

fn mk_step(xs: Vec<i64>) -> Vec<i64> {
  xs.iter()
    .zip(xs.get(1..).unwrap().iter())
    .map(|(a, b)| b - a)
    .collect()
}

fn next_value(seq: Vec<i64>) -> i64 {
  let mut steps: Vec<Vec<i64>> = vec![seq.clone()];
  let mut init_seq = seq;
  while !init_seq.into_iter().all(|x| x == 0) {
    let step = steps.last().map(|xs| mk_step(xs.clone())).unwrap();
    steps.push(step.clone());
    init_seq = step;
  }

  let init: Vec<i64> = [0_i64]
    .iter()
    .cycle()
    .take(steps.len() + 1)
    .cloned()
    .collect();

  let ss = steps.into_iter().rev().fold(init, |xs, ys| {
    let mut acc = vec![];
    let new_item = xs.last().unwrap() + ys.last().unwrap();
    acc.push(new_item);
    acc
  });
  ss.last().unwrap().clone()
}

fn load_report(input: Input) -> Vec<Vec<i64>> {
  input
    .lines
    .iter()
    .map(|l| {
      l.trim()
        .split_whitespace()
        .map(|x| x.parse::<i64>().unwrap())
        .collect()
    })
    .collect()
}

#[cfg(test)]

mod tests {
  use super::*;

  #[test]
  fn test_pred_history() {
    let seq = vec![10, 13, 16, 21, 30, 45];
    let exp = 68;
    assert_eq!(next_value(seq.clone()), exp);
    assert_eq!(next_value(seq.into_iter().rev().collect()), 5);
  }
}
