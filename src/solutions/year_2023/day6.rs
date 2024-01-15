use std::ops::Sub;
use aoc_lib::{create_solution, Input, Solver};

pub struct Day6 {}

fn solve_x(x: usize, y: usize) -> Vec<usize> {
  (1..x).fold(vec![], |mut acc, a| {
    if a * (x - a) > y {
      acc.push(a)
    }
    acc
  })
}

fn solve_xs(inp: Vec<(usize, usize)>) -> Vec<usize> {
  inp.into_iter().map(|(x, y)| solve_x(x, y).len()).collect()
}

fn solve_eq(x: usize, y: usize) -> usize {
  let s = ((x as f64).powf(2.0) - (4.0 * y as f64)).abs().sqrt();
  let root1: f64 = (x as f64 - s) / 2.0;
  let root2: f64 = (x as f64 + s) / 2.0;

  if root1.floor() == root1 {
    (root2 - (root1 + 1.0)) as usize + 1
  } else if root2.ceil() == root2 {
    ((root2 - 1.0) - root1) as usize + 1
  } else {
    (root2 - root1) as usize + 1
  }
}

fn solve_eqs(inp: Vec<(usize, usize)>) -> usize {
  inp.into_iter().map(|(x, y)| solve_eq(x, y)).product()
}

create_solution!(
  out_1 => usize;
  out_2 => usize;
  year => 2023;
  day => 6;
  sol_1 => |input: Input| {
    let inp = vec![
      (40, 219),
      (81, 1012),
      (77, 1365),
      (72, 1089),
    ];
    // Some(solve_xs(inp).iter().product())
    Some(solve_eqs(inp))
  };
  sol_2 => |input: Input| {
    let inp = vec![
      (40817772, 219101213651089),
    ];
    // Some(solve_xs(inp).iter().product())
    Some(solve_eqs(inp))
  };
);

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve_xs() {
    let inp = vec![
      (7, 9),
      (15, 40),
      (30, 200),
    ];

    let act: usize = solve_xs(inp).iter().product();
    assert_eq!(act, 288)
  }
}