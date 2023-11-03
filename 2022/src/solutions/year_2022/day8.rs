use aoc_lib::{Input, Solver};

impl Solver for Day8 {
  fn day() -> u8 {
      8
  }

  fn solution_part1(input: Input) -> Option<Self::OutputPart1> {
      let count = Day8::new(input)
          .reduce_grid(
              |xs| {
                  xs.iter()
                      .map(|x| x.0)
                      .reduce(|a, b| a || b)
                      .unwrap_or(false)
              },
              true,
          )
          .iter()
          .filter(|x| **x)
          .count();

      Some(count)
  }

  fn solution_part2(input: Input) -> Option<Self::OutputPart2> {
      let count = Day8::new(input)
          .reduce_grid(
              |xs| {
                  xs.iter()
                      .map(|x| x.1.len())
                      .reduce(|a, b| a * b)
                      .unwrap_or(1)
              },
              1,
          )
          .iter()
          .copied()
          .max();

      count
  }

  type OutputPart1 = usize;
  type OutputPart2 = usize;
}

#[derive(Debug)]
struct TreesInDirection {
    left: Option<Vec<usize>>,
    right: Option<Vec<usize>>,
    top: Option<Vec<usize>>,
    bottom: Option<Vec<usize>>,
}
pub struct Day8 {
    grid_expanded: Vec<Vec<(usize, TreesInDirection)>>,
}

impl Day8 {
    fn new(input: Input) -> Self {
        let grid: Vec<Vec<usize>> = input
            .lines
            .iter()
            .map(|x| {
                x.chars()
                    .map(|x| x.to_digit(10).unwrap_or(0) as usize)
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        let grid_expanded: Vec<Vec<(usize, TreesInDirection)>> = grid
            .iter()
            .enumerate()
            .map(|(row, ys)| {
                ys.iter()
                    .enumerate()
                    .map(|(col, c)| {
                        let left = col.checked_sub(1).and_then(|co| {
                            let ys = grid[row].iter().enumerate().take_while(|(i, _)| i <= &co);
                            if ys.clone().count() == 0 {
                                None // leftmost
                            } else {
                                Some({
                                    let mut ys = ys.map(|(_, x)| *x).collect::<Vec<_>>();
                                    ys.reverse();
                                    ys
                                })
                            }
                        });
                        let right = grid.get(row).and_then(|ys| {
                            let zs = ys.iter().enumerate().skip_while(|(i, _)| i <= &col);
                            if zs.clone().count() == 0 {
                                None // rightmost
                            } else {
                                Some(zs.map(|(_, x)| *x).collect())
                            }
                        });
                        let top = {
                            let rn = row.checked_sub(1);
                            if rn.is_none() {
                                None // topmost
                            } else {
                                rn.map(|t| {
                                    let l = grid.iter().take(t + 1);
                                    let mut xs =
                                        l.map(|cs| *cs.get(col).unwrap_or(&0)).collect::<Vec<_>>();
                                    xs.reverse();
                                    xs
                                })
                            }
                        };
                        let bottom = {
                            let l = grid.iter().skip(row + 1);
                            if l.len() == 0 {
                                None // bottommost
                            } else {
                                Some(l.map(|cs| *cs.get(col).unwrap_or(&0)).collect())
                            }
                        };
                        (
                            *c,
                            TreesInDirection {
                                left,
                                right,
                                top,
                                bottom,
                            },
                        )
                    })
                    .collect()
            })
            .collect();

        Day8 { grid_expanded }
    }

    fn reduce_grid<F, T: Copy>(&self, f: F, default_value: T) -> Vec<T>
    where
        F: Fn(Vec<(bool, Vec<usize>)>) -> T,
    {
        self.grid_expanded
            .iter()
            .flat_map(|x| {
                x.iter().map(|(c, ts)| {
                    let TreesInDirection {
                        left,
                        right,
                        top,
                        bottom,
                    } = ts;
                    if left.is_none() || right.is_none() || top.is_none() || bottom.is_none() {
                        default_value
                    } else {
                        f(vec![
                            check_sides(c, left.clone().unwrap()),
                            check_sides(c, right.clone().unwrap()),
                            check_sides(c, top.clone().unwrap()),
                            check_sides(c, bottom.clone().unwrap()),
                        ])
                    }
                })
            })
            .collect::<Vec<_>>()
    }
}

fn check_sides(c: &usize, xs: Vec<usize>) -> (bool, Vec<usize>) {
    let (b, xs, _) = xs.iter().fold(
        (true, vec![], false),
        |(b, mut acc, is_done): (bool, Vec<usize>, bool), x| {
            if c > x && b {
                if !is_done {
                    acc.push(*x);
                }
                (true, acc, false)
            } else {
                if !is_done {
                    acc.push(*x);
                }
                (false, acc, true)
            }
        },
    );
    (b, xs)
}
