use std::collections::HashSet;

use aoc_lib::{create_solution, Input, Solver};
pub struct Day10 {}

create_solution!(
  out_1 => usize;
  out_2 => usize;
  year => 2023;
  day => 10;
  sol_1 => |input: Input| {
    let loop_path = load_loop_path(input);
    Some((loop_path.len()).div_ceil(2))
  };

  sol_2 => |input: Input| {
    let loop_path = load_loop_path(input.clone());

    let report = load_report(input);
    let inside_points = inside_points(report.clone(), loop_path.clone());
    Some(inside_points.len())
  };
);

type Pos = (usize, usize);
type Map = Vec<Vec<Pipe>>;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Clone)]
enum Direction {
  North,
  East,
  South,
  West,
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Clone)]
enum Pipe {
  Vertical,
  Horizontal,
  NEBend,
  NWBend,
  SWBend,
  SEBend,
  Ground,
  Start,
}

impl Pipe {
  fn next_direction(
    &self,
    next_pipe: Self,
    incoming_dir: Direction,
  ) -> Option<Direction> {
    next_pipe.outgoing_dir(incoming_dir)
  }

  fn outgoing_dir(&self, incoming_dir: Direction) -> Option<Direction> {
    match (self, incoming_dir) {
      (Pipe::Vertical, Direction::North) => Some(Direction::North),
      (Pipe::Vertical, Direction::South) => Some(Direction::South),
      (Pipe::Horizontal, Direction::East) => Some(Direction::East),
      (Pipe::Horizontal, Direction::West) => Some(Direction::West),
      (Pipe::NEBend, Direction::North) => Some(Direction::West),
      (Pipe::NEBend, Direction::East) => Some(Direction::South),
      (Pipe::NWBend, Direction::North) => Some(Direction::East),
      (Pipe::NWBend, Direction::West) => Some(Direction::South),
      (Pipe::SWBend, Direction::South) => Some(Direction::East),
      (Pipe::SWBend, Direction::West) => Some(Direction::North),
      (Pipe::SEBend, Direction::South) => Some(Direction::West),
      (Pipe::SEBend, Direction::East) => Some(Direction::North),
      _ => None,
    }
  }

  fn get_loop_start_direction(
    start_pos: Pos,
    report: Map,
  ) -> Option<Direction> {
    let north_pos = next_pipe_pos(report.clone(), start_pos, Direction::South);
    let south_pos = next_pipe_pos(report.clone(), start_pos, Direction::North);
    let east_pos = next_pipe_pos(report.clone(), start_pos, Direction::West);
    let west_pos = next_pipe_pos(report.clone(), start_pos, Direction::East);

    if north_pos.is_some()
      && [Pipe::Vertical, Pipe::SWBend, Pipe::SEBend]
        .contains(&north_pos.unwrap().0)
    {
      Some(Direction::South)
    } else if south_pos.is_some()
      && [Pipe::Vertical, Pipe::NEBend, Pipe::NWBend]
        .contains(&south_pos.unwrap().0)
    {
      Some(Direction::North)
    } else if east_pos.is_some()
      && [Pipe::Horizontal, Pipe::NWBend, Pipe::SWBend]
        .contains(&east_pos.unwrap().0)
    {
      Some(Direction::West)
    } else if west_pos.is_some()
      && [Pipe::Horizontal, Pipe::NEBend, Pipe::SEBend]
        .contains(&west_pos.unwrap().0)
    {
      Some(Direction::East)
    } else {
      None
    }
  }
}

impl From<char> for Pipe {
  fn from(c: char) -> Self {
    match c {
      '|' => Pipe::Vertical,
      '-' => Pipe::Horizontal,
      'L' => Pipe::NEBend,
      'J' => Pipe::NWBend,
      '7' => Pipe::SWBend,
      'F' => Pipe::SEBend,
      '.' => Pipe::Ground,
      'S' => Pipe::Start,
      _ => panic!("Invalid pipe char: {}", c),
    }
  }
}

fn next_pipe_pos(
  report: Map,
  current_pos: Pos,
  incoming_dir: Direction,
) -> Option<(Pipe, Pos)> {
  let next_pos = match incoming_dir {
    Direction::North => (current_pos.0 + 1, current_pos.1),
    Direction::East if current_pos.1 > 0 => (current_pos.0, current_pos.1 - 1),
    Direction::South if current_pos.0 > 0 => (current_pos.0 - 1, current_pos.1),
    Direction::West => (current_pos.0, current_pos.1 + 1),
    _ => return None,
  };
  let next_pipe = report.get(next_pos.0)?.get(next_pos.1).cloned()?;
  Some((next_pipe, next_pos))
}

fn start_pos(report: Map) -> Pos {
  report
    .iter()
    .enumerate()
    .find_map(|(i, xs)| {
      xs.iter().enumerate().find_map(|(j, x)| {
        if x == &Pipe::Start {
          Some((i, j))
        } else {
          None
        }
      })
    })
    .unwrap()
}

fn do_loop(
  report: Map,
  current_pipe: Pipe,
  current_pipe_pos: Pos,
  dir: Direction,
  path: HashSet<Pos>,
) -> HashSet<Pos> {
  let np = next_pipe_pos(report.clone(), current_pipe_pos.clone(), dir.clone());
  if let Some((next_pipe, next_pos)) = np {
    if next_pipe == Pipe::Start {
      path
    } else {
      let next_dir =
        current_pipe.next_direction(next_pipe.clone(), dir).unwrap();
      let mut path = path;
      path.insert(next_pos.clone());

      do_loop(report.clone(), next_pipe.clone(), next_pos, next_dir, path)
    }
  } else {
    path
  }
}

fn load_report(input: Input) -> Map {
  input
    .lines
    .iter()
    .map(|l| l.trim().chars().map(|x| Pipe::from(x)).collect())
    .collect()
}

fn load_loop_path(input: Input) -> HashSet<Pos> {
  let report = load_report(input);
  let start_pos = start_pos(report.clone());

  let start_dir = Pipe::get_loop_start_direction(start_pos, report.clone());

  do_loop(
    report.clone(),
    Pipe::Start,
    start_pos,
    start_dir.unwrap(),
    HashSet::from_iter(vec![start_pos.clone()]),
  )
}

fn inside_points(report: Map, loop_path: HashSet<Pos>) -> HashSet<Pos> {
  let mut enclosed: HashSet<Pos> = HashSet::new();
  let mut inside_boundary = false;
  report.iter().enumerate().for_each(|(i, xs)| {
    inside_boundary = false;
    xs.iter().enumerate().for_each(|(j, p)| {
      if loop_path.contains(&(i, j)) {
        inside_boundary = inside_boundary
          ^ [Pipe::Vertical, Pipe::SEBend, Pipe::SWBend].contains(&p);
      } else {
        if inside_boundary {
          enclosed.insert((i, j));
        }
      }
    })
  });
  enclosed
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_load_report() {
    let lines = vec![
      "7-F7-".to_string(),
      ".FJ|7".to_string(),
      "SJLL7".to_string(),
      "|F--J".to_string(),
      "LJ.LJ".to_string(),
    ];
    let report = load_report(Input { lines });
    use Pipe::*;
    let exp: Map = vec![
      vec![SWBend, Horizontal, SEBend, SWBend, Horizontal],
      vec![Ground, SEBend, NWBend, Vertical, SWBend],
      vec![Start, NWBend, NEBend, NEBend, SWBend],
      vec![Vertical, SEBend, Horizontal, Horizontal, NWBend],
      vec![NEBend, NWBend, Ground, NEBend, NWBend],
    ];
    assert_eq!(report, exp);
  }

  #[test]
  fn test_loop() {
    let lines = vec![
      "7-F7-".to_string(),
      ".FJ|7".to_string(),
      "SJLL7".to_string(),
      "|F--J".to_string(),
      "LJ.LJ".to_string(),
    ];
    let report = load_report(Input { lines });

    let start_pos = start_pos(report.clone());
    assert_eq!(start_pos, (2, 0));

    let start_dir = Pipe::get_loop_start_direction(start_pos, report.clone());
    assert_eq!(start_dir, Some(Direction::North));

    let loop_path = do_loop(
      report.clone(),
      Pipe::Start,
      start_pos,
      start_dir.unwrap(),
      HashSet::new(),
    );

    assert_eq!(loop_path.len() + 1, 16);
  }
}
