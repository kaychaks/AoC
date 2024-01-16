use std::collections::{BTreeMap, HashSet};

use aoc_lib::{create_solution, Input, Solver};

#[derive(Debug, Clone)]
enum Direction {
  Left,
  Right,
}

fn walk_map(
  hs: BTreeMap<String, (String, String)>,
  key: &str,
  dir: Direction,
) -> Option<String> {
  match dir {
    Direction::Left => hs.get(key).cloned().map(|(l, _)| l.to_string()),
    Direction::Right => hs.get(key).cloned().map(|(_, r)| r.to_string()),
  }
}

fn search_zzz(
  hs: BTreeMap<String, (String, String)>,
  dirs: Vec<Direction>,
  init: String,
  pred: impl Fn(&String) -> bool,
) -> usize {
  let ds = dirs.iter().cycle();
  let mut key = init.clone();
  ds.take_while(|d| {
    if let Some(some_key) = walk_map(hs.clone(), &key, (*d).clone()) {
      key = some_key.clone();
      !pred(&some_key)
    } else {
      false
    }
  })
  .count()
    + 1
}

fn gcd(a: usize, b: usize) -> usize {
  if b == 0 {
    a
  } else {
    gcd(b, a.rem_euclid(b))
  }
}

fn lcm(a: usize, b: usize) -> usize {
  a * b / gcd(a, b)
}

fn search_z(
  hs: BTreeMap<String, (String, String)>,
  dirs: Vec<Direction>,
) -> usize {
  let keys = hs
    .keys()
    .cloned()
    .filter(|k| k.ends_with("A"))
    .collect::<HashSet<_>>();

  keys
    .into_iter()
    .map(|k| search_zzz(hs.clone(), dirs.clone(), k, |k| k.ends_with("Z")))
    .reduce(lcm)
    .unwrap_or_default()
}

fn load_hs(ls: Vec<String>) -> BTreeMap<String, (String, String)> {
  ls.into_iter().fold(BTreeMap::new(), |mut acc, l| {
    let mut cs = l
      .split("=")
      .take(2)
      .map(|x| x.trim())
      .map(|x| x.to_string());
    let key: Option<String> = cs.next();
    let lr: Option<(String, String)> = cs.next().and_then(|c| {
      let mut x = c
        .split(",")
        .take(2)
        .map(|c| c.trim())
        .map(|c| c.to_string());
      let l = x.next()?.split_off(1);
      let y = x.next()?.split_terminator(")").collect::<Vec<_>>().join("");
      Some((l, y))
    });

    if let (Some(key), Some((left, right))) = (key, lr) {
      acc.insert(key, (left, right));
    }
    acc
  })
}

fn load_dirs(ls: String) -> Vec<Direction> {
  ls.chars()
    .map(|c| match c {
      'L' => Direction::Left,
      'R' => Direction::Right,
      _ => panic!("Invalid direction"),
    })
    .collect()
}

pub struct Day8 {}

create_solution!(
  out_1 => usize;
  out_2 => usize;
  year => 2023;
  day => 8;
  sol_1 => |input: Input| {
    let dirs = load_dirs(input.lines.get(0).unwrap().clone());
    let hs = load_hs(input.lines.get(2..).unwrap().to_vec());
    Some(search_zzz(hs, dirs, "AAA".to_string(), |k| k == "ZZZ"))
  };
  sol_2 => |input: Input| {
    let dirs = load_dirs(input.lines.get(0).unwrap().clone());
    let hs = load_hs(input.lines.get(2..).unwrap().to_vec());
    Some(search_z(hs, dirs))
  };
);

#[cfg(test)]
mod tests {
  use std::vec;

  use super::*;

  #[test]
  fn test_search_zzz() {

    let hs = load_hs(vec![
      "AAA = (BBB, CCC)".to_string(),
      "BBB = (DDD, EEE)".to_string(),
      "CCC = (ZZZ, GGG)".to_string(),
      "DDD = (DDD, DDD)".to_string(),
      "EEE = (EEE, EEE)".to_string(),
      "GGG = (GGG, GGG)".to_string(),
      "ZZZ = (ZZZ, ZZZ)".to_string(),
    ]);


    let dirs = load_dirs("RL".to_string());
    assert_eq!(
      search_zzz(hs.clone(), dirs.clone(), "AAA".to_string(), |k| k == "ZZZ"),
      2
    );

    let hs = load_hs(vec![
      "11A = (11B, XXX)".to_string(),
      "11B = (XXX, 11Z)".to_string(),
      "11Z = (11B, XXX)".to_string(),
      "22A = (22B, XXX)".to_string(),
      "22B = (22C, 22C)".to_string(),
      "22C = (22Z, 22Z)".to_string(),
      "22Z = (22B, 22B)".to_string(),
      "XXX = (XXX, XXX)".to_string(),
    ]);
    let dirs = load_dirs("LR".to_string());
    assert_eq!(search_z(hs, dirs), 6);
  }
}
