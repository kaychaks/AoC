use std::cmp::Ordering;
use crate::solutions::year_2023::day5::Map::MapTuple;
use aoc_lib::{create_solution, Input, Solver};
use chrono::Offset;
use std::ops::Range;

type PropertyTuple = (usize, usize, usize);
type PropertyRange = (Range<usize>, Range<usize>);

#[derive(PartialEq, Debug, Clone, Eq)]
enum Map {
  MapTuple(PropertyTuple),
  MapRange(PropertyRange),
}

impl Ord for Map {
  fn cmp(&self, other: &Self) -> Ordering {
    self.partial_cmp(other).unwrap_or(Ordering::Equal)
  }
}

impl PartialOrd for Map {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    match self {
      Map::MapRange((start_range, _)) => {
        start_range.start.partial_cmp(match other {
          Map::MapRange((other_start_range, _)) => &other_start_range.start,
          Map::MapTuple((s, _, _)) => &s,
        })
      }
      Map::MapTuple((s, _, _)) => s.partial_cmp(match other {
        Map::MapTuple((s, _, _)) => s,
        Map::MapRange((start_range, _)) => &start_range.start,
      }),
    }
  }
}

impl Map {
  fn map_tuple(s: usize, d: usize, r: usize) -> Map {
    Map::MapTuple((s, d, r))
  }

  fn map_range(x: (usize, usize, usize)) -> Map {
    Map::MapRange(((x.0..x.0 + x.2), (x.1..x.1 + x.2)))
  }

  fn head(&self) -> usize {
    match self {
      MapTuple(x) => x.0,
      Map::MapRange(x) => x.0.start,
    }
  }

  fn map_tuple_iter(iter: Vec<(usize, usize, usize)>) -> Vec<Self> {
    iter.into_iter().map(|x| x.into()).collect()
  }

  fn map_range_iter(iter: Vec<(usize, usize, usize)>) -> Vec<Self> {
    iter.into_iter().map(|x| Map::map_range(x)).collect()
  }

  fn map_range_iters(iters: Vec<Vec<(usize, usize, usize)>>) -> Vec<Vec<Self>> {
    iters.into_iter().map(|x| Map::map_range_iter(x)).collect()
  }

  fn map_tuple_iters(iters: Vec<Vec<(usize, usize, usize)>>) -> Vec<Vec<Self>> {
    iters.into_iter().map(|x| Map::map_tuple_iter(x)).collect()
  }
}

impl From<(usize, usize, usize)> for Map {
  fn from(value: (usize, usize, usize)) -> Self {
    Map::map_tuple(value.0, value.1, value.2)
  }
}

fn get_mapped_value(
  key: usize,
  source_start: usize,
  destination_start: usize,
  range_length: usize,
) -> Option<usize> {
  // if key >= source_start then look for corresponding mapped_value
  if key >= source_start {
    // the distance of key from source_start <= range_length
    let distance = key - source_start;
    if distance <= range_length {
      // mapped_value = destination_start + distance
      Some(destination_start + distance)
    } else {
      None
    }
  } else {
    None
  }
}

/// If a is consumed in b then respond a tuple with left half as a and right as empty range
/// if not then respond with a tuple where left half represents consumed and right not consumed
/// if a and b are disjoint then respond None
fn range_mapping(
  a: Range<usize>,
  b: Range<usize>,
) -> Option<(Range<usize>, Range<usize>)> {
  if a.end <= b.start || a.start >= b.end {
    // disjoint
    None
  } else if a.start >= b.start && a.end <= b.end {
    // a is fully consumed within b
    Some((a, 0..0))
  } else if a.start >= b.start && a.end > b.end {
    // some part of a in the end is not consumed
    Some((a.start..b.end, b.end..a.end))
  } else if a.start < b.start && a.end <= b.end {
    // some part of a in the start is not consumed
    Some((b.start..a.end, a.start..b.start))
  } else {
    None
  }
}

/// get all the mapped destination ranges from the map for a key_range
fn range_mapping_over(
  key_range: Range<usize>,
  map: Vec<Map>,
) -> Vec<Range<usize>> {
  let ret = map
    .iter()
    .fold(
      (vec![], key_range.clone()),
      |(mut acc, current_unmapped), m| {
        match m {
          Map::MapRange((start_range, destination_range)) => {
            return if let Some((mapped, unmapped)) =
              range_mapping(current_unmapped.clone(), start_range.clone())
            {
              let destination_start_offset = mapped.start - start_range.start;
              let destination_start =
                destination_range.start + destination_start_offset;

              acc.push(destination_start..(destination_start + mapped.len()));
              acc.sort_by(|a, b| a.start.cmp(&b.start));

              (acc, unmapped)
            } else {
              (acc, current_unmapped)
            };
          }
          Map::MapTuple(_) => todo!(),
        };
      },
    );

  if ret.0.is_empty() {
    if ret.1.is_empty() {
      vec![key_range]
    } else {
      vec![ret.1]
    }
  } else {
    let mut curr_acc = ret.0.clone();
    if !ret.1.is_empty() {
      curr_acc.push(ret.1);
    }
    curr_acc
  }
}

fn range_mapping_over_maps(
  key_range: Range<usize>,
  maps: Vec<Vec<Map>>,
) -> Vec<Range<usize>> {
  maps.into_iter().fold(vec![key_range], |acc, mut map| {
    map.sort();
    let ret = acc
      .iter()
      .flat_map(|r| range_mapping_over(r.clone(), map.clone()))
      .collect::<Vec<_>>();
    ret
  })
}

fn range_mappings_over_maps(
  ranges: Vec<Range<usize>>,
  maps: Vec<Vec<Map>>,
) -> Vec<Range<usize>> {
  let mut ret: Vec<_> = ranges.into_iter().flat_map(|r| {
    let result_map = range_mapping_over_maps(r.clone(), maps.clone());
    result_map
  }).collect();
  ret.sort_by(|a, b| a.start.cmp(&b.start));
  ret
}

pub fn get_mapped_value_from(key: usize, map: Vec<Map>) -> usize {
  map
    .into_iter()
    .find_map(|m| match m {
      Map::MapTuple((source_start, destination_start, range_length)) => {
        get_mapped_value(key, source_start, destination_start, range_length)
      }
      Map::MapRange((start_range, destination_range)) => todo!(),
    })
    .unwrap_or(key)
}

pub fn get_mapped_value_from_maps(key: usize, maps: Vec<Vec<Map>>) -> usize {
  maps
    .into_iter()
    .fold(key, |acc, m| get_mapped_value_from(acc, m))
}

pub fn get_min_value(keys: Vec<usize>, maps: Vec<Vec<Map>>) -> usize {
  keys
    .into_iter()
    .fold(vec![], |mut xs, k| {
      let v = get_mapped_value_from_maps(k, maps.clone());
      xs.push(v.min(usize::MAX));
      xs
    })
    .iter()
    .min()
    .cloned()
    .unwrap_or_default()
}

fn vec_to_ranges(keys: Vec<usize>) -> Vec<Range<usize>> {
  keys
    .chunks(2)
    .map(|xs| (xs[0]..xs[0] + xs.get(1).unwrap_or(&0_usize)))
    .collect::<Vec<_>>()
}

pub fn get_min_value_over_ranges(
  keys: Vec<usize>,
  maps: Vec<Vec<Map>>,
) -> usize {
  vec_to_ranges(keys)
    .into_iter()
    .fold(vec![], |mut xs, ks| {
      let ks = ks.collect::<Vec<_>>();
      let min = get_min_value(ks, maps.clone());
      xs.push(min);
      xs
    })
    .iter()
    .min()
    .cloned()
    .unwrap_or_default()
}

fn get_mapped_value_range(
  key: usize,
  key_range: usize,
  source_start: usize,
  destination_start: usize,
  range_length: usize,
) -> Vec<usize> {
  (key..key + key_range)
    .map(|k| {
      get_mapped_value(k, source_start, destination_start, range_length)
        .unwrap_or(k)
    })
    .collect()
}

fn load_seeds(input: Input) -> Vec<usize> {
  input
    .lines
    .iter()
    .find_map(|l| {
      l.split_once("seeds:").map(|l| {
        l.1
          .trim()
          .split(' ')
          .map(|x| x.parse().unwrap_or(0))
          .collect::<Vec<usize>>()
      })
    })
    .unwrap_or_default()
}

fn load_map(line: &str, as_range: bool) -> Option<Map> {
  let vs: Vec<usize> = line
    .trim()
    .split(' ')
    .map(|xs| xs.parse().unwrap_or(0))
    .collect();
  if vs.iter().take(3).len() == 3 {
    if !as_range {
      Some(Map::map_tuple(vs[1], vs[0], vs[2]))
    } else {
      Some(Map::map_range((vs[1], vs[0], vs[2])))
    }
  } else {
    None
  }
}

fn load_maps(input: Input, as_ranges: bool) -> Vec<Vec<Map>> {
  input
    .lines
    .iter()
    .fold((vec![vec![]], None), |(mut ms, map_name), l| {
      if l.ends_with("map:") {
        // hold the current map name
        let map_name = l.trim().split_once(' ').map(|x| x.0);
        (ms, map_name)
      } else if !l.is_empty() && !l.starts_with("seeds:") {
        // load the records for the current map name
        if let Some(m) = load_map(&l, as_ranges) {
          if let Some(xs) = ms.last_mut() {
            xs.push(m);
          }

          (ms, map_name)
        } else {
          // ignore if for some reason records could not load
          (ms, map_name)
        }
      } else if l.is_empty() && map_name.is_some() {
        // setup for the next map name

        // sort the last map by the source
        if let Some(xs) = ms.last_mut() {
          xs.sort_by(|x, y| x.head().cmp(&y.head()))
        }
        ms.push(vec![]);
        (ms, None)
      } else {
        // ignore the seeds line
        (ms, map_name)
      }
    })
    .0
}

pub struct Day5 {}

create_solution!(
  out_1 => usize;
  out_2 => usize;
  year => 2023;
  day => 5;
  sol_1 => |input: Input| {
    let seeds = load_seeds(input.clone());
    let maps = load_maps(input, false);
    let v = get_min_value(seeds, maps);
    Some(v)
  };
  sol_2 => |input: Input| {
    let seeds = load_seeds(input.clone());
    let maps = load_maps(input, true);
    let vs = range_mappings_over_maps(vec_to_ranges(seeds.clone()), maps.clone());

    vs.first().map(|x| x.start)
  };
);

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_get_mapped_value_from() {
    let input: Vec<Map> =
      vec![Map::map_tuple(98, 50, 2), Map::map_tuple(50, 52, 48)];
    let v = get_mapped_value_from(79, input);
    assert_eq!(v, 81);
  }

  #[test]
  fn test_get_min_from() {
    let maps: Vec<Vec<Map>> = Map::map_tuple_iters(vec![
      vec![(98, 50, 2), (50, 52, 48)],
      vec![(15, 0, 37), (52, 37, 2), (0, 39, 15)],
      vec![(53, 49, 8), (11, 0, 42), (0, 42, 7), (7, 57, 4)],
      vec![(18, 88, 7), (25, 18, 70)],
      vec![(77, 45, 23), (45, 81, 19), (64, 68, 13)],
      vec![(69, 0, 1), (0, 1, 69)],
      vec![(56, 60, 37), (93, 56, 4)],
    ]);

    let keys: Vec<usize> = vec![79, 14, 55, 13];

    let act = get_min_value_over_ranges(keys, maps);
    assert_eq!(act, 46);
  }


  #[test]
  fn test_load_seeds() {
    let seeds_line = "seeds: 194657215 187012821 1093203236 6077151 44187305 148722449 2959577030 152281079 3400626717 198691716 1333399202 287624830 2657325069 35258407 1913289352 410917164 1005856673 850939 839895010 162018909";
    let act = load_seeds(Input {
      lines: vec![seeds_line.to_string()],
    });
    let exp: Vec<usize> = vec![
      194657215, 187012821, 1093203236, 6077151, 44187305, 148722449,
      2959577030, 152281079, 3400626717, 198691716, 1333399202, 287624830,
      2657325069, 35258407, 1913289352, 410917164, 1005856673, 850939,
      839895010, 162018909,
    ];
    assert_eq!(exp, act);
  }

  #[test]
  fn test_load_maps() {
    let maps_lines = r"seeds: 79 14 55 13

      seed-to-soil map:
      50 98 2
      52 50 48

      soil-to-fertilizer map:
      0 15 37
      37 52 2
      39 0 15

      fertilizer-to-water map:
      49 53 8
      0 11 42
      42 0 7
      57 7 4

      water-to-light map:
      88 18 7
      18 25 70

      light-to-temperature map:
      45 77 23
      81 45 19
      68 64 13

      temperature-to-humidity map:
      0 69 1
      1 0 69

      humidity-to-location map:
      60 56 37
      56 93 4
    ";
    let input = Input {
      lines: maps_lines.split("\n").map(|x| x.to_string()).collect(),
    };
    let act = load_maps(input.clone(), false);
    let act1 = load_maps(input.clone(), true);
    let exp: Vec<Vec<Map>> = Map::map_tuple_iters(vec![
      vec![(50, 52, 48), (98, 50, 2)],
      vec![(0, 39, 15), (15, 0, 37), (52, 37, 2)],
      vec![(0, 42, 7), (7, 57, 4), (11, 0, 42), (53, 49, 8)],
      vec![(18, 88, 7), (25, 18, 70)],
      vec![(45, 81, 19), (64, 68, 13), (77, 45, 23)],
      vec![(0, 1, 69), (69, 0, 1)],
      vec![(56, 60, 37), (93, 56, 4)],
    ]);

    let exp1: Vec<Vec<Map>> = Map::map_range_iters(vec![
      vec![(98, 50, 2), (50, 52, 48)],
      vec![(15, 0, 37), (52, 37, 2), (0, 39, 15)],
      vec![(53, 49, 8), (11, 0, 42), (0, 42, 7), (7, 57, 4)],
      vec![(18, 88, 7), (25, 18, 70)],
      vec![(77, 45, 23), (45, 81, 19), (64, 68, 13)],
      vec![(69, 0, 1), (0, 1, 69)],
      vec![(56, 60, 37), (93, 56, 4)],
    ]);

    assert_eq!(act, exp);
    assert_eq!(act1, exp1);
  }



  #[test]
  fn test_range_mapping_over_map() {

    let keys: Vec<usize> = vec![79, 14, 55, 13];


    let maps: Vec<Vec<Map>> = Map::map_range_iters(vec![
      vec![(98, 50, 2), (50, 52, 48)],
      vec![(15, 0, 37), (52, 37, 2), (0, 39, 15)],
      vec![(53, 49, 8), (11, 0, 42), (0, 42, 7), (7, 57, 4)],
      vec![(18, 88, 7), (25, 18, 70)],
      vec![(77, 45, 23), (45, 81, 19), (64, 68, 13)],
      vec![(69, 0, 1), (0, 1, 69)],
      vec![(56, 60, 37), (93, 56, 4)],
    ]);

    let ks = vec_to_ranges(keys);

    let x = range_mappings_over_maps(ks, maps);
    assert_eq!(x, vec![46..56, 56..60, 60..61, 82..85, 86..90, 94..97, 97..99])
  }
}
