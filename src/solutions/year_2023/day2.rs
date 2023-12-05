use aoc_lib::{create_solution, Input, Solver};

const RED: u8 = 12;
const GREEN: u8 = 13;
const BLUE: u8 = 14;

#[derive(Debug, Default, Clone)]
struct Cubes {
  red: u8,
  green: u8,
  blue: u8,
}

impl Cubes {
  fn power(&self) -> u32 {
    self.red as u32 * self.green as u32 * self.blue as u32
  }
}

#[derive(Debug)]
struct Sets(Vec<Cubes>);

impl FromIterator<Vec<(String, u8)>> for Sets {
  fn from_iter<T: IntoIterator<Item = Vec<(String, u8)>>>(iter: T) -> Self {
    Self(
      iter
        .into_iter()
        .map(|x| {
          let mut red = 0;
          let mut green = 0;
          let mut blue = 0;
          for (s, n) in x {
            match s.as_str() {
              "red" => red = n,
              "green" => green = n,
              "blue" => blue = n,
              _ => unreachable!(),
            }
          }
          Cubes { red, green, blue }
        })
        .collect(),
    )
  }
}

#[derive(Debug)]
struct Game {
  id: u16,
  sets: Sets,
}

impl From<(u16, Sets)> for Game {
  fn from((id, sets): (u16, Sets)) -> Self {
    Self { id, sets }
  }
}

impl Game {
  fn is_possible(&self) -> bool {
    self
      .sets
      .0
      .iter()
      .all(|cs| cs.red <= RED && cs.green <= GREEN && cs.blue <= BLUE)
  }

  fn fewest_cubes(&self) -> Cubes {
    self
      .sets
      .0
      .clone()
      .iter_mut()
      .reduce(|a, b| {
        a.blue = a.blue.max(b.blue);
        a.red = a.red.max(b.red);
        a.green = a.green.max(b.green);
        a
      })
      .unwrap()
      .clone()
  }
}

#[derive(Debug)]
pub struct Day2 {
  games: Vec<Game>,
}

impl Day2 {
  fn possible_games(&self) -> Vec<&Game> {
    self.games.iter().filter(|g| g.is_possible()).collect()
  }

  fn sum_ids_possible_games(&self) -> u32 {
    self.possible_games().iter().map(|g| g.id as u32).sum()
  }

  fn sum_power_min_cubes(&self) -> u32 {
    self.games.iter().map(|g| g.fewest_cubes().power()).sum()
  }

  fn load_games(input: Input) -> Self {
    Self {
      games: input
        .lines
        .iter()
        .map(|l| {
          let splitted = l.split_once(": ").unwrap();
          let game_id = splitted
            .0
            .rsplit_once(' ')
            .unwrap()
            .1
            .parse::<u16>()
            .unwrap();
          let sets = splitted
            .1
            .split("; ")
            .map(|x| {
              x.split(", ")
                .map(|x| {
                  x.split_once(" ")
                    .map(|(n, s)| (s.to_string(), n.parse::<u8>().unwrap()))
                    .unwrap()
                })
                .collect::<Vec<_>>()
            })
            .collect::<Sets>();

          (game_id, sets).into()
        })
        .collect::<Vec<_>>(),
    }
  }
}

create_solution!(
  out_1 => u32;
  out_2 => u32;
  year => 2023;
  day => 2;
  sol_1 => |input: Input| {
    let day2 = Day2::load_games(input);
    Some(day2.sum_ids_possible_games())
  };
  sol_2 => |input: Input| {
    let day2 = Day2::load_games(input);
    Some(day2.sum_power_min_cubes())
  };
);
