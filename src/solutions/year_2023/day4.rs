use aoc_lib::{create_solution, Input, Solver};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
struct Card {
  my_numbers: HashSet<u16>,
  winning_numbers: HashSet<u16>,
  id: usize,
}

impl Card {
  fn points(&self) -> u64 {
    let my_winning_numbers = self.my_winning_numbers();
    if my_winning_numbers.is_empty() {
      0
    } else {
      2u64.pow((my_winning_numbers.len() as u32).saturating_sub(1))
    }
  }

  fn my_winning_numbers(&self) -> Vec<u16> {
    self
      .my_numbers
      .intersection(&self.winning_numbers)
      .copied()
      .collect()
  }
}

pub struct Day4 {
  cards: Vec<Card>,
}

impl Day4 {
  fn total_cards(&mut self) -> u32 {
    self
      .cards
      .clone()
      .iter_mut()
      .fold((0, HashMap::new()), |(acc, mut copies), card| {
        let my_winning_numbers = card.my_winning_numbers();
        let current_card_copies =
          copies.get(&card.id).map(|x| *x + 1).unwrap_or(1);
        if my_winning_numbers.len() > 0 {
          (0..current_card_copies).for_each(|_| {
            (card.id + 1..=card.id + my_winning_numbers.len()).for_each(|i| {
              copies
                .entry(i)
                .and_modify(|c| {
                  *c += 1;
                })
                .or_insert(1);
            });
          });
        }
        (
          acc + current_card_copies as u32,
          copies,
        )
      })
      .0
  }

  fn load_cards(input: Input) -> Self {
    let cards = input
      .lines
      .iter()
      .map(|line| {
        let mut parts = line.split(": ");
        let id = parts
          .next()
          .unwrap()
          .split(" ")
          .last()
          .unwrap()
          .parse::<usize>()
          .unwrap();
        let mut numbers = parts.next().unwrap().split(" | ");
        let winning_numbers = numbers
          .next()
          .unwrap()
          .split(" ")
          .filter_map(|n| {
            if n.is_empty() {
              None
            } else {
              Some(n.trim().parse::<u16>().unwrap())
            }
          })
          .collect::<HashSet<u16>>();
        let my_numbers = numbers
          .next()
          .unwrap()
          .split(" ")
          .filter_map(|n| {
            if n.is_empty() {
              None
            } else {
              Some(n.trim().parse::<u16>().unwrap())
            }
          })
          .collect::<HashSet<u16>>();
        Card {
          my_numbers,
          winning_numbers,
          id,
        }
      })
      .collect::<Vec<Card>>();
    Self { cards }
  }
}

create_solution!(
  out_1 => u64;
  out_2 => u64;
  year => 2023;
  day => 4;
  sol_1 => |input: Input| {
    let day4 = Day4::load_cards(input);
    Some(day4.cards.iter().map(|c| c.points()).sum())
  };
  sol_2 => |input: Input| {
    let mut day4 = Day4::load_cards(input);
    Some(day4.total_cards() as u64)
  };
);
