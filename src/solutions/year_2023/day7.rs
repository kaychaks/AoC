use aoc_lib::{create_solution, Input, Solver};
use std::cmp::Ordering;
use std::collections::HashMap;

pub struct Day7 {}

#[derive(Eq, PartialEq, PartialOrd, Debug, Hash, Clone)]
struct Card(char);

impl Card {
  fn cmp_with_j(&self, other: &Self) -> Ordering {
    let order = "J23456789TQKA";
    order
      .find(self.0)
      .and_then(|i| order.find(other.0).map(|c| i.cmp(&c)))
      .unwrap_or(Ordering::Equal)
  }
}

impl Ord for Card {
  fn cmp(&self, other: &Self) -> Ordering {
    let order = "23456789TJQKA";
    order
      .find(self.0)
      .and_then(|i| order.find(other.0).map(|c| i.cmp(&c)))
      .unwrap_or(Ordering::Equal)
  }
}

impl From<char> for Card {
  fn from(value: char) -> Self {
    Card(value)
  }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct Hand {
  cards: [Card; 5],
}

impl From<[char; 5]> for Hand {
  fn from(value: [char; 5]) -> Self {
    Hand {
      cards: value.map(|c| c.into()),
    }
  }
}

#[derive(Hash, Debug, Eq, PartialEq, PartialOrd)]
enum HandTypeName {
  FIVE,
  FOUR,
  THREE,
  FULL,
  TWO,
  ONE,
  HIGH,
}

#[derive(Eq, PartialEq, PartialOrd, Hash, Debug)]
struct HandType {
  cards: [Card; 5],
  name: HandTypeName,
}

impl HandType {
  fn new(cards: [char; 5], name: HandTypeName) -> Self {
    HandType {
      cards: cards.map(|c| c.into()),
      name,
    }
  }

  fn from_char_with_j(cards: [Card; 5]) -> Self {
    let value = cards.map(|c| c.0);
    let vs: HashMap<char, usize> =
      value.iter().fold(HashMap::new(), |mut acc, c| {
        acc.entry(*c).and_modify(|x| *x += 1).or_insert(1);
        acc
      });
    let mut vs = vs.into_iter().collect::<Vec<_>>();
    vs.sort_by(|(_, a), (_, b)| a.cmp(b));

    vs.reverse();

    match vs.as_slice() {
      [(_, 5)] => HandType::new(value, HandTypeName::FIVE),
      [(k1, 4), (k2, _)] => {
        if k1 == &'J' || k2 == &'J' {
          HandType::new(value, HandTypeName::FIVE)
        } else {
          HandType::new(value, HandTypeName::FOUR)
        }
      }
      [(k1, 3), (k2, 2)] => {
        if k1 == &'J' || k2 == &'J' {
          HandType::new(value, HandTypeName::FIVE)
        } else {
          HandType::new(value, HandTypeName::FULL)
        }
      }
      [(k1, 3), (k2, _), (k3, _)] => {
        if k1 == &'J' || k2 == &'J' || k3 == &'J' {
          HandType::new(value, HandTypeName::FOUR)
        } else {
          HandType::new(value, HandTypeName::THREE)
        }
      }
      [(k1, 2), (k2, 2), (k3, 1)] => {
        if k1 == &'J' || k2 == &'J' {
          HandType::new(value, HandTypeName::FOUR)
        } else if k3 == &'J' {
          HandType::new(value, HandTypeName::FULL)
        } else {
          HandType::new(value, HandTypeName::TWO)
        }
      }
      [(k, 2), (k1, _), (k2, _), (k3, _)] => {
        if k == &'J' || k1 == &'J' || k2 == &'J' || k3 == &'J' {
          HandType::new(value, HandTypeName::THREE)
        } else {
          HandType::new(value, HandTypeName::ONE)
        }
      }
      _ => {
        if vs.iter().any(|(k, _)| k == &'J') {
          HandType::new(value, HandTypeName::ONE)
        } else {
          HandType::new(value, HandTypeName::HIGH)
        }
      }
    }
  }

  fn sort_by(a: &HandType, b: &HandType, with_j: bool) -> Ordering {
    let xs = [
      HandTypeName::HIGH,
      HandTypeName::ONE,
      HandTypeName::TWO,
      HandTypeName::THREE,
      HandTypeName::FULL,
      HandTypeName::FOUR,
      HandTypeName::FIVE,
    ];
    let xsm: HashMap<HandTypeName, usize> =
      HashMap::from_iter(xs.into_iter().enumerate().map(|(a, b)| (b, a)));
    let eq = xsm
      .get(&a.name)
      .and_then(|x| xsm.get(&b.name).map(|y| (x, y)))
      .map(|(x, y)| x.cmp(y))
      .unwrap_or(Ordering::Equal);

    if eq == Ordering::Equal {
      a.cards
        .iter()
        .zip(b.cards.iter())
        .find(|(a, b)| a != b)
        .map_or(Ordering::Equal, |(a, b)| {
          if with_j {
            Card::cmp_with_j(a, b)
          } else {
            a.cmp(b)
          }
        })
    } else {
      eq
    }
  }
}

impl From<[Card; 5]> for HandType {
  fn from(value: [Card; 5]) -> Self {
    let cs: [char; 5] = value.map(|c| c.0);
    cs.into()
  }
}

impl From<[char; 5]> for HandType {
  fn from(value: [char; 5]) -> Self {
    let vs = value.iter().fold(HashMap::new(), |mut acc, c| {
      acc.entry(*c).and_modify(|x| *x += 1).or_insert(1);
      acc
    });
    let mut vs = vs.values().collect::<Vec<&usize>>();
    vs.sort();
    vs.reverse();

    match vs.as_slice() {
      [5] => HandType::new(value, HandTypeName::FIVE),
      [4, _] => HandType::new(value, HandTypeName::FOUR),
      [3, 2] => HandType::new(value, HandTypeName::FULL),
      [3, _, _] => HandType::new(value, HandTypeName::THREE),
      [2, 2, 1] => HandType::new(value, HandTypeName::TWO),
      [2, _, _, _] => HandType::new(value, HandTypeName::ONE),
      _ => HandType::new(value, HandTypeName::HIGH),
    }
  }
}

fn from_str(s: &str) -> [char; 5] {
  s.chars()
    .take(5)
    .collect::<Vec<char>>()
    .try_into()
    .unwrap_or_default()
}

fn parse_input(input: Input) -> HashMap<Hand, usize> {
  input.lines.into_iter().fold(HashMap::new(), |mut hm, l| {
    let mut cs = l.split_whitespace().take(2);
    let hand: Option<Hand> = cs.next().map(|c| from_str(c).into());
    let weight: Option<usize> = cs.next().and_then(|w| w.parse().ok());
    if let (Some(hand), Some(weight)) = (hand, weight) {
      hm.insert(hand, weight);
    }
    hm
  })
}

create_solution!(
  out_1 => usize;
  out_2 => usize;
  year => 2023;
  day => 7;
  sol_1 => |input: Input| {
    let mut hand_map: Vec<(HandType, usize)> = parse_input(input)
      .into_iter()
      .map(|(h , w)| (h.cards.into(), w))
      .collect();
    hand_map.sort_by(|(a, _), (b, _)| HandType::sort_by(a, b, false));

    let p: usize = hand_map
      .into_iter()
      .enumerate()
      .map(|(i, (_h, w))| (i + 1) * w)
      .sum();

    Some(p)
  };
  sol_2 => |input: Input| {
    let mut hand_map: Vec<(HandType, usize)> = parse_input(input)
      .into_iter()
      .map(|(h , w)| (HandType::from_char_with_j(h.clone().cards), w))
      .collect();
    hand_map.sort_by(|(a, _), (b, _)| HandType::sort_by(a, b, true));

    let p: usize = hand_map
      .into_iter()
      .enumerate()
      .map(|(i, (_h, w))| (i + 1) * w)
      .sum();

    Some(p)
  };
);

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_hand_type() {
    let five = "AAAAA";
    let four = "AA8AA";
    let full = "23233";
    let act_five: HandType = from_str(five).into();
    let act_four: HandType = from_str(four).into();
    let act_full: HandType = from_str(full).into();
    assert_eq!(act_five.name, HandTypeName::FIVE);
    assert_eq!(act_four.name, HandTypeName::FOUR);
    assert_eq!(act_full.name, HandTypeName::FULL);
  }

  #[test]
  fn test_hand_type_ordering() {
    let hands = vec!["32T3K", "T55J5", "AA677", "AAAJQ", "KTJJT"];
    let mut act: Vec<HandType> =
      hands.into_iter().map(|h| from_str(h).into()).collect();
    act.sort_by(|a, b| HandType::sort_by(a, b, false));

    let exp = vec![
      HandType {
        cards: [Card('3'), Card('2'), Card('T'), Card('3'), Card('K')],
        name: HandTypeName::ONE,
      },
      HandType {
        cards: [Card('K'), Card('T'), Card('J'), Card('J'), Card('T')],
        name: HandTypeName::TWO,
      },
      HandType {
        cards: [Card('A'), Card('A'), Card('6'), Card('7'), Card('7')],
        name: HandTypeName::TWO,
      },
      HandType {
        cards: [Card('T'), Card('5'), Card('5'), Card('J'), Card('5')],
        name: HandTypeName::THREE,
      },
      HandType {
        cards: [Card('A'), Card('A'), Card('A'), Card('J'), Card('Q')],
        name: HandTypeName::THREE,
      },
    ];

    assert_eq!(act, exp);
  }

  #[test]
  fn test_parse_input() {
    let input: Input = Input {
      lines: r"32T3K 765
        T55J5 684
        KK677 28
        KTJJT 220
        QQQJA 483"
        .lines()
        .map(|l| l.to_string())
        .collect(),
    };
    let act = parse_input(input);
    let exp = vec![
      (
        Hand {
          cards: [Card('3'), Card('2'), Card('T'), Card('3'), Card('K')],
        },
        765,
      ),
      (
        Hand {
          cards: [Card('T'), Card('5'), Card('5'), Card('J'), Card('5')],
        },
        684,
      ),
      (
        Hand {
          cards: [Card('K'), Card('K'), Card('6'), Card('7'), Card('7')],
        },
        28,
      ),
      (
        Hand {
          cards: [Card('K'), Card('T'), Card('J'), Card('J'), Card('T')],
        },
        220,
      ),
      (
        Hand {
          cards: [Card('Q'), Card('Q'), Card('Q'), Card('J'), Card('A')],
        },
        483,
      ),
    ];
    assert_eq!(act, HashMap::from_iter(exp));
  }
}

#[test]
fn test_hand_deser() {
  let hand_map: HashMap<Hand, usize> = HashMap::from_iter(vec![
    (
      Hand {
        cards: [Card('3'), Card('2'), Card('T'), Card('3'), Card('K')],
      },
      765,
    ),
    (
      Hand {
        cards: [Card('T'), Card('5'), Card('5'), Card('J'), Card('5')],
      },
      684,
    ),
    (
      Hand {
        cards: [Card('K'), Card('K'), Card('6'), Card('7'), Card('7')],
      },
      28,
    ),
    (
      Hand {
        cards: [Card('K'), Card('T'), Card('J'), Card('J'), Card('T')],
      },
      220,
    ),
    (
      Hand {
        cards: [Card('Q'), Card('Q'), Card('Q'), Card('J'), Card('A')],
      },
      483,
    ),
  ]);

  let mut hands: Vec<HandType> = hand_map
    .keys()
    .map(|h| HandType::from_char_with_j(h.clone().cards))
    .collect();
  hands.sort_by(|a, b| HandType::sort_by(a, b, true));

  let p: usize = hands
    .into_iter()
    .map(|h| hand_map.get(&Hand { cards: h.cards }).unwrap_or(&1))
    .enumerate()
    .map(|(i, w)| (i + 1) * w)
    .sum();
  assert_eq!(p, 5905);
}
