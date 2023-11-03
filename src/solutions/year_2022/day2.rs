use aoc_lib::{Input, Solver};

#[derive(Clone, Copy)]
enum RPS {
    Rock,
    Paper,
    Scissor,
}

impl RPS {
    fn predict_move(outcome: &str, opponents_move: Self) -> Self {
        match (outcome, opponents_move) {
            ("Z", RPS::Rock) => RPS::Paper,
            ("Z", RPS::Paper) => RPS::Scissor,
            ("Z", RPS::Scissor) => RPS::Rock,
            ("X", RPS::Rock) => RPS::Scissor,
            ("X", RPS::Paper) => RPS::Rock,
            ("X", RPS::Scissor) => RPS::Paper,
            (_, m) => m,
        }
    }
}

impl TryFrom<&str> for RPS {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err("empty value".to_string())
        } else {
            match value {
                "X" | "A" => Ok(RPS::Rock),
                "Y" | "B" => Ok(RPS::Paper),
                "Z" | "C" => Ok(RPS::Scissor),
                _ => Err("invalid value".to_string()),
            }
        }
    }
}

#[derive(Clone, Copy)]
enum PType {
    Elf,
    Me,
}
#[derive(Clone, Copy)]
struct Player {
    who: PType,
    game_move: RPS,
}

impl Player {
    fn me(my_move: RPS) -> Self {
        Player {
            who: PType::Me,
            game_move: my_move,
        }
    }

    fn elf(elf_move: RPS) -> Self {
        Player {
            who: PType::Elf,
            game_move: elf_move,
        }
    }
}

struct PlayResult {
    is_draw: bool,
    winner: Option<Player>,
    players: [Player; 2],
}

impl PlayResult {
    fn my_score(&self) -> u32 {
        let my_move: Option<Player> = self
            .players
            .iter()
            .filter(|&x| match x.who {
                PType::Elf => false,
                PType::Me => true,
            })
            .copied()
            .collect::<Vec<_>>()
            .pop();
        let move_score = match my_move {
            Some(p) => match p.game_move {
                RPS::Rock => 1,
                RPS::Paper => 2,
                RPS::Scissor => 3,
            },
            _ => 0,
        };

        let winning_score = if self.is_draw {
            3
        } else {
            match self.winner {
                Some(p) => match p.who {
                    PType::Elf => 0,
                    PType::Me => 6,
                },
                _ => 0,
            }
        };

        move_score + winning_score
    }
}

impl PlayResult {
    fn winner(p: Player, ps: [Player; 2]) -> Self {
        PlayResult {
            is_draw: false,
            winner: Some(p),
            players: ps,
        }
    }

    fn draw(ps: [Player; 2]) -> Self {
        PlayResult {
            is_draw: true,
            winner: None,
            players: ps,
        }
    }
}
pub struct Day2 {}

impl Day2 {
    fn play(a: Player, b: Player) -> PlayResult {
        let ps = [a, b];
        match (a.game_move, b.game_move) {
            (RPS::Rock, RPS::Scissor) => PlayResult::winner(a, ps),
            (RPS::Scissor, RPS::Rock) => PlayResult::winner(b, ps),

            (RPS::Scissor, RPS::Paper) => PlayResult::winner(a, ps),
            (RPS::Paper, RPS::Scissor) => PlayResult::winner(b, ps),

            (RPS::Paper, RPS::Rock) => PlayResult::winner(a, ps),
            (RPS::Rock, RPS::Paper) => PlayResult::winner(b, ps),

            _ => PlayResult::draw(ps),
        }
    }

    fn play_rounds<F>(input: Input, map_player: F) -> Option<u32>
    where
        F: Fn((&str, &str)) -> (Option<Player>, Option<Player>),
    {
        input
            .lines
            .iter()
            .flat_map(|x| x.split_once(' '))
            .map(map_player)
            .filter_map(|(opta, optb)| match (opta, optb) {
                (Some(a), Some(b)) => Some((a, b)),
                _ => None,
            })
            .map(|(a, b)| Day2::play(a, b))
            .map(|p| p.my_score())
            .collect::<Vec<_>>()
            .into_iter()
            .reduce(|a, b| a + b)
    }
}

impl Solver for Day2 {
    type OutputPart1 = u32;
    type OutputPart2 = u32;
    fn year() -> u16 {
        2022
    }
    fn day() -> u8 {
        2
    }

    fn solution_part1(input: aoc_lib::Input) -> Option<Self::OutputPart1> {
        Day2::play_rounds(input, |(a, b)| {
            (
                RPS::try_from(a).ok().map(Player::elf),
                RPS::try_from(b).ok().map(Player::me),
            )
        })
    }

    fn solution_part2(input: aoc_lib::Input) -> Option<Self::OutputPart2> {
        Day2::play_rounds(input, |(a, b)| {
            let opponents_move = RPS::try_from(a).ok();
            let my_move = opponents_move.map(|m| RPS::predict_move(b, m));
            (opponents_move.map(Player::elf), my_move.map(Player::me))
        })
    }
}
