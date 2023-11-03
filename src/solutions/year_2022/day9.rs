use std::collections::HashSet;

use aoc_lib::Solver;

#[derive(Debug)]
pub struct Day9 {
    head_positions: Vec<Coord>,
    tail_positions: HashSet<Coord>,
    current_tail_position: Coord,
}

#[derive(Hash, PartialEq, Eq, Debug, Default, Clone, Copy, PartialOrd, Ord)]
struct Coord(i32, i32);

impl Solver for Day9 {
    fn solution_part1(input: aoc_lib::Input) -> Option<Self::OutputPart1> {
        let mut d = Day9::default();
        let instrs = input.lines.iter().map(MotionInstruction::from);

        instrs.for_each(|x| {
            for _ in 1..=x.distance() {
                d.move_rope(x).adjust_tail();
            }
        });

        Some(d.tail_positions.len())
    }
    fn solution_part2(input: aoc_lib::Input) -> Option<Self::OutputPart2> {
        let mut d = Day9::default();
        let instrs = input.lines.iter().map(MotionInstruction::from);
        let mut tails = [Coord::default()].repeat(9);

        instrs.for_each(|x| {
            for _ in 1..=x.distance() {
                d.move_rope(x);
                let new_head_pos = d.head_positions.last().expect("head position");
                tails[0] = d.get_new_tail_position(*new_head_pos, tails[0]);
                for i in 1..=8 {
                    tails[i] = d.get_new_tail_position(tails[i - 1], tails[i]);
                }
                d.tail_positions.insert(tails[8]);
            }
        });

        Some(d.tail_positions.len())
    }

    fn day() -> u8 {
        9
    }
    fn year() -> u16 {
        2022
    }

    type OutputPart1 = usize;
    type OutputPart2 = usize;
}

impl Default for Day9 {
    fn default() -> Self {
        Day9 {
            head_positions: vec![Coord::default()],
            tail_positions: HashSet::from([Coord::default()]),
            current_tail_position: Coord::default(),
        }
    }
}

fn move_closer(dest: i32, src: i32) -> i32 {
    if dest.abs_diff(src) <= 1 {
        src
    } else if src > dest {
        move_closer(dest, src - 1)
    } else {
        move_closer(dest, src + 1)
    }
}

impl Day9 {
    fn move_rope(&mut self, instr: MotionInstruction) -> &mut Self {
        let curr_head_pos = self
            .head_positions
            .last()
            .expect("head positions should not be empty");
        self.head_positions
            .push(self.get_new_head_position(instr, *curr_head_pos));
        self
    }

    fn get_new_head_position(
        &self,
        instr: MotionInstruction,
        Coord(curr_x, curr_y): Coord,
    ) -> Coord {
        match instr {
            MotionInstruction::Right(_) => Coord(curr_x + 1_i32, curr_y),
            MotionInstruction::Left(_) => Coord(curr_x - 1_i32, curr_y),
            MotionInstruction::Up(_) => Coord(curr_x, curr_y + 1_i32),
            MotionInstruction::Down(_) => Coord(curr_x, curr_y - 1_i32),
        }
    }

    fn get_new_tail_position(
        &self,
        Coord(curr_h_x, curr_h_y): Coord,
        Coord(curr_t_x, curr_t_y): Coord,
    ) -> Coord {
        let (mod_t_x, mod_t_y) = {
            if (curr_h_y == curr_t_y && curr_h_x.abs_diff(curr_t_x) > 1)
                || (curr_h_x == curr_t_x && curr_h_y.abs_diff(curr_t_y) > 1)
            {
                if curr_h_y == curr_t_y {
                    (move_closer(curr_h_x, curr_t_x), curr_t_y)
                } else {
                    (curr_t_x, move_closer(curr_h_y, curr_t_y))
                }
            } else if (curr_h_x.abs_diff(curr_t_x) > 1 && curr_h_y.abs_diff(curr_t_y) == 1)
                || (curr_h_y.abs_diff(curr_t_y) > 1 && curr_h_x.abs_diff(curr_t_x) == 1)
            {
                if curr_h_y.abs_diff(curr_t_y) == 1 {
                    (move_closer(curr_h_x, curr_t_x), curr_h_y)
                } else {
                    (curr_h_x, move_closer(curr_h_y, curr_t_y))
                }
            } else {
                (
                    move_closer(curr_h_x, curr_t_x),
                    move_closer(curr_h_y, curr_t_y),
                )
            }
        };

        Coord(mod_t_x, mod_t_y)
    }

    fn adjust_tail(&mut self) -> &mut Self {
        let curr_head_position = self
            .head_positions
            .last()
            .expect("head positions should not be empty");
        let Coord(mod_t_x, mod_t_y) =
            self.get_new_tail_position(*curr_head_position, self.current_tail_position);

        self.tail_positions.insert(Coord(mod_t_x, mod_t_y));
        self.current_tail_position = Coord(mod_t_x, mod_t_y);

        self
    }
}

#[derive(Debug, Clone, Copy)]
enum MotionInstruction {
    Right(usize),
    Left(usize),
    Up(usize),
    Down(usize),
}

impl MotionInstruction {
    fn distance(&self) -> usize {
        match self {
            MotionInstruction::Right(x) => *x,
            MotionInstruction::Left(x) => *x,
            MotionInstruction::Up(x) => *x,
            MotionInstruction::Down(x) => *x,
        }
    }
}

impl From<&String> for MotionInstruction {
    fn from(s: &String) -> Self {
        let xs = s.split(' ').collect::<Vec<_>>();
        let dist = xs.get(1).map(|n| n.parse::<usize>().expect("")).expect("");

        match *xs.first().expect("there should be some character") {
            "R" => MotionInstruction::Right(dist),
            "L" => MotionInstruction::Left(dist),
            "U" => MotionInstruction::Up(dist),
            "D" => MotionInstruction::Down(dist),
            _ => panic!("not possible"),
        }
    }
}
