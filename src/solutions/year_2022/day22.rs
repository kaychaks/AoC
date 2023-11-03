use std::{fmt::Debug, iter::repeat_with};

use aoc_lib::{Input, Solver};
use nom::{
    character::complete::{anychar, digit1},
    combinator::{map_res, opt},
    multi::many1,
    sequence::pair,
    IResult,
};

pub struct Day22 {}

const CUBE_FACES: [[(usize, usize); 4]; 6] = [
    [(0, 50), (0, 99), (49, 50), (49, 99)],       // back
    [(0, 100), (0, 149), (49, 100), (49, 149)],   // left
    [(50, 50), (50, 99), (99, 50), (99, 99)],     // top
    [(100, 50), (100, 99), (149, 50), (149, 99)], // front
    [(100, 0), (100, 49), (149, 0), (149, 49)],   // right
    [(150, 0), (150, 49), (199, 0), (199, 49)],   // base
];

const EDGE_WRAPS: [[[(usize, usize); 2]; 4]; 6] = [
    // [   Right   ,     Left  ,   Top     ,    Bottom   ]
    [
        [(0, 100), (49, 100)],
        [(100, 0), (149, 0)],
        [(150, 0), (199, 0)],
        [(50, 50), (50, 99)],
    ], // back
    [
        [(149, 99), (100, 99)],
        [(0, 99), (49, 99)],
        [(199, 0), (199, 49)],
        [(50, 99), (99, 99)],
    ], // left
    [
        [(49, 100), (49, 149)],
        [(100, 0), (100, 49)],
        [(49, 50), (49, 99)],
        [(100, 50), (100, 99)],
    ], // top
    [
        [(49, 149), (49, 100)],
        [(100, 49), (149, 99)],
        [(99, 50), (99, 99)],
        [(100, 49), (199, 49)],
    ], // front
    [
        [(100, 50), (149, 50)],
        [(49, 50), (0, 50)],
        [(50, 50), (99, 50)],
        [(150, 0), (150, 49)],
    ], // right
    [
        [(149, 50), (149, 99)],
        [(0, 50), (0, 99)],
        [(149, 0), (149, 49)],
        [(0, 100), (0, 149)],
    ], // base
];

impl Solver for Day22 {
    type OutputPart1 = usize;

    type OutputPart2 = usize;

    fn day() -> u8 {
        22
    }
    fn year() -> u16 {
        2022
    }

    fn solution_part1(input: aoc_lib::Input) -> Option<Self::OutputPart1> {
        let (mut board, commands) = Board::setup(input);
        commands.iter().for_each(|c| {
            board.play(c, false);
        });
        let (x, y, f) = board.current_pos;

        Some(1000 * (x + 1) + 4 * (y + 1) + usize::from(f))
    }

    fn solution_part2(input: aoc_lib::Input) -> Option<Self::OutputPart2> {
        let (mut board, commands) = Board::setup(input);
        commands.iter().for_each(|c| {
            board.play(c, true);
        });
        let (x, y, f) = board.current_pos;

        Some(1000 * (x + 1) + 4 * (y + 1) + usize::from(f))
    }
}

#[derive(Debug)]
enum CubeFaces {
    Top(usize, usize),
    Back(usize, usize),
    Front(usize, usize),
    Left(usize, usize),
    Right(usize, usize),
    Base(usize, usize),
}

impl From<(usize, usize)> for CubeFaces {
    fn from(i: (usize, usize)) -> Self {
        match i {
            (x, y) if (0..50).contains(&x) && (50..100).contains(&y) => CubeFaces::Back(x, y),
            (x, y) if (0..50).contains(&x) && (100..150).contains(&y) => CubeFaces::Left(x, y),
            (x, y) if (50..100).contains(&x) && (50..100).contains(&y) => CubeFaces::Top(x, y),
            (x, y) if (100..150).contains(&x) && (50..100).contains(&y) => CubeFaces::Front(x, y),
            (x, y) if (100..150).contains(&x) && (0..50).contains(&y) => CubeFaces::Right(x, y),
            (x, y) if (150..200).contains(&x) && (0..50).contains(&y) => CubeFaces::Base(x, y),
            _ => unreachable!(""),
        }
    }
}

impl CubeFaces {
    fn next_cell(&self, dir: Facing) -> (usize, usize) {
        println!("{:?}", self);
        let handle_back = |(&x, &y): (&usize, &usize)| {
            let (start_x, start_y) = (CUBE_FACES[0][0].0, CUBE_FACES[0][0].1);
            let (end_x, end_y) = (CUBE_FACES[0][3].0, CUBE_FACES[0][3].1);
            let (dx, dy) = (x - start_x, y - start_y);
            match dir {
                Facing::Right => {
                    let ny = y + 1;
                    if ny > end_y {
                        (EDGE_WRAPS[0][0][0].0 + dx, EDGE_WRAPS[0][0][1].1)
                    } else {
                        (x, ny)
                    }
                }
                Facing::Left => {
                    let ny = y - 1;
                    if ny < start_y {
                        (EDGE_WRAPS[0][1][0].0 + dx, EDGE_WRAPS[0][1][1].1)
                    } else {
                        (x, ny)
                    }
                }
                Facing::Up => {
                    let nx_opt =
                        x.checked_sub(1)
                            .and_then(|x| if x < start_x { None } else { Some(x) });
                    if let Some(nx) = nx_opt {
                        (nx, y)
                    } else {
                        (EDGE_WRAPS[0][2][0].0 + dx, EDGE_WRAPS[0][2][1].1)
                    }
                }
                Facing::Down => {
                    let nx = x + 1;
                    if nx > end_x {
                        (EDGE_WRAPS[0][2][0].0, EDGE_WRAPS[0][2][0].1 + dy)
                    } else {
                        (nx, y)
                    }
                }
            }
        };

        let handle_top = |(&x, &y): (&usize, &usize)| {
            let top_face = CUBE_FACES[2];
            let top_edge_wrap = EDGE_WRAPS[2];
            let (start_x, start_y) = (top_face[0].0, top_face[0].1);
            let (end_x, end_y) = (top_face[3].0, top_face[3].1);
            let (dx, dy) = (x - start_x, y - start_y);
            match dir {
                Facing::Right => {
                    let ny = y + 1;
                    if ny > end_y {
                        (top_edge_wrap[0][0].0, top_edge_wrap[0][0].1 + dy)
                    } else {
                        (x, ny)
                    }
                }
                Facing::Left => {
                    let ny = y - 1;
                    if ny < start_y {
                        (top_edge_wrap[1][0].0, top_edge_wrap[1][0].1 + dy)
                    } else {
                        (x, ny)
                    }
                }
                Facing::Up => {
                    let nx_opt =
                        x.checked_sub(1)
                            .and_then(|x| if x < start_x { None } else { Some(x) });
                    if let Some(nx) = nx_opt {
                        (nx, y)
                    } else {
                        (top_edge_wrap[2][0].0, top_edge_wrap[2][0].1 + dy)
                    }
                }
                Facing::Down => {
                    let nx = x + 1;
                    if nx > end_x {
                        (top_edge_wrap[2][0].0, top_edge_wrap[2][0].1 + dy)
                    } else {
                        (nx, y)
                    }
                }
            }
        };

        let handle_front = |(&x, &y): (&usize, &usize)| {
            let front_face = CUBE_FACES[3];
            let front_edge_wrap = EDGE_WRAPS[3];
            let (start_x, start_y) = (front_face[0].0, front_face[0].1);
            let (end_x, end_y) = (front_face[3].0, front_face[3].1);
            let (dx, dy) = (x - start_x, y - start_y);
            match dir {
                Facing::Right => {
                    let ny = y + 1;
                    if ny > end_y {
                        (front_edge_wrap[0][0].0, front_edge_wrap[0][0].1 - dy)
                    } else {
                        (x, ny)
                    }
                }
                Facing::Left => {
                    let ny = y - 1;
                    if ny < start_y {
                        (front_edge_wrap[1][0].0 + dx, front_edge_wrap[1][0].1 + dy)
                    } else {
                        (x, ny)
                    }
                }
                Facing::Up => {
                    let nx_opt =
                        x.checked_sub(1)
                            .and_then(|x| if x < start_x { None } else { Some(x) });
                    if let Some(nx) = nx_opt {
                        (nx, y)
                    } else {
                        (front_edge_wrap[2][0].0, front_edge_wrap[2][0].1 + dy)
                    }
                }
                Facing::Down => {
                    let nx = x + 1;
                    if nx > end_x {
                        (front_edge_wrap[2][0].0 + dx, front_edge_wrap[2][1].1)
                    } else {
                        (nx, y)
                    }
                }
            }
        };

        let handle_left = |(&x, &y): (&usize, &usize)| {
            let left_face = CUBE_FACES[1];
            let left_edge_wrap = EDGE_WRAPS[1];
            let (start_x, start_y) = (left_face[0].0, left_face[0].1);
            let (end_x, end_y) = (left_face[3].0, left_face[3].1);
            let (dx, dy) = (x - start_x, y - start_y);
            match dir {
                Facing::Right => {
                    let ny = y + 1;
                    if ny > end_y {
                        (left_edge_wrap[0][0].0 - dx, left_edge_wrap[0][1].1)
                    } else {
                        (x, ny)
                    }
                }
                Facing::Left => {
                    let ny = y - 1;
                    if ny < start_y {
                        (left_edge_wrap[1][0].0 + dx, left_edge_wrap[1][1].1)
                    } else {
                        (x, ny)
                    }
                }
                Facing::Up => {
                    let nx_opt =
                        x.checked_sub(1)
                            .and_then(|x| if x < start_x { None } else { Some(x) });
                    if let Some(nx) = nx_opt {
                        (nx, y)
                    } else {
                        (left_edge_wrap[2][0].0, left_edge_wrap[2][0].1 + dy)
                    }
                }
                Facing::Down => {
                    let nx = x + 1;
                    if nx > end_x {
                        (left_edge_wrap[2][0].0, left_edge_wrap[2][0].1 + dy)
                    } else {
                        (nx, y)
                    }
                }
            }
        };

        let handle_right = |(&x, &y): (&usize, &usize)| {
            let right_face = CUBE_FACES[4];
            let right_edge_wrap = EDGE_WRAPS[4];
            let (start_x, start_y) = (right_face[0].0, right_face[0].1);
            let (end_x, end_y) = (right_face[3].0, right_face[3].1);
            let (dx, dy) = (x - start_x, y - start_y);
            match dir {
                Facing::Right => {
                    let ny = y + 1;
                    if ny > end_y {
                        (right_edge_wrap[0][0].0 + dx, right_edge_wrap[0][1].1)
                    } else {
                        (x, ny)
                    }
                }
                Facing::Left => {
                    let ny_opt =
                        y.checked_sub(1)
                            .and_then(|y| if y < start_y { None } else { Some(y) });
                    if let Some(ny) = ny_opt {
                        (x, ny)
                    } else {
                        (right_edge_wrap[1][0].0 - dx, right_edge_wrap[1][1].1)
                    }
                }
                Facing::Up => {
                    let nx_opt =
                        x.checked_sub(1)
                            .and_then(|x| if x < start_x { None } else { Some(x) });
                    if let Some(nx) = nx_opt {
                        (nx, y)
                    } else {
                        (right_edge_wrap[2][0].0 + dx, right_edge_wrap[2][1].1)
                    }
                }
                Facing::Down => {
                    let nx = x + 1;
                    if nx > end_x {
                        (right_edge_wrap[2][0].0, right_edge_wrap[2][0].1 + dy)
                    } else {
                        (nx, y)
                    }
                }
            }
        };

        let handle_base = |(&x, &y): (&usize, &usize)| {
            let base_face = CUBE_FACES[5];
            let base_edge_wrap = EDGE_WRAPS[5];
            let (start_x, start_y) = (base_face[0].0, base_face[0].1);
            let (end_x, end_y) = (base_face[3].0, base_face[3].1);
            let (dx, dy) = (x - start_x, y - start_y);
            match dir {
                Facing::Right => {
                    let ny = y + 1;
                    if ny > end_y {
                        (base_edge_wrap[0][0].0, base_edge_wrap[0][0].1 + dy)
                    } else {
                        (x, ny)
                    }
                }
                Facing::Left => {
                    let ny_opt =
                        y.checked_sub(1)
                            .and_then(|y| if y < start_y { None } else { Some(y) });
                    if let Some(ny) = ny_opt {
                        (x, ny)
                    } else {
                        (base_edge_wrap[1][0].0, base_edge_wrap[1][0].1 + dy)
                    }
                }
                Facing::Up => {
                    let nx_opt =
                        x.checked_sub(1)
                            .and_then(|x| if x < start_x { None } else { Some(x) });
                    if let Some(nx) = nx_opt {
                        (nx, y)
                    } else {
                        (base_edge_wrap[2][0].0, base_edge_wrap[2][0].1 + dy)
                    }
                }
                Facing::Down => {
                    let nx = x + 1;
                    if nx > end_x {
                        (base_edge_wrap[2][0].0, base_edge_wrap[2][0].1 + dy)
                    } else {
                        (nx, y)
                    }
                }
            }
        };

        match self {
            CubeFaces::Top(x, y) => handle_top((x, y)),
            CubeFaces::Back(x, y) => handle_back((x, y)),
            CubeFaces::Front(x, y) => handle_front((x, y)),
            CubeFaces::Left(x, y) => handle_left((x, y)),
            CubeFaces::Right(x, y) => handle_right((x, y)),
            CubeFaces::Base(x, y) => handle_base((x, y)),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Cell {
    Blank,
    Open,
    Wall,
}

impl From<char> for Cell {
    fn from(cell: char) -> Self {
        match cell {
            '#' => Cell::Wall,
            '.' => Cell::Open,
            _ => Cell::Blank,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Turn {
    Clockwise,
    CounterClockwise,
}

#[derive(Debug)]
struct Command {
    steps: usize,
    turn: Option<Turn>,
}

impl Command {
    fn parse(input: &str) -> Vec<(usize, Option<char>)> {
        let p: IResult<&str, Vec<(usize, Option<char>)>> = many1(pair(
            map_res(digit1, |d: &str| d.parse::<usize>()),
            opt(anychar),
        ))(input);
        p.expect("could not parse commands").1
    }

    fn setup(steps: usize, t: Option<char>) -> Self {
        if let Some(t) = t {
            if t == 'R' {
                Command {
                    steps,
                    turn: Some(Turn::Clockwise),
                }
            } else {
                Command {
                    steps,
                    turn: Some(Turn::CounterClockwise),
                }
            }
        } else {
            Command { steps, turn: None }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Facing {
    Right,
    Left,
    Up,
    Down,
}

impl Facing {
    fn turn(&self, t: Turn) -> Self {
        match self {
            Facing::Right => match t {
                Turn::Clockwise => Facing::Down,
                Turn::CounterClockwise => Facing::Up,
            },
            Facing::Left => match t {
                Turn::Clockwise => Facing::Up,
                Turn::CounterClockwise => Facing::Down,
            },
            Facing::Up => match t {
                Turn::Clockwise => Facing::Right,
                Turn::CounterClockwise => Facing::Left,
            },
            Facing::Down => match t {
                Turn::Clockwise => Facing::Left,
                Turn::CounterClockwise => Facing::Right,
            },
        }
    }
}

impl From<Facing> for usize {
    fn from(f: Facing) -> Self {
        match f {
            Facing::Right => 0,
            Facing::Left => 2,
            Facing::Up => 3,
            Facing::Down => 1,
        }
    }
}

#[derive(Debug)]
struct Board {
    // each row will have list of cells + length of row
    cells: Vec<Vec<Cell>>,
    current_pos: (usize, usize, Facing),
    height: usize,
    width: usize,
}

impl Board {
    fn setup(input: Input) -> (Self, Vec<Command>) {
        let (board_input, commands_input) = input
            .lines
            .split(|p| p.is_empty())
            .collect::<Vec<_>>()
            .split_first()
            .map(|(a, xs)| (*a, *xs.first().expect("commands string is empty")))
            .expect("expecting a proper input");
        let xs = board_input
            .iter()
            .fold((0, (vec![])), |(width, mut xs), l| {
                let mut cs: Vec<Cell> = l.chars().map(Cell::from).collect();
                let c = cs.len();
                if c < width {
                    cs.extend(vec![Cell::Blank].repeat(width - c));
                    let c = cs.len();
                    xs.push((cs, c));
                    (c, xs)
                } else {
                    xs.push((cs, c));
                    (c, xs)
                }
            });
        let cells =
            xs.1.into_iter()
                .map(|(mut cs, sz)| {
                    let width = xs.0;
                    if sz < width {
                        cs.extend(vec![Cell::Blank].repeat(width - sz));
                        cs
                    } else {
                        cs
                    }
                })
                .collect::<Vec<_>>();
        let height = cells.len();
        let width = cells.first().map(|f| f.len()).expect("proper width");
        let current_pos = Board::tiles_in_a_dir(&cells, (0, 0), Facing::Right, height, width)
            .find(|o| match o {
                Some((_, _, c)) => match c {
                    Cell::Blank => false,
                    Cell::Open => true,
                    Cell::Wall => false,
                },
                None => false,
            })
            .map(|o| (o.unwrap().0, o.unwrap().1, Facing::Right))
            .unwrap();

        let commands = commands_input
            .first()
            .map(|cs| {
                Command::parse(cs)
                    .iter()
                    .map(|(sz, c)| Command::setup(*sz, *c))
                    .collect::<Vec<_>>()
            })
            .expect("could not parse commands");

        (
            Board {
                cells,
                current_pos,
                height,
                width,
            },
            commands,
        )
    }

    fn tiles_in_a_cube<'a>(
        cells: &'a [Vec<Cell>],
        start: (usize, usize),
        dir: Facing,
        // height: usize,
        // width: usize,
    ) -> Box<dyn Iterator<Item = Option<(usize, usize, Cell)>> + 'a> {
        println!("{:?} {:?}", start, dir);
        let mut current = start;
        Box::new(repeat_with(move || {
            current = CubeFaces::from(current).next_cell(dir);
            println!("{:?}", current);
            let row = cells.get(current.0);
            row.and_then(|r| r.get(current.1).map(|c| (current.0, current.1, *c)))
        }))
    }

    fn tiles_in_a_dir<'a>(
        cells: &'a [Vec<Cell>],
        start: (usize, usize),
        dir: Facing,
        height: usize,
        width: usize,
    ) -> Box<dyn Iterator<Item = Option<(usize, usize, Cell)>> + 'a> {
        match dir {
            Facing::Right => {
                let mut index = start.1;
                Box::new(repeat_with(move || {
                    let row = cells.get(start.0);
                    let ret = row.and_then(|r| r.get(index).map(|c| (start.0, index, *c)));
                    index = (index + 1) % width;
                    ret
                }))
            }
            Facing::Left => {
                let mut index = start.1;
                Box::new(repeat_with(move || {
                    let row = cells.get(start.0);
                    let ret = row.and_then(|r| r.get(index).map(|c| (start.0, index, *c)));
                    let i = index.checked_sub(1);
                    if let Some(i) = i {
                        index = i;
                    } else {
                        index = width;
                    }
                    ret
                }))
            }
            Facing::Up => {
                let mut index = start.0;
                Box::new(repeat_with(move || {
                    let row = cells.get(index);
                    let ret = row.and_then(|r| r.get(start.1).map(|c| (index, start.1, *c)));
                    let i = index.checked_sub(1);
                    if let Some(i) = i {
                        index = i;
                    } else {
                        index = height;
                    }
                    ret
                }))
            }
            Facing::Down => {
                let mut index = start.0;
                Box::new(repeat_with(move || {
                    let row = cells.get(index);
                    let ret = row.and_then(|r| r.get(start.1).map(|c| (index, start.1, *c)));
                    index = (index + 1) % height;
                    ret
                }))
            }
        }
    }

    fn play(&mut self, command: &Command, follow_cube: bool) -> &mut Self {
        println!("{:?}", command);
        let Command { steps, turn } = command;
        let (x, y, facing) = &self.current_pos;
        let handle_moves = |ms: Box<dyn Iterator<Item = Option<(usize, usize, Cell)>>>| {
            ms.filter(|o| o.map(|(_, _, c)| c != Cell::Blank).unwrap_or(false))
                .enumerate()
                .take_while(|(step, mv)| {
                    println!("{:?}", mv);
                    mv.map(|(_, _, cell)| {
                        if cell == Cell::Wall {
                            false
                        } else {
                            step <= steps
                        }
                    })
                    .unwrap_or(false)
                })
                .map(|x| x.1.expect("could not play"))
                .last()
        };
        if let Some(turn) = turn {
            let moves = {
                if !follow_cube {
                    Board::tiles_in_a_dir(&self.cells, (*x, *y), *facing, self.height, self.width)
                } else {
                    Board::tiles_in_a_cube(&self.cells, (*x, *y), *facing)
                }
            };
            let facing = facing.turn(*turn);
            if let Some((x, y, _)) = handle_moves(moves) {
                self.current_pos = (x, y, facing)
            } else {
                self.current_pos = (*x, *y, facing)
            }
        } else {
            let moves = {
                if !follow_cube {
                    Board::tiles_in_a_dir(&self.cells, (*x, *y), *facing, self.height, self.width)
                } else {
                    Board::tiles_in_a_cube(&self.cells, (*x, *y), *facing)
                }
            };
            if let Some((x, y, _)) = handle_moves(moves) {
                self.current_pos = (x, y, *facing)
            } else {
                self.current_pos = (*x, *y, *facing)
            }
        }
        self
    }
}
