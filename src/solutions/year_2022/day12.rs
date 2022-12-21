use std::{collections::BinaryHeap, fmt::Debug, hash::Hash, iter::repeat_with};

use aoc_lib::{Input, Solver};
use indexmap::{
    map::Entry::{Occupied, Vacant},
    IndexMap,
};

pub struct Day12 {
    grid: Vec<Vec<Pos>>,
    start: Pos,
    goal: Pos,
}

impl Solver for Day12 {
    fn day() -> u8 {
        12
    }
    type OutputPart1 = usize;
    type OutputPart2 = usize;
    fn solution_part1(input: aoc_lib::Input) -> Option<Self::OutputPart1> {
        let d = Day12::setup(input);
        let ret = shortest_path(&d.start, |p| d.successors(p, false), |g| d.reached_end(g));
        ret.map(|(xs, _)| xs.len() - 1)
    }
    fn solution_part2(input: aoc_lib::Input) -> Option<Self::OutputPart2> {
        let d = Day12::setup(input);
        let ret = shortest_path(&d.goal, |p| d.successors(p, true), |s| d.reached_a(s));
        ret.map(|(xs, _)| xs.len() - 1)
    }
}

impl Day12 {
    fn setup(input: Input) -> Self {
        let mut start = None;
        let mut goal = None;
        let grid = input
            .lines
            .iter()
            .enumerate()
            .map(|(row, line)| {
                line.chars()
                    .enumerate()
                    .map(|(col, c)| {
                        if c == 'S' {
                            let pos = Pos(row as i32, col as i32, 'a');
                            start = Some(pos);
                            pos
                        } else if c == 'E' {
                            let pos = Pos(row as i32, col as i32, 'z');
                            goal = Some(pos);
                            pos
                        } else {
                            Pos(row as i32, col as i32, c)
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<Vec<_>>>();
        Day12 {
            grid,
            start: start.unwrap(),
            goal: goal.unwrap(),
        }
    }

    fn index(&self, x: &i32, y: &i32, compare: &char, desc: bool) -> Option<Pos> {
        self.grid
            .get(*x as usize)
            .and_then(|r| r.get(*y as usize))
            .and_then(|c| {
                if (desc && (c.2 as u32) + 1 >= (*compare) as u32)
                    || (!desc && (c.2 as u32) <= ((*compare) as u32) + 1)
                {
                    Some(Pos(*x, *y, c.2))
                } else {
                    None
                }
            })
    }

    fn successors(&self, Pos(x, y, c): &Pos, desc: bool) -> Vec<(Pos, usize)> {
        vec![(-1, 0), (1, 0), (0, -1), (0, 1)]
            .iter()
            .map(|(dx, dy)| self.index(&(x + dx), &(y + dy), c, desc))
            .into_iter()
            .filter(|p| p.is_some())
            .map(|p| (p.unwrap(), 1))
            .collect()
    }

    fn reached_end(&self, p: &Pos) -> bool {
        p == &self.goal
    }

    fn reached_a(&self, p: &Pos) -> bool {
        p.2 == 'a'
    }
}

fn reverse_paths<Node: Eq + Hash + Clone + Copy, F>(
    prevs: &mut IndexMap<Node, (VisitedIndex, Weight)>,
    mut prev: F,
    start: usize,
) -> Vec<Node>
where
    F: FnMut(&(VisitedIndex, Weight)) -> usize,
{
    let mut i = start;
    let xs = repeat_with(|| {
        let ps = prevs.clone();
        ps.get_index(i)
            .map(|(n, t)| {
                i = prev(t);
                n
            })
            .copied()
    })
    .take_while(|x| x.is_some())
    .flatten();

    let mut y = xs.collect::<Vec<_>>();
    y.reverse();
    y
}

fn shortest_path<Node, FN, IN, FS>(
    start_node: &Node,
    mut next_nodes: FN,
    mut stop_check: FS,
) -> Option<(Vec<Node>, Weight)>
where
    Node: Eq + Hash + Clone + Debug + Copy,
    FN: FnMut(&Node) -> IN,
    IN: IntoIterator<Item = (Node, Weight)> + Debug,
    FS: FnMut(&Node) -> bool,
{
    let mut q: BinaryHeap<State> = BinaryHeap::new();
    let mut prev: IndexMap<Node, (VisitedIndex, Weight)> = IndexMap::new();

    // initiate queue
    q.push(State::default());
    // initiate prev
    prev.insert(*start_node, (usize::MAX, 0));

    let mut destination_index: Option<usize> = None;
    while let Some(State { weight, index }) = q.pop() {
        let (node, (_, w)) = prev.get_index(index).unwrap();
        // check if we need to stop
        if stop_check(node) {
            destination_index = Some(index);
            break;
        }

        // if we have already seen the  node and the last path to it was best then
        // don't bother, continue
        if weight > *w {
            continue;
        }

        // at this point we need to look at the successors
        let next_nodes = next_nodes(node);

        for (next_node, travel_weight) in next_nodes {
            let new_weight = weight + travel_weight;
            let i;
            match prev.entry(next_node) {
                Occupied(visited) => {
                    let visited_weight = visited.get().1;
                    if visited_weight > new_weight {
                        i = visited.index();
                        prev.insert(next_node, (index, new_weight));
                    } else {
                        continue;
                    }
                }
                Vacant(unvisited) => {
                    i = unvisited.index();
                    prev.insert(next_node, (index, new_weight));
                }
            }

            q.push(State {
                index: i,
                weight: new_weight,
            });
        }
    }

    destination_index.map(|target| {
        (
            reverse_paths(&mut prev, |&(p, _)| p, target),
            prev.get_index(target).unwrap().1 .1,
        )
    })
}

type VisitedIndex = usize;
type Weight = usize;

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct Pos(i32, i32, char);

#[derive(Debug, Default)]
struct State {
    weight: Weight,
    index: VisitedIndex,
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.weight == other.weight
    }
}
impl Eq for State {}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .weight
            .cmp(&self.weight) // this is key for Min-Priority heap
            .then_with(|| self.index.cmp(&other.index)) // when tied, compare the indexes
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        static GOAL: (i32, i32) = (4, 6);
        let result = shortest_path(
            &(1, 1),
            |&(x, y)| {
                vec![
                    (x + 1, y + 2),
                    (x + 1, y - 2),
                    (x - 1, y + 2),
                    (x - 1, y - 2),
                    (x + 2, y + 1),
                    (x + 2, y - 1),
                    (x - 2, y + 1),
                    (x - 2, y - 1),
                ]
                .into_iter()
                .map(|p| (p, 1))
            },
            |&p| p == GOAL,
        );
        assert_eq!(result.expect("no path found").1, 4);
    }
}
