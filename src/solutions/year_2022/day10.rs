use std::fmt::Debug;

use aoc_lib::Solver;

#[derive(Debug, Default)]
pub struct Day10 {
    cycles: Vec<(i32, i32, i32)>,
}

#[derive(Clone, Default)]
struct Crt {
    rows: Vec<PixelType>,
    current_sprite_pos: usize,
}

#[derive(Clone, Copy)]
enum PixelType {
    Lit,
    Dark,
}

impl Solver for Day10 {
  fn day() -> u8 {
      10
  }
  fn solution_part1(input: aoc_lib::Input) -> Option<Self::OutputPart1> {
      let mut d = Day10::default();
      d.load_instructions(input.lines.iter().map(Instruction::from), |_, _| {});

      let s = [20, 60, 100, 140, 180, 220]
          .map(|i| (i, d.cycles[i - 1]))
          .iter()
          .map(|(n, (_, during, _))| *n as i32 * during)
          .sum::<i32>();
      Some(s)
  }

  fn solution_part2(input: aoc_lib::Input) -> Option<Self::OutputPart2> {
      let mut crt = Crt::default();
      let mut d = Day10::default();
      d.load_instructions(input.lines.iter().map(Instruction::from), |x, y| {
          crt.handle_cyle(x, y);
      });

      println!("{:?}", crt.clone());
      Some(())
  }

  type OutputPart1 = i32;
  type OutputPart2 = ();
}

impl From<PixelType> for usize {
    fn from(val: PixelType) -> Self {
        match val {
            PixelType::Lit => 1,
            PixelType::Dark => 0,
        }
    }
}

impl From<PixelType> for String {
    fn from(p: PixelType) -> Self {
        match p {
            PixelType::Lit => "#".to_string(),
            PixelType::Dark => ".".to_string(),
        }
    }
}

impl Crt {
    fn handle_cyle(&mut self, register_val_after: i32, register_val_during: i32) {
        let crt_draw_pos = self.rows.iter().enumerate().last().map(|(index, _)| index);

        let sprite_pos = [
            (register_val_during as usize % 40).saturating_sub(1),
            register_val_during as usize % 40,
            register_val_during as usize % 40 + 1,
        ];
        let next_crt_pos = crt_draw_pos.map(|p| p % 40 + 1).unwrap_or(0);
        let should_lit = sprite_pos.contains(&next_crt_pos);

        if should_lit {
            self.rows.push(PixelType::Lit);
        } else {
            self.rows.push(PixelType::Dark)
        }
        self.current_sprite_pos = register_val_after as usize;
    }
}

impl Debug for Crt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.rows
            .chunks(40)
            .map(|r| r.iter().map(|x| String::from(*x)))
            .for_each(|x| {
                f.write_str(&x.collect::<Vec<_>>().join(" ")).expect("");
                f.write_str("\n").expect("")
            });
        Ok(())
    }
}

impl Day10 {
    fn load_instructions<F, G>(&mut self, instructions: F, mut cycle_callback: G) -> &mut Self
    where
        F: Iterator<Item = Instruction>,
        G: FnMut(i32, i32),
    {
        instructions.for_each(|instr| match instr {
            Instruction::Noop => {
                if self.cycles.is_empty() {
                    self.cycles.push((0, 1, 1));
                    cycle_callback(1, 0);
                } else {
                    let (_, _, after) = self
                        .cycles
                        .last()
                        .expect("expected some value in the register");
                    let after = *after;
                    self.cycles.push((after, after, after));
                    cycle_callback(after, after);
                }
            }
            Instruction::AddX(a) => {
                if self.cycles.is_empty() {
                    self.cycles.push((0, 1, 1));
                    cycle_callback(1, 0);
                    self.cycles.push((1, 1, 1 + a));
                    cycle_callback(1 + a, 1);
                } else {
                    let cycles_1 = self.cycles.clone();
                    let (_, _, after) = cycles_1
                        .last()
                        .expect("expected some value in the register");

                    self.cycles.push((*after, *after, *after));
                    cycle_callback(*after, *after);
                    self.cycles.push((*after, *after, after + a));
                    cycle_callback(*after + a, *after);
                }
            }
        });
        self
    }
}


#[derive(Debug)]
enum Instruction {
    AddX(i32),
    Noop,
}

impl From<&String> for Instruction {
    fn from(s: &String) -> Self {
        let xs = s.split(' ').collect::<Vec<_>>();
        match *xs.first().expect("expecting a string") {
            "addx" => Self::AddX(
                xs.get(1)
                    .map(|x| x.parse::<i32>().expect("expecting a number"))
                    .expect("not possible"),
            ),
            "noop" => Self::Noop,
            _ => panic!("not possible"),
        }
    }
}
