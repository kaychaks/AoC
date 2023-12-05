mod input;
mod solver;

pub use input::*;
pub use solver::*;

// proc macro to call the right method
#[macro_export]
macro_rules! solve {
  ($year:expr; $day:expr; $part:expr) => {
    paste::paste! {
        [<year_ $year>]::[<Day $day>]::[<solve_part $part>]()
    }
  };
}

#[macro_export]
macro_rules! create_solution {
  (
      out_1 => $out1:ty;
      out_2 => $out2:ty;
      year => $year:expr;
      day => $day:expr;
      sol_1 => $sol1:expr;
      sol_2 => $sol2:expr;
    ) => {
    paste::paste! {
          impl Solver for [<Day $day>] {
          type OutputPart1 = $out1;
          type OutputPart2 = $out2;
          fn year() -> u16 {
              $year
          }
          fn day() -> u8 {
              $day
          }
          fn solution_part1(input: Input) -> Option<Self::OutputPart1> {
              $sol1(input)
          }
          fn solution_part2(input: Input) -> Option<Self::OutputPart2> {
              $sol2(input)
          }
      }
    }
  };
}
