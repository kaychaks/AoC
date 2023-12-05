use std::{path::PathBuf, process::Command};

use aoc_lib::{create_solution, Input, Solver};

pub(crate) struct Day0 {}

fn sol_1(input: Input) -> Option<Out> {
  let lake_file_dir = PathBuf::new()
    .join("src")
    .join("solutions")
    .join(&format!("year_{}", 2023))
    .join(&format!("day{}", 0));
  let output = Command::new("lake")
    .args(["--dir", lake_file_dir.into_os_string().to_str().unwrap()])
    .args(["script", "run"])
    .arg("solution")
    .arg(input.lines.join("\n"))
    .output()
    .expect("could not find lake file");
  let result = String::from_utf8(output.stdout).unwrap();
  let result = result.trim();

  Some(result.into())
}

impl From<&str> for Out {
    fn from(value: &str) -> Self {
        Self(value.parse().unwrap())
    }
}

#[derive(Debug)]
pub struct Out(u16);

create_solution! {
  out_1 => Out;
  out_2 => ();
  year => 2023;
  day => 0;
  sol_1 => sol_1;
  sol_2 => |input: Input| {
    None
  };
}
