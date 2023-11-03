use std::{
    fmt::Debug,
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};

#[derive(Debug, Clone)]
pub struct Input {
    pub lines: Vec<String>,
}

pub struct Day {
    day: u8,
    year: u16,
}

pub enum Part {
    Part1,
    Part2,
}

impl Part {}

impl Debug for Day {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}", self.day))
    }
}

impl TryFrom<(u8, u16)> for Day {
    type Error = &'static str;
    fn try_from(value: (u8, u16)) -> Result<Self, Self::Error> {
        if (1..=25).contains(&value.0) {
            Ok(Day {
                day: value.0,
                year: value.1,
            })
        } else {
            Err("Day and Year values are invalid")
        }
    }
}

impl TryFrom<u8> for Part {
    type Error = &'static str;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Part::Part1),
            2 => Ok(Part::Part2),
            _ => Err("Part value should be either 1 or 2"),
        }
    }
}

impl Debug for Part {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Part::Part1 => f.write_str("1"),
            Part::Part2 => f.write_str("2"),
        }
    }
}

impl Input {
    pub fn new(day: Day) -> Self {
        // let mut d = String::from("day");
        // d.push_str(&day._day.to_string());
        // let p = match part {
        //     Part::Part1 => "part1.txt",
        //     Part::Part2 => "part2.txt",
        // };

        // let data_file_path = PathBuf::new().join("data").join(&d).join(p);
        // let year = year.unwrap_or(chrono::Utc::now().year());
        let data_file_path = PathBuf::new()
            .join("data")
            .join(format!("{}/day{}.txt", day.year, day.day));
        let data_file = File::open(&data_file_path)
            .unwrap_or_else(|_| panic!("could not open data file {:?}", data_file_path.as_path()));
        let buf_reader = BufReader::new(data_file);
        Input {
            lines: buf_reader
                .lines()
                .map(|x| Result::unwrap_or(x, "ERROR".to_string()))
                .filter(|x| x != "ERROR")
                .collect(),
        }
    }
}
