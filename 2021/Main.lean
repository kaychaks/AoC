import AoC

def main : IO Unit := do
  let (day1Part1, day1Part2)  <- Day1.run
  IO.println (day1Part1, day1Part2)

#eval main
