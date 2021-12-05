namespace Day2

inductive Command where
| forward | up | down
deriving Repr

structure CommandWithVal where
  command: Command
  val: Nat

structure Location where
  horizontal: Nat
  depth: Nat
deriving Repr

structure LocationEnhanced extends Location where
  aim: Nat
deriving Repr

namespace Location

def default (h: Nat := 0) (d: Nat := 0) (a: Nat := 0): Location := {horizontal := h, depth := d}

def locToString (l: Location): String :=
  s!"\{horizontal: {l.horizontal}, depth: {l.depth} }"

instance : ToString Location where
  toString := locToString

end Location

namespace LocationEnhanced

def default (l: Location := Location.default) (a: Nat := 0): LocationEnhanced := 
  { horizontal := l.horizontal, depth := l.depth, aim := a}

end LocationEnhanced

namespace Command

def fromString (s:String): OptionM Command :=
  match s with
  | "forward" => some forward
  | "up" => some up
  | "down" => some down
  | _ => none

end Command

namespace CommandWithVal

def fromString (s:String): OptionM CommandWithVal :=
  let xs := s.splitOn
  (fun x y => {command := x, val := y}) <$> (xs.head?.bind Command.fromString) <*> xs.getLast?.map String.toNat!

def operation (loc: Location) (c: CommandWithVal): Location := 
  match c.command with
    | Command.forward => { horizontal := loc.horizontal + c.val, depth := loc.depth }
    | Command.down => { horizontal := loc.horizontal, depth := loc.depth + c.val }
    | Command.up => { horizontal := loc.horizontal, depth := loc.depth - c.val }

def operationEnhanced (loc: LocationEnhanced) (c: CommandWithVal): LocationEnhanced := 
  match c.command with
    | Command.forward => 
      { horizontal := loc.horizontal + c.val, depth := loc.depth + (loc.aim * c.val), aim := loc.aim }
    | Command.down => 
      { horizontal := loc.horizontal, depth := loc.depth, aim := loc.aim + c.val }
    | Command.up => 
      { horizontal := loc.horizontal, depth := loc.depth, aim := loc.aim - c.val }

end CommandWithVal

def commands (xs: Array String): Array CommandWithVal :=
  xs.filterMap CommandWithVal.fromString

def solvePart1 (xs: Array CommandWithVal): Nat := 
  let loc := xs.foldl CommandWithVal.operation Location.default
  loc.horizontal * loc.depth

def solvePart2 (xs: Array CommandWithVal): Nat := 
  let loc := xs.foldl CommandWithVal.operationEnhanced LocationEnhanced.default
  loc.horizontal * loc.depth

open IO.FS
def run: IO Unit := do
  let day2 := "./data/day2.txt"
  let readings <- lines day2
  let parsedCommands := commands readings

  let part1 := solvePart1 parsedCommands
  let part2 := solvePart2 parsedCommands

  IO.println (part1, part2)

end Day2
