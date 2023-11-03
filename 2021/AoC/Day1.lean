namespace Day1

open IO.FS

section Common
  variable {α β γ ζ : Type u}
  variable {a : Type u -> Type v}

  def liftA3 [Applicative a] (f: α -> β -> γ -> ζ) (x: a α) (y: a β) (z: a γ) : a ζ := f <$> x <*> y <*> z
end Common

section Part1
  open Prod
  def measureLarge (x: Nat × String) (z: String) : Nat × String := 
    if x.snd.toNat! < z.toNat! then (x.fst + 1, z) else (x.fst, z)
  
  def measureLargeNat (x: Nat × Nat) (z: Nat) : Nat × Nat := 
    if x.snd < z then (x.fst + 1, z) else (x.fst, z)

  def totalIncr (s: Sum Nat String) (xs: Array String := #[]) (ys: Array Nat := #[]): Nat :=
    match s with
    | Sum.inr _ =>  Prod.fst <| Array.foldl measureLarge (0, Array.get! xs 0) xs
    | _ => Prod.fst <| Array.foldl measureLargeNat (0, Array.get! ys 0) ys
    
end Part1

section Part2

  def lookAheadSum? (xs: Array String) (idx: Nat) : OptionM Nat :=
    let size := xs.size
    if size = 0 then
      Option.none
    else
      let x: OptionM Nat := String.toNat! <$> xs.get? idx
      let y: OptionM Nat := String.toNat! <$> xs.get? (idx + 1)
      let z: OptionM Nat := String.toNat! <$> xs.get? (idx + 2)
      liftA3 (· + · + ·) x y z
             

  def windowEndIndex (length: Nat): Nat := length - (length % 3)

  /-
    below code might look like imperative shit but Lean is smart enough
    to convert the same via join points. So, everything gets converted 
    to functional code in the end
  -/
  def threeWindowSums (xs: Array String): Array Nat := do
    let mut ys := #[]
    for id in [:windowEndIndex xs.size] do
      match (lookAheadSum? xs id) with
      | some x => ys := ys.push x
      | _ => ()
    return ys
  
end Part2

def run : IO Unit := do
  let day1 := "./data/day1.txt"
  let readings <- lines day1

  let part1Total := totalIncr (Sum.inr "") readings
  let part2Total := totalIncr (Sum.inl 0) (ys := threeWindowSums readings)

  IO.println (part1Total, part2Total)

  end Day1


