namespace Day3

inductive Bit where
  | zero 
  | one
 deriving Repr, Inhabited, DecidableEq

namespace Bit

  def bitToString : Bit -> String :=
    fun b => match b with
    | Bit.zero => "zero"
    | Bit.one => "one"
  
  instance : ToString Bit where
    toString := bitToString

  def fromChar (c: Char): OptionM Bit :=
    match c with
    | '0' => some Bit.zero
    | '1' => some Bit.one
    | _ => none

  def toNat (b: Bit): Nat :=
    match b with
      | Bit.zero => 0
      | Bit.one => 1

  def parse (xs: String): Array Bit :=
    xs.foldl (fun acc c => flip Option.getD acc <| Option.map acc.push <| fromChar c) #[]

  def toDecimal : Array Bit -> Nat 
  | #[] => 1
  | bs =>
    bs.reverse.foldl 
      (fun acc b => (acc.fst + b.toNat * 2 ^ acc.snd, acc.snd + 1)) 
      (0, 0) |>
    Prod.fst
end Bit

structure BitCount where
  zero : Nat
  one: Nat
deriving Repr, Inhabited

namespace BitCount

  def bitCountToString : BitCount -> String :=
    fun bs => s!"\{zero:= {bs.zero}, one:= {bs.one}}"

  instance : ToString BitCount where
    toString := bitCountToString

  def default : BitCount := { zero := 0, one := 0 }

  def bitCountAdd (a: BitCount) (b: BitCount): BitCount := 
   { zero := a.zero + b.zero,  one := a.one + b.one }

  instance : Add BitCount where
    add := bitCountAdd

  def restArr (len: Nat) (idx: Nat): Array BitCount :=
    if (len > idx) then
      #[]
     else
      Array.mkArray (idx + 1 - len) BitCount.default 

  def update (bs: Array BitCount) (b: Bit) (idx: Nat): Array BitCount :=
    let ys: BitCount × Array BitCount := 
      bs.get? idx |> 
      Option.map (fun a => (a, bs)) |> 
      flip Option.getD (BitCount.default, bs.append <| restArr bs.size idx)

    match b with
    | Bit.zero => ys.snd.setD idx { ys.fst with zero := ys.fst.zero + 1 }
    | Bit.one =>  ys.snd.setD idx { ys.fst with one := ys.fst.one + 1 }

    
  def fromBits (xs: Array Bit): Array BitCount :=
    xs.foldl (fun (acc, idx) b => (update acc b idx, idx + 1)) (#[], 0) |>
    Prod.fst

  def updateBitCount : BitCount -> Bit -> BitCount
  | bc, Bit.zero => { bc with zero := bc.zero + 1 }
  | bc, Bit.one => { bc with one := bc.one + 1 }

  def fromBitsToBitCount (xs: Array Bit): BitCount :=
    xs.foldl updateBitCount BitCount.default

  def filterCommon : Nat -> BitCount -> (BitCount -> Bit) -> Array Bit -> Bool :=
    fun idx b f xs => 
      let x: OptionM Bool := do 
        let v <- xs.get? idx 
        if v = f b then true else none
      
      x.getD false

  def mostCommon : BitCount -> Bit := fun b => if b.one >= b.zero then Bit.one else Bit.zero
  def leastCommon : BitCount -> Bit := fun b => if b.one >= b.zero then Bit.zero else Bit.one

  def intermediateAcc : Array (Array Bit) × Array (Array Bit) -> Nat -> BitCount × BitCount :=
    fun out index =>
      (fromBitsToBitCount <| out.fst.map (·.get! index), fromBitsToBitCount <| out.snd.map (·.get! index))

  def checkEmpty (xs: Array α) (ys: Array α): Array α :=
    if (ys.size = 0) then xs else ys
  
  def filterTuple : Array (Array Bit) × Array (Array Bit) -> Nat -> BitCount × BitCount -> Array (Array Bit) × Array (Array Bit) :=
    fun out index acc =>
        (
          checkEmpty out.fst <| out.fst.filter (filterCommon index acc.fst mostCommon), 
          checkEmpty out.snd <| out.snd.filter (filterCommon index acc.snd leastCommon)
        )


  partial def update':  Array (Array Bit) × Array (Array Bit) -> Array (Array Bit) × Array (Array Bit) :=
    fun (xs, ys) => do
      let rec  loop (out: Array (Array Bit) × Array (Array Bit)) (acc: BitCount × BitCount) (index: Nat) := 
        if (out.fst.size <= 1 && out.snd.size <= 1) || (index + 1 = (out.fst.get! 0).size)  then
          out
        else
          loop 
            (filterTuple out index acc)
            (intermediateAcc (filterTuple out index acc) (index + 1))
            (index + 1)
      
      loop (xs, ys) (intermediateAcc (xs, ys) 0) 0


  def fromBits' (xs: Array (Array Bit)): Array (Array Bit) × Array (Array Bit) :=
    update' (xs, xs) 
   

  def merge (x: Array BitCount) (y: Array BitCount): Array BitCount :=
    if x.size > 0
    then 
      if y.size > 0 then
        x.zipWith y Add.add
      else
        x
    else
      y

  def mergeAll (xs: Array (Array BitCount)): Array BitCount :=
    xs.foldl (fun acc ys => merge acc ys) #[]

  def toGammaEpsilon (bc: BitCount): Bit × Bit :=
    if bc.one > bc.zero then (Bit.one, Bit.zero) else (Bit.zero, Bit.one)
   
  def toRates (bs: Array BitCount): (Array Bit × Array Bit) :=
    bs.map toGammaEpsilon |> 
    Array.foldl 
      (fun (gs, es) (g, e) => (gs.push g, es.push e)) 
      (#[] , #[])

end BitCount



def part1 (bs: Array (Array Bit)): Nat := do
    bs.map BitCount.fromBits |>
    BitCount.mergeAll |> 
    BitCount.toRates |>
    Prod.map Bit.toDecimal Bit.toDecimal |>
    fun (g, e) => g * e


def part2 (bs: Array (Array Bit)) := 
  let (o2, co2) := Prod.map (Bit.toDecimal <| ·.get! 0) (Bit.toDecimal <| ·.get! 0) <| BitCount.fromBits' bs
  o2 * co2

open IO.FS

def run : IO Unit := do
  let day3 := "./data/day3.txt"
  let readings <- lines day3

  let bits := readings.map Bit.parse

  let part1Result <- part1 bits
  let part2Result <- part2 bits

  IO.println <| ( part1Result, part2Result )

end Day3
