import Lake
open Lake DSL

package «day0»

def f1 {α: Type} {β : Type} {γ : Type} (x : α × (β ⊕ γ)) : (α × β) ⊕ (α × γ) :=
  match x.snd with
  | Sum.inl y => Sum.inl (x.fst, y)
  | Sum.inr z => Sum.inr (x.fst, z)

def f2 {α: Type} (x: Bool × α) : α ⊕ α :=
  match x.fst with
  | true => Sum.inl x.snd
  | false => Sum.inr x.snd

#eval f1 (false,  Sum.inr 1) (β := String)


script solution (args) do
  if h : 0 < args.length then
    let input := args[0]'h
    let x := input.splitOn "\n" |> List.map String.toNat! |> List.foldl Nat.add 0
    IO.print x
  return 0
