type index = i64

module maybe = {
  type t 'a = #some a
            | #none

  def map 'a 'b (f: a -> b) (m: t a): t b =
    match m
    case #some x -> #some (f x)
    case #none -> #none

  def join 'a (mm: t (t a)): t a =
    match mm
    case #some m -> m
    case #none -> #none

  def bind 'a 'b (f: a -> t b) (m: t a): t b =
    map f m |> join

  def or 'a (f: () -> a) (m: t a) =
    match m
    case #some x -> x
    case #none -> f ()
}
