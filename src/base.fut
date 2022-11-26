type index = i64

type maybe 'a = #some a
              | #none

def maybe_map 'a 'b (f: a -> b) (m: maybe a): maybe b =
  match m
  case #some x -> #some (f x)
  case #none -> #none

def maybe_join 'a (mm: maybe (maybe a)): maybe a =
  match mm
  case #some m -> m
  case #none -> #none
