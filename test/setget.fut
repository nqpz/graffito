import "../src/setget"

def test () =
  let elems = setget4.set 1 2 3 4
  in setget4.get elems (\a b c d -> a == 1 && b == 2 && c == 3 && d == 4)
