import "../src/setget"

local module tests = {
  def get =
    let elems = setget4.set 1 2 3 4
    in setget4.get elems (\a b c d -> a == 1 && b == 2 && c == 3 && d == 4)

  def map =
    let elems = setget3.set 10 20 30
    let elems' = setget3.map (\n -> (n > 15, n + 5)) elems
    in setget3.get elems' (\a b c -> a == (false, 15) && b == (true, 25) && c == (true, 35))

  def fold =
    let elems = setget6.set 10 20 30 40 100 200
    in setget6.fold (+) elems == 400
}

def test () =
  assert tests.get true
  && assert tests.map true
  && assert tests.fold true
