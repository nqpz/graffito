import "../src/seq"

local module tests = {
  def get =
    let elems = seq4.set 1 2 3 4
    in seq4.get elems (\a b c d -> a == 1 && b == 2 && c == 3 && d == 4)

  def map =
    let elems = seq3.set 10 20 30
    let elems' = seq3.map (\n -> (n > 15, n + 5)) elems
    in seq3.get elems' (\a b c -> a == (false, 15) && b == (true, 25) && c == (true, 35))

  def fold =
    let elems = seq6.set 10 20 30 40 100 200
    in seq6.fold (+) elems == 400

  def functional =
    let elems = seq2.set (+ 1) (* 2)
                |> seq2.map (\f -> f 10)
    in seq2.get elems (\a b -> a == 11 && b == 20)

  def nonfunctional =
    let elems = seq2.nf.set 1 2
    in seq2.nf.get elems (\a b -> a == 1 && b == 2)
}

def test () =
  assert tests.get true
  && assert tests.map true
  && assert tests.fold true
  && assert tests.functional true
  && assert tests.nonfunctional true
