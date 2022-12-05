import "../src/base"
import "../src/seq"
import "../src/random"

local module tests = {
  def get =
    let elems = seq4.set 1 2 3 4
    in seq4.get elems (\a b c d -> a == 1 && b == 2 && c == 3 && d == 4)

  def zip =
    let elems0 = seq3.set 10 20 30
    let elems1 = seq3.set 4 3 2
    let elems = seq3.zip elems0 elems1
    in seq3.get elems (\a b c -> a == (10, 4) && b == (20, 3) && c == (30, 2))

  def map =
    let elems = seq3.set 10 20 30
    let elems' = seq3.map (\n -> (n > 15, n + 5)) elems
    in seq3.get elems' (\a b c -> a == (false, 15) && b == (true, 25) && c == (true, 35))

  def foldr_basic =
    let elems = seq6.set 10 20 30 40 100 200
    in seq6.foldr (+) elems == 400

  def foldr_direction_important =
    let elems = seq3.set 10 30 1
    in seq3.foldr (-) elems == -19 -- 10 - (30 - 1)

  def find_first_basic =
    let elems = seq6.set 10 20 30 40 100 200
    let first: maybe.t i64 =
      seq6.find_first (\k -> if k > 25 && k < 35
                             then #some (i64.i32 k)
                             else #none) elems
    in first == #some 30i64

  def find_first_direction_important =
    let elems = seq6.set 10 20 30 40 100 200
    let first: maybe.t i64 =
      seq6.find_first (\k -> if k > 25
                             then #some (i64.i32 k)
                             else #none) elems
    in first == #some 30i64

  def find_first' =
    let elems = seq6.set 10 20 30 40 100 200
    let first = seq6.find_first' (\k -> k > 50) elems
    in first == 100

  def find_first'_last =
    let elems = seq6.set 10 20 30 40 100 200
    let first = seq6.find_first' (\k -> k > 1000) elems
    in first == 200

  def replicate =
    let elems = seq4.replicate 5
    in seq4.get elems (\a b c d -> a == 5 && b == 5 && c == 5 && d == 5)

  def range =
    let elems = seq7.range
    in seq7.get elems (\a b c d e f g -> a == 0 && b == 1 && c == 2 && d == 3
                                         && e == 4 && f == 5 && g == 6)

  def functional =
    let elems = seq2.set (+ 1) (* 2)
                |> seq2.map (\f -> f 10)
    in seq2.get elems (\a b -> a == 11 && b == 20)

  def nonfunctional =
    let elems = seq2.nf.set 1 2
    in seq2.nf.get elems (\a b -> a == 1 && b == 2)

  def random =
    let elems = seq4.set 10 30 4 1
    let rng = rnge.rng_from_seed [i32.u32 123456]
    let (elem, _rng) = seq4.random elems rng
    in elem == 30
}

entry test =
  assert tests.get true
  && assert tests.zip true
  && assert tests.map true
  && assert tests.foldr_basic true
  && assert tests.foldr_direction_important true
  && assert tests.find_first_basic true
  && assert tests.find_first_direction_important true
  && assert tests.find_first' true
  && assert tests.find_first'_last true
  && assert tests.replicate true
  && assert tests.functional true
  && assert tests.nonfunctional true
  && assert tests.random true
