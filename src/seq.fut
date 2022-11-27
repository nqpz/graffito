-- | Library for setting and getting values.  Also supports functional values.
--
-- This library exposes a number of modules seqN, for N >= 1, which support
-- setting a series of values and getting those values back.  For example, you
-- can have one piece of code produce some values:
--
--    let elems = seq5.set x y z v w
--
-- And then another piece of code consume those values:
--
--    let result = seq5.get elems (\a b c d e -> (a + b, c * d * e))
--
-- which will return the result of the expression of the lambda.
--
-- Each module has a submodule `nf` which only supports nonfunctional
-- elements. This is useful if you need to store the result in an array or
-- return it from a branch.
--
-- If you need to extend this module to support more elements, you need to
-- extend both the modules in the local module `internal` and the externally
-- visible modules in the end of the file.

import "base"

-- | Setting and getting values.
module type seq = {
  type^ elems '^base
  type^ f '^base '^a
  val set '^base: f base (elems base)
  val get '^base '^a: elems base -> f base a -> a
  val zip '^t '^u: elems t -> elems u -> elems (t, u)
  val map '^from '^to: (from -> to) -> elems from -> elems to
  val fold '^base: (base -> base -> base) -> elems base -> base
  val find_first 't 'u: (t -> maybe u) -> elems t -> maybe u
  val replicate '^t: t -> elems t

  module nf: {
    type elems 'base
    type^ f 'base 'a
    val set 'base: f base (elems base)
    val get 'base 'a: elems base -> f base a -> a
    val zip 't 'u: elems t -> elems u -> elems (t, u)
    val map 'from 'to: (from -> to) -> elems from -> elems to
    val fold 'base: (base -> base -> base) -> elems base -> base
    val find_first 't 'u: (t -> maybe u) -> elems t -> maybe u
    val replicate 't: t -> elems t
  }
}

-- Used while doing the incremental building of the internal modules.
local module type seq_intermediate = {
  type^ t_setf '^base '^a
  val setf '^base '^a: (base -> a) -> t_setf base a

  type^ elems '^base
  type^ t_get '^base '^a
  val get '^base '^a: elems base -> (base -> t_get base a) -> a

  val zip '^t '^u: elems t -> elems u -> elems (t, u)
  val map '^from '^to: (from -> to) -> elems from -> elems to
  val fold '^base: (base -> base -> base) -> elems base -> base
  val find_first 't 'u: (t -> maybe u) -> elems t -> maybe u
  val replicate '^t: t -> elems t

  module nf: {
    type^ t_setf 'base 'a
    val setf 'base 'a: (base -> a) -> t_setf base a

    type elems 'base
    type^ t_get 'base 'a
    val get 'base 'a: elems base -> (base -> t_get base a) -> a

    val zip 't 'u: elems t -> elems u -> elems (t, u)
    val map 'from 'to: (from -> to) -> elems from -> elems to
    val fold 'base: (base -> base -> base) -> elems base -> base
    val find_first 't 'u: (t -> maybe u) -> elems t -> maybe u
    val replicate 't: t -> elems t
  }
}

local module specialize (sg: seq_intermediate) = {
  open sg
  type^ f '^base '^a = base -> t_get base a
  def set = setf id

  module nf = {
    open sg.nf
    type^ f 'base 'a = base -> t_get base a
    def set = setf id
  }
}

local module increment (prev: seq_intermediate) = specialize {
  type^ t_setf '^base '^a = base -> prev.t_setf base (base, a)
  def setf f x = prev.setf (f >-> \o -> (x, o))

  type^ elems '^base = (base, prev.elems base)
  type^ t_get '^base '^a = base -> prev.t_get base a
  def get (x, prev_xs) f = prev.get prev_xs (f x)

  def zip (x, prev_xs) (y, prev_ys) = ((x, y), prev.zip prev_xs prev_ys)
  def map f (x, prev_xs) = (f x, prev.map f prev_xs)
  def fold f (x, prev_xs) = f x (prev.fold f prev_xs)
  def find_first f (x, prev_xs) =
    match f x
    case #some y -> #some y
    case #none -> prev.find_first f prev_xs
  def replicate x = (x, prev.replicate x)

  module nf = {
    module prev = prev.nf

    type^ t_setf 'base 'a = base -> prev.t_setf base (base, a)
    def setf f x = prev.setf (f >-> \o -> (x, o))

    type elems 'base = (base, prev.elems base)
    type^ t_get 'base 'a = base -> prev.t_get base a
    def get (x, prev_xs) f = prev.get prev_xs (f x)

    def zip (x, prev_xs) (y, prev_ys) = ((x, y), prev.zip prev_xs prev_ys)
    def map f (x, prev_xs) = (f x, prev.map f prev_xs)
    def fold f (x, prev_xs) = f x (prev.fold f prev_xs)
    def find_first f (x, prev_xs) =
      match f x
      case #some y -> #some y
      case #none -> prev.find_first f prev_xs
    def replicate x = (x, prev.replicate x)
  }
}

local module internal = {
  module seq1 = specialize {
    type^ t_setf '^base '^a = base -> a
    def setf f x = f x

    type^ elems '^base = base
    type^ t_get '^base '^a = a
    def get x f = f x

    def zip x y = (x, y)
    def map f x = f x
    def fold _f x = x
    def find_first f x = f x
    def replicate x = x

    module nf = {
      type^ t_setf 'base 'a = base -> a
      def setf f x = f x

      type elems 'base = base
      type^ t_get 'base 'a = a
      def get = get

      def zip = zip
      def map = map
      def fold = fold
      def find_first = find_first
      def replicate = replicate
    }
  }

  module seq2 = increment seq1
  module seq3 = increment seq2
  module seq4 = increment seq3
  module seq5 = increment seq4
  module seq6 = increment seq5
  module seq7 = increment seq6
  module seq8 = increment seq7
}

local module expose (sg: seq): seq with f '^base '^a = sg.f base a
                                   with nf.f 'base 'a = sg.nf.f base a = {
  open sg

  -- Provide a get function that applies the elements in the expected
  -- order. It's only at this point that we know that `get` can be called with
  -- `set`, so we cannot have this fix earlier.
  def get = flip get set >-> get

  module nf = {
    open sg.nf

    def get = flip get set >-> get
  }
}

-- Externally visible.
module seq1 = expose internal.seq1
module seq2 = expose internal.seq2
module seq3 = expose internal.seq3
module seq4 = expose internal.seq4
module seq5 = expose internal.seq5
module seq6 = expose internal.seq6
module seq7 = expose internal.seq7
module seq8 = expose internal.seq8
