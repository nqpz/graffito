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
import "compose"
import "random"

local module type seq_core_f = {
  type^ elems '^base
  type^ f '^base '^a
  val set '^base: f base (elems base)
  val get '^base '^a: elems base -> f base a -> a
  val zip '^t '^u: elems t -> elems u -> elems (t, u)
  val map '^from '^to: (from -> to) -> elems from -> elems to
  val foldr '^base: (base -> base -> base) -> elems base -> base
  val find_first '^t 'u: (t -> maybe u) -> elems t -> maybe u
  -- | Pick the last element if the function returns false for all previous
  -- elements.
  val find_first' 't: (t -> bool) -> elems t -> t
  val replicate '^t: t -> elems t
  val range: elems i32
  val length: i32
}

local module type seq_core_nf = {
  type elems 'base
  type^ f 'base 'a
  val set 'base: f base (elems base)
  val get 'base 'a: elems base -> f base a -> a
  val zip 't 'u: elems t -> elems u -> elems (t, u)
  val map 'from 'to: (from -> to) -> elems from -> elems to
  val foldr 'base: (base -> base -> base) -> elems base -> base
  val find_first '^t 'u: (t -> maybe u) -> elems t -> maybe u
  val find_first' 't: (t -> bool) -> elems t -> t
  val replicate 't: t -> elems t
  val range: elems i32
  val length: i32
}

local module type seq_core = {
  module nf: seq_core_nf

  include seq_core_f
}

module type seq = {
  module nf: {
    include seq_core_nf

    val random 't: elems t -> rng -> (t, rng)
  }

  include seq_core_f

  val random '^t: elems t -> rng -> (t, rng)
}

-- Used while doing the incremental building of the internal modules.
local module type seq_intermediate_f = {
  type^ t_setf '^base '^a
  val setf '^base '^a: (base -> a) -> t_setf base a

  type^ elems '^base
  type^ t_get '^base '^a
  val get '^base '^a: elems base -> (base -> t_get base a) -> a

  val zip '^t '^u: elems t -> elems u -> elems (t, u)
  val map '^from '^to: (from -> to) -> elems from -> elems to
  val foldr '^base: (base -> base -> base) -> elems base -> base
  val find_first '^t 'u: (t -> maybe u) -> elems t -> maybe u
  val find_first' 't: (t -> bool) -> elems t -> t
  val replicate '^t: t -> elems t
  val range: elems i32
  val length: i32
}

local module type seq_intermediate_nf = {
  type^ t_setf 'base 'a
  val setf 'base 'a: (base -> a) -> t_setf base a

  type elems 'base
  type^ t_get 'base 'a
  val get 'base 'a: elems base -> (base -> t_get base a) -> a

  val zip 't 'u: elems t -> elems u -> elems (t, u)
  val map 'from 'to: (from -> to) -> elems from -> elems to
  val foldr 'base: (base -> base -> base) -> elems base -> base
  val find_first '^t 'u: (t -> maybe u) -> elems t -> maybe u
  val find_first' 't: (t -> bool) -> elems t -> t
  val replicate 't: t -> elems t
  val range: elems i32
  val length: i32
}

local module type seq_intermediate = {
  module nf: seq_intermediate_nf

  include seq_intermediate_f
}

local module type seq_intermediate_extra = {
  module nf: {
    include seq_intermediate_nf
    type^ f 'base 'a = base -> t_get base a
    val set 'base: f base (elems base)
  }

  include seq_intermediate_f
  type^ f '^base '^a = base -> t_get base a
  val set '^base: f base (elems base)
}

local module specialize (seq: seq_intermediate) = {
  open seq
  type^ f '^base '^a = base -> t_get base a
  def set = setf id

  module nf = {
    open seq.nf
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
  def foldr f (x, prev_xs) = f x (prev.foldr f prev_xs)
  def find_first f (x, prev_xs) =
    match f x
    case #some y -> #some y
    case #none -> prev.find_first f prev_xs
  def find_first' f (x, prev_xs) =
    if f x
    then x
    else prev.find_first' f prev_xs
  def replicate x = (x, prev.replicate x)
  def range = (0i32, prev.map (+ 1i32) prev.range)
  def length = 1 + prev.length

  module nf = {
    local module prev = prev.nf

    type^ t_setf 'base 'a = base -> prev.t_setf base (base, a)
    def setf f x = prev.setf (f >-> \o -> (x, o))

    type elems 'base = (base, prev.elems base)
    type^ t_get 'base 'a = base -> prev.t_get base a
    def get (x, prev_xs) f = prev.get prev_xs (f x)

    def zip (x, prev_xs) (y, prev_ys) = ((x, y), prev.zip prev_xs prev_ys)
    def map f (x, prev_xs) = (f x, prev.map f prev_xs)
    def foldr f (x, prev_xs) = f x (prev.foldr f prev_xs)
    def find_first f (x, prev_xs) =
      match f x
      case #some y -> #some y
      case #none -> prev.find_first f prev_xs
    def find_first' f (x, prev_xs) =
      if f x
      then x
      else prev.find_first' f prev_xs
    def replicate x = (x, prev.replicate x)
    def range = (0i32, prev.map (+ 1i32) prev.range)
    def length = length
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
    def foldr _f x = x
    def find_first f x = f x
    def find_first' _f x = x
    def replicate x = x
    def range = 0i32
    def length = 1i32

    module nf = {
      type^ t_setf 'base 'a = base -> a
      def setf f x = f x

      type elems 'base = base
      type^ t_get 'base 'a = a
      def get = get

      def zip = zip
      def map = map
      def foldr = foldr
      def find_first = find_first
      def find_first' = find_first'
      def replicate = replicate
      def range = range
      def length = length
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

local module extend_core (seq_core: seq_core): seq with f '^base '^a = seq_core.f base a
                                                   with nf.f 'base 'a = seq_core.nf.f base a = {
  open seq_core

  def random elems rng =
    let (rng, i) = dist_i32.rand (0, length - 1) rng
    let (_, x) = find_first' ((.0) >-> (== i)) (zip range elems)
    in (x, rng)

  module nf = {
    open seq_core.nf

    def random elems rng =
      let (rng, i) = dist_i32.rand (0, length - 1) rng
      let (_, x) = find_first' ((.0) >-> (== i)) (zip range elems)
      in (x, rng)
  }
}

local module expose (seq: seq_intermediate_extra)
                    (compose: compose with f '^inp '^outp = seq.f inp outp
                                      with nf.f 'inp 'outp = seq.nf.f inp outp)
             : seq with f '^base '^a = seq.f base a
                   with nf.f 'base 'a = seq.nf.f base a = extend_core {
  open seq

  -- Provide a function that applies the elements in the expected order. It's
  -- only at this point that we know that `get` can be called with `set`, so we
  -- cannot have this fix earlier.
  def set = compose.compose set (flip get set)

  module nf = {
    open seq.nf

    def set = compose.nf.compose set (flip get set)
  }
}

-- Externally visible.
module seq1 = expose internal.seq1 compose1
module seq2 = expose internal.seq2 compose2
module seq3 = expose internal.seq3 compose3
module seq4 = expose internal.seq4 compose4
module seq5 = expose internal.seq5 compose5
module seq6 = expose internal.seq6 compose6
module seq7 = expose internal.seq7 compose7
module seq8 = expose internal.seq8 compose8
