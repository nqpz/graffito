-- | Library for setting and getting values.  Also supports functional values.
--
-- This library exposes a number of modules setgetN, for N >= 1, which support
-- setting a series of values and getting those values back.  For example, you
-- can have one piece of code produce some values:
--
--    let elems = setget5.set x y z v w
--
-- And then another piece of code consume those values:
--
--    let result = setget5.get elems (\a b c d e -> (a + b, c * d * e))
--
-- which will return the result of the expression of the lambda.
--
-- If you need to extend this module to support more elements, you need to
-- extend both the modules in the local module `internal` and the externally
-- visible modules in the end of the file.

-- | Setting and getting values.
module type setget = {
  type^ elems '^base
  type^ f '^base '^a
  val set '^base: f base (elems base)
  val get '^base '^a: elems base -> f base a -> a
  val map '^from '^to: (from -> to) -> elems from -> elems to
  val fold '^base: (base -> base -> base) -> elems base -> base
}

-- Used while doing the incremental building of the internal modules.
local module type setget_intermediate = {
  type^ t_setf '^base '^a
  val setf '^base '^a: (base -> a) -> t_setf base a

  type^ elems '^base
  type^ t_get '^base '^a
  val get '^base '^a: elems base -> (base -> t_get base a) -> a

  val map '^from '^to: (from -> to) -> elems from -> elems to
  val fold '^base: (base -> base -> base) -> elems base -> base
}

local module specialize (sg: setget_intermediate) = {
  open sg
  type^ f '^base '^a = base -> t_get base a
  def set = setf id
}

local module increment (prev: setget_intermediate) = specialize {
  type^ t_setf '^base '^a = base -> prev.t_setf base (base, a)
  def setf f x = prev.setf (f >-> \o -> (x, o))

  type^ elems '^base = (base, prev.elems base)
  type^ t_get '^base '^a = base -> prev.t_get base a
  def get (x, prev_xs) f = prev.get prev_xs (f x)

  def map f (x, prev_xs) = (f x, prev.map f prev_xs)
  def fold f (x, prev_xs) = f x (prev.fold f prev_xs)
}

local module internal = {
  module setget1 = specialize {
    type^ t_setf '^base '^a = base -> a
    def setf f x = f x

    type^ elems '^base = base
    type^ t_get '^base '^a = a
    def get x f = f x

    def map f x = f x
    def fold _f x = x
  }

  module setget2 = increment setget1
  module setget3 = increment setget2
  module setget4 = increment setget3
  module setget5 = increment setget4
  module setget6 = increment setget5
  module setget7 = increment setget6
  module setget8 = increment setget7
}

local module expose (sg: setget): setget with f '^base '^a = sg.f base a = {
  open sg

  -- Provide a get function that applies the elements in the expected
  -- order. It's only at this point that we know that `get` can be called with
  -- `set`, so we cannot have this fix earlier.
  def get = flip get set >-> get
}

-- Externally visible.
module setget1 = expose internal.setget1
module setget2 = expose internal.setget2
module setget3 = expose internal.setget3
module setget4 = expose internal.setget4
module setget5 = expose internal.setget5
module setget6 = expose internal.setget6
module setget7 = expose internal.setget7
module setget8 = expose internal.setget8
