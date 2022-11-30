import "base"
import "seq"
import "stencil"

-- Temporary modules.  The real ones seem to be causing problems in this setting
-- even though they maybe work in the interpreter?  These modules only correctly
-- implement the things that are needed by the stencil code and skips over the
-- unneeded parts.
module seq4' = {
  type^ elems '^base = (base, base, base, base)
  type^ f '^base '^a = base -> base -> base -> base -> a
  def set a b c d = (a, b, c, d)
  def get (a, b, c, d) v = v a b c d
  def zip (a0, b0, c0, d0) (a1, b1, c1, d1) =
    ((a0, a1), (b0, b1), (c0, c1), (d0, d1))
  def map v (a, b, c, d) = (v a, v b, v c, v d)
  def foldr v (a, b, c, d) = v a (v b (v c d))
  def find_first v (a, b, c, d) = v a
  def find_first' v (a, b, c, d) = a
  def range = (0, 1, 2, 3)
  def length = 4
  def replicate x = (x, x, x, x)
  def random (a, b, c, d) rng = (a, rng)

  module nf = {
    type elems 'base = (base, base, base, base, base, base, base, base)
    type^ f 'base 'a = base -> base -> base -> base -> base -> base -> base -> base -> a
    def set a b c d e f g h = (a, b, c, d, e, f, g, h)
    def get (a, b, c, d, e, f, g, h) v = v a b c d e f g h
    def zip (a0, b0, c0, d0, e0, f0, g0, h0) (a1, b1, c1, d1, e1, f1, g1, h1) =
      ((a0, a1), (b0, b1), (c0, c1), (d0, d1), (e0, e1), (f0, f1), (g0, g1), (h0, h1))
    def map v (a, b, c, d, e, f, g, h) = (v a, v b, v c, v d, v e, v f, v g, v h)
    def foldr v (a, b, c, d, e, f, g, h) = v a (v b (v c (v d (v e (v f (v g h))))))
    def find_first v (a, b, c, d, e, f, g, h) = v a
    def find_first' v (a, b, c, d, e, f, g, h) = a
    def range = (0, 1, 2, 3, 4, 5, 6, 7)
    def length = 8
    def replicate x = (x, x, x, x, x, x, x, x)
    def random (a, b, c, d, e, f, g, h) rng = (a, rng)
  }
}

module seq8' = {
  type^ elems '^base = (base, base, base, base, base, base, base, base)
  type^ f '^base '^a = base -> base -> base -> base -> base -> base -> base -> base -> a
  def set a b c d e f g h = (a, b, c, d, e, f, g, h)
  def get (a, b, c, d, e, f, g, h) v = v a b c d e f g h
  def zip (a0, b0, c0, d0, e0, f0, g0, h0) (a1, b1, c1, d1, e1, f1, g1, h1) =
    ((a0, a1), (b0, b1), (c0, c1), (d0, d1), (e0, e1), (f0, f1), (g0, g1), (h0, h1))
  def map v (a, b, c, d, e, f, g, h) = (v a, v b, v c, v d, v e, v f, v g, v h)
  def foldr v (a, b, c, d, e, f, g, h) = v a (v b (v c (v d (v e (v f (v g h))))))
  def find_first v (a, b, c, d, e, f, g, h) = v a
  def find_first' v (a, b, c, d, e, f, g, h) = a
  def range = (0, 1, 2, 3, 4, 5, 6, 7)
  def length = 8
  def replicate x = (x, x, x, x, x, x, x, x)
  def random (a, b, c, d, e, f, g, h) rng = (a, rng)

  module nf = {
    type elems 'base = (base, base, base, base, base, base, base, base)
    type^ f 'base 'a = base -> base -> base -> base -> base -> base -> base -> base -> a
    def set a b c d e f g h = (a, b, c, d, e, f, g, h)
    def get (a, b, c, d, e, f, g, h) v = v a b c d e f g h
    def zip (a0, b0, c0, d0, e0, f0, g0, h0) (a1, b1, c1, d1, e1, f1, g1, h1) =
      ((a0, a1), (b0, b1), (c0, c1), (d0, d1), (e0, e1), (f0, f1), (g0, g1), (h0, h1))
    def map v (a, b, c, d, e, f, g, h) = (v a, v b, v c, v d, v e, v f, v g, v h)
    def foldr v (a, b, c, d, e, f, g, h) = v a (v b (v c (v d (v e (v f (v g h))))))
    def find_first v (a, b, c, d, e, f, g, h) = v a
    def find_first' v (a, b, c, d, e, f, g, h) = a
    def range = (0, 1, 2, 3, 4, 5, 6, 7)
    def length = 8
    def replicate x = (x, x, x, x, x, x, x, x)
    def random (a, b, c, d, e, f, g, h) rng = (a, rng)
  }
}


local module corner_conditions = {
  module pixel_grid = {
    def is_not_in_corner (s: coordinate_state): bool =
      s.y > 0 && s.y < s.h - 1 && s.x > 0 && s.x < s.w - 1
  }
}

module stencil_kinds = {
  module cross = {
    module seq = seq4'
    open corner_conditions.pixel_grid

    def neighbor_offsets: seq.elems neighbor_offset =
      seq.set (-1,  0, \s -> s.y > 0)
              ( 0, -1, \s -> s.x > 0)
              ( 0,  1, \s -> s.x < s.w - 1)
              ( 1,  0, \s -> s.y < s.h - 1)
  }

  module square = {
    module seq = seq8'
    open corner_conditions.pixel_grid

    def neighbor_offsets: seq.elems neighbor_offset =
      seq.set (-1, -1, \s -> s.y > 0 && s.x > 0)
              (-1,  0, \s -> s.y > 0)
              (-1,  1, \s -> s.y > 0 && s.x < s.w - 1)
              ( 0, -1, \s -> s.x > 0)
              ( 0,  1, \s -> s.x < s.w - 1)
              ( 1, -1, \s -> s.y < s.h - 1 && s.x > 0)
              ( 1,  0, \s -> s.y < s.h - 1)
              ( 1,  1, \s -> s.y < s.h - 1 && s.x < s.w - 1)
  }
}
