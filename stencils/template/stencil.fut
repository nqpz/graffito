-- | Basic template stencil.

-- Basic imports.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/stencil"
import "../../src/stencil_kinds"
-- Support getting random numbers.
import "../../src/random"
-- Support starting with a random grid.
import "../../src/utils"

-- We define our stencil in a module and call the mk_stencil functor to make it
-- into a stencil module.
module template = mk_stencil {
  -- Each cell looks up, right, down, and left.
  open stencil_kinds.cross

  -- Use two vaguely named floats and a pseudorandom number generator as the
  -- cell state.
  type cell = {a: f32, b: f32, rng: rng}

  -- Generate a new cell based on its contents and the contents of its 4
  -- neighbors.
  def new_cell cell neighbors =
    -- To support corners, where one or more neighbors may not exist (we don't
    -- have wraparound), each neighbor has type maybe.t cell.  We would like to
    -- do our computation on cells directly, without the maybe.t wrapping, so we
    -- convert them here and default to the current cell if a neighbor doesn't
    -- exist.
    let or_default (x: maybe.t cell) =
      match x
      case #some c -> c
      case #none -> cell

    -- Take the average.
    let mix c0 c1 f = (f c0 + f c1) / 2

    in seq.get (seq.map or_default neighbors)
               (\top right bottom left ->
                  -- Mix the y axis and x axis separately.
                  let a' = mix top bottom (.a)
                  let b' = mix left right (.b)
                  -- Add some randomness.
                  let (rng, a_extra) = dist.rand (-0.1, 0.1) cell.rng
                  let (rng, b_extra) = dist.rand (-0.1, 0.1) rng
                  -- Create a new cell with the new a and b and the updated rng.
                  in {a=a' + a_extra,
                      b=b' + b_extra,
                      rng})

  -- Render a and b as red and green, and mark blue and alpha as constant.
  def render_cell {a, b, rng=_} =
    argb.from_rgba a b 1 1

  -- Build the starting set of pseudorandom cells.
  open create_random_cells {
    type cell = cell
    def random_cell rng =
      -- Floats between 0 and 1.
      let (rng, a) = dist.rand (0, 1) rng
      let (rng, b) = dist.rand (0, 1) rng
      in (rng, {a, b, rng})
  }
}

-- Construct a lys module that can be used with the lys visualization tool.
module lys = mk_stencil_lys template
