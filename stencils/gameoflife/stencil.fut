-- | Straightforward Game of Life implementation with no frills.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"

module gameoflife = mk_stencil {
  open stencil_kinds.square

  type cell = bool

  def new_cell alive neighbors =
    let check (mx: maybe bool): i32 =
      match mx
      case #some x -> i32.bool x
      case #none -> 0

    let n_neighbors = setget.fold (+) (setget.map check neighbors)
    in if alive
       then if n_neighbors < 2 || n_neighbors > 3
            then false
            else alive
       else if n_neighbors == 3
            then true
            else alive

  def render_cell alive =
    if alive
    then argb.white
    else argb.black

  open create_random_cells {
    type cell = cell
    def random_cell rng =
      let (rng, alive) = dist_int.rand (0, 1) rng
      in (rng, bool.i64 alive)
  }
}

module lys = mk_stencil_lys gameoflife
