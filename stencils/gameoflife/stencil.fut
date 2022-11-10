-- | Straightforward Game of Life implementation with no frills.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"

module gameoflife = mk_stencil {
  open specialize_stencil_kind stencil_kinds.square {
    type cell = bool -- alive or dead
  }

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

  def random_alive rng =
    let (rng, alive) = dist_int.rand (0, 1) rng
    in (rng, bool.i64 alive)

  type create_input = rng
  type create_output = rng
  def create_cells (h: i64) (w: i64) (rng: rng): ([h][w]cell, rng) =
    let (rngs, cells) = rnge.split_rng (h * w) rng
                        |> map random_alive
                        |> unzip
    in (unflatten h w cells, rnge.join_rng rngs)
}

module lys = mk_stencil_lys gameoflife
