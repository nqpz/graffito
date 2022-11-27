-- | Probabilistic Game of Life.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"
import "../../src/oklab"

module gameoflifeprob = mk_stencil {
  open stencil_kinds.square

  module two = import "two"
  module three = import "three"

  type certain_state = #alive | #dead
  type cell = {base: certain_state, actual: f32, decay: f32, rng: rng}

  def decay_factor = 0.0001f32

  def build_cell (base: certain_state) (decay: f32) (rng: rng): cell =
    let actual =
      match base
      case #alive -> (1f32 - decay)
      case #dead -> (0f32 + decay)
    in {base, actual, decay, rng}

  def collapse (p: f32) (rng: rng): (certain_state, rng) =
    let (rng, v) = dist.rand (0, 1) rng
    let p' = if v <= p
             then #alive
             else #dead
    in (p', rng)

  def new_cell (cell: cell) neighbors =
    let extract (mx: maybe cell): f32 =
      match mx
      case #some c -> c.actual
      case #none -> 0

    let ns = seq.map extract neighbors
    in seq.get ns (\p0 p1 p2 p3 p4 p5 p6 p7 ->
                        let two = two.calc p0 p1 p2 p3 p4 p5 p6 p7
                        let three = three.calc p0 p1 p2 p3 p4 p5 p6 p7
                        let p = cell.actual * (two + three)
                                + (1 - cell.actual) * three
                        let (base, rng) = collapse p cell.rng
                        let decay = if base == cell.base
                                    then cell.decay + decay_factor
                                    else 0
                        in build_cell base decay rng)

  def render_cell (cell: cell) =
    let c = oklab_to_linear_srgb (from_LCh {L=cell.actual, C=1, h=cell.decay * 100})
    in argb.from_rgba c.r c.g c.b 1

  def create_gradient_cell (h: i64) (rng: rng) (y: index): (rng, cell) =
    let (rng, k) = dist_int.rand (0, h) rng
    let cell = build_cell (if k < y then #alive else #dead) 0 rng
    in (rng, cell)

  type create_input = rng
  type create_output = rng
  def create_cells (h: i64) (w: i64) (rng: rng): ([h][w]cell, rng) =
    let rngs = rnge.split_rng (h * w) rng
    let coordinates = tabulate_2d h w (\y _x -> y)
                      |> flatten_to (h * w)
    let (rngs, cells) = map2 (create_gradient_cell h) rngs coordinates
                        |> unzip
    in (unflatten h w cells, rnge.join_rng rngs)

  -- open create_random_cells {
  --   type cell = cell
  --   def random_cell rng =
  --     let (rng, alive) = dist_int.rand (0, 1) rng
  --     let cell = build_cell (if alive == 1 then #alive else #dead) 0 rng
  --     in (rng, cell)
  -- }
}

module lys = mk_stencil_lys gameoflifeprob
