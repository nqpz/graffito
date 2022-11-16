-- | Probabilistic Game of Life.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"

def collapse (p: f32) (rng: rng): (f32, rng) =
  let (rng, v) = dist.rand (0, 1) rng
  let p' = if v <= p
           then 1
           else 0
  in (p', rng)

module gameoflifeprob = mk_stencil {
  open stencil_kinds.square

  type cell = (f32, rng)

  module two = import "two"
  module three = import "three"

  def new_cell (alive, rng) neighbors =
    let check (mx: maybe cell): f32 =
      match mx
      case #some (x, _) -> x
      case #none -> 0

    let ns = setget.map check neighbors
    in setget.get ns (\p0 p1 p2 p3 p4 p5 p6 p7 ->
                        let two = two.calc p0 p1 p2 p3 p4 p5 p6 p7
                        let three = three.calc p0 p1 p2 p3 p4 p5 p6 p7
                        let p = alive * (two + three)
                                + (1 - alive) * three

                        -- Collapse the probability in some cases in order to
                        -- see anything interesting at all.
                        let (p', rng) = if p > 0.409 && p <= 0.7
                                        then collapse p rng
                                        else (p, rng)
                        in (p', rng))

  def render_cell (alive, _) =
    argb.gray alive

  open create_random_cells {
    type cell = cell
    def random_cell rng =
      let (rng, alive) = dist.rand (0, 1) rng
      in (rng, (alive, rng))
  }
}

module lys = mk_stencil_lys gameoflifeprob
