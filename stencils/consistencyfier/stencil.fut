-- | Consistencyfier
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"
import "../../src/oklab"

module consistencyfier = mk_stencil {
  open stencil_kinds.cross

  type cell = {color: Lab,
               rng: rng}

  def find_color_at_distance ({L, a, b}: Lab) (d: f32) (rng: rng): (rng, Lab) =
    -- (L1 - L0)**2 + (a1 - a0)**2 + (b1 - b0)**2 = d**2
    let (rng, var) = dist_i32.rand (0, 2) rng
    let new_color =
      if var == 0
      then {L=L + d, a, b}
      else if var == 1
      then {L, a = a + d, b}
      else {L, a, b = b + d}
    in (rng, new_color)

  def merge_color ({L=L0, a=a0, b=b0}: Lab) ({L=L1, a=a1, b=b1}: Lab): Lab =
    {L=L0 + L1, a=a0 + a1, b=b0 + b1}

  def avg_color (len: f32) ({L, a, b}: Lab): Lab =
    {L=L / len, a=a / len, b=b / len}

  def new_cell (cell: cell) neighbors: cell =
    let extract (mx: maybe.t cell): Lab =
      match mx
      case #some c -> find_color_at_distance c.color 0.0001 c.rng |> (.1)
      case #none -> cell.color

    let color = seq.map extract neighbors
                |> seq.foldr merge_color
                |> avg_color (r32 seq.length)
    let (rng, _) = dist.rand (0, 1) cell.rng -- Progress RNG for real.
    in {color, rng}

  def render_cell (cell: cell) =
    cell.color
    |> oklab_to_linear_srgb
    |> \c -> argb.from_rgba c.r c.g c.b 1

  open create_random_cells {
    type cell = cell

    def random_cell rng =
      let (rng, L) = dist.rand (0, 1) rng
      let (rng, C) = dist.rand (0, 1) rng
      let (rng, h) = dist.rand (0, 100) rng
      let color = from_LCh {L, C, h}

      let cell = {color, rng}
      in (rng, cell)
  }
}

module lys = mk_stencil_lys consistencyfier
