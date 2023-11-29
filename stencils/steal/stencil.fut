-- | Steal
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"
import "../../src/oklab"

module steal = mk_stencil {
  open stencil_kinds.square

  type cell = {weight: f32,
               y: i32,
               x: i32,
               hue: f32}

  def new_cell (cell: cell) neighbors: cell =
    let extract (mx: maybe.t cell): cell =
      match mx
      case #some c -> c
      case #none -> {weight= -1, y=0, x=0, hue=0}

    let extract_offset (y, x, _) = {y, x}

    let merge (cell1, off1) (cell2, off2) =
      if cell1.weight > cell2.weight
      then (cell1, off1)
      else (cell2, off2)

    let (cell_largest, off_largest) =
      seq.zip (seq.map extract neighbors) (seq.map extract_offset neighbor_offsets)
      |> seq.foldr merge
    in if cell_largest.weight > cell.weight
       then {weight = cell.weight + 0.01,
             y = cell_largest.y + i32.i64 off_largest.y,
             x = cell_largest.x + i32.i64 off_largest.x,
             hue = cell_largest.hue * 0.25 + cell.hue * 0.75}
       else cell

  def render_cell (cell: cell) =
    let dist_from_center = f32.sqrt (r32 (cell.y**2 + cell.x**2))
    in {L=0.3, C=f32.min 1 (dist_from_center / 200), h=cell.hue}
       |> from_LCh
       |> oklab_to_linear_srgb
       |> \c -> argb.from_rgba c.r c.g c.b 1

  open create_random_cells {
    type cell = cell

    def random_cell rng: (rng, cell) =
      let (rng, weight) = dist.rand (0, 1) rng
      let (rng, hue) = dist.rand (0, 100) rng
      in (rng, {weight, y=0, x=0, hue})
  }
}

module lys = mk_stencil_lys steal
