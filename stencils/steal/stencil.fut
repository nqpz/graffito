-- | Steal
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"

module steal = mk_stencil {
  open stencil_kinds.square

  type cell = {weight: f32,
               y: i32,
               x: i32}

  def new_cell (cell: cell) neighbors: cell =
    let extract (mx: maybe.t cell): cell =
      match mx
      case #some c -> c
      case #none -> {weight= -1, y=0, x=0}

    let extract_offset (y, x, _) = {y, x}

    let merge (cell1, off1) (cell2, off2) =
      if cell1.weight > cell2.weight
      then (cell1, off1)
      else (cell2, off2)

    let (cell_largest, off_largest) =
      seq.zip (seq.map extract neighbors) (seq.map extract_offset neighbor_offsets)
      |> seq.foldr merge
    in if cell_largest.weight > cell.weight
       then {weight=cell.weight + 0.01,
             y=cell_largest.y + i32.i64 off_largest.y,
             x=cell_largest.x + i32.i64 off_largest.x}
       else cell with y = cell.y - i32.sgn cell.y
                 with x = cell.x - i32.sgn cell.x

  def render_cell (cell: cell) =
    argb.from_rgba (r32 cell.x / 100) (r32 cell.y / 100) 0 1

  open create_random_cells {
    type cell = cell

    def random_cell rng: (rng, cell) =
      let (rng, weight) = dist.rand (0, 1) rng
      in (rng, {weight, y=0, x=0})
  }
}

module lys = mk_stencil_lys steal
