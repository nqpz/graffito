-- | Lines
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"

module steal = mk_stencil {
  open stencil_kinds.cross

  type cell = {y_size: i32,
               x_size: i32,
               gen0: bool,
               rng: rng}

  def new_cell (cell: cell)
               (neighbors: seq.elems (maybe.t cell))
               : cell =
    let extract_sizes (mx: maybe.t cell): (i32, i32, bool) =
      match mx
      case #some c -> (c.y_size, c.x_size, c.gen0)
      case #none -> (0, 0, true)

    let extract_offset (y, x, _) = (i32.i64 y, i32.i64 x)

    let extract_size ((y_size, x_size, gen0), (y_offset, x_offset)) =
      (y_size * i32.abs y_offset, x_size * i32.abs x_offset, gen0)

    let merge_size (y_size0, x_size0, gen00) (y_size1, x_size1, gen01) =
      let g0 = i32.bool (gen00 == cell.gen0)
      let g1 = i32.bool (gen01 == cell.gen0)
      in (g0 * y_size0 + g1 * y_size1, g0 * x_size0 + g1 * x_size1, cell.gen0)

    let (y_size, x_size, _) =
      seq.zip (seq.map extract_sizes neighbors)
              (seq.map extract_offset neighbor_offsets)
      |> seq.map extract_size
      |> seq.foldr merge_size
    let size = y_size + x_size

    in if size < 10000
       then let (rng, v) = dist.rand (0, 1) cell.rng
            let cond = v >= 1 / (1 + r32 size)
            let (y_size, x_size) = if cond
                                   then (y_size + 1, x_size + 1)
                                   else (cell.y_size, cell.x_size)
            in {y_size, x_size, gen0=cell.gen0, rng}
       else {y_size=0, x_size=0, gen0=not cell.gen0, rng=cell.rng}

  def render_cell (cell: cell) =
    let v = f32.bool (0 == cell.y_size + cell.x_size)
    in argb.from_rgba v v v 1

  open create_random_cells {
    type cell = cell

    def random_cell rng: (rng, cell) =
      (rng, {y_size=0, x_size=0, gen0=true, rng})
  }
}

module lys = mk_stencil_lys steal
