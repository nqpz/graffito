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

  module point = {
    type t = {y: i32, x: i32}

    def zero: t = {y=0, x=0}

    def length ({y, x}: t): f32 =
      f32.sqrt (r32 (y**2 + x**2))

    def map2 (f: i32 -> i32 -> i32) (p0: t) (p1: t): t =
      {y=f p0.y p1.y, x=f p0.x p1.x}
  }
  type point = point.t

  type bounds = {upper_left: point,
                 lower_right: point}

  def bounds_size ({upper_left, lower_right}: bounds): f32 =
    point.map2 (-) lower_right upper_left
    |> point.length
    |> (+ 1)

  def merge_bounds (bounds0: bounds) (bounds1: bounds): bounds =
    {upper_left=point.map2 i32.min bounds0.upper_left bounds1.upper_left,
     lower_right=point.map2 i32.max bounds0.lower_right bounds1.lower_right}

  type cell = {weight: f32,
               point: point,
               center_hue: f32,
               hue: f32,
               bounds: bounds,
               bounds_size: f32}

  def recalculate_bounds neighbors (cell: cell): cell =
    let extract (mx: maybe.t cell) =
      match mx
      case #some c -> if c.center_hue == cell.center_hue
                      then c.bounds
                      else {upper_left=cell.point, lower_right=cell.point}
      case #none -> {upper_left=cell.point, lower_right=cell.point}

    let bounds = seq.map extract neighbors
                 |> seq.foldr merge_bounds
    in cell with bounds = bounds

  def new_cell (cell: cell) neighbors: cell =
    let extract (mx: maybe.t cell): cell =
      match mx
      case #some c -> c
      case #none -> {weight= -1, point=point.zero, center_hue=0, hue=0, bounds={upper_left=point.zero, lower_right=point.zero}, bounds_size=1}

    let extract_offset (y, x, _) = {y=i32.i64 y, x=i32.i64 x}

    let merge (cell1, off1) (cell2, off2) =
      if cell1.weight > cell2.weight
      then (cell1, off1)
      else (cell2, off2)

    let (cell_largest, off_largest) =
      seq.zip (seq.map extract neighbors)
              (seq.map extract_offset neighbor_offsets)
      |> seq.foldr merge
    in let cell = if cell_largest.weight > cell.weight
                  then let p = point.map2 (+) cell_largest.point off_largest
                       in (cell_largest with weight = cell.weight + 0.01 / (1 + f32.log (point.length cell.point) / 50)
                                        with point = p
                                        with hue = 0.01 * cell_largest.center_hue
                                                   + 0.99 * (0.25 * cell_largest.hue
                                                             + 0.75 * cell.hue))
                          |> recalculate_bounds neighbors
                          |> \(cell: cell) -> cell with bounds = merge_bounds cell.bounds {upper_left=p, lower_right=p}
                  else recalculate_bounds neighbors cell
       in cell with bounds_size = bounds_size cell.bounds

  def render_cell (cell: cell) =
    {L=0.5, C=f32.min 1 (point.length cell.point / cell.bounds_size), h=cell.hue}
    |> from_LCh
    |> oklab_to_linear_srgb
    |> \c -> argb.from_rgba c.r c.g c.b 1

  open create_random_cells {
    type cell = cell

    def random_cell rng: (rng, cell) =
      let (rng, weight) = dist.rand (0, 1) rng
      let (rng, hue) = dist.rand (0, 100) rng
      in (rng, {weight, point=point.zero, center_hue=hue, hue,
                bounds={upper_left=point.zero, lower_right=point.zero},
                bounds_size=1})
  }
}

module lys = mk_stencil_lys steal
