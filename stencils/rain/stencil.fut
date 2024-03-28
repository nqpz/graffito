import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/random"
import "../../src/utils"

module rain = mk_stencil {
  open stencil_kinds.cross

  type cell = {value: f32, min: f32, max: f32,
               lower_bound: f32, upper_bound: f32,
               lower_bound_diff: f32, upper_bound_diff: f32,
               steps_without_regen: f32,
               rng: rng}

  def new_cell (cell: cell) neighbors =
    let or_default (x: maybe.t cell) =
      match x
      case #some c -> c.value
      case #none -> 1
    let neighbor_values = seq.map or_default neighbors
    let value = cell.value * (seq.foldr (%) neighbor_values) * (seq.foldr (+) neighbor_values)
    let value = f32.min cell.upper_bound (f32.max cell.lower_bound value)
    let min = f32.min cell.value (seq.foldr f32.min neighbor_values)
    let max = f32.max cell.value (seq.foldr f32.max neighbor_values)
    let lower_bound = cell.lower_bound + cell.lower_bound_diff
    let upper_bound = cell.upper_bound + cell.upper_bound_diff
    let (rng, lower_bound_diff_diff) = dist.rand (-0.1, 0.1) cell.rng
    let (rng, upper_bound_diff_diff) = dist.rand (-0.1, 0.1) rng
    let lower_bound_diff = cell.lower_bound_diff + lower_bound_diff_diff
    let upper_bound_diff = cell.upper_bound_diff + upper_bound_diff_diff
    let (rng, c) = dist.rand (0, 100000) rng
    let (rng, value, lb, ub, lbd, ubd, steps_without_regen) =
      if c < cell.steps_without_regen
      then let (rng, value) = dist.rand (-1, 1) rng
           in (rng, value, -1000, 1000, 0, 0, 0)
      else (rng, value, lower_bound, upper_bound, lower_bound_diff, upper_bound_diff, cell.steps_without_regen + 1)
    in {value, min, max,
        lower_bound=lb, upper_bound=ub,
        lower_bound_diff=lbd, upper_bound_diff=ubd,
        steps_without_regen,
        rng}

  def render_cell (cell: cell) =
    argb.from_rgba 0 0 ((cell.max - cell.lower_bound) / (cell.upper_bound - cell.lower_bound)) 1

  open create_random_cells {
    type cell = cell
    def random_cell rng =
      let (rng, value) = dist.rand (-1, 1) rng
      in (rng, {value, min=0f32, max=0f32,
                lower_bound= -1000f32, upper_bound=1000f32,
                lower_bound_diff=0f32, upper_bound_diff=0f32,
                steps_without_regen=0f32,
                rng})
  }
}

module lys = mk_stencil_lys rain
