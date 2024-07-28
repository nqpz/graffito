-- | Based on the Collatz conjecture operations.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"

module collatz = mk_stencil {
  open stencil_kinds.square

  type cell = {value: i64,
               global_min: i64,
               global_max: i64}

  def next (n: i64): i64 =
    if n % 2 == 0
    then n / 2
    else 3 * n + 1

  def f (n: i64): i64 =
    let (_, count) =
      loop (n, count) = (n, 0)
      while n > 1
      do (next n, count + 1)
    in count

  def new_cell (cell: cell) neighbors =
    let get_number (mn: maybe.t cell): i64 =
      match mn
      case #some neighbor -> neighbor.value
      case #none -> 0
    let sum_neighbors = seq.foldr (i64.+) (seq.map get_number neighbors)
    let value =
      i64.f32 (0.98 * f32.i64 cell.value + 0.02 * f32.i64 (f sum_neighbors))

    let get_min_number (mn: maybe.t cell): i64 =
      match mn
      case #some neighbor -> neighbor.value
      case #none -> i64.highest
    let global_min =
      i64.min value (seq.foldr i64.min (seq.map get_min_number neighbors))

    let get_max_number (mn: maybe.t cell): i64 =
      match mn
      case #some neighbor -> neighbor.value
      case #none -> i64.lowest
    let global_max =
      i64.max value (seq.foldr i64.max (seq.map get_max_number neighbors))

    in {value, global_min, global_max}

  def render_cell ({value=_, global_min, global_max}: cell) =
    argb.from_rgba
    0.1
    0.1 -- (f32.i64 (value - global_min) / f32.i64 (global_max - global_min))
    (f32.i64 global_min / f32.i64 global_max)
    1

  open create_random_cells {
    type cell = cell
    def random_cell rng =
      let (rng, value) = dist_i64.rand (0, 10000000) rng
      in (rng, {value, global_min=value, global_max=value})
  }
}

module lys = mk_stencil_lys collatz
