import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/random"
import "../../src/utils"
import "../../src/oklab"

module squarespiral = mk_stencil {
  open stencil_kinds.cross

  def deltas = seq.map (\(y, x, _) -> (x, y)) neighbor_offsets
  def reverse_deltas = seq.map (\(x, y) -> (-x, -y)) deltas

  type delta = (i64, i64)
  type cell = {delta: delta, length: i64, remaining: i64, power: f32, step_size: i64, hue: f32}

  def rotate ((x, y): delta): delta =
    if y == 0
    then (0, x)
    else (-y, 0)

  def new_cell (cell: cell) (neighbors: seq.elems (maybe.t cell)) =
    let check_direction and_cond (neighbor: (maybe.t cell, delta)) =
      match neighbor
      case (#some c, d) ->
        if d == c.delta && and_cond c
        then #some c
        else #none
      case (#none, _) -> #none

    let find_first_direction ds and_cond =
      seq.find_first id (seq.map (check_direction and_cond) (seq.zip neighbors ds))

    -- Check if this cell can be populated from an ongoing spiral.
    let cell =
      match find_first_direction reverse_deltas (\c -> cell.hue != c.hue)
      case #some c ->
        if c.remaining > 0
        then c with remaining = c.remaining - 1
        else let length = c.length + c.step_size
             in c with delta = rotate c.delta
                  with length = length + 1
                  with remaining = length
                  with power = 1
      case #none ->
        let power = cell.power - 0.001
        in if power <= 0
           then cell with delta = (0, 0)
                     with length = 0
           else cell with power = power

    -- Check if this cell can be marked as not influencing future targets.
    let cell =
      if cell.delta == (0, 0)
      then cell
      else match find_first_direction deltas (\c -> cell.hue == c.hue)
           case #some _ ->
             cell with delta = (0, 0)
           case #none ->
             cell

    in cell

  def render_cell (cell: cell) =
    let c = oklab_to_linear_srgb (from_LCh {L=cell.power, C=cell.power, h=cell.hue})
    in argb.from_rgba c.r c.g c.b 1

  open create_random_cells {
    type cell = cell

    def random_cell rng =
      let (rng, prob) = dist.rand (0, 1) rng
      let (rng, cell) =
        if prob > 0.001
        then (rng, {delta=(0i64, 0i64), length=0i64, remaining=0i64, power=0f32, step_size=0, hue=0})
        else let (rng, hue) = dist.rand (0, 360) rng
             let (rng, step_size) = dist_i64.rand (0, 2) rng
             in (rng, {delta=(0i64, -1i64), length=1i64, remaining=0i64, power=1f32, step_size, hue})
      in (rng, cell)
  }
}

module lys = mk_stencil_lys squarespiral
