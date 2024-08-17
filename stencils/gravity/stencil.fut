-- | Basic gravity.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"

module gravity = mk_stencil {
  open stencil_kinds.cross

  type cell = {exists: bool, reached_bottom: bool, accel: f32, relpos: f32}

  local def step_accel: f32 -> f32 = (+ 9.8 / 10000)

  def new_cell (cell: cell) neighbors =
    if cell.exists
    then if cell.relpos < 1
         then cell with accel = step_accel cell.accel
                   with relpos = cell.relpos + cell.accel
         else seq.get neighbors
                      (\_ _ _ (bottom: maybe.t cell) ->
                         match bottom
                         case #some cell_bottom ->
                           if cell_bottom.exists
                           then if cell_bottom.reached_bottom
                                then cell with reached_bottom = true
                                else cell
                           else cell with exists = false
                         case #none -> cell with reached_bottom = true)
    else seq.get neighbors
                 (\(top: maybe.t cell) _ _ _ ->
                    match top
                    case #some cell_top ->
                      if cell_top.relpos > 1
                      then let relpos' = cell_top.relpos - 1
                           in cell_top with relpos = relpos'
                      else cell
                    case #none -> cell)

  def render_cell (cell: cell) =
    if cell.exists
    then argb.white
    else argb.black

  open create_random_cells {
    type cell = cell
    def random_cell rng =
      let (rng, k) = dist_i32.rand (0, 99) rng
      let exists = k == 0
      let (rng, relpos) = if exists
                          then dist.rand (0, 0.999) rng
                          else (rng, 0)
      in (rng, {exists, reached_bottom=false, accel=0f32, relpos})
  }
}

module lys = mk_stencil_lys gravity
