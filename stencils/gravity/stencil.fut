-- | Basic gravity.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"

module gravity = mk_stencil_multipass {
  open stencil_kinds.cross

  type cell = {exists: bool, weight: f32, accel: f32, relpos: f32}

  local def step_accel: f32 -> f32 = (+ 9.8 / 10000)

  local def update_relpos_accel (cell: cell) =
    let relpos' = cell.relpos + cell.accel
    in (cell with accel = step_accel cell.accel
             with relpos = relpos')

  def new_cell is_first_pass (cell: cell) neighbors =
    let update_relpos_accel' c = if is_first_pass
                                 then update_relpos_accel c
                                 else c
    let maybe_replace_with_cell_top (cell_top: cell) (get_cell_fallback: () -> cell) =
      if cell_top.exists && cell_top.relpos >= 1
      then let cell_new = cell_top with relpos = cell_top.relpos - 1
           in (cell_new, cell_new.relpos >= 1)
      else (get_cell_fallback (), false)

    let cell_top_maybe: maybe.t cell =
      seq.get neighbors
              (\(top: maybe.t cell) _ _ _ ->
                 match top
                 case #some cell_top ->
                   #some (update_relpos_accel' cell_top)
                 case #none ->
                   #none)
    in if cell.exists
       then let cell = update_relpos_accel' cell
            in if cell.relpos >= 1
               then match cell_top_maybe
                    case #some cell_top ->
                      maybe_replace_with_cell_top cell_top (const (cell with exists = false))
                    case #none ->
                      (cell with exists = false, false)
               else match cell_top_maybe
                    case #some cell_top ->
                      if cell_top.exists && cell_top.relpos >= 1
                      then let weight_merged = cell.weight + cell_top.weight
                           let cell_merged = cell with accel = (cell.accel * cell.weight
                                                                + cell_top.accel * cell_top.weight)
                                                               / weight_merged
                                                  with relpos = (cell.relpos * cell.weight
                                                                 + cell_top.relpos * cell_top.weight)
                                                                / weight_merged
                                                  with weight = weight_merged
                           in (cell_merged, cell_merged.relpos >= 1)
                      else (cell, false)
                    case #none ->
                      (cell, false)
    else match cell_top_maybe
         case #some cell_top ->
           maybe_replace_with_cell_top cell_top (const cell)
         case #none ->
           (cell, false)

  def render_cell (cell: cell) =
    if cell.exists
    then let secondary_color = f32.max 0 (1 - cell.weight / 10)
         in argb.from_rgba 1 secondary_color secondary_color 1
    else argb.black

  open create_random_cells {
    type cell = cell
    def random_cell rng =
      let (rng, k) = dist_i32.rand (0, 9) rng
      let exists = k == 0
      let (rng, accel, relpos) =
        if exists
        then let (rng, accel) = dist.rand (0, 0.999) rng
             let (rng, relpos) = dist.rand (0, 0.999) rng
             in (rng, accel, relpos)
        else (rng, 0, 0)
      in (rng, {exists, weight=1f32, accel, relpos})
  }
}

module lys = mk_stencil_lys gravity
