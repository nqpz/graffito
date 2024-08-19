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

    let replace_with_cell_top (cell_top: cell) =
      cell_top with relpos = cell_top.relpos - 1

    let if_cell_top f f_fallback =
      let fallback () = (f_fallback (), false)
      in seq.get neighbors
                 (\(top: maybe.t cell) _ _ _ ->
                    match top
                    case #some cell_top ->
                      if cell_top.exists
                      then let cell_top = update_relpos_accel' cell_top
                           in if cell_top.relpos >= 1
                              then let cell_new = f cell_top
                                   in (cell_new, cell_new.relpos >= 1)
                              else fallback ()
                      else fallback ()
                    case #none ->
                   fallback ())

    in if cell.exists
       then let cell = update_relpos_accel' cell
            in if cell.relpos >= 1
               then if_cell_top replace_with_cell_top
                                (\() -> cell with exists = false)
               else if_cell_top
                    (\cell_top ->
                       let weight_merged = cell.weight + cell_top.weight
                       let avg_weighted f = (f cell * cell.weight
                                             + f cell_top * cell_top.weight)
                                            / weight_merged
                       in cell with accel = avg_weighted (.accel)
                               with relpos = avg_weighted (.relpos)
                               with weight = weight_merged)
                    (const cell)
       else if_cell_top replace_with_cell_top
                        (const cell)

  def render_cell (cell: cell) =
    if cell.exists
    then let secondary_color = f32.max 0 (1 - cell.weight / 20)
         in argb.from_rgba 1 secondary_color secondary_color 1
    else argb.black

  open create_random_cells {
    type cell = cell
    def random_cell rng =
      let (rng, k) = dist_i32.rand (0, 9) rng
      let exists = k == 0
      let (rng, weight, accel, relpos) =
        if exists
        then let (rng, weight) = dist.rand (1, 9) rng
             let (rng, accel) = dist.rand (0, 0.999) rng
             let (rng, relpos) = dist.rand (0, 0.999) rng
             in (rng, weight, accel, relpos)
        else (rng, 0, 0, 0)
      in (rng, {exists, weight, accel, relpos})
  }
}

module lys = mk_stencil_lys gravity
