import "../lib/github.com/athas/matte/colour"
import "base"
import "seq"

module type cell = {
  type cell
}

module type create = {
  include cell
  type create_input
  type create_output

  val create_cells: i64 -> i64 -> create_input -> ([][]cell, create_output)
}

type coordinate_state = {y: index, x: index, h: i64, w: i64}
type^ neighbor_offset = (index, index, coordinate_state -> bool)

module type stencil_input = {
  include create

  module seq: seq

  val neighbor_offsets: seq.elems neighbor_offset

  val is_not_in_corner: coordinate_state -> bool

  val new_cell: cell -> seq.elems (maybe.t cell) -> cell

  val render_cell: cell -> argb.colour
}

module type stencil = {
  include create

  val step [h][w]: *[h][w]cell -> *[h][w]cell

  val render [h][w]: [h][w]cell -> [h][w]argb.colour
}

module mk_stencil (stencil_input: stencil_input):
  stencil with cell = stencil_input.cell
          with create_input = stencil_input.create_input
          with create_output = stencil_input.create_output =
{
  open stencil_input

  def step_cell [h][w] (cells: [h][w]cell) (cell: cell) ((y, x): (index, index)) =
    let calc_cell = new_cell cell
    let indices = seq.map (\(dy, dx, cond) -> (y + dy, x + dx, cond)) neighbor_offsets
    let cs = {y, x, h, w}
    in if is_not_in_corner cs
       then calc_cell (seq.map (\(y', x', _) -> #some (#[unsafe] cells[y', x'])) indices)
       else calc_cell (seq.map (\(y', x', cond) ->
                                     if (cond cs)
                                     then #some (#[unsafe] cells[y', x'])
                                     else #none)
                                  indices)

  def step [h][w] (cells: *[h][w]cell): *[h][w]cell =
    map2 (map2 (step_cell cells)) cells (tabulate_2d h w (\y x -> (y, x)))

  def render [h][w]: [h][w]cell -> [h][w]argb.colour =
    map (map render_cell)
}
