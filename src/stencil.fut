import "../lib/github.com/athas/matte/colour"
import "base"
import "setget"

module type cell = {
  type cell
}

module type create = {
  include cell
  type create_input
  type create_output

  val create_cells: i64 -> i64 -> create_input -> ([][]cell, create_output)
}

type^ neighbor_offset = (index, index, {y: index, x: index, h: i64, w: i64} -> bool)

module type stencil_kind = {
  module setget: setget

  val neighbor_offsets: setget.elems neighbor_offset
}

module type specialized_stencil_kind = {
  include cell
  include stencil_kind
}

module specialize_stencil_kind (stencil_kind: stencil_kind) (cell: cell):
  specialized_stencil_kind with cell = cell.cell
                           with setget.f '^base '^a = stencil_kind.setget.f base a =
{
  open cell
  open stencil_kind
}

module type stencil_input = {
  include stencil_kind
  include create

  val new_cell: cell -> setget.elems (maybe cell) -> cell

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

  def step_cell [h][w] (cells: [h][w]cell) (y: i64) (x: i64) =
    let calc_cell = new_cell cells[y, x]
    let indices = setget.map (\(dy, dx, cond) -> (y + dy, x + dx, cond))
                             neighbor_offsets
    in if y > 0 && y < h - 1 && x > 0 && x < w - 1
       then #[unsafe] calc_cell (setget.map (\(y', x', _) -> #some cells[y', x']) indices)
       else calc_cell (setget.map (\(y', x', cond) ->
                                     if (cond {y, x, h, w})
                                     then #some cells[y', x']
                                     else #none)
                                  indices)

  def step [h][w] (cells: *[h][w]cell): *[h][w]cell =
    tabulate_2d h w (step_cell cells)

  def render [h][w]: [h][w]cell -> [h][w]argb.colour =
    map (map stencil_input.render_cell)
}
