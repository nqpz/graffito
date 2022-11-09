import "../lib/github.com/athas/matte/colour"
import "base"
import "setget"

module type stencil = {
  type cell
  type create_input
  type create_output

  val create_cells: i64 -> i64 -> create_input -> ([][]cell, create_output)

  val step [h][w]: *[h][w]cell -> *[h][w]cell

  val render [h][w]: [h][w]cell -> [h][w]argb.colour
}

module type stencil_core = {
  type cell
  type elems
  type create_input
  type create_output

  val create_cells: i64 -> i64 -> create_input -> ([][]cell, create_output)

  val collect_neighbors [h][w]: [h][w]cell -> index -> index -> elems

  val new_cell: cell -> elems -> cell

  val render_cell: cell -> argb.colour
}

module type stencil_kind = {
  include setget

  val collect_neighbors [h][w] 'cell: [h][w]cell -> index -> index -> elems (maybe cell)
}

module specialize_stencil_kind (stencil_kind: stencil_kind) (cell: { type cell }) = {
  open stencil_kind
  open cell
  type elems = elems (maybe cell)
}

module mk_stencil (stencil_core: stencil_core):
  stencil with cell = stencil_core.cell
          with create_input = stencil_core.create_input
          with create_output = stencil_core.create_output = {
  open stencil_core

  def step_cell [h][w] (cells: [h][w]cell) (y: i64) (x: i64) =
    new_cell cells[y, x] (collect_neighbors cells y x)

  def step [h][w] (cells: *[h][w]cell): *[h][w]cell =
    tabulate_2d h w (\y x -> step_cell cells y x)

  def render [h][w] (cells: [h][w]cell): [h][w]argb.colour =
    map (map stencil_core.render_cell) cells
}
