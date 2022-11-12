import "../lib/github.com/athas/matte/colour"
import "base"
import "setget"

module type cell = {
  type cell
}

module type create = {
  type create_input
  type create_output
}

module type setget_module = {
  module setget: setget
}

module type stencil = {
  include cell
  include create

  val create_cells: i64 -> i64 -> create_input -> ([][]cell, create_output)

  val step [h][w]: *[h][w]cell -> *[h][w]cell

  val render [h][w]: [h][w]cell -> [h][w]argb.colour
}

type^ collect_neighbors [h][w] 'cell 'elems = [h][w]cell -> index -> index -> elems

module type stencil_core = {
  include cell
  include create
  include setget_module

  val create_cells: i64 -> i64 -> create_input -> ([][]cell, create_output)

  val collect_neighbors [h][w]: collect_neighbors [h][w] cell (setget.elems (maybe cell))

  val new_cell: cell -> setget.elems (maybe cell) -> cell

  val render_cell: cell -> argb.colour
}

module type stencil_kind = {
  include setget_module

  val collect_neighbors [h][w] 'cell: collect_neighbors [h][w] cell (setget.elems (maybe cell))
}

module type specialized_stencil_kind = {
  include cell
  include setget_module
  val collect_neighbors [h][w]: collect_neighbors [h][w] cell (setget.elems (maybe cell))
}

module specialize_stencil_kind (stencil_kind: stencil_kind) (cell: cell):
  specialized_stencil_kind with cell = cell.cell
                           with setget.f 'base 'a = stencil_kind.setget.f base a =
{
  open cell
  open stencil_kind
}

module mk_stencil (stencil_core: stencil_core):
  stencil with cell = stencil_core.cell
          with create_input = stencil_core.create_input
          with create_output = stencil_core.create_output =
{
  open stencil_core

  def step_cell [h][w] (cells: [h][w]cell) (y: i64) (x: i64) =
    new_cell cells[y, x] (collect_neighbors cells y x)

  def step [h][w] (cells: *[h][w]cell): *[h][w]cell =
    tabulate_2d h w (\y x -> step_cell cells y x)

  def render [h][w] (cells: [h][w]cell): [h][w]argb.colour =
    map (map stencil_core.render_cell) cells
}
