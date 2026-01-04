-- | Coral reef mimicry.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"

module coralreef = mk_stencil {
  open stencil_kinds.cross

  type delta = {x: i32, y: i32}
  type cell = {delta: delta, rng: rng}

  local def dir_deltas =
    seq.map (\(y, x, _) -> {x= i32.i64 (-x), y= i32.i64 (-y)})
            neighbor_offsets

  def new_cell (cell: cell)
               (neighbors: seq.elems (maybe.t cell))
               : cell =
    let or_default (x: maybe.t cell) =
      match x
      case #some c -> c
      case #none -> {delta={x=0, y=0}, rng=cell.rng}

    let new (neighbor: cell, delta: delta) =
      neighbor with delta = {x=neighbor.delta.x + delta.x,
                             y=neighbor.delta.y + delta.y}

    let apply = seq.get (seq.zip (seq.map or_default neighbors) dir_deltas)

    let (rng, i) = dist_i32.rand (0, 3) cell.rng
    let cell' =
      if i < 2
      then (if i == 0
            then apply (\c _ _ _ -> new c)
            else apply (\_ c _ _ -> new c))
      else (if i == 2
            then apply (\_ _ c _ -> new c)
            else apply (\_ _ _ c -> new c))
    in cell' with rng = rng

  def render_cell (cell: cell): argb.colour =
    argb.from_rgba
    (r32 cell.delta.x / 100)
    (r32 cell.delta.y / 100)
    1 1

  open create_random_cells {
    type cell = cell

    def random_cell rng: (rng, cell) =
      (rng, {delta={x=0, y=0}, rng})
  }
}

module lys = mk_stencil_lys coralreef
