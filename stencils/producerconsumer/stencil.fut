-- | Producer-consumer.

import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/random"
import "../../src/utils"

module producerconsumer = mk_stencil {
  open stencil_kinds.cross

  type dir = #up
           | #right
           | #bottom
           | #left
  def directions: seq.elems dir = seq.set #up #right #bottom #left

  type kind = #producer
            | #consumer
            | #copier dir

  type cell = {kind: kind, value: i64, rng: rng}

  def random_cell rng =
    let (rng, i) = dist_i32.rand (0, 999) rng
    let (rng, kind): (rng, kind) =
      if i < 10
      then (rng, #producer)
      else if i < 20
      then (rng, #consumer)
      else let (rng, i) = dist_i32.rand (0, 3) rng
           let dir = if i == 0 then #up
                     else if i == 1 then #right
                     else if i == 2 then #bottom
                     else #left
           in (rng, #copier dir)
    in (rng, {kind, value=0i64, rng})

  def update_cell (cell: cell) neighbors =
    match cell.kind
    case #producer ->
      let (rng, v) = dist_i64.rand (0, 99) cell.rng
      in cell with value = v
              with rng = rng
    case #consumer ->
      let value_or_zero (mx: maybe.t cell): i64 =
        match mx
        case #some cell -> cell.value
        case #none -> 0
      in cell with value = cell.value + seq.foldr (+) (seq.map value_or_zero neighbors)
    case #copier dir ->
      let value_of_non_consumer_or_zero (mx: maybe.t cell): i64 =
        match mx
        case #some c -> (match c.kind
                         case #producer -> c.value
                         case #copier _ -> c.value
                         case #consumer -> 0i64)
        case #none -> 0i64
      in cell with value =
        neighbors
        |> seq.map value_of_non_consumer_or_zero
        |> seq.zip directions
        |> seq.map (\(d, v) -> if d == dir then v else 0i64)
        |> seq.foldr (+)

  def new_cell (cell: cell) neighbors =
    let (rng, c) = dist_i32.rand (0, 999) cell.rng
    in if c == 0
       then (random_cell rng).1
       else update_cell (cell with rng = rng) neighbors

  def render_cell (cell: cell) =
    let v = f32.i64 cell.value / 100
    in match cell.kind
       case #producer ->
         argb.from_rgba v 1 1 1
       case #consumer ->
         argb.from_rgba 1 v 1 1
       case #copier _ ->
         argb.from_rgba v v v 1

  open create_random_cells {
    type cell = cell
    def random_cell = random_cell
  }
}

module lys = mk_stencil_lys producerconsumer
