-- | Route finder
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"
import "../../src/seq"

module routefinder = mk_stencil {
  open stencil_kinds.cross

  type ground = {movement_cost: f32}

  module Direction = {
    type t = #north
           | #west
           | #east
           | #south

    def all: seq.elems t =
      seq.set #north #west #east #south

    def random (rng: rng): (t, rng) =
      seq.random all rng

    def color (direction: t): argb.colour =
      match direction
      case #north -> argb.blue
      case #west -> argb.yellow
      case #east -> argb.violet
      case #south -> argb.red
  }

  local module Building = {
    module seq = seq5

    def from_nf = flip seq5.nf.get seq5.set
    def to_nf = flip seq5.get seq5.nf.set

    type t = #kitchen
           | #bathroom
           | #recroom
           | #bedroom
           | #library

    def all: seq.elems t =
      seq.set #kitchen #bathroom #recroom #bedroom #library

    def random (rng: rng): (t, rng) =
      seq.random all rng

    def color (building: t): argb.colour =
      match building
      case #kitchen -> argb.green
      case #bathroom -> argb.brown
      case #recroom -> argb.magenta
      case #bedroom -> argb.yellow
      case #library -> argb.orange

    module get = {
      local def on_tuple = (flip seq.get (\a b c d e -> (a, b, c, d, e)) >->)

      def kitchen = on_tuple (.0)
      def bathroom = on_tuple (.1)
      def recroom = on_tuple (.2)
      def bedroom = on_tuple (.3)
      def library = on_tuple (.4)
    }
  }

  local module Building_directions = {
    type base = {cost: f32, shortest_direction: Direction.t}
    type^ t = Building.seq.elems base
    type t' = Building.seq.nf.elems base
  }

  type person = {target: Building.t, priority: f32}

  type cell = {ground: ground,
               building: maybe.t Building.t,
               building_directions: Building_directions.t',
               person: maybe.t person,
               rng: rng}

  module Building = {
    open Building

    def update (cell: cell): cell =
      let (rng, det) = dist.rand (0, 1) cell.rng
      let (rng, building) =
        if det < 0.0001
        then match cell.building
             case #some _ -> (rng, #none)
             case #none -> let (b, rng) = random rng
                           in (rng, #some b)
        else (rng, cell.building)
      in cell with rng = rng
              with building = building
  }

  module Building_directions = {
    open Building_directions

    local def direction_cost (accessor: Building_directions.t -> Building_directions.base)
                             (target: Building.t)
                             (neighbor: cell)
                             : f32 =
      neighbor.building
      |> maybe.bind (\building -> if building == target then #some 0 else #none)
      |> maybe.or (\() -> (accessor (Building.from_nf neighbor.building_directions)).cost)

    def update (neighbors: seq.elems (maybe.t cell)) (cell: cell): cell =
      let update_building (accessor, (building, building_direction)) =
        let (dir, subcost) =
          neighbors
          |> seq.map (maybe.map (direction_cost accessor building))
          |> seq.map (maybe.or (\() -> f32.inf))
          |> seq.zip Direction.all
          |> seq.foldr (\(d0, c0) (d1, c1) -> if c0 < c1 then (d0, c0) else (d1, c1))
        let dir' = if subcost == building_direction.cost
                   then building_direction.shortest_direction
                   else dir
        in {cost=subcost + cell.ground.movement_cost, shortest_direction=dir'}
      in cell with building_directions = Building.from_nf cell.building_directions
                                         |> Building.seq.zip Building.all
                                         |> Building.seq.zip Building.seq.accessors
                                         |> Building.seq.map update_building
                                         |> Building.to_nf
  }

  def new_cell cell neighbors =
    cell
    |> id -- Building.update
    |> Building_directions.update neighbors

  def render_cell (cell: cell) =
    cell.building_directions
    |> Building.from_nf
    |> Building.get.kitchen -- Just render the kitchens for now
    |> (.shortest_direction)
    |> Direction.color

  open create_random_cells {
    type cell = cell

    def random_cell rng =
      let (rng, movement_cost) = dist.rand (0, 1) rng
      let ground = {movement_cost}

      let (rng, building_det) = dist.rand (0, 1) rng
      let has_building = building_det < 0.0001
      let (rng, building: maybe.t Building.t) =
        if has_building
        then let (building, rng) = Building.random rng
             in (rng, #some building)
        else (rng, #none)

      let (rng, has_person) =
        if has_building
        then (rng, false)
        else let (rng, person_det) = dist.rand (0, 1) rng
             in (rng, person_det < 0.0001)
      let (rng, person: maybe.t person) =
        if has_person
        then let (target, rng) = Building.random rng
             let (rng, priority) = dist.rand (0, 1) rng
             in (rng, #some {target, priority})
        else (rng, #none)

      let (dirs, dir_rngs) =
        rnge.split_rng (i64.i32 Building.seq.length) rng
        |> map Direction.random
        |> unzip
      let dirs' = Building.seq.from_list dirs
      let rng = rnge.join_rng dir_rngs

      let building_directions =
        Building.seq.map (\dir -> {cost=f32.inf, shortest_direction=dir}) dirs'
        |> Building.to_nf

      let cell = {ground, building, person, building_directions, rng}
      in (rng, cell)
  }
}

module lys = mk_stencil_lys routefinder
