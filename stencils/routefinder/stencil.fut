-- | Route finder
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"
import "../../src/seq"

module type seq_t_input = {
  module seq: seq

  type t

  val compare: t -> t -> bool

  val all: seq.elems t

  val colors: seq.elems argb.colour
}

module mk_seq_t (input: seq_t_input) = {
  open input

  def random (rng: rng): (t, rng) =
    seq.random all rng

  def color (x: t): argb.colour =
    seq.assoc_find (compare x) all colors
}

module routefinder = mk_stencil {
  open stencil_kinds.cross

  type ground = {movement_cost: f32}

  module Direction = mk_seq_t {
    module seq = seq

    type t = #north
           | #west
           | #east
           | #south

    def compare: t -> t -> bool = (==)

    def all: seq.elems t =
      seq.set #north #west #east #south

    def colors: seq.elems argb.colour =
      seq.set argb.blue argb.yellow argb.violet argb.red
  }

  local module Building = mk_seq_t {
    module seq = seq5

    type t = #kitchen
           | #bathroom
           | #recroom
           | #bedroom
           | #library

    def compare: t -> t -> bool = (==)

    def all: seq.elems t =
      seq.set #kitchen #bathroom #recroom #bedroom #library

    def colors: seq.elems argb.colour =
      seq.set argb.green argb.brown argb.magenta argb.yellow argb.orange
  }

  local module Building_directions = {
    type base = {minimum_cost: f32, signal_age: i32, shortest_direction: Direction.t, void_signal_age: i32}
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

    def from_nf = flip seq.nf.get seq.set
    def to_nf = flip seq.get seq.nf.set

    module get = {
      local def on_tuple = (flip seq.get (\a b c d e -> (a, b, c, d, e)) >->)

      def kitchen = on_tuple (.0)
      def bathroom = on_tuple (.1)
      def recroom = on_tuple (.2)
      def bedroom = on_tuple (.3)
      def library = on_tuple (.4)
    }

    def update (cell: cell): cell =
      let (rng, det) = dist.rand (0, 1) cell.rng
      let (rng, building, building_removed): (rng, maybe.t Building.t, maybe.t Building.t) =
        match cell.building
        case #some b ->
          if det < 0.1
          then (rng, #none, #some b)
          else (rng, #some b, #none)
        case #none ->
          if det < 0.00000001
          then let (b, rng) = random rng
               in (rng, #some b, #none)
          else (rng, #none, #none)
      let building_directions =
        match building_removed
        case #none -> cell.building_directions
        case #some removed ->
          cell.building_directions
          |> from_nf
          |> seq.zip all
          |> seq.map (\((b, d): (t, Building_directions.base)) ->
                        if b == removed then d else d)
          |> to_nf
      in cell with rng = rng
              with building = building
              with building_directions = building_directions
  }

  module Building_directions = {
    open Building_directions

    local def direction_cost (accessor: Building_directions.t -> Building_directions.base)
                             (target: Building.t)
                             (neighbor: cell)
                             : (f32, i32, i32) =
      neighbor.building
      |> maybe.bind (\building -> if building == target then #some (0, 0, 0) else #none)
      |> maybe.or (\() ->
                     let base = accessor (Building.from_nf neighbor.building_directions)
                     in (base.minimum_cost, base.signal_age, base.void_signal_age))

    local def void_direction_cost (accessor: Building_directions.t -> Building_directions.base)
                                  (neighbor: cell)
                                  : i32 =
      let base = accessor (Building.from_nf neighbor.building_directions)
      in base.void_signal_age

    def update (neighbors: seq.elems (maybe.t cell)) (cell: cell): cell =
      let update_building (accessor, (building, building_direction)) =
        let (dir, (subcost, subage, _)) =
          neighbors
          |> seq.map (maybe.map (direction_cost accessor building))
          |> seq.map (maybe.or (\() -> (f32.inf, 0, 0)))
          |> seq.zip Direction.all
          |> seq.foldr (\(d0, (c0, a0, va0)) (d1, (c1, a1, va1)) ->
                          if c0 < c1 && a0 <= va0
                          then (d0, (c0, a0, va0))
                          else (d1, (c1, a1, va1)))
        let dir' = if subcost == building_direction.minimum_cost
                   then building_direction.shortest_direction
                   else dir

        let void_subage =
          neighbors
          |> seq.map (maybe.map (void_direction_cost accessor))
          |> seq.map (maybe.or (\() -> 0i32))
          |> seq.foldr i32.max

        in {minimum_cost=subcost + cell.ground.movement_cost,
            signal_age=subage + 1,
            shortest_direction=dir',
            void_signal_age=void_subage + 1}
      in cell with building_directions = Building.from_nf cell.building_directions
                                         |> Building.seq.zip Building.all
                                         |> Building.seq.zip Building.seq.accessors
                                         |> Building.seq.map update_building
                                         |> Building.to_nf
  }

  def new_cell cell neighbors =
    cell
    |> Building.update
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
        Building.seq.map (\dir -> {minimum_cost=f32.inf, signal_age=0i32,
                                   shortest_direction=dir, void_signal_age=0i32}) dirs'
        |> Building.to_nf

      let cell = {ground, building, person, building_directions, rng}
      in (rng, cell)
  }
}

module lys = mk_stencil_lys routefinder
