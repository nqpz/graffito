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
           | #no_direction -- kind of have this one around, but also ignore it a bit

    type t_with_cost = {direction: t, cost: f32}

    def all: seq.elems t =
      seq.set #north #west #east #south
  }

  local module Building = {
    module seq = seq5.nf

    type t = #kitchen
           | #bathroom
           | #recroom
           | #bedroom
           | #library

    def all: seq.elems t =
      seq.set #kitchen #bathroom #recroom #bedroom #library

    def color (building: t): argb.colour =
      match building
      case #kitchen -> argb.green
      case #bathroom -> argb.brown
      case #recroom -> argb.magenta
      case #bedroom -> argb.yellow
      case #library -> argb.orange

    def random (rng: rng): (t, rng) =
      seq.random all rng
  }

  local module Building_directions = {
    type t = Building.seq.elems Direction.t_with_cost
  }

  type person = {target: Building.t, priority: f32}

  type cell = {ground: ground,
               building: maybe Building.t,
               building_directions: Building_directions.t,
               person: maybe person,
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

    def get_direction_with_cost (target: Building.t) (building_directions: t)
                                : Direction.t_with_cost =
      (Building.seq.find_first'
       (\(_dir, dir_target) -> dir_target == target)
       (Building.seq.zip building_directions Building.all)).0

    def update_direction_with_cost (cell: cell)
                                   (neighbors: seq.elems (maybe cell))
                                   (target: Building.t): Direction.t_with_cost =
      if cell.building == #some target
      then {direction=#no_direction, cost=0}
      else let get_building : maybe cell -> maybe Building.t =
             maybe_join <-< maybe_map (.building)
           let make_direction_with_cell_cost dir =
             {direction=dir, cost=cell.ground.movement_cost}
           let buildings = seq.map get_building neighbors
           let dir_direct = seq.find_first
                            (\(b, dir): maybe Direction.t_with_cost ->
                               if b == #some target
                               then #some (make_direction_with_cell_cost dir)
                               else #none)
                            (seq.zip buildings Direction.all)
           in match dir_direct
              case #some d -> d
              case #none ->
                let get_cost: maybe cell -> maybe f32 =
                  maybe_join <-< (maybe_map ((\(dwc: Direction.t_with_cost) ->
                                                if dwc.direction == #no_direction
                                                then #none
                                                else #some dwc.cost)
                                             <-< get_direction_with_cost target <-< (.building_directions)))
                let costs = seq.map get_cost neighbors
                let (mcost, dir) = seq.foldr (\(mcost0, dir0) (mcost1, dir1) ->
                                                match (mcost0, mcost1)
                                                case (#some cost0, #some cost1) ->
                                                  if cost0 < cost1
                                                  then (mcost0, dir0)
                                                  else (mcost1, dir1)
                                                case (#some _, #none) -> (mcost0, dir0)
                                                case _ -> (mcost1, dir1))
                                             (seq.zip costs Direction.all)
                in match mcost
                   case #some cost -> {direction=dir, cost=cell.ground.movement_cost + cost}
                   case #none -> {direction=#no_direction, cost=f32.inf}

    def update (neighbors: seq.elems (maybe cell)) (cell: cell): cell =
      cell with building_directions = Building.seq.map (update_direction_with_cost cell neighbors) Building.all
  }

  def new_cell cell neighbors =
    cell
    |> id -- Building.update
    |> Building_directions.update neighbors

  def render_cell (cell: cell) =
    match (Building_directions.get_direction_with_cost #kitchen cell.building_directions).direction
    case #north -> argb.blue
    case #west -> argb.yellow
    case #east -> argb.violet
    case #south -> argb.red
    case #no_direction -> argb.black

  -- def render_cell (cell: cell) =
  --   let background = argb.gray cell.ground.movement_cost
  --   in match maybe_map Building.color cell.building
  --      case #some c -> c -- argb.mix 0.5 background 0.5 c
  --      case #none -> argb.scale background 0.1

  open create_random_cells {
    type cell = cell

    def random_cell rng =
      let (rng, movement_cost) = dist.rand (0, 1) rng
      let ground = {movement_cost}

      let (rng, building_det) = dist.rand (0, 1) rng
      let has_building = building_det < 0.0001
      let (rng, building: maybe Building.t) =
        if has_building
        then let (building, rng) = Building.random rng
             in (rng, #some building)
        else (rng, #none)

      let (rng, has_person) =
        if has_building
        then (rng, false)
        else let (rng, person_det) = dist.rand (0, 1) rng
             in (rng, person_det < 0.0001)
      let (rng, person: maybe person) =
        if has_person
        then let (target, rng) = Building.random rng
             let (rng, priority) = dist.rand (0, 1) rng
             in (rng, #some {target, priority})
        else (rng, #none)

      let empty_dwc: Direction.t_with_cost = {direction=#no_direction, cost=f32.inf}
      let building_directions = Building.seq.set empty_dwc empty_dwc empty_dwc empty_dwc empty_dwc

      let cell = {ground, building, person, building_directions, rng}
      in (rng, cell)
  }
}

module lys = mk_stencil_lys routefinder
