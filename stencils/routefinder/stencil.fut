-- | Route finder
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"

module routefinder = mk_stencil {
  open stencil_kinds.cross

  type ground = {movement_cost: f32}

  type building = #kitchen
                | #bathroom
                | #recroom
                | #bedroom

  def building_color (building: building): argb.colour =
    match building
    case #kitchen -> argb.green
    case #bathroom -> argb.brown
    case #recroom -> argb.magenta
    case #bedroom -> argb.yellow

  type person = {target: building, priority: f32}

  type direction = #north
                 | #west
                 | #east
                 | #south
                 | #no_direction

  def raw_directions: setget.elems direction =
    setget.set #north #west #east #south

  type direction_with_cost = {direction: direction, cost: f32}

  type directions = {kitchen: direction_with_cost,
                     bathroom: direction_with_cost,
                     recroom: direction_with_cost,
                     bedroom: direction_with_cost}

  type cell = {ground: ground,
               building: maybe building,
               person: maybe person,
               directions: directions,
               rng: rng}

  module Building = {
    def random (rng: rng): (rng, building) =
      let (rng, building_i) = dist_int.rand (0, 3) rng
      let building = if building_i == 0
                     then #kitchen
                     else if building_i == 1
                     then #bathroom
                     else if building_i == 2
                     then #recroom
                     else #bedroom
      in (rng, building)

    def update (cell: cell): cell =
      let (rng, det) = dist.rand (0, 1) cell.rng
      let (rng, building) =
        if det < 0.001
        then match cell.building
             case #some _ -> (rng, #none)
             case #none -> let (rng, b) = random rng
                           in (rng, #some b)
        else (rng, cell.building)
      in cell with rng = rng
              with building = building
  }

  module Directions = {
    def update_direction_with_cost (cell: cell)
                                   (neighbors: setget.elems (maybe cell))
                                   (get_direction_with_cost: directions -> direction_with_cost)
                                   (target: building): direction_with_cost =
      if cell.building == #some target
      then {direction=#no_direction, cost=0}
      else let make_direction_with_cell_cost dir =
             {direction=dir, cost=cell.ground.movement_cost}
           let get_cost: maybe cell -> maybe f32 =
             maybe_join <-< (maybe_map ((\(dwc: direction_with_cost) ->
                                           if dwc.direction == #no_direction
                                           then #none
                                           else #some dwc.cost)
                                        <-< get_direction_with_cost <-< (.directions)))
           let get_building : maybe cell -> maybe building =
             maybe_join <-< maybe_map (.building)

           let buildings = setget.map get_building neighbors
           let dir_direct = setget.find (\(b, dir) ->
                                           if b == #some target
                                           then #some (make_direction_with_cell_cost dir)
                                           else #none)
                                        (setget.zip buildings raw_directions)
           in match dir_direct
              case #some d -> d
              case #none ->
                let costs = setget.map get_cost neighbors
                let (mcost, dir) = setget.fold (\(mcost0, dir0) (mcost1, dir1) ->
                                                  match (mcost0, mcost1)
                                                  case (#some cost0, #some cost1) ->
                                                    if cost0 < cost1
                                                    then (mcost0, dir0)
                                                    else (mcost1, dir1)
                                                  case (#some _, #none) -> (mcost0, dir0)
                                                  case _ -> (mcost1, dir1))
                                               (setget.zip costs raw_directions)
                in match mcost
                   case #some cost -> {direction=dir, cost=cell.ground.movement_cost + cost}
                   case #none -> get_direction_with_cost cell.directions

    def update (cell: cell) (neighbors: setget.elems (maybe cell)): cell =
      cell with directions = {kitchen= update_direction_with_cost cell neighbors (.kitchen)  #kitchen,
                              bathroom=update_direction_with_cost cell neighbors (.bathroom) #bathroom,
                              recroom= update_direction_with_cost cell neighbors (.recroom)  #recroom,
                              bedroom= update_direction_with_cost cell neighbors (.bedroom)  #bedroom}
  }

  def new_cell cell neighbors =
    let cell = Building.update cell
    let cell = Directions.update cell neighbors
    in cell

  def render_cell (cell: cell) =
    match cell.directions.kitchen.direction
    case #north -> argb.blue
    case #west -> argb.yellow
    case #east -> argb.violet
    case #south -> argb.red
    case #no_direction -> argb.black

  -- def render_cell (cell: cell) =
  --   let background = argb.gray cell.ground.movement_cost
  --   in match maybe_map building_color cell.building
  --      case #some c -> c -- argb.mix 0.5 background 0.5 c
  --      case #none -> argb.scale background 0.1

  open create_random_cells {
    type cell = cell

    def random_cell rng =
      let (rng, movement_cost) = dist.rand (0, 1) rng
      let ground = {movement_cost}

      let (rng, building_det) = dist.rand (0, 1) rng
      let has_building = building_det < 0.0001
      let (rng, building: maybe building) =
        if has_building
        then let (rng, building) = Building.random rng
             in (rng, #some building)
        else (rng, #none)

      let (rng, has_person) =
        if has_building
        then (rng, false)
        else let (rng, person_det) = dist.rand (0, 1) rng
             in (rng, person_det < 0.0001)
      let (rng, person: maybe person) =
        if has_person
        then let (rng, target) = Building.random rng
             let (rng, priority) = dist.rand (0, 1) rng
             in (rng, #some {target, priority})
        else (rng, #none)

      let empty_dwc: direction_with_cost = {direction=#no_direction, cost=f32.inf}
      let directions = {kitchen=empty_dwc,
                        bathroom=empty_dwc,
                        recroom=empty_dwc,
                        bedroom=empty_dwc}

      let cell = {ground, building, person, directions, rng}
      in (rng, cell)
  }
}

module lys = mk_stencil_lys routefinder
