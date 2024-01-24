type maybe 'a = #some a | #none

module type dir = {
  type dir

  val inverse_dir : dir -> dir
}

type^ propagator 'state 'dir = (state -> maybe dir, state -> state, state -> state)
type^ decider 'state 'dir = (state, dir -> bool) -> maybe dir

module type propdec = {
  include dir

  type state

  val propagate : propagator state dir

  val decide : decider state dir
}

module dirs = {
  module cross : dir with dir = #north | #east | #south | #west = {
    type dir = #north | #east | #south | #west

    def inverse_dir (dir: dir): dir =
      match dir
      case #north -> #south
      case #east -> #west
      case #south -> #north
      case #west -> #east
  }
}

module test_move: propdec = {
  open dirs.cross

  type state = {occupied: bool, target_if_occupied: dir}

  def propagate: propagator state dir =
    (\s -> if s.occupied
           then #some s.target_if_occupied
           else #none,

     \(s_neighbor: state) -> s_neighbor with occupied = true,

     \(s: state) -> s with occupied = false)

  def decide: decider state dir =
    \(s, check) ->
      if s.occupied
      then #none
      else #some (if check #north
                  then #north
                  else if check #south
                  then #south
                  else if check #east
                  then #east
                  else #west)
}
