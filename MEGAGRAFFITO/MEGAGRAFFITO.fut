import "../lib/github.com/diku-dk/lys/lys"
import "../src/random"

-- FIXME: This file should be auto-generated.

module stencils = {
  module template = import "../stencils/template/stencil"
  module steal = import "../stencils/steal/stencil"
  module gameoflife = import "../stencils/gameoflife/stencil"
  module gameoflifeprob = import "../stencils/gameoflifeprob/stencil"
  module closingframe = import "../stencils/closingframe/stencil"
  module diamonds = import "../stencils/diamonds/stencil"
  module routefinder = import "../stencils/routefinder/stencil"
  module consistencyfier = import "../stencils/consistencyfier/stencil"
  module lines = import "../stencils/lines/stencil"
  module producerconsumer = import "../stencils/producerconsumer/stencil"
  module rain = import "../stencils/rain/stencil"
}

-- All stencils have the same text_content type.
type text_content = (i64, stencils.template.lys.text_content)

module lys: lys with text_content = text_content = {
  type~ stencil_state = #template stencils.template.lys.state
                      | #steal stencils.steal.lys.state
                      | #gameoflife stencils.gameoflife.lys.state
                      | #gameoflifeprob stencils.gameoflifeprob.lys.state
                      | #routefinder stencils.routefinder.lys.state
                      | #consistencyfier stencils.consistencyfier.lys.state
                      | #lines stencils.lines.lys.state
                      | #producerconsumer stencils.producerconsumer.lys.state
                      | #rain stencils.rain.lys.state
                      -- | #closingframe stencils.closingframe.lys.state -- broken?
                      -- | #diamonds stencils.diamonds.lys.state

  type~ state = {seed: u32, h: i64, w: i64, stencil: stencil_state}

  def grab_mouse = false

  def init (seed: u32) (h: i64) (w: i64): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let (_, i) = dist_i32.rand (0, 8) rng
    let stencil = match i
                  case 0 -> #template (stencils.template.lys.init seed h w)
                  case 1 -> #steal (stencils.steal.lys.init seed h w)
                  case 2 -> #gameoflife (stencils.gameoflife.lys.init seed h w)
                  case 3 -> #gameoflifeprob (stencils.gameoflifeprob.lys.init seed h w)
                  case 4 -> #routefinder (stencils.routefinder.lys.init seed h w)
                  case 5 -> #consistencyfier (stencils.consistencyfier.lys.init seed h w)
                  case 6 -> #lines (stencils.lines.lys.init seed h w)
                  case 7 -> #producerconsumer (stencils.producerconsumer.lys.init seed h w)
                  case _ -> #rain (stencils.rain.lys.init seed h w)
    in {seed, h, w, stencil}

  def next_stencil (s: state): state =
    s with stencil = match s.stencil
                     case #template _ -> #steal (stencils.steal.lys.init s.seed s.h s.w)
                     case #steal _ -> #gameoflife (stencils.gameoflife.lys.init s.seed s.h s.w)
                     case #gameoflife _ -> #gameoflifeprob (stencils.gameoflifeprob.lys.init s.seed s.h s.w)
                     case #gameoflifeprob _ -> #routefinder (stencils.routefinder.lys.init s.seed s.h s.w)
                     case #routefinder _ -> #consistencyfier (stencils.consistencyfier.lys.init s.seed s.h s.w)
                     case #consistencyfier _ -> #lines (stencils.lines.lys.init s.seed s.h s.w)
                     case #lines _ -> #producerconsumer (stencils.producerconsumer.lys.init s.seed s.h s.w)
                     case #producerconsumer _ -> #rain (stencils.rain.lys.init s.seed s.h s.w)
                     case #rain _ -> #template (stencils.template.lys.init s.seed s.h s.w)

  def prev_stencil (s: state): state =
    s with stencil = match s.stencil
                     case #template _ -> #rain (stencils.rain.lys.init s.seed s.h s.w)
                     case #steal _ -> #template (stencils.template.lys.init s.seed s.h s.w)
                     case #gameoflife _ -> #steal (stencils.steal.lys.init s.seed s.h s.w)
                     case #gameoflifeprob _ -> #gameoflife (stencils.gameoflife.lys.init s.seed s.h s.w)
                     case #routefinder _ -> #gameoflifeprob (stencils.gameoflifeprob.lys.init s.seed s.h s.w)
                     case #consistencyfier _ -> #routefinder (stencils.routefinder.lys.init s.seed s.h s.w)
                     case #lines _ -> #consistencyfier (stencils.consistencyfier.lys.init s.seed s.h s.w)
                     case #producerconsumer _ -> #lines (stencils.lines.lys.init s.seed s.h s.w)
                     case #rain _ -> #producerconsumer (stencils.producerconsumer.lys.init s.seed s.h s.w)

  def resize (h: i64) (w: i64) (s: state): state =
    s with h = h
      with w = w
      with stencil = match s.stencil
                     case #template s' -> #template (stencils.template.lys.resize h w s')
                     case #steal s' -> #steal (stencils.steal.lys.resize h w s')
                     case #gameoflife s' -> #gameoflife (stencils.gameoflife.lys.resize h w s')
                     case #gameoflifeprob s' -> #gameoflifeprob (stencils.gameoflifeprob.lys.resize h w s')
                     case #routefinder s' -> #routefinder (stencils.routefinder.lys.resize h w s')
                     case #consistencyfier s' -> #consistencyfier (stencils.consistencyfier.lys.resize h w s')
                     case #lines s' -> #lines (stencils.lines.lys.resize h w s')
                     case #producerconsumer s' -> #producerconsumer (stencils.producerconsumer.lys.resize h w s')
                     case #rain s' -> #rain (stencils.rain.lys.resize h w s')

  def stencil_event (e: event) (s: state): state =
    s with stencil = match s.stencil
                     case #template s' -> #template (stencils.template.lys.event e s')
                     case #steal s' -> #steal (stencils.steal.lys.event e s')
                     case #gameoflife s' -> #gameoflife (stencils.gameoflife.lys.event e s')
                     case #gameoflifeprob s' -> #gameoflifeprob (stencils.gameoflifeprob.lys.event e s')
                     case #routefinder s' -> #routefinder (stencils.routefinder.lys.event e s')
                     case #consistencyfier s' -> #consistencyfier (stencils.consistencyfier.lys.event e s')
                     case #lines s' -> #lines (stencils.lines.lys.event e s')
                     case #producerconsumer s' -> #producerconsumer (stencils.producerconsumer.lys.event e s')
                     case #rain s' -> #rain (stencils.rain.lys.event e s')

  def event (e: event) (s: state): state =
    match e
    case #keydown {key} ->
      if key == SDLK_RIGHT
      then next_stencil s
      else if key == SDLK_LEFT
      then prev_stencil s
      else stencil_event e s
    case _ -> stencil_event e s

  def render (s: state) =
    match s.stencil
    case #template s' -> stencils.template.lys.render s'
    case #steal s' -> stencils.steal.lys.render s'
    case #gameoflife s' -> stencils.gameoflife.lys.render s'
    case #gameoflifeprob s' -> stencils.gameoflifeprob.lys.render s'
    case #routefinder s' -> stencils.routefinder.lys.render s'
    case #consistencyfier s' -> stencils.consistencyfier.lys.render s'
    case #lines s' -> stencils.lines.lys.render s'
    case #producerconsumer s' -> stencils.producerconsumer.lys.render s'
    case #rain s' -> stencils.rain.lys.render s'

  type text_content = text_content

  def text_format () =
    "Name: %["
    ++ "template"
    ++ "|steal"
    ++ "|gameoflife"
    ++ "|gameoflifeprob"
    ++ "|routefinder"
    ++ "|consistencyfier"
    ++ "|lines"
    ++ "|producerconsumer"
    ++ "|rain"
    ++ "]\n"
    ++ stencils.template.lys.text_format ()

  def text_content fps (s: state): text_content =
    match s.stencil
    case #template s' -> (0, stencils.template.lys.text_content fps s')
    case #steal s' -> (1, stencils.steal.lys.text_content fps s')
    case #gameoflife s' -> (2, stencils.gameoflife.lys.text_content fps s')
    case #gameoflifeprob s' -> (3, stencils.gameoflifeprob.lys.text_content fps s')
    case #routefinder s' -> (4, stencils.routefinder.lys.text_content fps s')
    case #consistencyfier s' -> (5, stencils.consistencyfier.lys.text_content fps s')
    case #lines s' -> (6, stencils.lines.lys.text_content fps s')
    case #producerconsumer s' -> (7, stencils.producerconsumer.lys.text_content fps s')
    case #rain s' -> (8, stencils.rain.lys.text_content fps s')

  def text_colour = const argb.yellow
}
