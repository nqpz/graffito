import "base"
import "stencil"
import "random"

import "../lib/github.com/diku-dk/lys/lys"

local type text_content = i32
module mk_stencil_lys (stencil: stencil with create_input = rng
                                        with create_output = rng)
       : lys with text_content = text_content = {
  type state_sized [h][w] = {h: i64, w: i64, rng: rng, time: f32, paused: bool, cells: [h][w]stencil.cell}
  type~ state = state_sized [][]

  def grab_mouse = false

  def init (seed: u32) (h: i64) (w: i64): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let (cells, rng) = stencil.create_cells h w rng
    in {h, w, rng, time=0, paused=false, cells}

  def resize (h: i64) (w: i64) (s: state): state =
    let (cells, rng) = stencil.create_cells h w s.rng
    in s with h = h
         with w = w
         with cells = cells
         with rng = rng

  def step (s: state): state =
    s with cells = stencil.step (copy s.cells)

  def spice_up_rng (rng: rng) (time: f32): rng =
    let (_, seed) = rnge.rand rng
    in rnge.rng_from_seed [t32 (3007 * time) ^ i32.u64 seed]

  def event (e: event) (s: state): state =
    match e
    case #step td ->
      let s = s with time = s.time + td
      in if s.paused
         then s
         else step s
    case #keydown {key} ->
      if key == SDLK_SPACE
      then s with paused = !s.paused
      else if key == SDLK_s
      then step s
      else if key == SDLK_r
      then let rng = spice_up_rng s.rng s.time
           let (cells, rng) = stencil.create_cells s.h s.w rng
           in s with rng = rng
                with cells = cells
      else s
    case _ -> s

  def render (s: state): [][]argb.colour =
    stencil.render s.cells

  type text_content = text_content

  def text_format () = "FPS: %d"

  def text_content (render_duration: f32) (_: state): text_content =
    t32 render_duration

  def text_colour = const argb.green
}
