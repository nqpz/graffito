-- | Basic stencil example with colors.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"

module closingframe = mk_stencil {
  open specialize_stencil_kind stencil_kinds.cross {
    type cell = argb.colour
  }

  def new_cell _cell neighbors =
    let mix_input (x: maybe argb.colour) =
      match x
      case #some color ->
        let (r, g, b, _) = argb.to_rgba color
        in argb.scale color (r * g * b)
      case #none -> argb.scale argb.white 0.26

    let mix color0 color1 = argb.add_linear color0 color1

    in get neighbors (\a b c d ->
                        mix (mix (mix_input a) (mix_input b))
                            (mix (mix_input c) (mix_input d)))

  def render_cell cell = cell

  def random_color (rng: rng): (rng, argb.colour) =
    let (rng, r) = dist.rand (0.25, 0.75) rng
    let (rng, g) = dist.rand (0.25, 0.75) rng
    let (rng, b) = dist.rand (0.25, 0.75) rng
    in (rng, argb.from_rgba r g b 1)

  type create_input = rng
  type create_output = rng
  def create_cells (h: i64) (w: i64) (rng: rng): ([h][w]cell, rng) =
    let (rngs, cells) = rnge.split_rng (h * w) rng
                        |> map random_color
                        |> unzip
    in (unflatten h w cells, rnge.join_rng rngs)
}

module lys = mk_stencil_lys closingframe
