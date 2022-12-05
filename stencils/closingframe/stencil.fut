-- | Basic stencil example with colors.
import "../../lib/github.com/athas/matte/colour"
import "../../src/mk_stencil_lys"
import "../../src/base"
import "../../src/random"
import "../../src/stencil"
import "../../src/stencil_kinds"
import "../../src/utils"

module closingframe = mk_stencil {
  open stencil_kinds.cross

  type cell = argb.colour

  def new_cell _cell neighbors =
    let mix_input (x: maybe.t argb.colour) =
      match x
      case #some color -> argb.scale color 0.26
      case #none -> argb.black

    let mix color0 color1 = argb.add_linear color0 color1
    in seq.get (seq.map mix_input neighbors)
                  (\a b c d -> mix (mix a b) (mix c d))

  def render_cell cell = cell

  open random_color_rgb
}

module lys = mk_stencil_lys closingframe
