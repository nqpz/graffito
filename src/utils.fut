import "../lib/github.com/athas/matte/colour"
import "random"
import "stencil"

module create_random_cells (create_cell: {
  include cell
  val random_cell: rng -> (rng, cell)
}): create with cell = create_cell.cell
           with create_input = rng
           with create_output = rng = {
  type cell = create_cell.cell
  type create_input = rng
  type create_output = rng
  def create_cells (h: i64) (w: i64) (rng: rng): ([h][w]cell, rng) =
    let (rngs, cells) = rnge.split_rng (h * w) rng
                        |> map create_cell.random_cell
                        |> unzip
    in (unflatten h w cells, rnge.join_rng rngs)
}

module random_color_rgb = {
  open create_random_cells {
    type cell = argb.colour
    def random_cell rng =
      let (rng, r) = dist.rand (0.25, 0.75) rng
      let (rng, g) = dist.rand (0.25, 0.75) rng
      let (rng, b) = dist.rand (0.25, 0.75) rng
      in (rng, argb.from_rgba r g b 1)
  }
}
