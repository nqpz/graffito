module random_color_rgb = {
  import "../lib/github.com/athas/matte/colour"
  import "random"

  def random_color (rng: rng): (rng, argb.colour) =
    let (rng, r) = dist.rand (0.25, 0.75) rng
    let (rng, g) = dist.rand (0.25, 0.75) rng
    let (rng, b) = dist.rand (0.25, 0.75) rng
    in (rng, argb.from_rgba r g b 1)

  type create_input = rng
  type create_output = rng
  def create_cells (h: i64) (w: i64) (rng: rng): ([h][w]argb.colour, rng) =
    let (rngs, cells) = rnge.split_rng (h * w) rng
                        |> map random_color
                        |> unzip
    in (unflatten h w cells, rnge.join_rng rngs)
}
