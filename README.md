# graffito

A small cellular automaton framework for quick design and visualization.

![](screenshots/steal.png)

![](screenshots/coralreef.png)

![](screenshots/gameoflifeprob.png)


## Building and running

Dependencies:

- The [futhark](http://futhark-lang.org) compiler
- A C compiler
- SDL2 and SDL2-ttf libraries with associated header files
- Python
- The xxd hex dump utility

You can either
- install these dependencies manually on your system of choice, or
- use the associated shell.nix file with [Nix](https://nixos.org/):
  `nix-shell -p futhark --run nix-shell`

Once you have the dependencies under control, run `futhark pkg sync`
once to fetch pinned Futhark package dependencies.

Then run `make` to build all cellular automata in the repository.  You
can find the resulting programs in the `bin` directory.  Here's a
curated list of the coolest-looking ones:

- `steal`: An area-stealing algorithm, kind of.
- `coralreef`: Shifting colors under the sea.
- `gameoflifeprob`: A probabilistic Game of Life.
- `rain`: Blue, long droplets.

Here's a template with some added documentation for what the different
parts mean, useful for getting started on your own:

- `template`: A small template with comments in order to get started.

These ones are also pretty neat:

- `routefinder`: Originally made to visualize route finding, but ended
  up being more about building mountains.
- `lines`: Scraggly lines.
- `squarespiral`: Many spiraling square shapes.
- `collatz`: A quiet sea (takes a while to get started).
- `gravity`: A simple implementation of gravity as we know it.  Can
  require multiple passes of the stencil.

The rest haven't ended up amazing but might still give some inspiration:

- `closingframe`: A growing black frame around a square of colors.
- `gameoflife`: A very basic, no-frills implementation of Game of Life.
- `diamonds`: Growing diamonds.
- `consistencyfier`: Not sure yet.
- `producerconsumer`: Glittering small movements.

Pass `--help` to any program to see which options can be changed.

By default, graffito uses [OpenCL](https://www.khronos.org/opencl/) as
its backend to compute the animations.  You can change the backend by
setting the `LYS_BACKEND` environment variable before calling `make`.
This is useful if you don't have OpenCL, or if you want to use something
else.  See [the lys GitHub
page](https://github.com/diku-dk/lys?tab=readme-ov-file#configuring-the-backend)
for the supported backends.

Controls in the visualizations:

- Space: Pause
- S: Step once
- R: Reset
- Mouse wheel: Zoom in and out
- Mouse: Move around when zoomed in


## Adding automata

- Create a new directory under `stencils/` with a `stencil.fut` file and
  a Makefile.
- Extend the root `Makefile`.


## MEGAGRAFFITO

See also the [MEGAGRAFFITO](./MEGAGRAFFITO) subdirectory for a single
program containing all the automata (not built by default).
