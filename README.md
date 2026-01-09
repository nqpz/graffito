# graffito

A small cellular automaton framework for quick design and visualization.

![](screenshots/steal.png)

![](screenshots/coralreef.png)

![](screenshots/gameoflifeprob.png)

Requires [Futhark](http://futhark-lang.org) and SDL2 and SDL2-ttf
libraries with associated header files.


## Building and running

First run `futhark pkg sync` once.

Then run `make` to build all programs.  You can find them in the `bin`
directory.  Here's a curated list of the coolest-looking ones:

- `steal`: An area-stealing algorithm, kind of.
- `coralreef`: Shifting colors under the sea.
- `gameoflifeprob`: A probabilistic Game of Life.
- `rain`: Blue, long droplets.

Here's a template with some added documentation for what the different
parts mean:

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

Controls in the visualizations:

- Space: Pause
- S: Step once
- R: Reset


## Adding automata

- Create a new directory under `stencils/` with a `stencil.fut` file and
  a Makefile.
- Extend the root `Makefile`.


## MEGAGRAFFITO

See also the [MEGAGRAFFITO](./MEGAGRAFFITO) subdirectory for a single
program containing all the automata (not built by default).
