import "base"
import "setget"

local def when 'a (cond: bool) (get: () -> a): maybe a =
  if cond
  then #some (get ())
  else #none

module stencil_kinds = {
  module cross = {
    module setget = setget4

    def collect_neighbors [h][w] 'cell (cells: [h][w]cell) (y: index) (x: index): setget.elems (maybe cell) =
      setget.set (when (y > 0) (\() -> cells[y - 1, x]))
                 (when (x > 0) (\() -> cells[y, x - 1]))
                 (when (x < w - 1) (\() -> cells[y, x + 1]))
                 (when (y < h - 1) (\() -> cells[y + 1, x]))
  }

  module square = {
    module setget = setget8

    def collect_neighbors [h][w] 'cell (cells: [h][w]cell) (y: index) (x: index): setget.elems (maybe cell) =
      setget.set (when (y > 0 && x > 0) (\() -> cells[y - 1, x - 1]))
                 (when (y > 0) (\() -> cells[y - 1, x]))
                 (when (y > 0 && x < w - 1) (\() -> cells[y - 1, x + 1]))
                 (when (x > 0) (\() -> cells[y, x - 1]))
                 (when (x < w - 1) (\() -> cells[y, x + 1]))
                 (when (y < h - 1 && x > 0) (\() -> cells[y + 1, x - 1]))
                 (when (y < h - 1) (\() -> cells[y + 1, x]))
                 (when (y < h - 1 && x < w - 1) (\() -> cells[y + 1, x + 1]))
  }
}
