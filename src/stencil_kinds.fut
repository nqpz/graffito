import "base"
import "setget"
import "stencil"

local module corner_conditions = {
  module pixel_grid = {
    def is_not_in_corner (s: coordinate_state): bool =
      s.y > 0 && s.y < s.h - 1 && s.x > 0 && s.x < s.w - 1
  }
}

module stencil_kinds = {
  module cross = {
    module setget = setget4
    open corner_conditions.pixel_grid

    def neighbor_offsets: setget.elems neighbor_offset =
      setget.set (-1,  0, \s -> s.y > 0)
                 ( 0, -1, \s -> s.x > 0)
                 ( 0,  1, \s -> s.x < s.w - 1)
                 ( 1,  0, \s -> s.y < s.h - 1)
  }

  module square = {
    module setget = setget8
    open corner_conditions.pixel_grid

    def neighbor_offsets: setget.elems neighbor_offset =
      setget.set (-1, -1, \s -> s.y > 0 && s.x > 0)
                 (-1,  0, \s -> s.y > 0)
                 (-1,  1, \s -> s.y > 0 && s.x < s.w - 1)
                 ( 0, -1, \s -> s.x > 0)
                 ( 0,  1, \s -> s.x < s.w - 1)
                 ( 1, -1, \s -> s.y < s.h - 1 && s.x > 0)
                 ( 1,  0, \s -> s.y < s.h - 1)
                 ( 1,  1, \s -> s.y < s.h - 1 && s.x < s.w - 1)
  }
}
