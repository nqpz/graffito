import "function"

module type compose = {
  type^ f '^inp '^outp

  val compose '^inp '^outp: f inp outp -> (outp -> outp) -> f inp outp

  module nf: {
    type^ f 'inp 'outp

    val compose 'inp 'outp: f inp outp -> (outp -> outp) -> f inp outp
  }
}

local module mk_compose (function: function)
                        (compose_function: {
                           val compose '^inp '^outp: function.f inp outp
                                                  -> (outp -> outp)
                                                  -> function.f inp outp
                        })
                        (compose_function_nf: {
                           val compose 'inp 'outp: function.nf.f inp outp
                                                  -> (outp -> outp)
                                                  -> function.nf.f inp outp
                         }): compose with f '^inp '^outp = function.f inp outp
                                     with nf.f 'inp 'outp = function.nf.f inp outp = {
  type^ f '^inp '^outp = function.f inp outp

  def compose = compose_function.compose

  module nf = {
    type^ f 'inp 'outp = function.nf.f inp outp

    def compose = compose_function_nf.compose
  }
}

local module internal = {
  -- def increment = ((>->) (<-<) <-<)
  def increment c f g = (c f) (g <-<)

  module compose1 = { def compose = (>->) }
  module compose2 = { def compose = increment compose1.compose }
  module compose3 = { def compose = increment compose2.compose }
  module compose4 = { def compose = increment compose3.compose }
  module compose5 = { def compose = increment compose4.compose }
  module compose6 = { def compose = increment compose5.compose }
  module compose7 = { def compose = increment compose6.compose }
  module compose8 = { def compose = increment compose7.compose }
}

module compose1 = mk_compose function1 internal.compose1 internal.compose1
module compose2 = mk_compose function2 internal.compose2 internal.compose2
module compose3 = mk_compose function3 internal.compose3 internal.compose3
module compose4 = mk_compose function4 internal.compose4 internal.compose4
module compose5 = mk_compose function5 internal.compose5 internal.compose5
module compose6 = mk_compose function6 internal.compose6 internal.compose6
module compose7 = mk_compose function7 internal.compose7 internal.compose7
module compose8 = mk_compose function8 internal.compose8 internal.compose8
