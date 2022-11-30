-- Function types for inp -> inp -> ... -> outp.

module type function = {
  type^ f '^inp '^outp

  module nf: {
    type^ f 'inp 'outp
  }
}

local module increment (prev: function) = {
  type^ f '^inp '^outp = prev.f inp (inp -> outp)

  module nf = {
    type^ f 'inp 'outp = prev.nf.f inp (inp -> outp)
  }
}

module function1 = {
  type^ f '^inp '^outp = inp -> outp

  module nf = {
    type^ f 'inp 'outp = inp -> outp
  }
}

module function2 = increment function1
module function3 = increment function2
module function4 = increment function3
module function5 = increment function4
module function6 = increment function5
module function7 = increment function6
module function8 = increment function7
