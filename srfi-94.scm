(module srfi-94 ()
  (import (except scheme
                  remainder
                  modulo
                  abs
                  make-polar
                  make-rectangular
                  quotient))
  (import (chicken base))
  (import (chicken module))
  (import (chicken platform))

  (import srfi-60) ;ash, integer-length

  ; srfi-23 is already part of Chicken core, so no need to import it
  ;(import 'srfi-23)          ;error

  (register-feature! 'srfi-94)

  (export
    real-exp
    real-ln
    real-log
    real-sin
    real-cos
    real-tan
    real-asin
    real-acos
    real-atan
    atan
    real-sqrt
    integer-sqrt
    integer-log
    integer-expt
    real-expt
    quo
    rem
    mod
    ln
    make-rectangular
    make-polar
    abs
    quotient
    remainder
    modulo)

  (include "srfi-94-impl.scm"))

