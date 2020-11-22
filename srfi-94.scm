(module srfi-94 ()
  (import scheme)
  (import (chicken base))
  (import (chicken module))
  (import (chicken platform))

  (import 'srfi-60)          ;ash, integer-length

  ; srfi-23 is already part of Chicken core, so no need to import it
  ;(import 'srfi-23)          ;error

  (register-feature! 'srfi-94)

  (include "srfi-94-impl.scm"))

