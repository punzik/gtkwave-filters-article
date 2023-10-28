#!/usr/bin/env guile
!#

(import (ice-9 textual-ports))

(let loop ()
  (let ((str (get-line (current-input-port))))
    (when (not (eof-object? str))
      (let ((key (string->number str)))
        (display
         (cond
          ((not key) "?dark red?NaN")
          ((< key 0) (format #f "?blue4?~a" key))
          ((< key 4) key)
          (else (format #f "?brown4?~a" key))))
        (newline)
        (force-output))
      (loop))))
