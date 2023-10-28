#!/usr/bin/env guile
!#

;; -*- geiser-scheme-implementation: guile -*-

(import (ice-9 textual-ports))

(define (eprintln str)
  (display str (current-error-port))
  (newline (current-error-port)))

(define (rprintln str)
  (display str)
  (newline)
  (force-output))

(let loop ()
  (let ((line (get-line (current-input-port))))
    (when (not (eof-object? line))
      (eprintln line)
      (when (string-prefix? "$comment data_end" line)
        (rprintln "$name dump")
        (rprintln "#0 TEST")
        (rprintln "$finish"))
      (loop))))
