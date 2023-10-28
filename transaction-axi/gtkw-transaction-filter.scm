;; -*- geiser-scheme-implementation: guile -*-

(define-module (gtkw-transaction-filter))

;;;
;;; Transaction filter pseudocode:
;;;
;;; while (!eof) {
;;;     str = read_line();
;;;
;;;     if (str == "$comment data_end ...") {
;;;         println("$name ..."); flush();
;;;         ...
;;;         println("$finish ..."); flush();
;;;     }
;;; }
;;;
;;; Important notes:
;;; 1. GTKWave waits for a response after each VCD bundle ("$comment data_start" ... "$comment data_end").
;;; 2. After each new line the output buffer of the stdout must be flushed.
;;;

(export
 ;; (parse-input-vcd-bundle [port]) -> bundle
 ;; Read and parse VCD from port (stdin by default)
 parse-input-vcd-bundle

 ;; (bundle-name bndl) -> string
 ;; Get VCD bundle name
 bundle-name

 ;; (bundle-timescale bndl) -> number
 ;; Get bundle timescale value as time multiplier (e.g. 10ns -> 1e-8)
 bundle-timescale

 ;; (bundle-tmin bndl) -> number
 ;; Get VCD left time (VCD comment min_time)
 bundle-tmin

 ;; (bundle-tmax bndl) -> number
 ;; Get VCD right time (VCD comment max_time)
 bundle-tmax

 ;; (bundle-signals bndl) -> list of signals
 ;; Get signals description list.
 ;; Items of list uses with functions signal-*
 bundle-signals

 ;; (bundle-samples bndl) -> list of samples
 ;; Get list of VCD samples sorted by time.
 ;; Items of list uses with functions sample-*
 bundle-samples

 ;; (bundle-sig-filter bndl pred) -> list of signals
 ;; Filter signals by predicate pred
 bundle-sig-filter

 ;; (bundle-sig-by-p bndl pred) -> signal
 ;; Find signal by predicate. Returns #f if no signal found
 bundle-sig-by-p

 ;; (bundle-sig-by-id bndl id) -> signal
 ;; Find signal by ID. Returns #f if no signal found
 bundle-sig-by-id

 ;; (bundle-sig-by-name bndl name #:with-scope #:ci) -> signal
 ;; Find signal by name. Returns #f if no signal found.
 ;; #:with-scope - use name with scope (default: #f)
 ;; #:ci         - case insensitive (default: #f)
 bundle-sig-by-name

 ;; (bundle-sig-by-prefix bndl prefix #:with-scope #:ci) -> signal
 ;; Find signal by name prefix. Returns #f if no signal found.
 ;; #:with-scope - use name with scope (default: #f)
 ;; #:ci         - case insensitive (default: #f)
 bundle-sig-by-prefix

 ;; (bundle-sig-by-suffix bndl suffix #:with-scope #:ci) -> signal
 ;; Find signal by name suffix. Returns #f if no signal found.
 ;; #:with-scope - use name with scope (default: #f)
 ;; #:ci         - case insensitive (default: #f)
 bundle-sig-by-suffix

 ;; (signal-id signal) -> number
 ;; Get signal ID
 signal-id

 ;; (signal-full-name signal) -> string
 ;; Get signal name with scope (e.g. "top.mod0_impl.mod1_impl.signal0")
 signal-full-name

 ;; (signal-scope signal) -> list
 ;; Get signal scope as string list. CAR of list is the name of the deepest
 ;; scope level. E.g. scope "top.mod0_impl.mod1_impl" represented as
 ;; list '("mod1_impl" "mod0_impl" "top").
 signal-scope

 ;; (signal-scope-name signal) -> string
 ;; Get signal scope name
 signal-scope-name

 ;; (signal-name signal) -> string
 ;; Get signal name without scope
 signal-name

 ;; (signal-width signal) -> number
 ;; Get signal width
 signal-width

 ;; (signal-dimstr signal) -> string
 ;; Get signal dimension (e.g. "[31:0]")
 signal-dimstr

 ;; (sample-time sample) -> number
 ;; Get sample timestamp in timescale units
 sample-time

 ;; (sample-value sample id) -> logic
 ;; Get current signal value by ID.
 ;; The 'logic' type is a vector of bits, where ONE and ZERO are
 ;; numbers 1 and 0, and the values of X and Z are represented
 ;; as symbols 'x and 'z
 sample-value

 ;; (logic->number v #:x-replace #:z-replace) -> number
 ;; Convert logic vector to number. If no conversion is possible
 ;; (vector has bits with state Z or X) then function returns #f.
 ;; #:x-replace - value with which the bits with state X will be replaced (default: 'x)
 ;; #:z-replace - value with which the bits with state Z will be replaced (default: 'z)
 logic->number

 ;; (logic->hex v) -> string
 ;; Convert logic vector to hex string
 logic->hex

 ;; (logic->bin v) -> string
 ;; Convert logic vector to binary string
 logic->bin

 ;; (number->logic x width) -> logic
 ;; Convert number to logic verctor with width 'width'
 number->logic

 ;; (logic-*? v) -> bool
 ;; Logic vector predicates
 logic-zero?
 logic-one?
 logic-has-x?
 logic-has-z?

 ;; (logic-valid? v) -> bool
 ;; Check for the vector does not contain X and Z bits
 logic-valid?

 ;; (logic-concat v ...) -> vector
 ;; Concatenate vectors (from least significant to most significant)
 logic-concat)

(import (ice-9 textual-ports)           ; read port by lines
        (srfi srfi-1)                   ; lists
        (srfi srfi-11)                  ; let-values
        (srfi srfi-13)                  ; strings
        (srfi srfi-26)                  ; cut
        (srfi srfi-28)                  ; simple format
        (srfi srfi-43))                 ; vectors

;;;
;;; Insert x between list items
;;;
(define (insert-between lst x)
  (if (or (null? lst)
          (null? (cdr lst)))
      lst
      (cons* (car lst) x
             (insert-between (cdr lst) x))))

;;;
;;; Convert VCD binary to scheme vector
;;; See Table 21-9 of IEEE Std 1800-2012
;;;
;;; Characters will be replaced with numbers 1, 0 and symbols 'x and 'z
;;;
(define (vcd-bin-list->logic w l)
  (list->vector
   (map (lambda (c) (cond
                ((char=? c #\0) 0)
                ((char=? c #\1) 1)
                ((char-ci=? c #\x) 'x)
                ((char-ci=? c #\z) 'z)
                (else #f)))
        (take (if (>= (length l) w)
                  l
                  (let ((msb (last l)))
                    (append
                     l
                     (make-list (- w (length l))
                                (if (char=? #\1 msb)
                                    #\0
                                    msb)))))
              w))))

;;;
;;; (parse-timescale string) -> number
;;; Parse timescale value (1ps, 100ns, etc)
;;;
(define (parse-timescale ts)
  (let ((dim-idx (string-skip ts char-numeric?)))
    (if dim-idx
        (let* ((n (string->number (substring ts 0 dim-idx)))
               (unit (substring ts dim-idx))
               (k (assoc unit '(("s" 1) ("ms" 1e-3) ("us" 1e-6) ("ns" 1e-9) ("ps" 1e-12) (fs 1e-15)))))
          (if k
              (* n (cadr k))
              (raise `(vcd-syntax-error
                       ,(format "Unknown timescale unit '~a'" unit)))))
        (raise `(vcd-syntax-error
                 ,(format "Wrong timescale '~a'" ts))))))

;; (define get-line-old get-line)
;; (define (get-line port)
;;   (let ((l (get-line-old port)))
;;     (display l (current-error-port))
;;     (newline (current-error-port))
;;     l))

;;;
;;; Parse VCD header (definitions)
;;; Returns header data:
;;; '(<bundle-name>
;;;   <timescale>
;;;   <min-time>
;;;   <max-time>
;;;   ;; signals info
;;;   (<vcd-id> <scope> <name> <width> <slice>)
;;;   (<vcd-id> <scope> <name> <width> <slice>)
;;;   ...)
;;;
;;; <timescale> - timescale multiplier (e.g. for 10ns timescale value is 1e-8)
;;; <vcd-id>    - VCD identifier string
;;; <scope>     - signal scope as string list. CAR of list is the name of the deepest
;;;               scope level. E.g. scope "top.mod0_impl.signal0" represented as
;;;               list '("signal0" "mod0_impl" "top").
;;; <name>      - signal name without scope
;;; <slice>     - signal dimension string (e.g. "[31:0]")
;;;
;;; If EOF, return #f
;;;
(define* (parse-vcd-header #:optional (port (current-input-port)))
  (let ((name "")
        (timescale 1e-12)
        (min-time 0)
        (max-time 0)
        (scope '())
        (signals '()))
    (let next-line ()
      (let ((s (get-line port)))
        (cond
         ;; End of file. Return false
         ((eof-object? s) #f)

         ;; Bundle name
         ((string-prefix? "$comment name" s)
          (set! name (string-trim-both
                      (substring s 13 (- (string-length s)
                                         (if (string-suffix? " $end" s) 4 0)))))
          (next-line))

         ;; Min_time
         ((string-prefix? "$comment min_time" s)
          (set! min-time (string->number (third (string-split s #\space))))
          (next-line))

         ;; Max_time
         ((string-prefix? "$comment max_time" s)
          (set! max-time (string->number (third (string-split s #\space))))
          (next-line))

         ;; Timescale
         ((string-prefix? "$timescale" s)
          (set! timescale (parse-timescale (second (string-split s #\space))))
          (next-line))

         ;; Scope (ignore scope type)
         ((string-prefix? "$scope" s)
          (set! scope (cons (third (string-split s #\space)) scope))
          (next-line))

         ;; Upscope
         ((string-prefix? "$upscope" s)
          (set! scope (cdr scope))
          (next-line))

         ;; Signal description
         ((string-prefix? "$var" s)
          (set! signals
                (let ((si (string-split s #\space)))
                  (cons
                   `( ;; id
                     ,(fourth si)
                     ;; scope
                     ,scope
                     ;; name
                     ,(first (string-split (fifth si) #\[))
                     ;; width
                     ,(string->number (third si))
                     ;; slice
                     ,(let ((ni (cdr (string-split (fifth si) #\[))))
                        (if (null? ni)
                            ""
                            (string-concatenate
                             (map (cut string-append "[" <>) ni)))))
                   signals)))
          (next-line))

         ;; Done
         ((string-prefix? "$enddefinitions" s)
          `(,name
            ,timescale
            ,min-time
            ,max-time
            ,@(sort signals
                    (lambda (s1 s2)
                      (string<? (car s1)
                                (car s2))))))

         ;; Skip line
         (else (next-line)))))))

;;;
;;; Parse VCD body
;;; Returns VCD changes as samples list:
;;; '((<time> (<vcd-id> <lsb> ... <msb>) (<id> <lsb> ... <msb>) ...)
;;;   (<time> (<vcd-id> <lsb> ... <msb>) (<id> <lsb> ... <msb>) ...)
;;;   ...
;;; )
;;;
;;; If EOF, return #f
;;;
(define* (parse-vcd-body #:optional (port (current-input-port)))
  (let next-line ((time #f)
                  (sample '())
                  (signals '()))
    (let ((s (get-line port)))
      (cond
       ;; End of file. Return false
       ((eof-object? s) #f)

       ;; Empty line
       ((string-null? s) (next-line time sample signals))

       ;; End of VCD data
       ((string-prefix? "$comment data_end" s)
        (reverse
         (if (and time (not (null? sample)))
             ;; Add last sample
             (cons (cons time sample) signals)
             signals)))

       ;; Time stamp
       ((char=? #\# (string-ref s 0))
        (next-line (string->number (substring s 1))
                   '()
                   (if (and time (not (null? sample)))
                       (cons (cons time sample) signals)
                       signals)))

       ;; Scalar
       ((let ((c (string-ref s 0)))
          (or (char-ci=? c #\0)
              (char-ci=? c #\1)
              (char-ci=? c #\x)
              (char-ci=? c #\z)))
        (next-line time
                   (cons `(,(substring s 1)
                           ,(char-downcase
                             (string-ref s 0)))
                         sample)
                   signals))

       ;; Vector
       ((char=? #\b (string-ref s 0))
        (let ((si (string-split s #\space)))
          (next-line time
                     (cons (cons
                            (second si)
                            (reverse
                             (string->list
                              (string-downcase
                               (substring (first si) 1)))))
                           sample)
                     signals)))

       ;; TODO: Parse real?

       ;; Skip line
       (else (next-line time sample signals))))))

;;;
;;; Parse simplified VCD
;;; Returns bundle:
;;;   '(;; header
;;;     (<bundle-name>
;;;      <timescale>
;;;      <min-time>
;;;      <max-time>
;;;      (<id> <scope> <name> <width> <dimension>)
;;;      (<id> <scope> <name> <width> <dimension>)
;;;      ...)
;;;     ;; samples
;;;     (<timestamp> (<id> . <logic>) (<id> . <logic>) ...)
;;;     (<timestamp> (<id> . <logic>) (<id> . <logic>) ...)
;;;     ...)
;;;
;;; <timescale>  - Timescale multiplier (e.g. 1e-8 for 10ns timescale)
;;; <id>         - Signal identifier number (note: it's not a VCD id)
;;; <full-name>  - Signal name with scope (e.g. mod1.get0.sig)
;;; <width>      - Signal width
;;; <dimension>  - Signal dimension string (e.g. "[31:0]")
;;; <header>     - List returned by (parse-vcd-header)
;;; <scope>      - Signal scope as string list. CAR of list is the name of the deepest
;;;                scope level. E.g. scope "top.mod0_impl.mod1_impl" represented as
;;;                list '("mod1_impl" "mod0_impl" "top").
;;; <name>       - Signal name without scope
;;; <logic>      - Vector of bits. ONE and ZERO states are represented as the
;;;                numbers 1 and 0. 'X' and 'Z' states are represented as symbols
;;;                'x and 'z.
;;; <timestamp>  - Sample time (number)
;;;
(define* (parse-input-vcd-bundle #:optional (port (current-input-port)))
  (let* ((header (parse-vcd-header port))
         (samples (parse-vcd-body port)))
    (if (and header samples)
        ;; Make VCD data
        (let* ((hdr-info (take header 4))
               (signals (drop header 4))
               (sample-widths (map fourth signals))
               (sample-vcd-ids (map first signals))
               (sample-ids (iota (length signals))))
          (cons
           ;; Header
           (append hdr-info
                   (map (lambda (s id) (cons id (cdr s)))
                        signals
                        sample-ids))
           ;; Samples
           (let next ((samples samples)
                      (result '())
                      (last-sample (map (const #\x) signals)))
             (if (null? samples)
                 (reverse result)
                 (let* ((sample (car samples))
                        (time (car sample))
                        (changes (cdr sample))
                        (new-sample
                         (map (lambda (s w id)
                                (let ((new (assoc id changes)))
                                  (if new
                                      (vcd-bin-list->logic w (cdr new))
                                      s)))
                              last-sample
                              sample-widths
                              sample-vcd-ids)))
                   (next (cdr samples)
                         (cons (cons time (map cons sample-ids new-sample)) result)
                         new-sample))))))

        ;; Return #f when incomplete VCD received
        #f)))

;;; (bundle-header bndl) -> list of bundle header itens
;;; PRIVATE
(define bundle-header car)

;;; (bundle-samples bndl) -> list of samples
;;; Get list of VCD samples sorted by time.
;;; Items of list uses with functions sample-*
(define bundle-samples cdr)

;;; (bundle-name bndl) -> string
;;; Get VCD bundle name
(define (bundle-name bndl)
  (first (bundle-header bndl)))

;;; (bundle-timescale bndl) -> number
;;; Get bundle timescale value as time multiplier (e.g. 10ns -> 1e-8)
(define (bundle-timescale bndl)
  (second (bundle-header bndl)))

;;; (bundle-tmin bndl) -> number
;;; Get VCD left time (VCD comment min_time)
(define (bundle-tmin bndl)
  (third (bundle-header bndl)))

;;; (bundle-tmax bndl) -> number
;;; Get VCD right time (VCD comment max_time)
(define (bundle-tmax bndl)
  (fourth (bundle-header bndl)))

;;; (bundle-signals bndl) -> list of signals
;;; Get signals description list.
;;; Items of list uses with functions signal-*
(define (bundle-signals bndl)
  (cddddr (bundle-header bndl)))

;;; (bundle-sig-filter bndl pred) -> list of signals
;;; Filter signals by predicate pred
(define (bundle-sig-filter bndl pred)
  (filter pred (bundle-signals bndl)))

;;; (bundle-sig-by-p bndl pred) -> signal
;;; Find signal by predicate. Returns #f if no signal found
(define (bundle-sig-by-p bndl pred)
  (find pred (bundle-signals bndl)))

;;; (bundle-sig-by-id bndl id) -> signal
;;; Find signal by ID. Returns #f if no signal found
(define (bundle-sig-by-id bndl id)
  (assq id (bundle-signals bndl)))

;;; (bundle-sig-by-name bndl name #:with-scope #:ci) -> signal
;;; Find signal by name. Returns #f if no signal found.
;;; #:with-scope - use name with scope (default: #f)
;;; #:ci         - case insensitive (default: #f)
(define* (bundle-sig-by-name bndl name #:key (with-scope #f) (ci #f))
  (let ((eq (if ci string-ci= string=))
        (nm (if with-scope signal-full-name signal-name)))
    (bundle-sig-by-p bndl (lambda (s) (eq name (nm s))))))

;;; (bundle-sig-by-prefix bndl prefix #:with-scope #:ci) -> signal
;;; Find signal by name prefix. Returns #f if no signal found.
;;; #:with-scope - use name with scope (default: #f)
;;; #:ci         - case insensitive (default: #f)
(define* (bundle-sig-by-prefix bndl prefix #:key (with-scope #f) (ci #f))
  (let ((pr (if ci string-prefix-ci? string-prefix?))
        (nm (if with-scope signal-full-name signal-name)))
    (bundle-sig-by-p bndl (lambda (s) (pr prefix (nm s))))))

;;; (bundle-sig-by-suffix bndl suffix #:with-scope #:ci) -> signal
;;; Find signal by name suffix. Returns #f if no signal found.
;;; #:with-scope - use name with scope (default: #f)
;;; #:ci         - case insensitive (default: #f)
(define* (bundle-sig-by-suffix bndl suffix #:key (with-scope #f) (ci #f))
  (let ((sf (if ci string-suffix-ci? string-suffix?))
        (nm (if with-scope signal-full-name signal-name)))
    (bundle-sig-by-p bndl (lambda (s) (sf suffix (nm s))))))

;;; (signal-id signal) -> number
;;; Get signal ID
(define signal-id first)

;;; (signal-scope signal) -> list
;;; Get signal scope as string list. CAR of list is the name of the deepest
;;; scope level. E.g. scope "top.mod0_impl.mod1_impl" represented as
;;; list '("mod1_impl" "mod0_impl" "top").
(define signal-scope second)

;;; (signal-scope-name signal) -> string
;;; Get signal scope name
(define (signal-scope-name signal)
  (string-concatenate
   (reverse
    (insert-between
     (signal-scope signal)
     "."))))

;;; (signal-full-name signal) -> string
;;; Get signal name with scope (e.g. "top.mod0_impl.mod1_impl.signal0")
(define (signal-full-name signal)
  (string-concatenate
   (reverse
    (insert-between
     (cons (signal-name signal)
           (signal-scope signal))
     "."))))

;;; (signal-name signal) -> string
;;; Get signal name without scope
(define signal-name third)

;;; (signal-width signal) -> number
;;; Get signal width
(define signal-width fourth)

;;; (signal-dimstr signal) -> string
;;; Get signal dimension (e.g. "[31:0]")
(define signal-dimstr fifth)

;;; (sample-time sample) -> number
;;; Get sample timestamp in timescale units
(define sample-time first)

;;; (sample-value sample id) -> logic
;;; Get current signal value by ID.
;;; The 'logic' type is a vector of bits, where ONE and ZERO are
;;; numbers 1 and 0, and the values of X and Z are represented
;;; as symbols 'x and 'z
(define (sample-value sample id)
  (let ((ss (assq id (cdr sample))))
    (and ss (cdr ss))))

;;; (logic->number v #:x-replace #:z-replace) -> number
;;; Convert logic vector to number. If no conversion is possible
;;; (vector has bits with state Z or X) then function returns #f.
;;; #:x-replace - value with which the bits with state X will be replaced (default: 'x)
;;; #:z-replace - value with which the bits with state Z will be replaced (default: 'z)
(define* (logic->number v #:key (x-replace 'x) (z-replace 'z))
  (let ((v (vector-map (lambda (i bit) (cond
                                   ((eq? bit 'x) x-replace)
                                   ((eq? bit 'z) z-replace)
                                   (else bit)))
                       v)))
    (and (vector-every number? v)
         (vector-fold (lambda (i sum bit) (+ sum (* bit (expt 2 i)))) 0 v))))

;;; (logic->hex v) -> string
;;; Convert logic vector to hex string
(define (logic->hex v)
  (let logic-list->hex ((l (vector->list v)))
    (let ((nibble
           (fold (lambda (bit exp sum)
                   (cond
                    ((not (number? bit)) bit)
                    ((not (number? sum)) sum)
                    (else (+ sum (* bit exp)))))
                 0 l '(1 2 4 8))))
      (let ((nibble-str
             (if (number? nibble)
                 (string (string-ref "0123456789abcdef" nibble))
                 (symbol->string nibble))))
        (if (<= (length l) 4)
            nibble-str
            (string-append (logic-list->hex (drop l 4))
                           nibble-str))))))

;;; (logic->bin v) -> string
;;; Convert logic vector to binary string
(define (logic->bin v)
  (list->string
   (reverse
    (map (lambda (x)
           (if (number? x)
               (if (zero? x) #\0 #\1)
               (if (eq? x 'x) #\x #\z)))
         (vector->list v)))))

;;; (number->logic x width) -> logic
;;; Convert number to logic verctor with width 'width'
(define (number->logic width x)
  (vcd-bin-list->logic
   width
   (reverse
    (let ((bin (string->list (number->string x 2))))
      (if (>= (length bin) width)
          (take bin width)
          bin)))))

;;; (logic-*? v) -> bool
;;; Logic vector predicates
;;; Returns #t if all bits of vector is 0
(define (logic-zero? v)
  (vector-every (cut eq? 0 <>) v))

;;; Returns #t if all bits of vector is 1
(define (logic-one? v)
  (vector-every (cut eq? 1 <>) v))

;;; Returns #t if any bit of vector is 'x
(define (logic-has-x? v)
  (vector-any (cut eq? 'x <>) v))

;;; Returns #t if any bit of vector is 'z
(define (logic-has-z? v)
  (vector-any (cut eq? 'z <>) v))

;;; (logic-valid? v) -> bool
;;; Check for the vector does not contain X and Z bits
(define (logic-valid? v)
  (vector-every number? v))

;;; (logic-concat v ...) -> vector
;;; Concatenate vectors (from least significant to most significant)
(define (logic-concat . args)
  (vector-concatenate args))
