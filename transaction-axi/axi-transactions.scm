#!/usr/bin/env -S guile -e "main" -s
!#

;; -*- geiser-scheme-implementation: guile -*-

(add-to-load-path (dirname (car (command-line))))

(import (gtkw-transaction-filter)
        (optargs))

(import (ice-9 textual-ports)           ; read port by lines
        (srfi srfi-1)                   ; lists
        (srfi srfi-9)                   ; Records
        (srfi srfi-11)                  ; let-values
        (srfi srfi-13)                  ; strings
        (srfi srfi-26)                  ; cut
        (srfi srfi-28)                  ; simple format
        (srfi srfi-43))                 ; vectors

;;;
;;; Formatted print with newline
;;;
(define (rprintln fmt . args)
  (display (apply format (cons fmt args)))
  (newline)
  (force-output))

(define (eprintln fmt . args)
  (display (apply format (cons fmt args)) (current-error-port))
  (newline (current-error-port)))

(define* (event time text #:optional (color #f))
  `(,time ,color ,text))

(define* (event->vcd ev)
  (format "#~a ~a~a"
          (first ev)
          (let ((color (second ev)))
            (if color (format "?~a?" color) ""))
          (third ev)))

(define (make-color-generator)
  (let ((colors '("blue4" "cyan4" "gold4" "khaki4")))
    (lambda ()
      (let ((next (car colors)))
        (set! colors (append (cdr colors) `(,next)))
        next))))

(define error-color "red4")

;;; (bus-ids bundle signals) -> list
;;; signals: list of symbols
;;; returns: list of pairs (signal-symbol . signal-id)
;;; If signal is not found in bundle, returns (signal-symbol . #f)
(define (bus-ids bundle signals)
  (map (lambda (signal-sym)
         (cons signal-sym
               (let ((sig (bundle-sig-by-suffix
                           bundle
                           (symbol->string signal-sym))))
                 (and sig (signal-id sig)))))
       signals))

(define (extract-bus-axi bundle)
  (let* ((signals
          '( ;; Write address
            awid awaddr awlen awsize awburst awlock awcache awprot awqos awvalid awready
            ;; Write data
            wid wdata wstrb wlast wvalid wready
            ;; Write response
            bid bresp bvalid output bready
            ;; Read address
            arid araddr arlen arsize arburst arlock arcache arprot arqos arvalid arready
            ;; Read data
            rid rdata rresp rlast rvalid rready))
         (ids (bus-ids bundle signals)))
    (lambda (sig-sym)
      (let ((symid (assq sig-sym ids)))
        (and symid (cdr symid))))))


;;; Returns: lambda (signal-symbol) -> signal-id
;;; Returns #f if any signal is missing
(define* (make-bus bundle signals #:key (allow-missing #f))
  (let ((ids (bus-ids bundle signals)))
    (and (or (every cdr ids) allow-missing)
         (lambda (sig-sym)
           (let ((symid (assq sig-sym ids)))
             (and symid (cdr symid)))))))

;;; Returns: lambda (sample) -> list of bus signal values
(define (make-bus-lgetter bus signals)
  (let ((ids (map bus signals)))
    (lambda (sample)
      (map (cut sample-value sample <>) ids))))

;;; Returns: lambda (sample) -> bus signal values
(define (make-bus-vgetter bus signals)
  (let ((get (make-bus-lgetter bus signals)))
    (lambda (sample)
      (apply values (get sample)))))

;;; Returns: lambda (sample) -> list of pairs of (id . value)
(define (make-bus-lgetter-id bus signals)
  (let ((ids (map bus signals)))
    (lambda (sample)
      (map
       (lambda (id sig) (cons sig (sample-value sample id)))
       ids signals))))

;;; Extract transaction from valid/ready channel
;;; Transaction: '(valid-time ready-time (signal-id . value) ...)
(define (channel-transactions bundle
                              clock-sym reset-sym
                              valid-sym ready-sym
                              . signals)
  (let* ((all-signals (cons* clock-sym reset-sym valid-sym ready-sym signals))
         (channel (make-bus bundle all-signals)))
    (and
     channel
     (let ((get-c-r-v-r (make-bus-vgetter channel `(,clock-sym
                                                    ,reset-sym
                                                    ,valid-sym
                                                    ,ready-sym)))
           (get-signals (make-bus-lgetter-id channel all-signals)))
       (let next ((samples (bundle-samples bundle))
                  (valid-time #f)
                  (transactions '()))
         (if (null? samples)
             ;; Complete
             (reverse transactions)
             ;; Check sample
             (let* ((sample (car samples))
                    (rest (cdr samples))
                    (time (sample-time sample)))
               (let-values (((clock reset valid ready)
                             (get-c-r-v-r sample)))
                 (if (and (logic-zero? reset)
                          (logic-one? clock))
                     (cond
                      ;; Complete transaction
                      ((and (logic-one? valid)
                            (logic-one? ready))
                       (next rest #f (cons (cons* (or valid-time time) time (get-signals sample))
                                           transactions)))

                      ;; Unexpected reset valid
                      ((and valid-time (not (logic-one? valid)))
                       (next rest #f (cons (list valid-time time #f) transactions)))

                      ;; Unknown ready signal
                      ((and (logic-one? valid)
                            (or (logic-has-x? ready)
                                (logic-has-z? ready)))
                       (next rest #f (cons (list (or valid-time time) time #f) transactions)))

                      ;; Valid start
                      ((and (not valid-time)
                            (logic-one? valid))
                       (next rest time transactions))

                      ;; Next sample
                      (else (next rest valid-time transactions)))

                     (next rest valid-time transactions))))))))))

;;; Extract AXI read channel events
(define (extract-axi-read bundle)
  (define (tr-sig-val tr sig-sym)
    (cdr (assq sig-sym (cddr tr))))

  (define (trs->events trs)
    (reverse
     (fold
      (lambda (tr events)
        (let ((valid-time (caar tr))
              (ready-time (cadar tr))
              (color (cadr tr))
              (text (caddr tr)))
          ;; (cons (event valid-time text color) events)
          (if (= valid-time ready-time)
              (cons (event ready-time text color) events)
              (cons* (event ready-time text color) (event valid-time "") events))))
      '() trs)))

  (define (logic-has-xz? l)
    (or (logic-has-x? l)
        (logic-has-z? l)))

  (let ((color (make-color-generator)))
    (let* ((ar-transaction
            (map (lambda (tr)
                   (cons tr
                         (if (third tr)
                             (let ((araddr (tr-sig-val tr 'araddr))
                                   (arid (tr-sig-val tr 'arid))
                                   (arlen (tr-sig-val tr 'arlen))
                                   (arsize (tr-sig-val tr 'arsize)))
                               (cond
                                ((logic-has-xz? araddr)
                                 (list error-color "BAD ADDRESS"))

                                ((logic-has-xz? arid)
                                 (list error-color "BAD ARID"))

                                ((logic-has-xz? arlen)
                                 (list error-color "BAD ARLEN"))

                                ((logic-has-xz? arsize)
                                 (list error-color "BAD ARSIZE"))

                                ((let ((araddr (logic->number araddr))
                                       (araddr-end (+ (logic->number araddr)
                                                      (* (+ (logic->number arlen) 1)
                                                         (expt 2 (logic->number arsize)))))
                                       (mask (lognot 4095)))
                                   (not (= (logand araddr mask)
                                           (logand araddr-end mask))))
                                 (list error-color "BURST BOUNDARY"))

                                (else
                                 (list (color) (format "~a:A:~a" (logic->hex arid) (logic->hex araddr))))))
                             (list error-color "BAD TRANSACTION"))))
                 (channel-transactions bundle 'clock 'reset 'arvalid 'arready 'arid 'arlen 'arsize 'araddr)))
           (r-transaction
            (map (lambda (tr)
                   (cons tr
                         (if (third tr)
                             (let* ((rid (tr-sig-val tr 'rid))
                                    (color
                                     (let ((ar-tr
                                            (find (lambda (tr) (equal? rid (tr-sig-val (car tr) 'arid)))
                                                  ar-transaction)))
                                       (if ar-tr (car (cdr ar-tr)) (color)))))
                               (list color
                                     (format "~a:D:~a"
                                             (logic->hex rid)
                                             (logic->hex (tr-sig-val tr 'rdata)))))
                             (list error-color "BAD TRANSACTION"))))
                 (channel-transactions bundle 'clock 'reset 'rvalid 'rready 'rid 'rdata))))

      (values (trs->events ar-transaction)
              (trs->events r-transaction)))))

;;;
;;; Main help
;;;
(define (print-help app-name)
  (define (-> . fmt) (apply eprintln fmt))
  (with-output-to-port (current-error-port)
    (lambda ()
      (-> "Usage: ~a [OPTION]... [REST]" app-name)
      (-> "GTKWave Transaction Filter")
      (-> "")
      (-> "Options:")
      (-> "  -a, --aoption VALUE   A-option with value.")
      (-> "  -b, --boption         B-option without value.")
      (-> "  -c, --coption VALUE   C-option can be appears multiple.")
      (-> "  -h, --help            Print this message and exit")
      (-> "")
      (-> "Source code and issue tracker: <https://github.com/punzik/>"))))

;;;
;;; Main
;;;
(define (main args)
  (let-values
      (((opts rest err)
        (parse-opts (cdr args)
                    '(("aoption" #\a) required)
                    '(("boption" #\b) none)
                    '(("coption" #\c) multiple)
                    '(("help" #\h) none))))
    (if err
        (begin
          (eprintln "Error or unknown option: '~a'\n" err)
          (print-help (car args)))

        (let ((help (option-get opts "help"))
              (opt-a (option-get opts "aoption"))
              (opt-b (option-get opts "boption"))
              (opt-c (option-get opts "coption")))

          (eprintln "A:'~a', B:'~a', C:'~a', REST:'~a'"
                    opt-a opt-b opt-c rest)

          ;; Process input VCD
          (let process ()
            (let ((bndl (parse-input-vcd-bundle)))
              (when bndl
                (let-values (((ar-events r-events)
                              (extract-axi-read bndl)))
                  (rprintln "$name AXI AR")
                  (for-each (lambda (event) (rprintln "~a" (event->vcd event))) ar-events)
                  (rprintln "$next")
                  (rprintln "$name AXI R")
                  (for-each (lambda (event) (rprintln "~a" (event->vcd event))) r-events)
                  (rprintln "$finish"))
                (process))))))))
