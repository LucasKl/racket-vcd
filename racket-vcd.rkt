#lang racket

(define (error-at in msg)
  (define-values (line column next) (port-next-location in))
  (error (format "~a at ~a:~a" msg line column)))


(struct signal (name short type width [data #:mutable]) #:transparent)


(struct trace (name file timescale scopes signals))

;--------------------------------------------------------------------------------

(define (read-token in [buf ""] [fail-on-eof #t])
  (define c (read-string 1 in))
  (if (eof-object? c)
      (if fail-on-eof (error-at in "No EOF expected") buf)
      (case c
        [(" " "\n" "\t") (if (string=? buf "") (read-token in buf) buf)]
        [else (read-token in (string-append buf c) fail-on-eof)])))

(define (read-block in)
  "read one block of vcd data"
  (define (read-block-content tokens)
    (define token (read-token in))
    (case token
      [("$end") tokens]
      [else (read-block-content (append tokens (list token)))]))

  (define section (read-token in))
  (list (substring section 1) (read-block-content '())))

(define (read-header in [blocks '()])
  (define block (read-block in))
  (case (car block)
    [("enddefinitions") blocks]
    [else (read-header in (append blocks (list block)))]))

(define (get-signals header [scope '()] [signals '()])
  (match header
    ['() signals]
    [(cons (cons block-type (cons block-data '())) header-rest)
     (case block-type
       [("var")
        (define name (string->symbol (string-append (string-join scope "."  #:after-last ".") (fourth block-data))))
        (define short (third block-data))
        (define type (first block-data))
        (define width (second block-data))
        (define new-signal (signal name short type width '()))
        (get-signals (cdr header) scope (append signals (list new-signal)))]
       [("scope") (get-signals header-rest (append scope (list (second block-data))) signals)]
       [("upscope") (get-signals header-rest (drop-right scope 1) signals)]
       [else (get-signals header-rest scope signals)])]))

(define (make-signal-map signals)
  (for/hash ([signal signals])
    (values (signal-short signal) signal)))

;----------------------------------------------------------------------------------------------------
; Reading Signal Changes and Time
;----------------------------------------------------------------------------------------------------

(define (read-timestamp in)
  (define timestamp (read-token in "" #f))
  (case (string-ref timestamp 0)
    [(#\#) (define time (string->number (substring timestamp 1)))
           (unless time (error-at in "Expected timestamp1"))
           time]
    [else (error-at in "Expected timestamp2")]))

(define (read-delta-block in [changes '()])
  (define (read-signal-change in)
    (case (read-char in)
      [(#\0) (cons 0 (read-token in))]
      [(#\1) (cons 1 (read-token in))]
      [(#\x) (cons #\x (read-token in))]
      [(#\b) (cons (read-token in) (read-token in))]
      [else (error-at in "Unsopported signal radix")]))
  (define next (peek-char in))
  (cond
    [(eof-object? next) changes]
    [(member next '(#\$ #\#)) changes]
    [else
     (define change (read-signal-change in))
     (read-delta-block in (append changes (list change)))]))

(define (read-dumpvars in)
  (cond
    [(string=? (read-token in) "$dumpvars")
     (define changes (read-delta-block in))
     (unless (string=? (read-token in) "$end") (error-at in "Expected $end"))
     changes]
    [else (error-at in "Expected $dumpvars command")]))


;----------------------------------------------------------------------------------------------------
; Updating Signal Values with Deltas
;----------------------------------------------------------------------------------------------------

(define (update-signal-values old deltas)
  (for/fold ([new old])
            ([delta deltas])
    (define key (cdr delta))
    (define sig (hash-ref old key))
    (set-signal-data! sig (car delta))
    (hash-set new (cdr delta) sig)))

(define (process-vcd in callback)
  (define signals (make-signal-map (get-signals (read-header in))))

  (define (loop)
    (unless (eof-object? (peek-char in))
      (define time (read-timestamp in))
      (define deltas (case (peek-char in)
                       [(#\$) (read-dumpvars in)]
                       [else (read-delta-block in)]))
      (update-signal-values signals deltas)
      (callback time signals)
      (loop)))
  (loop))
          
  
;----------------------------------------------------------------------------------------------------
; Testing
;----------------------------------------------------------------------------------------------------
(define in (open-input-file "tests/test.vcd"))
(port-count-lines! in)

(define (print-vcd time data)
  (printf "~a: " time)
  (for ([v (hash-values data)])
    (printf "~a " (signal-data v)))
  (displayln ""))

(process-vcd in print-vcd)

(close-input-port in)