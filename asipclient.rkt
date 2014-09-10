#lang racket


; bit-operations
(require file/sha1)
; Needed for setting the terminal baudrate
(require racket/system)
; Needed for keeping track of the current pins
(require rnrs/bytevectors-6)

(define IO_SERVICE              "I")
(define PIN_MODE                "P")
(define DIGITAL_WRITE           "d")
(define ANALOG_WRITE            "a")
(define ANALOG_DATA_REQUEST     "A")
(define PORT_DATA               "d")
(define ANALOG_VALUE            "a")
(define PORT_MAPPING            "M")

(define EVENT_HANDLER           "@")
(define ERROR_MESSAGE_HEADER     "~")
(define DEBUG_MESSAGE_HEADER     "!")

(define BAUDRATE           57600)
(define MAX_NUM_DIGITAL_PINS  72) 
(define MAX_NUM_ANALOG_PINS   16) 

(define ANALOG-IO-PINS (make-vector MAX_NUM_ANALOG_PINS))
(define DIGITAL-IO-PINS (make-vector MAX_NUM_DIGITAL_PINS))


(define (analog-write! pin value)
  (vector-set! ANALOG-IO-PINS pin value))

(define (digital-write! pin value)
  (vector-set! DIGITAL-IO-PINS pin value))

(define (digital-read pin)
  (vector-ref DIGITAL-IO-PINS pin))

(define (analog-read pin)
  (vector-ref ANALOG-IO-PINS pin))


(define (process-input input)
  (let  ([char (substring input 0 1)])
    (cond
      [(equal? char EVENT_HANDLER)         (handle-input-event input)]
      [(equal? char ERROR_MESSAGE_HEADER)  (handle-input-event input)]
      [(equal? char DEBUG_MESSAGE_HEADER)  (handle-input-event input)])))

  
;(define (process-pin-mapping mapping)
  
  
(define (handle-input-event input)
  (let ([char (substring input 1 2)])
    (cond 
      [(equal? char IO_SERVICE)
       (let ([service (substring input 3 4)])
         (cond 
           [(equal? service PORT_DATA)
            (process-port-data input)]
           [(equal? service PORT_MAPPING)
            (process-pin-data input)]
           [(equal? service ANALOG_VALUE)
            (process-analog-values input)]))])))
    
                       

