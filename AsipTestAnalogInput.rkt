#lang racket/base

(require "AsipMain.rkt")

;; A simple example reading a pot and setting the
;; blink delay for 3 LEDS according to the analog input

(define led1 11)
(define led2 12)
(define led3 13)

;; Remember that this is the analog pin number!
(define potPin 0) 

(define prepareTheBoard 
  (λ ()
    (open-asip)  
    ;; Setting 3 pins to OUTPUT_MODE
    (set-pin-mode led1 OUTPUT_MODE)
    (set-pin-mode led2 OUTPUT_MODE)
    (set-pin-mode led3 OUTPUT_MODE)

    ;; Analog pins on Arduino UNO do not need to
    ;; be initialised in the current version of ASIP.
    ;; Their value is reported every 50 ms
  
    ;; Turning the three pins off
    (map (λ (x) (digital-write x LOW)) (list led1 led2 led3))
    
    ) ;; end of lambda
  ) ;; end of setup

(define lightLoop 
  (λ ()
    (digital-write led1 HIGH)
    (sleep (+ 0.1 (/ (analog-read 2) 1000)))
    (digital-write led1 LOW)
    (digital-write led2 HIGH)
    (sleep (+ 0.1 (/ (analog-read 2) 1000)))
    (digital-write led2 LOW)
    (digital-write led3 HIGH)
    (sleep (+ 0.1 (/ (analog-read 2) 1000)))
    (digital-write led3 LOW)
    (lightLoop)
    )
  )

(prepareTheBoard)
(lightLoop)
