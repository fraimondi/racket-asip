#lang racket/base

(require "AsipMain.rkt")

;; A simple example reading a pot and setting the
;; blink delay according to the analog input

(define led1 11)
(define led2 12)
(define led3 13)

;; Analog pins on Arduino UNO go from 14 to 19 
;; when you need to set the pin mode. However,
;; you need to use the analog number when reading
;; the value with analog-read.
;; This will be fixed in the future.
(define potPin 2) 

(define setup 
  (λ ()
    (open-asip)  
    ;; Setting 3 pins to OUTPUT_MODE
    (set-pin-mode led1 OUTPUT_MODE)
    (set-pin-mode led2 OUTPUT_MODE)
    (set-pin-mode led3 OUTPUT_MODE)
    (set-pin-mode (+ potPin 14) ;; see note above
                  ANALOG_MODE)
  
    ;; Turning the three pins off
    (map (λ (x) (digital-write x LOW)) (list led1 led2 led3))
    
    ;; You need to set the reporting interval for analog pins (in ms)
    (set-autoreport 100)
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

(setup)
(lightLoop)
