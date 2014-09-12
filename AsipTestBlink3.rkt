#lang racket/base

(require "AsipMain.rkt")

;; A simple example with 3 LEDs in a loop
(define led1 11)
(define led2 12)
(define led3 13)

(define setup 
  (λ ()
    (open-asip)  
    ;; Setting 3 pins to OUTPUT_MODE
    (set-pin-mode led1 OUTPUT_MODE)
    (set-pin-mode led2 OUTPUT_MODE)
    (set-pin-mode led3 OUTPUT_MODE)
  
    ;; Turning the three pins off
    (map (λ (x) (digital-write x LOW)) (list led1 led2 led3))
    ) ;; end of lambda
  ) ;; end of setup

(define lightLoop 
  (λ ()
    (digital-write led1 HIGH)
    (sleep 1)
    (digital-write led1 LOW)
    (digital-write led2 HIGH)
    (sleep 1)
    (digital-write led2 LOW)
    (digital-write led3 HIGH)
    (sleep 1)
    (digital-write led3 LOW)
    (lightLoop)
    )
  )

(setup)
(lightLoop)