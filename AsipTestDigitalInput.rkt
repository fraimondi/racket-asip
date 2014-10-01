#lang racket/base

(require "AsipMain.rkt")

;; A simple example reading an INPUT_PULLUP pin
;; and setting 3 pins to high accordingly

(define led1 11)
(define led2 12)
(define led3 13)

;; Analog pins on Arduino UNO go from 14 to 19 
;; when you need to set the pin mode. However,
;; you need to use the analog number when reading
;; the value with analog-read.
;; This will be fixed in the future.
(define inputPin 2) 

(define setup 
  (λ ()
    (open-asip)  
    ;; Setting 3 pins to OUTPUT_MODE
    (sleep 0.5)
    (set-pin-mode led1 OUTPUT_MODE)
    (set-pin-mode led2 OUTPUT_MODE)
    (set-pin-mode led3 OUTPUT_MODE)
    (set-pin-mode inputPin INPUT_PULLUP_MODE)
  
    ;; Turning the three pins off
    (map (λ (x) (digital-write x LOW)) (list led1 led2 led3))
    ) ;; end of lambda
  ) ;; end of setup

;; We store the value of current and old input pin
(define curInput null)
(define oldInput null)

(define lightLoop 
  (λ ()
    (set! curInput (digital-read inputPin))
    (cond ( (not (equal? curInput oldInput))
            ;; We only send messages when the state of the button changes
            ;; (otherwise we'd flood the serial port)
            (cond ( (equal? (digital-read inputPin) LOW)
                    ;; it's a pull-up, so LOW means pressed            
                    (digital-write led1 HIGH)
                    (digital-write led2 HIGH)
                    (digital-write led3 HIGH)
                    )
                  (else 
                   (digital-write led1 LOW)
                   (digital-write led2 LOW)
                   (digital-write led3 LOW)
                   )
                  )
            )
          )
    (set! oldInput curInput)
    (sleep 0.01)
    (lightLoop)
    ) ;; end of lambda
  )

(setup)
(lightLoop)