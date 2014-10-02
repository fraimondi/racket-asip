#lang racket/base

(require "AsipMain.rkt")

;; Racket IO test

;; digital pins
(define relay    2)
(define piezo    3)
(define servo    5)
(define led      6)
(define rgb_B    9)
(define rgb_G    10)
(define rgb_R    11)
(define button   12)
(define switch   13)
;; analog pins
(define pot        2)
(define hall       3)
(define thermister 4)
(define photocell  5)


(define setup 
  (λ ()
    (open-asip)  
    ;; Setting 3 pins to OUTPUT_MODE
    (sleep 0.5)
    (set-pin-mode led OUTPUT_MODE)
    (set-pin-mode rgb_R OUTPUT_MODE)
    (set-pin-mode rgb_B OUTPUT_MODE)
    (set-pin-mode rgb_G OUTPUT_MODE)
    (set-pin-mode piezo OUTPUT_MODE)
    (set-pin-mode button INPUT_MODE) ;; button uses pull-down
    (set-pin-mode switch INPUT_MODE) ;; switch uses pull-down
  
    ;; Turning the LED pins off
    (map (λ (x) (digital-write x LOW)) (list led rgb_R rgb_G rgb_B))
    ) ;; end of lambda
  ) ;; end of setup

;; We store the value of current and old input pin
(define curInput null)
(define oldInput null)
(define prevTime 0)
(define duration 0)
(define SWITCH_UP 0) ;; 0 FOR PULL-DOWN, 1 FOR PULL-UP


(define loop 
  (λ ()
    (set! curInput (digital-read button))
    (cond ( (not (equal? curInput oldInput))
            ;; We only send messages when the state of the button changes
            ;; (otherwise we'd flood the serial port)
            (cond ( (equal? curInput SWITCH_UP)                   
                    (set! duration (- (current-milliseconds) prevTime)) 
                    (set! prevTime (current-milliseconds))               
                    (printf "Down duration was ~a ~n" duration)  
                    )
                  (else 
                   (set! duration (- (current-milliseconds) prevTime))        
                   (set! prevTime (current-milliseconds))                  
                   (printf "Up duration was ~a ~n" duration)    
                   )
                  )
            (set! oldInput curInput)
            (printf "Pot value is ~a ~n" (analog-read pot))
            
            )
          )
   ;; (printf "Pot value is ~a ~n" (analog-read pot)) 
    (sleep 0.01)
    (loop)
    ) ;; end of lambda
  )

(setup)
(loop)