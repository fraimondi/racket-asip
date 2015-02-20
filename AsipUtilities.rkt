#lang racket/base

;; This file collects various utility functions taken from racket-firmata.rkt
;; and developed mainly by Nikos. The functions are used to detect the OS and to
;; find the serial port to which Arduino is attached. Only get-port is exported.

(require racket/system)
(require racket/list)
(require racket/string)
(require racket/match)

(provide get-port
	 detect-os)


;; Operating system: can be linux, mac, win
;; The os is needed to open the serial port in the appropriate way.
(define (detect-os)
  (define racket-os (system-type))
  (cond
    [(equal? racket-os 'windows) "win"]
    [else 
     (let ([machine (first (string-split (system-type 'machine)))])
       (match machine
         ["Darwin" "mac"]
         ["Linux" "linux"]
         [_ (raise "Unknown OS detected in firmata")] 
         ))]))

;; don't connect devices through USB that expose serial interfaces at the 
;; same time as the arduino
;; (such as mobile phones with tethering on)
;; OR anything on the serial ports
(define (find-port check-port prefix num)
  (if (<= 0 num 255)
      (let ([port (string-append prefix (number->string num))])
        (if (check-port port) port (find-port check-port prefix (+ 1 num))))
      #f))

(define (find-mac-port)
  (string-append "/dev/" (path->string (first (for/list ([f (directory-list "/dev")] #:when (regexp-match? "tty.usbmodem*|tty.usbserialtty.usbmodem*|tty.usbserial*" f))
     f))))
  )

(define (valid-com-port port)
  (system (string-append "mode " port " > NUL")))

(define (get-port)
  (define os (detect-os))
  (match os
    ("linux" (or (find-port file-exists? "/dev/ttyACM" 0)  (find-port file-exists? "/dev/ttyAMA" 0) (find-port file-exists? "/dev/ttyUSB" 0) ))
    ;; hopefully there is nothing attached above port 3... You may need to change this.
    ("win" (find-port valid-com-port "COM" 3))
    ("mac" (find-mac-port))
    (_ (raise "Don't know how to find port on this OS"))
    ))
