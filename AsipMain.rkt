#lang racket

;; ****************************
;; **** RACKET ASIP CLIENT ****
;; ****************************

;; Authors: Franco Raimondi and Micky Silas
;; For information regarding the ASIP protocol, please
;; see https://github.com/michaelmargolis/asip

;; The basic idea is similar to racket-firmata: we set up input and output 
;; streams attached to a serial port. The main thread (this one) deals with
;; writing, a separate thread handles the input. The input thread writes 
;; values to arrays of bit values, see below.

;; TODO [FR]: Here we may want to export something, commented for the moment.
;; (provide is-pin-set?,
;;          is-arduino-pin-set?,
;;          etc...
;; )


; bit-operations
(require file/sha1)
; Needed for setting the terminal baudrate
(require racket/system)
;; Needed to detect the port.
(require "AsipUtilities.rkt") 

;; These are the input and output ports and the thread handling input messages.
(define in                null) 
(define out               null)
(define read-thread       null)



;; *** BEGIN SECTION TO DEFINE SERIAL CONNECTION ***
;; This function creates in and out and sets the read thread.
;; Copied from racket-firmata.rkt
(define BAUDRATE           "57600")

(define (open-asip) 
  (define port-name (get-port))
  ;; We set the command line instruction to configure the serial port according to the OS;
  ;; we also configure the file name of the port to be opened (it is different in win)
  (define call-string null)
  (define filename null)
  (define os (detect-os))
  (cond 
   ( (equal? os "linux") 
     (set! call-string (string-append  "stty -F " port-name " cs8 " BAUDRATE " ignbrk -brkint -icrnl -imaxbel -opost -onlcr -isig -icanon -iexten -echo -echoe -echok -echoctl -echoke noflsh -ixon -crtscts"))
     (set! filename port-name))        
   ( (equal? os "mac") 
     (set! call-string (string-append  "stty -f " port-name " " BAUDRATE " cs8 cread clocal"))
     (set! filename port-name))
   ( (equal? os "win") 
     (set! call-string (string-append  "mode " port-name ": baud=" BAUDRATE " parity=N data=8 stop=1"))
     (set! filename (string-append "\\\\.\\" port-name)))
   ) ;; end of cond to set stty or mode string and filename.

  (cond ( (equal? os "win") 
	  (if (system call-string) ;; here we set the port
	      (begin
	       (let-values ([(in-port out-port) (open-input-output-file filename)])
			   (set! in in-port)
			   (set! out out-port)
			   (file-stream-buffer-mode out 'none)
			   )
	       (sleep 3)
	       (set! read-thread (thread (lambda ()  (read-hook)))) ;; we set the reading thread
	       #t)
	    (error "Failed to open the connection with " port-name " verify if your microcontroller is plugged in correctly"))            
	  )
        (else   
         (set! out (open-output-file port-name #:mode 'binary #:exists 'append))
         (set! in  (open-input-file  port-name #:mode 'binary))
         (file-stream-buffer-mode out 'none)
         (sleep 2)
         (if (system call-string) ;; here we set the port
             (begin
	      (sleep 1)
	      (set! read-thread (thread (lambda ()  (read-hook)))) ;; we set the reading thread
	      (printf "Success opening the serial port\n")
	      #t)
	   (error "Failed to open the connection with " port-name " verify if your microcontroller is plugged in correctly"))            
	 )               
	)
  ) ;; end of open-asip
;; *** END SECTION TO SET UP SERIAL CONNECTION ***


;; *** ASIP GENERIC CONSTANTS ***
(define EVENT_HANDLER           "@")
(define ERROR_MESSAGE_HEADER     "~")
(define DEBUG_MESSAGE_HEADER     "!")

;; Usually followed by a time interval in milliseconds to set autoevent status 
;; (time=0 means disable autoevents)
(define AUTOEVENT_MESSAGE        "A")

;; Used to re-map pins
(define REMAP_PIN_MESSAGE        "M")

;; A standard event (such as reporting distance, etc.)
(define ASIP_EVENT               "e")
;; END OF ASIP GENERIC CONSTANTS


;; *** DEFINITION OF ASIP CONSTANTS FOR I/O SERVICE ***
(define IO_SERVICE              "I")
(define PIN_MODE                "P")
(define DIGITAL_WRITE           "d")
(define ANALOG_WRITE            "a")
(define PORT_DATA               "d")
(define ANALOG_VALUE            "a")
(define PORT_MAPPING            "M")

;; Pin modes
(define UNKNOWN_MODE             0)
(define INPUT_MODE               1)
(define INPUT_PULLUP_MODE        2)
(define OUTPUT_MODE              3)
(define ANALOG_MODE              4)
(define PWM_MODE                 5)
(define RESERVED_MODE            6)
(define OTHER_SERVICE_MODE       7)

(define HIGH                     1)
(define LOW                      0)
;; *** END ASIP CONSTANTS FOR I/O SERVICE ***


;; *** We store digital and analog pins in fixed-length array.
;; FIXME: this could be improved in the future, building the arrays after
;; querying the board capabilities.
(define MAX_NUM_DIGITAL_PINS  72) 
(define MAX_NUM_ANALOG_PINS   16) 

(define ANALOG-IO-PINS (make-vector MAX_NUM_ANALOG_PINS))
(define DIGITAL-IO-PINS (make-vector MAX_NUM_DIGITAL_PINS))

;; We store port mapping in a hash table
(define PORT-MAPPING-TABLE (make-hash))


;; *** DEFINTIONS TO WRITE MESSAGES **TO** ARDUINO ***

;; Setting a pin to a certain mode (INPUT, OUTPUT, PWM, etc.)
(define (set-pin-mode pin mode)
  (printf "DEBUG -> Sending: ~a \n" (string-append IO_SERVICE "," PIN_MODE "," (number->string pin) "," 
			(number->string mode) ))
  (write-string (string-append IO_SERVICE "," PIN_MODE "," (number->string pin) "," 
			(number->string mode) "\n") out)
  #t
)

;; Writing a value (high or low) to a digital pin
(define (digital-write pin value)
  (write-string (string-append IO_SERVICE "," DIGITAL_WRITE "," (number->string pin) 
			"," (number->string value) "\n") out)
  #t
)

(define (analog-write pin value)
  (write-string (string-append IO_SERVICE "," ANALOG_WRITE "," (number->string pin) 
			"," (number->string value) "\n") out)
)

;; Set auto-reporting for I/O to a certain time in ms (needed for analog input pins)
(define (set-autoreport timems)
  (write-string (string-append IO_SERVICE "," AUTOEVENT_MESSAGE "," (number->string timems) "\n") out)
)

;; Utility functions for compatibility with old Firmata code
(define (set-arduino-pin! pin) (digital-write pin HIGH))
(define (clear-arduino-pin! pin) (digital-write pin LOW))
(define set-pin-mode! set-pin-mode)

;; *** END OF FUNCTIONS TO WRITE TO ARDUINO ***


;; *** FUNCTIONS TO HANDLE MESSAGES FROM ARDUINO AND TO READ VALUES ***
(define (digital-read pin)
  (vector-ref DIGITAL-IO-PINS pin))

(define (analog-read pin)
  (vector-ref ANALOG-IO-PINS pin))


(define (read-hook)
  (printf "Read thread started ...")
  (read-loop))

(define (read-loop)
  ;; We read a whole line (ASIP messages are terminated with a \n
  (process-input (read-line in))
  (read-loop))

(define (process-input input)
  (printf "DEBUG -> I have received: ~a \n" input)

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


;; Placeholders. TODO: code them!!
(define (process-port-data input) null)
(define (process-analog-values) null)
                  
;; Processing port mapping is the most complicated part of ASIP. The initial message tells how to
;; map port bits to pins. Example message FROM Arduino:
;; - @I,M,20,{4:1,4:2,4:4,4:8,4:10,4:20,4:40,4:80,2:1,2:2,2:4,2:8,2:10,2:20,3:1,3:2,3:4,3:8,3:10,3:20}
;; (this is the mapping of pins: pin 0 is mapped to the first bit of port
;; 4, pin 1 to the second bit of port 4, etc. MAPPING IS IN HEX! so 20 is
;; 32. Take the conjunction of this with the port and you get the pin
;; value)
(define (process-pin-data input)
  
  ;; First we take the string between brackets (str-index-of is defined below)
  (define ports (string-split (substring input 
                           (+ (str-index-of input "{") 1)
                           (str-index-of input "}") ) ",") )
    
  ;; We iterate over the list
  (for ([i (length ports)])
    ;; the pin is i; the port is the first element of the pair; the bit is the second element.
    ;; we attach #x in front to denote that it's a hex number
    (define port (string->number (string-append "#x" (first (string-split (list-ref ports i) ":")))))
    (define position (string->number (string-append "#x" 
                                                    (second (string-split (list-ref ports i) ":")))))
  
    (cond ( (hash-has-key? PORT-MAPPING-TABLE port)
            ;; there is already a key for this port. Let's get it and
            ;; add the new entry position -> pin
            (hash-set! (hash-ref PORT-MAPPING-TABLE port) position i)
            )
          (else
           ;; we create a new hash table for position -> pin and we add it
           ;; as a value for the key "port"
           (hash-set! PORT-MAPPING-TABLE port (make-hash (list (cons position  i))))
           )
          )
  )
  (printf "DEBUG -> PORT-MAPPING-TABLE is ~a \n" PORT-MAPPING-TABLE)
)

;; Find the index of something in a list (I couldn't find a function for this!)
;; Copied from stackoverflow and slightly modified. str and x need to be string
;; and we only look for the first character of x. Really horrible code...
(define (str-index-of str x)
  (define l (string->list str))
  (for/or ([y l] [i (in-naturals)] #:when (equal? (string-ref x 0) y)) i))

;; - @I,a,3,{0:320,1:340,2:329}
;; (this are analog pins: 3 of them are set, analog pins 0, 1 and 2 in
;; this case, and their values are in bracket).

