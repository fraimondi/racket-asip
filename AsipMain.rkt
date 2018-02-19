#lang racket

;; ****************************
;; **** RACKET ASIP CLIENT ****
;; ****************************

;; Authors: Franco Raimondi
;; For information regarding the ASIP protocol, please
;; see https://bitbucket.com/mdxmase/asip

;; The basic idea is similar to racket-firmata: we set up input and output
;; streams attached to a serial port. The main thread (this one) deals with
;; writing, a separate thread handles the input. The input thread writes
;; values to arrays of bit values, see below.

;; How to use it: you can use any of the functions and constants exported
;; below in the (provide... ) block.

(provide open-asip
         close-asip
         set-pin-mode
         digital-write
         analog-write
         digital-read
         analog-read
         set-autoreport
         set-arduino-pin! ;; shorthand for digital-write, for backward compatibility
         clear-arduino-pin! ;; shorthand for digital-write, for backward compatibility
         set-pin-mode! ;; synonym of set-pin-mode, for backward compatibility

         setServo ;; a function to move a servo (TODO: check that the board supports servos!)
         playTone ;; a function to play a tone (TODO: check that the board supports tones!)

         ;; pin modes
         UNKNOWN_MODE
         INPUT_MODE
         INPUT_PULLUP_MODE
         OUTPUT_MODE
         ANALOG_MODE
         PWM_MODE
         RESERVED_MODE
         OTHER_SERVICE_MODE

         ;; Arduino HIGH and LOW (1 and 0)
         HIGH
         LOW

         ;; Myrtle-specific functions
         w1-stopMotor
         w2-stopMotor
         stopMotors
         setMotor
         setMotors
         readCount
         getCount
         resetCount
         getIR
         leftBump?
         rightBump?
         enableIR
         enableBumpers
         enableCounters
         setLCDMessage
         clearLCD
         enableDistance
         getDistance
         )

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
(define SLEEP_SERIAL 0.03) ;; a constant sleep at the end of each write

(define (open-asip [port "NONE"])
  (define port-name port)
  (cond ( (equal? port "NONE")
          (set! port-name (get-port))
          )
        )
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
     (set! call-string (string-append  "mode " port-name ": baud=" BAUDRATE " parity=N data=8 stop=1 dtr=on"))
     (set! filename (string-append "\\\\?\\" port-name)))
   ) ;; end of cond to set stty or mode string and filename.

  (cond ( (equal? os "win")
	  (if (system call-string) ;; here we set the port
	      (begin
	       (let-values ([(in-port out-port) (open-input-output-file filename #:mode 'binary #:exists 'append)])
			   (set! in in-port)
			   (set! out out-port)
			   (file-stream-buffer-mode out 'none)
                           ;(file-stream-buffer-mode in 'line)
			   )
	       (sleep 2)
               ;; This is here for a reason. But I won't tell you. Ah ah ah!!!
               ;;;(set-pin-mode 14 ANALOG_MODE)
               ;;;(sleep 0.1)
               ;;;(set-autoreport 50)
               ;;;(sleep 0.1)
	       (set! read-thread (thread (lambda ()  (read-hook)))) ;; we set the reading thread
	       #t)
	    (error "Failed to open the connection with " port-name " verify if your microcontroller is plugged in correctly"))
	  )
        (else
         (sleep 2)
         (if (system call-string) ;; here we set the port
             (begin
	      (sleep 1)
				(set! out (open-output-file port-name #:mode 'binary #:exists 'append))
				(set! in  (open-input-file  port-name #:mode 'binary))
				(file-stream-buffer-mode out 'none)
	      (set! read-thread (thread (lambda ()  (read-hook)))) ;; we set the reading thread
	      (printf "Success opening the serial port\n")
	      #t)
	   (error "Failed to open the connection with " port-name " verify if your microcontroller is plugged in correctly"))
	 )
	)
  (sleep 1)
  ;; We request port mapping so that we know that we have it later on
  (request-port-mapping)
  (sleep 0.2)
  (request-port-mapping)
  (sleep 0.2)
  ;(define orig-exception (uncaught-exception-handler))
  ;(uncaught-exception-handler (λ (e) (close-asip) (orig-exception e)))
  ) ;; end of open-asip

(define (close-asip)
  (when (not (null? read-thread))
    (printf "Killing thread .... \n")
    (kill-thread read-thread)
    (set! read-thread null)
    (printf "Closing input .... \n")
    (close-input-port in)
    (printf "Flushing output .... \n")
    (flush-output out)
    (printf "Closing output .... \n")
    (close-output-port out)
    (set! in null)
    (set! out null)
    (printf "Serial connection closed .... \n"))
  ) ;; end of close-asip
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


;; *** OTHER SERVICES provided by https://bitbucket.org/mdxmase/asip-additional-services ***
;; *** DEFINITION OF ASIP CONSTANTS FOR SERVO SERVICE ***
(define SERVO_SERVICE               "S")
(define SET_SERVO_ANGLE             "W")

;; *** DEFINITION OF ASIP CONSTANTS FOR TONE SERVICE ***
(define TONE_SERVICE               "T")
(define PLAY_TONE                  "P")


;; *** DEFINITION OF ASIP CONSTANTS FOR PIXEL SERVICE ***
(define PIXEL_SERVICE               "P")
(define SET_PIXELS                  "P")
;; FIXME: Implement these!

;; OTHER SERVICES FOR THE MIRTO ROBOT, see https://bitbucket.org/mdxmase/asip-mirtle2015
;; *** DEFINITION OF ASIP CONSTANTS FOR MOTOR (HUB-EE WHEELS) SERVICE ***
(define MOTOR_SERVICE           "M")
(define SET_MOTOR_SPEED         "m")

;; *** DEFINITION OF ASIP CONSTANTS FOR ENCODERS (HUB-EE WHEELS) SERVICE ***
(define ENCODER_SERVICE         "M")
;; Remember: use ASIP_EVENT and AUTOEVENT_MESSAGE
;; to read and configure this service

;; *** DEFINITION OF ASIP CONSTANTS FOR IR SERVICE
(define IR_SERVICE              "R")
;; Remember: use ASIP_EVENT and AUTOEVENT_MESSAGE
;; to read and configure this service

;; *** DEFINITION OF ASIP CONSTANTS FOR BUMPER SERVICE
(define BUMPER_SERVICE              "B")
;; Remember: use ASIP_EVENT and AUTOEVENT_MESSAGE
;; to read and configure this service

;; *** DEFINITION OF ASIP CONSTANTS FOR LCD SERVICE
(define LCD_SERVICE                 "L")
(define LCD_WRITE                   "W")
(define LCD_CLEAR                   "C")


;; *** DEFINITION OF ASIP CONSTANTS FOR DISTANCE SERVICE
(define DISTANCE_SERVICE              "D")
;; Remember: use ASIP_EVENT and AUTOEVENT_MESSAGE
;; to read and configure this service

;; *** We store digital and analog pins in fixed-length array.
;; FIXME: this could be improved in the future, building the arrays after
;; querying the board capabilities.
(define MAX_NUM_DIGITAL_PINS  72)
(define MAX_NUM_ANALOG_PINS   16)

(define ANALOG-IO-PINS (make-vector MAX_NUM_ANALOG_PINS))
(define DIGITAL-IO-PINS (make-vector MAX_NUM_DIGITAL_PINS))

;; We store port mapping in a hash table
(define PORT-MAPPING-TABLE (make-hash))

;; To store the value of encoders
(define MOTOR-COUNT (make-vector 2))

;; To store the value of IR
(define IR-VALUES (make-vector 3))

;; To store the value of Bump sensors
(define BUMP-VALUES (make-vector 2 #f))

;; To store the value of distance sensors (we assume up to 16 sensors)
(define DISTANCE-VALUES (make-vector 16 0))

;; *** DEFINTIONS TO WRITE MESSAGES **TO** ARDUINO ***

;; Setting a pin to a certain mode (INPUT, OUTPUT, PWM, etc.)
(define (set-pin-mode pin mode)
;;  (printf "DEBUG -> Sending: ~a \n" (string-append IO_SERVICE "," PIN_MODE "," (number->string pin) ","
;;			(number->string mode) ))
  (write-string (string-append IO_SERVICE "," PIN_MODE "," (number->string pin) ","
			(number->string mode) "\n") out)
  (flush-output out)
  (sleep SLEEP_SERIAL)
  #t
)

;; Writing a value (high or low) to a digital pin
(define (digital-write pin value)
  (write-bytes (string->bytes/locale (string-append IO_SERVICE "," DIGITAL_WRITE "," (number->string pin)
			"," (number->string value) "\n")) out)
  (flush-output out)
  #t
)

(define (analog-write pin value)
  (write-string (string-append IO_SERVICE "," ANALOG_WRITE "," (number->string pin)
			"," (number->string value) "\n") out)
  (flush-output out)
  (sleep SLEEP_SERIAL)
)

;; Set auto-reporting for I/O to a certain time in ms (needed for analog input pins)
(define (set-autoreport timems)
  (write-string (string-append IO_SERVICE "," AUTOEVENT_MESSAGE "," (number->string timems) "\n") out)
  (flush-output out)
  (sleep SLEEP_SERIAL)
)

;; Utility functions for compatibility with old Firmata code
(define (set-arduino-pin! pin) (digital-write pin HIGH))
(define (clear-arduino-pin! pin) (digital-write pin LOW))
(define set-pin-mode! set-pin-mode)

;; Just request the port mapping
(define request-port-mapping (λ ()
                               (write-string (string-append IO_SERVICE "," PORT_MAPPING "\n") out)
                               (flush-output out)
  (sleep SLEEP_SERIAL)
                               ) )

;; Messages for Mirtle services

;; Stopping the motor with utility functions
(define w1-stopMotor
  (λ () (setMotor 0 0))
  )
(define w2-stopMotor
  (λ () (setMotor 1 0))
  )
(define stopMotors
  (λ ()
    (setMotor 0 0)
    (setMotor 1 0)
    )
  )

;; Setting both motors at the same time
(define setMotors
  (λ (s1 s2)
    (setMotor 0 s1)
    (sleep SLEEP_SERIAL)
    (setMotor 1 s2)
    )
  )

;; The only useful one for motors is this one
;; that sends the message

(define setMotor
  (λ (m s)
    (write-string (string-append MOTOR_SERVICE ","
                                 SET_MOTOR_SPEED ","
                                 (number->string m ) ","
                                 (number->string (round (* (/ s 255) 100)) )
                                 "\n")
                  out)
    (flush-output out)
    (sleep SLEEP_SERIAL)
    )
  )

(define enableIR
  (λ (interval)
    (write-string (string-append IR_SERVICE ","
                                 AUTOEVENT_MESSAGE ","
                                 (number->string interval) 
                                 "\n")
                  out)
    (flush-output out)
    (sleep SLEEP_SERIAL)
    )
  )

(define enableBumpers
  (λ (interval)
    (write-string (string-append BUMPER_SERVICE ","
                                 AUTOEVENT_MESSAGE ","
                                 (number->string interval)
                                 "\n")
                  out)
    (flush-output out)
    (sleep SLEEP_SERIAL)
    )
  )

(define enableCounters
  (λ (interval)
    (write-string (string-append ENCODER_SERVICE ","
                                 AUTOEVENT_MESSAGE ","
                                 "1";; notice: frequency cannot be modified in 2018 version
                                 "\n")
                  out)
    (flush-output out)
    (sleep SLEEP_SERIAL)
    )
  )

(define setServo
  (λ (m s)
    (write-string (string-append SERVO_SERVICE ","
                                 SET_SERVO_ANGLE ","
                                 (number->string m) ","
                                 (number->string s)
                                 "\n")
                  out)
    (flush-output out)
    (sleep SLEEP_SERIAL)
    )
  )

;; This one is blocking (see sleep below)
(define playTone
  (λ (t d) ;; t in Hz, d in ms is the duration
    (write-string (string-append TONE_SERVICE ","
                                 PLAY_TONE ","
                                 (number->string t) ","
                                 (number->string d)
                                 "\n")
                  out)
    (flush-output out)
    (sleep (/ d 1000))
    (sleep 0.01)
    )
  )

(define setLCDMessage
  (λ (m r) ;; m = message to be displayed; r = row (max 4 rows)
    (write-string (string-append LCD_SERVICE ","
                                 LCD_WRITE ","
                                 (number->string r) ","
                                 m
                                 "\n")
                  out)
    (flush-output out)
    (sleep SLEEP_SERIAL)
    )
  )

(define clearLCD
  (λ ()
    (write-string (string-append LCD_SERVICE ","
                                 LCD_CLEAR
                                 "\n"
                                 )
                  out)
    (flush-output out)
    (sleep SLEEP_SERIAL)
    )
  )


(define enableDistance
  (λ (interval)
    (write-string (string-append DISTANCE_SERVICE ","
                                 AUTOEVENT_MESSAGE ","
                                 (number->string interval)
                                 "\n")
                  out)
    (flush-output out)
    (sleep SLEEP_SERIAL)
    )
  )

;; *** END OF FUNCTIONS TO WRITE TO ARDUINO ***


;; *** FUNCTIONS TO HANDLE MESSAGES FROM ARDUINO AND TO READ VALUES ***

;; Just report the value of a digital pin stored in the vector
(define (digital-read pin)
  (vector-ref DIGITAL-IO-PINS pin))

;; Just report the value of an analog pin stored in the vector
(define (analog-read pin)
  (vector-ref ANALOG-IO-PINS pin))


;; This is the function that creates a read loop on input
(define (read-hook)
  (printf "Read thread started ...")
  (read-loop))

;; The infinite loop: it keeps waiting for lines on input and
;; then calls process-input
(define (read-loop)
  ;; We read a whole line (ASIP messages are terminated with a \n
  ;;(process-input (our-read-line in))

  ;; Franco: usual old problem on win machines?
  (define incomingData (read-line in))
  (cond ( (not (eof-object? incomingData))
          (with-handlers ([(lambda (v) #t) (lambda (v) #t)])
            (process-input incomingData)
            )
          )
        )
  (read-loop))

(define our-read-line (λ (in)
                        (define gohere
                          (λ (curmsg)
                            (define curchar (read-byte in))
                            (cond
                              ((or (eof-object? curchar) (equal? curchar 10)) (bytes->string/locale curmsg))
                              (else (gohere (bytes-append curmsg (bytes curchar))))
                              )
                            )
                          )
                        (gohere (bytes))
                        )
  )

(define (process-input input)
  ;;(printf "DEBUG -> I have received: ~a \n" input)
  (cond ( (> (string-length input) 1)
          (let  ([char (substring input 0 1)])
            (cond
              [(equal? char EVENT_HANDLER)         (handle-input-event input)]
              [(equal? char ERROR_MESSAGE_HEADER)  (handle-error-event input)]
              [(equal? char DEBUG_MESSAGE_HEADER)  (handle-debug-event input)])
            ;; FIXME: add error handling for unknown messages?
            ;; FIXME: handle different messages in different ways
            )
          )
        )

  )

(define (handle-error-event input)
  (printf "DEBUG -> I have received the following error: ~a \n" input)
  )

(define (handle-debug-event input)
  (printf "DEBUG -> I have received the following debug message ~a \n" input)
  )


(define (handle-input-event input)
  ;; We look at the first character and dispatch the
  ;; input to the appropriate function
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
            (process-analog-values input)]))]

      [(equal? char MOTOR_SERVICE)
       (process-motor-service input)]

      [(equal? char ENCODER_SERVICE)
       (process-encoder-service input)]

      [(equal? char IR_SERVICE)
       (process-ir-service input)
       ]

      [(equal? char BUMPER_SERVICE)
       (process-bump-service input)]

      [(equal? char DISTANCE_SERVICE)
       (process-distance-service input)]

;;       (define MOTOR_SERVICE           "M")
;; (define SET_MOTOR_SPEED         "m")

;; *** DEFINITION OF ASIP CONSTANTS FOR ENCODERS (HUB-EE WHEELS) SERVICE ***
;; (define ENCODER_SERVICE         "E")
;; Remember: use ASIP_EVENT and AUTOEVENT_MESSAGE
;; to read and configure this service

;; *** DEFINITION OF ASIP CONSTANTS FOR IR SERVICE
;;(define IR_SERVICE              "R")
;; Remember: use ASIP_EVENT and AUTOEVENT_MESSAGE
;; to read and configure this service

;; *** DEFINITION OF ASIP CONSTANTS FOR BUMPER SERVICE
;;(define BUMPER_SERVICE              "B")


      )))


;; Processing port mapping is the most complicated part of ASIP. The initial message tells how to
;; map port bits to pins. Example message FROM Arduino:
;; - @I,M,20,{4:1,4:2,4:4,4:8,4:10,4:20,4:40,4:80,2:1,2:2,2:4,2:8,2:10,2:20,3:1,3:2,3:4,3:8,3:10,3:20}
;; (this is the mapping of pins: pin 0 is mapped to the first bit of port
;; 4, pin 1 to the second bit of port 4, etc. MAPPING IS IN HEX! so 20 is
;; 32. Take the conjunction of this with the port and you get the pin
;; value)
;; Here we set up this initial mapping. We use the hash map PORT-MAPPING-TABLE. This table
;; maps a port number to another hash map. In this second hash map we map positions in the port
;; (expressed as powers of 2, so 1 means position 0, 16 means position 5, etc.)
;; Overall, this looks something like (see message above)
;; PORT=4 ---> (POSITION=1 ---> PIN=0)
;;             (POSITION=2 ---> PIN=1)
;;             ...
;; PORT=2 ---> (POSITION=1 ---> PIN=8)
;;             ...
;;             (POSITION=16 ---> PIN=12)
;;             ...
;; and so on.
(define (process-pin-data input)

  ;; First we take the string between brackets (str-index-of is defined below)
  (define ports (string-split (substring input
                           (+ (str-index-of input "{") 1)
                           (str-index-of input "}") ) ",") )

  ;; We iterate over the list
  (for ([i (length ports)])
    ;; the pin is i; the port is the first element of the pair; the bit is the second element.
    ;; we attach #x in front to denote that it's a hex number
    (define port (string->number (first (string-split (list-ref ports i) ":"))))
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
;;  (printf "DEBUG -> PORT-MAPPING-TABLE is ~a \n" PORT-MAPPING-TABLE)
) ;; End of process-pin-data


;; If a digital pin set to input mode changes, the board notifies
;; us with a message on the input stream. The message has the form:
;; @I,d,4,AB
;; where 4 is the port number and AB is a hex number with the value
;; of the pins in that port. For instance, AB in binary is  10101011
;; meaning that the pin corresponding to position 1 in port 4 has value 1,
;; pin corresponding to position 2 in port 4 has value 0, etc..
(define (process-port-data input)
  ;; FIXME: we should really check that PORT-MAPPING-TABLE exists before doing
  ;; anything here...
  (define port (string->number (string-append "#x" (substring input 5 6))))
  (define bitmask (string->number (string-append "#x" (substring input 7))))
  ;;(printf "DEBUG -> The values for port ~a are ~a \n" port bitmask)

  ;; Now we need to convert the value of a port back to pin values.
  ;; Let's retrieve the mapping for this port, making sure we have this port:
  (cond ( (hash-has-key? PORT-MAPPING-TABLE port)
          (define singlePortMap (hash-ref PORT-MAPPING-TABLE port))

          ;; Easy: we take the bitwise-and of the port with the position;
          ;; if it is not zero we set pin to HIGH, and to LOW otherwise
          (hash-for-each singlePortMap
                         (lambda (x y)
                           (vector-set! DIGITAL-IO-PINS y
                                        (cond
                                          ( (equal? (bitwise-and bitmask x) 0) LOW)
                                          (else HIGH)
                                          )
                                        )
                           )
                         )
          )
        )
  ;;(printf "DEBUG -> The current pin values are: ~a" DIGITAL-IO-PINS)
  ) ;; end of process-port-data


;; A message from Arduino had this shape: @I,a,3,{0:320,1:340,2:329}
;; (this are analog pins: 3 of them are set, analog pins 0, 1 and 2 in
;; this case, and their values are in brackets).
;; REMEMBER to set auto-reporting with set-autoreport, or it won't work :-)
(define (process-analog-values input)
  ;; First we take the string between brackets (str-index-of is defined below)
  ;; and split to obtain a list of the form "0:320" "1:340" etc.

  (define analogValues (string-split (substring input
                                                (+ (str-index-of input "{") 1)
                                                (str-index-of input "}") ) ",") )

  ;; we then map a function to set the analog pins.
  (map (λ (x) (vector-set! ANALOG-IO-PINS
                           (string->number (first (string-split x ":")))  ;; the pin
                           (string->number (second (string-split x ":"))) ;; the value
                           ) ) analogValues ;; end of lambda
                               ) ;; end of map

  ;;(printf "The current value of analog pins is: ~a \n" ANALOG-IO-PINS)

  ) ;; end process-analog-values



;; Find the index of something in a list (I couldn't find a function for this!)
;; Copied from stackoverflow and slightly modified. str and x need to be string
;; and we only look for the first character of x. Really horrible code...
(define (str-index-of str x)
  (define l (string->list str))
  (for/or ([y l] [i (in-naturals)] #:when (equal? (string-ref x 0) y)) i))


;; *** FUNCTIONS RELATED TO INCOMING MESSAGES FOR MIRTO SERVICES

(define process-motor-service
  (λ (input)
    (process-encoder-service input)
    )
  )

;; A message is something like "“@E,e,2,{3000:110,3100:120}"
;; where 3000 is the pulse and 110 is the counter (since last
;; message). Here I only store the counter.
(define process-encoder-service
  (λ (input)
  ;;(printf "DEBUG -> I have received: ~a \n" input)
    (let ([service (substring input 3 4)])
      (cond
        [(equal? service ASIP_EVENT)
         (define encoderValues (string-split (substring input
                                                        (+ (str-index-of input "{") 1)

                                                        (str-index-of input "}") ) ",") )
         ;; The increment of the first counter
         (define delta0
           (string->number (list-ref (string-split (list-ref encoderValues 0) ":") 0))
           )

         ;; The increment of the second counter
         (define delta1
           (string->number (list-ref (string-split (list-ref encoderValues 1) ":") 0))
           )

         (vector-set! MOTOR-COUNT 0
                      (+ (vector-ref MOTOR-COUNT 0)
                         delta0 )
                      )

         (vector-set! MOTOR-COUNT 1
                      (+ (vector-ref MOTOR-COUNT 1)
                         delta1 )
                      )
         ]
        [else (printf "DEBUG: unkown message for encoder service: ~a \n" input)]
        )
      )
    )
) ;; end process-encode-service

;; An IR message is of the form @R,e,3,{100,200,300}
(define process-ir-service
  (λ (input)
  ;;(printf "DEBUG -> I have received: ~a \n" input)
    (let ([service (substring input 3 4)])
      (cond
        [(equal? service ASIP_EVENT)
         (define irValues (string-split (substring input
                                                        (+ (str-index-of input "{") 1)

                                                        (str-index-of input "}") ) ",") )
         (set! IR-VALUES (list->vector
                          (map (λ (x) (string->number x)) irValues)
                          )
               )
         ]
        [else (printf "DEBUG: unkown message for IR service: ~a \n" input)]
        )
      )
    )
) ;; end process-ir-service

;; A bumper message is of the form @B,e,2,{0,1}
(define process-bump-service
  (λ (input)
  ;;(printf "DEBUG -> I have received: ~a \n" input)
    (let ([service (substring input 3 4)])
      (cond
        [(equal? service ASIP_EVENT)
         (define bumperValues (string-split (substring input
                                                        (+ (str-index-of input "{") 1)

                                                        (str-index-of input "}") ) ",") )
         (set! BUMP-VALUES (list->vector
                          (map (λ (x)
                                 (cond [(equal? x "1") #f]
                                       [else #t]))
                                 bumperValues)
                          )
               )
         ]
        [else (printf "DEBUG: unkown message for Bumper service: ~a \n" input)]
        )
      )
    )
  ) ;; end of process-bump-service

;; A bumper message is of the form @B,e,2,{0,1}
(define process-distance-service
  (λ (input)
  ;;(printf "DEBUG -> I have received: ~a \n" input)
    (let ([service (substring input 3 4)])
      (cond
        [(equal? service ASIP_EVENT)
         (define distanceValues (string-split (substring input
                                                        (+ (str-index-of input "{") 1)

                                                        (str-index-of input "}") ) ",") )
         (set! DISTANCE-VALUES (list->vector
                                (map (λ (x) (string->number x))
                                 distanceValues )
                          )
               )
         ]
        [else (printf "DEBUG: unkown message for Distance service: ~a \n" input)]
        )
      )
    )
  ) ;; end of process-bump-service

;;       (define MOTOR_SERVICE           "M")
;; (define SET_MOTOR_SPEED         "m")

;; *** DEFINITION OF ASIP CONSTANTS FOR ENCODERS (HUB-EE WHEELS) SERVICE ***
;; (define ENCODER_SERVICE         "E")
;; Remember: use ASIP_EVENT and AUTOEVENT_MESSAGE
;; to read and configure this service

;; *** DEFINITION OF ASIP CONSTANTS FOR IR SERVICE
;;(define IR_SERVICE              "R")
;; Remember: use ASIP_EVENT and AUTOEVENT_MESSAGE
;; to read and configure this service

;; *** DEFINITION OF ASIP CONSTANTS FOR BUMPER SERVICE
;;(define BUMPER_SERVICE              "B")


;; Other utility functions for Myrtle
;; TODO: Add error checking, esp. for out
;; of bounds requests!

;; reset and read encoders
(define resetCount
  (λ (num)
    (vector-set! MOTOR-COUNT num 0)
    )
  )

(define readCount
  (λ (num)
    (vector-ref MOTOR-COUNT num)
    )
  )

(define getCount readCount) ;; just a synonim

;; read one IR value
(define getIR
  (λ (num)
    (vector-ref IR-VALUES num)
    )
  )

;; Boolean functions for bump sensors
(define rightBump?
  (λ () (vector-ref BUMP-VALUES 1))
  )
(define leftBump?
    (λ () (vector-ref BUMP-VALUES 0))
  )

;; Get the value of distance sensor pos.
;; If no pos is provided, return the value in position 0
(define getDistance (λ ([pos 0])
  (vector-ref DISTANCE-VALUES pos)
  )
  )
(define (testLoop)
  (setMotor 0 150)
  (sleep 1)
  (stopMotors)
  (setMotor 1 150)
  (sleep 1)
  (stopMotors)
  (setMotors 255 255)
  (sleep 2)
  (w1-stopMotor)
  (w2-stopMotor)
  (sleep 1)
  (printf "IR Values: ~a ~a ~a \n" (getIR 0) (getIR 1) (getIR 2))
  (printf "Bumpers: ~a ~a \n" (leftBump?) (rightBump?))
  (printf "Count values: ~a ~a \n" (readCount 0) (readCount 1))
  (testLoop)
)

(define (test)
  (open-asip)
  (enableIR 100)
  (sleep 0.1)
  (enableCounters 100)
  (sleep 0.1)
  (enableBumpers 1)
  (sleep 0.1)
  (displayln "Playing 3 notes")
  (playTone 262 500)
  (playTone 294 500)
  (playTone 330 500)
  (sleep 5)
  (displayln "Clearing LCD and sending messages")
  (clearLCD)
  (sleep 0.1)
  (setLCDMessage "0123456789ABCDEFGHILMNOPQRST" 1)
  (sleep 0.1)
  (setLCDMessage "      Hello" 2)
  (sleep 0.1)
  (setLCDMessage "      World" 3)
  (sleep 0.1)
  (setLCDMessage "   **********" 4)
  (sleep 1)
  (displayln "Playing 3 notes")
  (playTone 330 500)
  (playTone 294 500)
  (playTone 262 500)
  (testLoop)
  )
