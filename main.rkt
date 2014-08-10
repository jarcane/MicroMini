#lang racket

(require racket/fixnum)

; MicroMini - A vintage minicomputer inspired stack machine
;
; Copyright 2014 John S. Berry III
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

; Constants
(define DATA-BUS 8) ; the data bus width
(define DATA-MASK (sub1 (expt 2 DATA-BUS))) ; The bitmask used to constrain add/sub
(define CARRY-MASK (expt 2 DATA-BUS)) ; The bitmask used to check for carry
(define ADDRESS-BUS 16) ; the address bus width
(define STACK-SIZE 16)
(define RAM-SIZE (expt 2 ADDRESS-BUS))

;; Helper Functions

; Byte-string maker 
; given a number of bits that are a multiple of 8, returns bytestring of that many bytes
(define (bits x)
  (make-bytes (/ x 8) 0))

; Turns bytes into integer decimals for internal use by Racket
(define (decode bytes)
  (if (= (bytes-length bytes) 1)
      (bytes-ref bytes 0)
      (integer-bytes->integer bytes #f #t)))

; Converts a boolean to 1 for #t and 0 for #f
(define (bool->int x)
  (if (false? x)
      0
      1))

; increments the program pointer
(define (add1-progptr)
  (set-register-progptr! cpu (add1 (register-progptr cpu))))

; gets an address from the next two bytes in RAM
(define (get-address)
  (let ([x 0]
        [y 0])
    (add1-progptr)
    (set! x (vector-ref ram (register-progptr cpu)))
    (add1-progptr)
    (set! y (vector-ref ram (register-progptr cpu)))
    (decode (bytes-append x y))))

; Add with overflow, returns fxvector with result and carry flag
(define (add/overflow a b) 
  (let ([x (fx+ a b)])
    (fxvector (fxand DATA-MASK x) (fxrshift (fxand CARRY-MASK x) DATA-BUS))))

; Subtraction with overflow, returns fxvector with result and carry flag
(define (sub/overflow a b) 
  (let ([x (fx- a b)]) 
    (fxvector (fxand DATA-MASK x) (fxrshift (fxand CARRY-MASK x) DATA-BUS))))

;; Main Variables

; Instruction Table
(define INSTRUCTIONS (hash #"\x00" (lambda () (void))   ; No OPeration
                           #"\x01" (lambda () (set-register-halt! cpu 1))   ; HaLT
                           #"\x02" (lambda () 
                                     (add1-progptr)
                                     (set-register-progptr! 
                                      cpu
                                      (+ (register-progptr cpu)
                                         (decode (vector-ref ram (register-progptr cpu))))))  ; DATA
                           #"\x10" (lambda ()
                                     (let ([result (add/overflow (pop) (pop))])
                                       (push (fxvector-ref result 0))
                                       (set-register-carry! cpu (fxvector-ref result 1))))   ; ADD
                           #"\x20" (lambda ()
                                     (let ([result (sub/overflow (pop) (pop))])
                                       (push (fxvector-ref result 0))
                                       (set-register-carry! cpu (fxvector-ref result 1))))   ; SUBtract
                           #"\x30" (lambda ()
                                     (push (bitwise-and (pop) (pop))))   ; AND
                           #"\x31" (lambda ()
                                     (push (bitwise-ior (pop) (pop))))    ; OR
                           #"\x32" (lambda ()
                                     (push (bitwise-xor (pop) (pop))))   ; XOR
                           #"\x33" (lambda ()
                                     (push (bitwise-not (pop))))   ; NOT
                           #"\x40" (lambda ()
                                     (push (bool->int (= (pop) (pop)))))   ; EQuals?
                           #"\x41" (lambda ()
                                     (push (bool->int (< (pop) (pop)))))  ; LESser?
                           #"\x42" (lambda ()
                                     (push (bool->int (> (pop) (pop)))))  ; GReaTer?
                           #"\x50" (lambda ()
                                     (add1-progptr)
                                     (push (decode (vector-ref ram (register-progptr cpu)))))  ; PUSH
                           #"\x51" (lambda ()
                                     (push (decode (vector-ref ram (get-address)))))  ; PUsh From Address
                           #"\x60" (lambda () (pop))   ; POP
                           #"\x61" (lambda ()
                                     (vector-set! ram (get-address) (bytes (pop))))  ; POp To Address
                           #"\x70" 'JMP   ; JuMP
                           #"\x71" 'RET   ; RETurn
                           #"\x72" 'JIF   ; Jump IF
                           #"\x80" 'TRMI  ; TeRMinal Input
                           #"\x81" 'TRWI  ; TeRminal Wait for Input
                           #"\x90" (lambda ()
                                     (display (bytes (pop))))  ; TeRMinal Output
                           #"\x98" 'TRWO)) ; TeRminal Wait for Output

; The CPU contains two pointers (program, return), the main stack, 
;  and a cycle counter
(struct register (progptr retptr stkptr cycles carry halt)
  #:mutable #:transparent)

; Now we create the initial CPU instance
(define cpu (register 
             0   ; program pointer
             0   ; return pointer
             -1  ; stack pointer
             0   ; Cycle counter
             0   ; Carry flag
             0)) ; Halt-bit

; Create the stack, STACK-SIZE long
; This is kept as integer values internally (rather than bytes) to avoid repetitive decode-encode cycles
; PUSH/POP instructions instead decode/encode any operations that transmit between RAM or I/O
(define stack (make-vector STACK-SIZE 0))

; Next we create the RAM array, a vector of bytes as long as RAM-SIZE
; This is kept as bytestrings, for easier dumping and hash-referencing of instructions
(define ram (make-vector RAM-SIZE (bits DATA-BUS)))

;; Stack Functions

; push - Takes an integer, then puts it in the stack
; moving the pointer to match.
(define (push num)
  (set-register-stkptr! cpu (add1 (register-stkptr cpu)))
  (vector-set! stack (register-stkptr cpu) num))

; pop - returns the current top value of the stack and removes it, decrementing the stack pointer
(define (pop)
  (define ret (vector-ref stack (register-stkptr cpu)))
  (vector-set! stack (register-stkptr cpu) 0)
  (set-register-stkptr! cpu (sub1 (register-stkptr cpu)))
  ret)

;; Execution cycle

; run - the main run-time loop for the CPU
; Steps through ram, decoding then executing instruction, then incrementing progptr and cycles
(define (run)
  (let loop ()
    (let ([instr (vector-ref ram (register-progptr cpu))]) ; Grab the next instruction
      (execute instr) ; execute the instr
      (add1-progptr) ; increment program register
      (when (>= (register-progptr cpu) RAM-SIZE) 
        (set-register-halt! cpu 1)) ; If the program counter reaches the bounds of memory, halt
      (if (byte? (register-cycles cpu))  ; check if the cycle counter is under a byte
          (set-register-cycles! cpu (add1 (register-cycles cpu))) ; incr if so
          (set-register-cycles! cpu 0)))  ; reset if too big
    (when (= (register-halt cpu) 0) (loop)))) ; check the halt bit, and keep running if off

; execute - executes a given instruction, with all requisite side effects.
(define (execute instr)
  ((hash-ref INSTRUCTIONS instr))) ; looks up the instruction in the INSTRUCTION hash and executes

(vector-set! ram 0 #"\x51") ; PUSH from address
(vector-set! ram 1 #"\x00") ; 
(vector-set! ram 2 #"\x08") ; #x0008
(vector-set! ram 3 #"\x61") ; POTA
(vector-set! ram 4 #"\x00") ; 
(vector-set! ram 5 #"\x09") ; #x0009
(vector-set! ram 6 #"\x00") ; NOP
(vector-set! ram 7 #"\x01") ; HALT
(vector-set! ram 8 #"\x49") 
(run)