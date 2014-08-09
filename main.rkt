#lang racket

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

;; Helper Functions

; Byte-string maker 
; given a number of bits that are a multiple of 8, returns bytestring of that many bytes
(define (bits x)
  (make-bytes (/ x 8) 0))

(define (numerate bytes)
  (integer-bytes->integer bytes #f #t))

;; Main Variables

; Constants
(define DATA-BUS 8) ; the data bus width
(define ADDRESS-BUS 16) ; the address bus width
(define STACK-SIZE 16)
(define RAM-SIZE (expt 2 ADDRESS-BUS))

; The CPU contains two pointers (program, return), the main stack, 
;  and a cycle counter
(struct register (progptr retptr stkptr cycles)
  #:mutable #:transparent)

; Now we create the initial CPU instance
(define cpu (register 
             0 ; program pointer
             0 ; return pointer
             -1 ; stack pointer
             0)) ; Cycle counter

; Create the stack, STACK-SIZE long and DATA-BUS wide
(define stack (make-vector STACK-SIZE (bits DATA-BUS)))

; Next we create the RAM array, a vector of bytes as long as RAM-SIZE
(define ram (make-vector RAM-SIZE (bits DATA-BUS)))

;; Stack Functions

; push - Takes a bytestring, checks that it's not wider than data-bus, then puts it in the stack
; moving the pointer to match.
(define (push bytes)
  (set-register-stkptr! cpu (add1 (register-stkptr cpu)))
  (vector-set! stack (register-stkptr cpu) bytes))

; pop - returns the current top value of the stack and removes it, decrementing the stack pointer
(define (pop)
  (define ret (vector-ref stack (register-stkptr cpu)))
  (vector-set! stack (register-stkptr cpu) (bits DATA-BUS))
  (set-register-stkptr! cpu (sub1 (register-stkptr cpu)))
  ret)

