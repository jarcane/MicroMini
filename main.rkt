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

; Constants
(define DATA-BUS 8) ; the data bus width
(define ADDRESS-BUS 16) ; the address bus width
(define STACK-SIZE 128)
(define RAM-SIZE (expt 2 ADDRESS-BUS))

; Byte-string maker 
; given a number of bits that are a multiple of 8, returns bytestring of that many bytes
(define (bits x)
  (if (= (modulo x 8) 0)
      (make-bytes (/ x 8) 0)
      (error 'Incorrect-bit-multiple)))

; The following structs are used for organizing the CPU components

; The CPU contains two pointers (program, return), the main stack, 
; A & B internal operand registers for math, and a cycle counter
(struct register (progptr retptr stkptr stack aop bop cycles)
  #:mutable #:transparent)

; Now we create the initial CPU instance, using a vector of bytes for the stack
(define cpu (register 
             (bits ADDRESS-BUS) ; program pointer
             (bits ADDRESS-BUS) ; return pointer
             (bits DATA-BUS) ; stack pointer
             (make-vector STACK-SIZE (bits DATA-BUS)) ; stack
             (bits DATA-BUS) ; a-operand register
             (bits DATA-BUS) ; b-operand register
             (bits DATA-BUS))) ; Cycle counter

; Next we create the RAM array, a vector of bytes as long as RAM-SIZE
(define ram (make-vector RAM-SIZE (bits DATA-BUS)))