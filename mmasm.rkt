#lang racket

;; MMASM - The MicroMini Assembler
;
; Translates MicroMini assembler into executable binaries for the MicroMini virtual machine.
;
; Copyright 2014 John S Berry III
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

(require racket/cmdline)

; The Lookup Table

(define LOOKUP (hash "NOP"  #"\x00"
                     "HLT"  #"\x01"
                     "DATA" #"\x02"
                     "ADD"  #"\x10"
                     "SUB"  #"\x20"
                     "AND"  #"\x30"
                     "OR"   #"\x31"
                     "XOR"  #"\x32"
                     "NOT"  #"\x33"
                     "EQ?"  #"\x40"
                     "LES?" #"\x41"
                     "GRT?" #"\x42"
                     "PUSH" #"\x50"
                     "PUFA" #"\x51"
                     "PUCA" #"\x52"
                     "PUTI" #"\x53"
                     "POP"  #"\x60"
                     "POTA" #"\x61"
                     "JMP"  #"\x70"
                     "JSR"  #"\x71"
                     "JIF"  #"\x72"
                     "RET"  #"\x73"
                     "TRMI" #"\x80"
                     "TRMO" #"\x90"))

; Get the filename to be assembled from the command line
(define filenames
  (command-line
   #:args (asm-file bin-file)
   (list asm-file bin-file)))

; Initialize the empty byte stream
(define binary #"")

;; Main Functions

; Converts numbers or characters to bytes
(define (input-bytes input)
  (cond
    [(string->number input) (let ([num (string->number input)])
                              (cond [(byte? num) (bytes num)]
                                    [(< input (expt 2 16)) (integer->integer-bytes num 2 #f #t)]
                                    [else (error "MASM only supports 8-bit or 16-bit numbers")]))]
    [(string? input) (let ([bstr #""])
                       (for ([i input])
                         (if (byte? (char->integer i))
                             (set! bstr (bytes-append bstr (bytes (char->integer i))))
                             (error "Unsupported ASCII character")))
                       bstr)]
    [else (error "Unidentified or unsupported input")]))

; Encodes an input into bytes and appends it to the running binary
(define (encode input)
  (if (hash-has-key? LOOKUP input)
      (set! binary (bytes-append binary (hash-ref LOOKUP input)))
      (set! binary (bytes-append binary (input-bytes input)))))

; Parses the file and sends it to encode
(define (parse)
  (for ([c (in-port read-line (open-input-file (first filenames)))])
    (let ([s (string-split c)])
      (for ([i s])
        (encode i)))))

; Writes the binary to a file
(define (write-binary)
  (with-output-to-file (second filenames)
    (lambda () (write-bytes binary)) #:exists 'replace))

;; Main Execution
(parse)
(write-binary)