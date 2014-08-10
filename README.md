MicroMini
=========

A simple 8-bit virtual stack machine, with basic terminal I/O powered by CharTerm.

Depends on a unix stty device, and thus only runs in those environs currently.

Requirements
------------

MicroMini is built with Racket 6.1. If you wish to edit or run the source directly, you will need this installed. 

Usage
-----

From source:

```
 racket main.rkt <mm-binary-file>
```

From binary:

```
 mmini <mm-binary-file>
```
MicroMini expects a stream of 8-bit bytes of correct instructions. At present, there is minimal error checking, and MM will crash if you do bad things to its poor widdle head.

Documentation
-------------

Enclosed in the repo are MicroMini.lyx and MicroMini.html files, which document the instruction set for the MM virtual machine.

Licensing
---------

Copyright 2014 John Berry

Licensed via the GPL v3

CharTerm is Copyright 2012-2013 Neil Van Dyke, licensed vial LGPL v3
