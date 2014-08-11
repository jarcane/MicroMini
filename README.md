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
MicroMini expects a stream of 8-bit bytes not more than 64kb in length, of correct instructions, which it will execute from address #x0000 to the best of its ability. There is basic checking for a few standard errors: stack over/underflow, and invalid instructions, and MM will halt and report the address the error occured in.

Documentation
-------------

Enclosed in the repo are MicroMini.lyx and MicroMini.html files, which document the instruction set for the MM virtual machine.

Licensing
---------

Copyright 2014 John Berry

Licensed via the GPL v3

CharTerm is Copyright 2012-2013 Neil Van Dyke, licensed vial LGPL v3
