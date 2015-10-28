#lang s-exp syntax/module-reader
mini-hdl/runtime
#:read hdl-read
#:read-syntax hdl-read-syntax
#:whole-body-readers? #t
(require mini-hdl/parser)
