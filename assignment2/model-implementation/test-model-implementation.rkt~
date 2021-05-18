#lang racket
(require "regToDfa.rkt") ;Don't worry if you do not find regToDfa.rkt
(define r "(a|b)*bba | cc*")
(define t (maketree r))
(define nullable (buildNullable t))
(define firstpos (buildFirst t))
(define lastpos (buildLast t))
(define followpos (buildFollow t))
(define graph (buildGraph r))
;(printGraph graph)
(matches? graph "bbab")
(matches? graph "bba")
(matches? graph "")
(matches? graph "cc")
(define r1 "@|((a|b)*bba | cc*)")
(define t1 (maketree r1))

(define graph1 (buildGraph r1))
(matches? graph1 "")
