#!/usr/bin/env csi
(require-extension ports args)

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " [options...] [files...]")
      (newline)
      (print (args:usage opts))
      (print "Report bugs to zbigniewsz at gmail.")))
  (exit 1))

(define opts
  (list (args:make-option (h help) #:none "Display this text" (usage))))

(define cli-args (command-line-arguments))

(define (command-ls args)
  (print args)
  (exit 1))

(define (dispatch-command cmd args)
  (match cmd
    [ls (command-ls args)]))

(define (split-on-first-operand args-right #!optional (args-left '()))
  (let ([head (car args-right)])
    (if (irregex-match '(: "-" (+ (or "-" alpha))) (symbol->string head))
        (split-on-first-operand (cdr args-right) (cons head args-left))
        (list (reverse args-left) args-right))))

(receive (options operands) (args:parse (command-line-arguments) opts
                                        #:unrecognized-proc args:ignore-unrecognized-options)
  (dispatch-command (car operands) (cdr operands)))
