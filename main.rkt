#lang racket
(require racket/cmdline)

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(module+ test
  ;; Tests to be run with raco test
  )


(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.

  (define verbose-mode (make-parameter #f))
  (define profiling-on (make-parameter #f))
  (define optimize-level (make-parameter 0))
  (define link-flags (make-parameter null))

  (require "private/cmd-ls.rkt")
  (require "private/config.rkt")

  (define (dispatch-cmd cmd argv)
    (if (string=? "ls" cmd)
        (command-line #:program "main ls"
                      #:argv argv
                      #:once-each
                      [("-v" "--verbose") "Run in verbose mode" (verbose-mode #t)])
        (displayln "else nothing happens"))
    (cmd-ls-all-lists (file->config "./private/api-config.rkt")))

  (define (multi-command [argv (current-command-line-arguments)])
    (command-line #:argv argv #:args (command . cmd-option)
                  (dispatch-cmd command cmd-option)))
  (multi-command)
  ) ;; end main


