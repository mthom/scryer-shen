#lang racket

(require data/gvector
         racket/runtime-path
         racket/system)

(provide add-multiplexed-input-port-pipe!
         add-multiplexed-output-port-pipe!
         scryer-prolog-err
         scryer-prolog-in
         scryer-prolog-out)

(define-runtime-path scryer-prolog-path   "../dist/bin/scryer-prolog")
(define-runtime-path scryer-shen-toplevel "../scryer-server/scryer-shen-toplevel.pl")

(define-values (scryer-prolog-process in out err)
  (subprocess #f #f #f scryer-prolog-path "-f" scryer-shen-toplevel))

(struct multiplexed-input-port [name wrapped-in child-outs]
  #:property prop:input-port (struct-field-index wrapped-in))

(define (make-multiplexed-input-port name in)
  (define child-outs (make-gvector))
  (define wrapped-in (make-input-port/read-to-peek
                      name
                      (lambda (bstr)
                        (unless (eq? (subprocess-status scryer-prolog-process) 'running)
                          (error "Scryer Prolog is no longer running!"))
                        (define result (read-bytes! bstr in))
                        (for ([pipe (in-gvector child-outs)])
                          (write-bytes bstr pipe))
                        result)
                      #f
                      (thunk (close-input-port in))))
  (multiplexed-input-port name wrapped-in child-outs))

(struct multiplexed-output-port [name wrapped-out child-outs]
  #:property prop:output-port (struct-field-index wrapped-out))

(define (make-multiplexed-output-port name out)
  (define child-outs (make-gvector))
  (define wrapped-out (make-output-port
                       name
                       always-evt
                       (lambda (bstr start-pos end-pos can-block? blocks?)
                         (unless (eq? (subprocess-status scryer-prolog-process) 'running)
                           (error "Scryer Prolog is no longer running!"))
                         (define result (write-bytes
                                         bstr
                                         out
                                         start-pos
                                         end-pos))
                         (flush-output out)
                         (for ([pipe (in-gvector child-outs)])
                           (write-bytes bstr pipe start-pos end-pos))
                         result)
                       (thunk (close-output-port out))))
  (multiplexed-output-port name wrapped-out child-outs))

(define (add-multiplexed-input-port-pipe! in pipe)
  (gvector-add! (multiplexed-input-port-child-outs in) pipe))

(define (add-multiplexed-output-port-pipe! out pipe)
  (gvector-add! (multiplexed-output-port-child-outs out) pipe))

(define scryer-prolog-in  (make-multiplexed-input-port 'scryer-prolog-out in))
(define scryer-prolog-err (make-multiplexed-input-port 'scryer-prolog-err err))
(define scryer-prolog-out (make-multiplexed-output-port 'scryer-prolog-in out))

(fprintf scryer-prolog-out "'$scryer-shen-toplevel':repl.~n")

(define sp-executor (make-will-executor))

(will-register sp-executor scryer-prolog-process (lambda (_) (fprintf scryer-prolog-out "end_of_file.~n")))

(will-register sp-executor scryer-prolog-in close-input-port)
(will-register sp-executor scryer-prolog-out close-output-port)
(will-register sp-executor scryer-prolog-err close-input-port)
