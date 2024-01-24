#lang racket/gui

(require "scryer-prolog-interface.rkt")

(provide open-prolog-debug-gui)

(define (open-prolog-debug-gui)
  (define-values (sp-connector-in sp-connector-out)
    (make-pipe))

  (add-multiplexed-input-port-pipe! scryer-prolog-in sp-connector-out)
  (add-multiplexed-input-port-pipe! scryer-prolog-err sp-connector-out)
  (add-multiplexed-output-port-pipe! scryer-prolog-out sp-connector-out)

  (define frame (new frame%
                     [label "Scryer Prolog Debug Window"]
                     [width 800]
                     [height 600]))

  (define scryer-prolog-canvas
    (new editor-canvas%
         [parent frame]
         [style '(auto-vscroll auto-hscroll no-focus)]))

  (define scryer-prolog-text
    (new text%
         [auto-wrap #t]))

  (send scryer-prolog-canvas set-editor scryer-prolog-text)

  ; Show the frame by calling its show method
  (send frame show #t)

  (thread (lambda ()
            (let loop ()
              (define line (read-line sp-connector-in))
              (unless (eof-object? line)
                (send scryer-prolog-text insert line)
                (send scryer-prolog-text insert "\n"))
              (loop)))))

