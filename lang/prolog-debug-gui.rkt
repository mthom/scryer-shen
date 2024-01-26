#lang racket/gui

(require "scryer-prolog-interface.rkt")

(provide open-prolog-debug-gui
         (rename-out [sp-connector-out scryer-prolog-log-out]))

(define-values (sp-connector-in sp-connector-out)
  (make-pipe))

(define (open-prolog-debug-gui)
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
         [style '(auto-hscroll auto-vscroll resize-corner)]))

  (send scryer-prolog-canvas set-canvas-background
        (make-object color% "black"))

  (define scryer-prolog-text
    (new text%
         [auto-wrap #t]))

  (send scryer-prolog-canvas set-editor scryer-prolog-text)
  (send scryer-prolog-text change-style
        (let ([color-delta (make-object style-delta% 'change-normal-color)])
          (send color-delta set-delta-background "black")
          (send color-delta set-delta-foreground "green")
          color-delta))
  (send scryer-prolog-text change-style
        (let ([font-delta  (make-object style-delta% 'change-family 'modern)])
          (send font-delta set-delta 'change-size 16)
          font-delta))

  ; Show the frame by calling its show method
  (send frame show #t)

  (thread (lambda ()
            (let loop ()
              (define line (read-line sp-connector-in))
              (unless (eof-object? line)
                (send scryer-prolog-text insert line)
                (send scryer-prolog-text insert "\n"))
              (loop)))))

