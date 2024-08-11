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

  (let ([prolog-debug-es (make-eventspace)])
    (parameterize ([current-eventspace prolog-debug-es])
      (define frame (new frame%
                         [label "Scryer Prolog Debug Window"]
                         [width 800]
                         [height 600]))

      (define scryer-prolog-text
        (new text%
             [auto-wrap #t]))

      (define scryer-prolog-canvas
        (new editor-canvas%
             [parent frame]
             [editor scryer-prolog-text]
             [style '(auto-hscroll auto-vscroll resize-corner)]))

      (send scryer-prolog-canvas set-canvas-background
            (make-object color% "black"))

      (send scryer-prolog-text change-style
            (let ([color-delta (make-object style-delta% 'change-normal-color)])
              (send color-delta set-delta-foreground "green")
              color-delta))

      (send scryer-prolog-text change-style
            (let ([font-delta  (make-object style-delta% 'change-family 'modern)])
              (send font-delta set-delta 'change-size 16)
              font-delta))

      (send frame show #t)

      (add-text-keymap-functions (send scryer-prolog-text get-keymap))

      (send (send scryer-prolog-text get-keymap)
            map-function
            "c:c"
            "copy-clipboard")

      (define debug-output-port
        (open-output-text-editor scryer-prolog-text #:eventspace prolog-debug-es))

      (thread (lambda ()
                (let loop ()
                  (define line (read-line sp-connector-in))
                  (unless (eof-object? line)
                    (write-string line debug-output-port)
                    (write-string "\n" debug-output-port))
                  (loop)))))))

