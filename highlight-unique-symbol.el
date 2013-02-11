;;; (require 'highlight-unique-symbol)
;;; (highlight-unique-symbol t)

(require 'cl)
(require 'deferred)

(defvar highlight-unique-symbol:inteerval
  0.1
  "*Interval"
  )

(defvar highlight-unique-symbol:timer
  nil
  "*Timer"
  )

(defun highlight-unique-symbol:chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

(defun highlight-unique-symbol:git-project-p ()
  (string=
   (highlight-unique-symbol:chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(defun highlight-unique-symbol:git-root-directory ()
  (cond ((highlight-unique-symbol:git-project-p)
         (highlight-unique-symbol:chomp
          (shell-command-to-string "git rev-parse --show-toplevel")))
        (t
         "")))

(defun highlight-unique-symbol:check ()
  (interactive)
  (lexical-let
      (
       (current-symbol (thing-at-point 'symbol))
       (current-overlay (highlight-unique-symbol:overlay))
       )
    (when (highlight-unique-symbol:git-project-p)
      (unless (and  current-overlay (string= (overlay-get current-overlay 'highlight-unique-symbol:symbol) current-symbol))
        (progn
          (overlay-put current-overlay 'highlight-unique-symbol:symbol current-symbol)
          (deferred:$
            (deferred:process-shell (format "cd %s && git grep --word-regexp --name-only %s | wc -l" (highlight-unique-symbol:git-root-directory) (shell-quote-argument current-symbol)))
            (deferred:nextc it
              (lambda (res)
                (lexical-let
                    ((appear-count (string-to-number res)))
                  (when (string= current-symbol (thing-at-point 'symbol))
                    (if (<= appear-count 1)
                        (progn
                          (highlight-unique-symbol:warn current-overlay)
                          )
                      (highlight-unique-symbol:ok current-overlay))
                    ))))))))))

(defun highlight-unique-symbol:warn (overlay)
  (overlay-put overlay 'face font-lock-warning-face))

(defun highlight-unique-symbol:ok (overlay)
  (overlay-put overlay 'face nil))

(defun highlight-unique-symbol:overlay ()
  (save-excursion
    (let*
        (
         (overlays (overlays-in (beginning-of-thing 'symbol) (end-of-thing 'symbol)))
         (overlay (find-if '(lambda (ovl) (overlay-get ovl 'highlight-unique-symbol:overlay)) overlays))
         )
      (if (and overlay (overlay-get overlay 'highlight-unique-symbol:overlay)) overlay (highlight-unique-symbol:create-overlay)))))

(defun highlight-unique-symbol:create-overlay ()
  (save-excursion
    (let*
        (
         (begin (beginning-of-thing 'symbol))
         (end (end-of-thing 'symbol))
         (overlay (make-overlay begin end))
         )
      (overlay-put overlay 'highlight-unique-symbol:overlay 1)
      (overlay-put overlay 'modification-hooks
                   '((lambda (overlay after-p begin end &optional length)
                      (delete-overlay overlay))))
      (overlay-put overlay 'insert-in-front-hooks
                   '((lambda (overlay after-p begin end &optional length)
                      (delete-overlay overlay))))
      (overlay-put overlay 'insert-behind-hooks
                   '((lambda (overlay after-p begin end &optional length)
                      (delete-overlay overlay))))
      overlay)))

(defun highlight-unique-symbol (start)
  "Start highlighting unique symbols"
  (when (and start highlight-unique-symbol:timer) (highlight-unique-symbol nil))
  (if start
      (setq highlight-unique-symbol:timer (run-with-idle-timer
                                           highlight-unique-symbol:inteerval
                                           t
                                           'highlight-unique-symbol:check))
  (when highlight-unique-symbol:timer
    (cancel-timer highlight-unique-symbol:timer)
    (setq highlight-unique-symbol:timer nil)))
  )

(provide 'highlight-unique-symbol)