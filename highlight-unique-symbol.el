;;; (require 'highlight-unique-symbol)
;;; (highlight-unique-symbol t)

(require 'cl)
(require 'deferred)
(require 'vc)
(require 'vc-git)

(defcustom highlight-unique-symbol:interval 0.5
  "Interval to check symbol's appearance count"
  :group 'highlight-unique-symbol
  :type 'float)

(defface highlight-unique-symbol:face
    '((t (:foreground "white")))
  "*Face used for unique symbol."
  :group 'highlight-unique-symbol)

(defvar highlight-unique-symbol:timer
  nil
  "*Timer"
  )

(defun highlight-unique-symbol:chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

(defun highlight-unique-symbol:git-project-p ()
  (not (string= (highlight-unique-symbol:git-root-directory) "")))

(defun highlight-unique-symbol:git-root-directory ()
  (or (vc-file-getprop default-directory 'highlight-unique-symbol-git-root-directory)
      (vc-file-setprop default-directory 'highlight-unique-symbol-git-root-directory
                       (or (vc-git-root default-directory) ""))))

(defun highlight-unique-symbol:check ()
  (interactive)
  (lexical-let*
      (
       (current-symbol (thing-at-point 'symbol))
       (current-overlay (and current-symbol (highlight-unique-symbol:overlay)))
       )
    (when (and
           current-symbol
           current-overlay
           (highlight-unique-symbol:git-project-p)
           (highlight-unique-symbol:is-overlay-changed  current-overlay current-symbol)
           (not (eq (face-at-point) 'font-lock-comment-face))
           (not (eq (face-at-point) 'font-lock-doc-face))
           )
      (overlay-put current-overlay 'highlight-unique-symbol:symbol current-symbol)
      (deferred:$
        (deferred:process-shell (format
                                 ;; -I Don't match the pattern in binary files
                                 "git --no-pager grep --cached --word-regexp -I --fixed-strings --quiet -e %s -- %s"
                                 (shell-quote-argument current-symbol)
                                 (highlight-unique-symbol:git-root-directory)
                                 ))
        ;; success when found
        (deferred:nextc it
          (lambda (res)
            (highlight-unique-symbol:ok current-overlay)))
        ;; error when not found
        (deferred:error it
          (lambda (res)
            (highlight-unique-symbol:warn current-overlay)))
        ))))

(defun highlight-unique-symbol:is-overlay-changed (overlay symbol-at-point)
  (not (string= (overlay-get overlay 'highlight-unique-symbol:symbol) symbol-at-point)))

(defun highlight-unique-symbol:warn (overlay)
  (overlay-put overlay 'face 'highlight-unique-symbol:face))

(defun highlight-unique-symbol:ok (overlay)
  (overlay-put overlay 'face nil))

(defun highlight-unique-symbol:overlay ()
  (save-excursion
    (let*
        (
         (begin (beginning-of-thing 'symbol))
         (end (end-of-thing 'symbol))
         (overlays (overlays-in begin end))
         (overlay (find-if
                   '(lambda (ovl) (overlay-get ovl 'highlight-unique-symbol:is-highlight-overlay))
                   overlays))
         )
      (if overlay
          overlay
        (highlight-unique-symbol:create-overlay)))))

(defun highlight-unique-symbol:create-overlay ()
  (save-excursion
    (let*
        (
         (begin (beginning-of-thing 'symbol))
         (end (end-of-thing 'symbol))
         (overlay (make-overlay begin end))
         (on-modify '((lambda (overlay after-p begin end &optional length)
                      (delete-overlay overlay))))
         )
      (overlay-put overlay 'highlight-unique-symbol:is-highlight-overlay 1)
      (overlay-put overlay 'modification-hooks on-modify)
      (overlay-put overlay 'insert-in-front-hooks on-modify)
      (overlay-put overlay 'insert-behind-hooks on-modify)
      overlay)))

(defun highlight-unique-symbol (start)
  "Start highlighting unique symbols"
  (when (and start highlight-unique-symbol:timer) (highlight-unique-symbol nil))
  (if start
      (setq highlight-unique-symbol:timer (run-with-idle-timer
                                           highlight-unique-symbol:interval
                                           t
                                           'highlight-unique-symbol:check))
  (when highlight-unique-symbol:timer
    (cancel-timer highlight-unique-symbol:timer)
    (setq highlight-unique-symbol:timer nil)))
  )

(provide 'highlight-unique-symbol)
