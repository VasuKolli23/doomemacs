(require 'outline)

(defvar lsdyna-mode-hook nil)
(defvar lsdyna-load-hook nil)

(defconst lsdyna-font-lock-keywords
  (list
   '("^[\$].*$" . font-lock-comment-face) ;; lines starting with $ - comment
   '("^\\*.*" . font-lock-keyword-face) ;; *Keyword1
  )
)

;; Keyboard shortcuts ;;
(defvar lsdyna-mode-map ()
  "Keymap used in `lsdyna-mode' buffers.")
(if lsdyna-mode-map
    nil
  (setq lsdyna-mode-map (make-sparse-keymap))
  (define-key lsdyna-mode-map (kbd "TAB")       'outline-toggle-children)
)

;; ruler
(defvar lsdyna-ruler "$...>....1....>....2....>....3....>....4....>....5....>....6....>....7....>....8"
  "*The ruler `lsdyna-insert-ruler' inserts."
)

(defun lsdyna-insert-ruler ()
  "Insert a ruler with comments."
  (interactive)
  (end-of-line)
  (insert lsdyna-ruler)
)

;; folding
(defun lsdyna-outline-setup ()
  "Setup folding for sections with more than 10 lines between * lines."
  (outline-minor-mode 1) ; Enable outline mode for folding.
  (goto-char (point-min)) ; Start from the beginning of the buffer.
  (let ((last-star-pos (point-min)) ; Position of the last * line.
        (line-count 0)) ; Number of lines since the last * line.
    ;; Loop through the buffer looking for lines that start with *
    (while (not (eobp))
      (let ((line-starts-with-star (looking-at "^\\*")))
        (when line-starts-with-star
          ;; Check if the distance from the last * line is more than 20 lines
          (when (> line-count 10)
            ;; If more than 20 lines, fold the previous section
            (save-excursion
              (goto-char last-star-pos)
              (outline-hide-entry)))
          (setq last-star-pos (point)) ; Update the position of the last * line
          (setq line-count 0)) ; Reset line count
        ;; Increment line count if not at a * line or the first * line
        (unless line-starts-with-star
          (setq line-count (1+ line-count))))
      (forward-line 1))))

;; Run lsdyna-outline-setup every time lsdyna-mode is activated
(add-hook 'lsdyna-mode-hook 'lsdyna-outline-setup)

;; Actual major mode definiton ;;
(defun lsdyna-mode ()
  "Major mode for editing LSDYNA files."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "lsdyna")
  (setq major-mode 'lsdyna-mode
        mode-name              "lsdyna"
        font-lock-defaults     '(lsdyna-font-lock-keywords)
        require-final-newline  t)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(lsdyna-font-lock-keywords))
  (font-lock-mode 1)
  ;; set comments
  (setq-local comment-start "$")
  (setq-local comment-end "")
  (run-hooks 'lsdyna-mode-hook)
  (use-local-map lsdyna-mode-map)
  (display-line-numbers-mode 1) ; display line numbers
  (font-lock-flush)
  (font-lock-ensure))

(provide 'lsdyna-mode)
(run-hooks 'lsdyna-load-hook)

;; automatically loads into lsdyna mode for *.k, *.key files
(add-to-list 'auto-mode-alist '("\\.k\\'" . lsdyna-mode))
(add-to-list 'auto-mode-alist '("\\.key\\'" . lsdyna-mode))
