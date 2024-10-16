;; abaqus.el
;; Author:  Vasu Kolli - vasukolli23@gmail.com
(require 'outline)
(defvar abaqus-mode-hook nil)
(defvar abaqus-load-hook nil)

;; Syntax highlighting
(defconst abaqus-font-lock-keywords
  (list
   '("^[*][*].*$" . font-lock-comment-face) ;; lines starting with ** - comment
   '("^\*[a-zA-Z]*\s?[a-zA-Z]*[,|\n]" . font-lock-keyword-face) ;; *Keyword1 keyword2
   '("\s[a-zA-Z\s]*=[a-zA-Z0-9<>.\\_\s/\-]*[,\n]" . font-lock-string-face) ;; keyword1 keyword2 = Keyword3[,|\n]
   '("\s[a-zA-Z]*[,\n]" . font-lock-constant-face))) ;; keyword1[,|\n]

;; Keyboard shortcuts ;;
(defvar abaqus-mode-map ()
  "Keymap used in `abaqus-mode' buffers.")
(if abaqus-mode-map
    nil
  (setq abaqus-mode-map (make-sparse-keymap))
  (define-key abaqus-mode-map (kbd "TAB")       'outline-toggle-children))

;; folding
(defun abaqus-outline-setup ()
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

;; Run abaqus-outline-setup every time abaqus-mode is activated
(add-hook 'abaqus-mode-hook 'abaqus-outline-setup)

;; Actual major mode definition ;;
(define-derived-mode abaqus-mode fundamental-mode "Abaqus"
  "Major mode for editing Abaqus input files."
  (setq font-lock-defaults '(abaqus-font-lock-keywords))
  ;; set comments
  (setq-local comment-start "**")
  (setq-local comment-end "")
  (use-local-map abaqus-mode-map)
  (display-line-numbers-mode 1) ; display line numbers
  (font-lock-flush)
  (font-lock-ensure))

(provide 'abaqus-mode)
(run-hooks 'abaqus-load-hook)

;; Automatically load into abaqus mode for *.inp and *.inc files
(add-to-list 'auto-mode-alist '("\\.\\(inp\\|inc\\|pes\\)\\'" . abaqus-mode))
