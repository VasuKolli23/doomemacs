;; abaqus.el
;; Author:  Vasu Kolli - vasukolli23@gmail.com

(require 'outline)

(defvar abaqus-mode-hook nil)
(defvar abaqus-load-hook nil)

;; Customizable threshold for folding
(defvar abaqus-outline-threshold 10
  "Number of lines after which a section is folded in Abaqus mode.")

;; Syntax highlighting
(defconst abaqus-font-lock-keywords
  (list
   ;; Comments starting with **
   '("^[*][*].*$" . font-lock-comment-face)
   ;; Keywords starting with *
   '("^\\*\\([A-Za-z]+\\)" . font-lock-keyword-face)
   ;; Parameters: word = value
   '("\\b\\([A-Za-z]+\\)\\s-*=" . font-lock-variable-name-face)
   ;; Values after =
   '("=\\s-*\\([A-Za-z0-9<>._/\\-]+\\)" . font-lock-string-face))
  "Font lock keywords for `abaqus-mode`.")

;; Keymap
(defvar abaqus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'outline-toggle-children)
    map)
  "Keymap for `abaqus-mode`.")

;; Syntax table
(defvar abaqus-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Define `**` as a comment start
    (modify-syntax-entry ?\* ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for `abaqus-mode`.")

;; Folding function
(defun abaqus-outline-setup ()
  "Setup folding for sections with more than `abaqus-outline-threshold` lines between * lines."
  (outline-minor-mode 1)
  (setq outline-regexp "^\\*")
  (save-excursion
    (goto-char (point-min))
    (let ((last-star-pos (point-min))
          (last-star-line (line-number-at-pos (point-min)))
          (while (re-search-forward "^\\*" nil t)
            (let ((current-star-pos (match-beginning 0))
                  (current-star-line (line-number-at-pos (point)))
                  (when (> (- current-star-line last-star-line) abaqus-outline-threshold)
                    (goto-char last-star-pos)
                    (outline-hide-subtree))
                  (setq last-star-pos current-star-pos)
                  (setq last-star-line current-star-line))))))))

;; Run abaqus-outline-setup every time abaqus-mode is activated
(add-hook 'abaqus-mode-hook 'abaqus-outline-setup)

;; Actual major mode definition
(define-derived-mode abaqus-mode fundamental-mode "Abaqus"
  "Major mode for editing Abaqus input files."
  :syntax-table abaqus-mode-syntax-table
  (setq font-lock-defaults '(abaqus-font-lock-keywords nil t)) ; Case-insensitive
  ;; Set comments
  (setq comment-start "**")
  (setq comment-end "")
  ;; Use the defined keymap
  (use-local-map abaqus-mode-map)
  ;; Display line numbers
  (display-line-numbers-mode 1))

(provide 'abaqus-mode)
(run-hooks 'abaqus-load-hook)

;; Automatically load into abaqus mode for *.inp, *.inc, and *.pes files
(add-to-list 'auto-mode-alist '("\\.\\(inp\\|inc\\|pes\\)\\'" . abaqus-mode))
