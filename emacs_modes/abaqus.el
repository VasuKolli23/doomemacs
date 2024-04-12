;; abaqus.el
;; $Id: abaqus.el,v 1.5 2002/07/03 12:13:43 jorgen Exp $
;;
;; Author:   Jorgen S Bergstrom <jorgen@polymerFEM.com>
;; URL:      http://www.polymerFEM.com
;;
;; Modified by: James Lockley <safricanjames@yahoo.co.uk>
;;              Martin Lüthi
;; Modified by: Nidish Narayanaa Balaji <nidbid@gmail.com> (on 2020/08/14)
;;
;; Installation:
;;    add the following lines to your .emacs file:
;;
;;       ; abaqus
;;       (add-hook 'abaqus-mode-hook 'turn-on-font-lock)
;;       (autoload 'abaqus-mode "abaqus" "Enter abaqus mode." t)
;;
;;    copy this file the emacs site-lisp directory:
;;
;;       cp abaqus.el [path to emacs site-lisp directory]
(require 'outline)

(defvar abaqus-mode-hook nil)
(defvar abaqus-load-hook nil)

;; ;; Syntax highlighting
;; (defconst abaqus-font-lock-keywords
;;   (list
;;    '("^\*[a-zA-Z].*[^a-zA-Z]" . font-lock-keyword-face) ;; lines starting with * - keyword line
;;    '("^[*][*].*$" . font-lock-comment-face) ;; lines starting with ** - comment
;;    '("^[\s]*$" . highlight) ;; empty line with whitespaces
;;   )
;; )

(defconst abaqus-font-lock-keywords
  (list
   '("^[*][*].*$" . font-lock-comment-face) ;; lines starting with ** - comment
   '("^\*[a-zA-Z]*\s?[a-zA-Z]*[,|\n]" . font-lock-keyword-face) ;; *Keyword1 keyword2
   '("\s[a-zA-Z\s]*=[a-zA-Z0-9<>.\\_\s/\-]*[,\n]" . font-lock-string-face) ;; keyword1 keyword2 = Keyword3[,|\n]
   '("\s[a-zA-Z]*[,\n]" . font-lock-constant-face) ;; keyword1[,|\n]
  )
)

;; Keyboard shortcuts ;;
(defvar abaqus-mode-map ()
  "Keymap used in `abaqus-mode' buffers.")
(if abaqus-mode-map
    nil
  (setq abaqus-mode-map (make-sparse-keymap))
  (define-key abaqus-mode-map (kbd "TAB")       'outline-toggle-children)
)

;; ;; code foldign setup
;; (defun abaqus-outline-setup ()
;;   ;; All lines starting with * are considered as headers
;;   (setq-local outline-regexp "^\\*")
;;   ;; Enable outline-minor-mode
;;   (outline-minor-mode 1)
;;   ;; Hide all bodies of code
;;   (outline-hide-body))

;; (defun abaqus-outline-setup ()
;;   ;; Enable outline-minor-mode
;;   (outline-minor-mode 1)
;;   ;; Hide all bodies of code
;;   (outline-hide-body)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((prev-line-number nil)
;;           (current-line-number nil)
;;           (line-diff 0))
;;       (while (not (eobp))
;;         (beginning-of-line)
;;         ;; Check if the line starts with your keyword pattern
;;         (if (looking-at "^[*][a-zA-Z]")
;;             (progn
;;               (setq current-line-number (line-number-at-pos))
;;               (when prev-line-number
;;                 (setq line-diff (- current-line-number prev-line-number))
;;                 ;; (message "Keyword line = %s, difference = %d" current-line-number line-diff)
;;                 ;; If the line difference is less than 10, toggle outline
;;                 (when (< line-diff 10)
;;                   ;; (message "Showing line %s" prev-line-number)
;;                   (save-excursion
;;                     (forward-line prev-line-number)
;;                     (outline-show-entry))))
;;               (setq prev-line-number current-line-number)))
;;         (forward-line 1)))))

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

;; Actual major mode definiton ;;
(defun abaqus-mode ()
  "Major mode for editing ABAQUS files."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "abaqus")
  (setq major-mode 'abaqus-mode
        mode-name              "Abaqus"
        font-lock-defaults     '(abaqus-font-lock-keywords)
        require-final-newline  t)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(abaqus-font-lock-keywords))
  (font-lock-mode 1)
  ;; set comments
  (setq-local comment-start "**")
  (setq-local comment-end "")
  (run-hooks 'abaqus-mode-hook)
  (use-local-map abaqus-mode-map)
  (display-line-numbers-mode 1) ; display line numbers
  (font-lock-flush)
  (font-lock-ensure))

(provide 'abaqus-mode)
(run-hooks 'abaqus-load-hook)

;; automatically loads into abaqus mode for *.inp and *.inc files
(add-to-list 'auto-mode-alist '("\\.inp\\'" . abaqus-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . abaqus-mode))
(add-to-list 'auto-mode-alist '("\\.pes\\'" . abaqus-mode))
