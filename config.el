;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Vasu Kolli"
      user-mail-address "vasukolli23@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-oceanic-next)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(menu-bar-mode t)

;; for emacs GUI
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(beacon-mode 1)

;; (setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 16))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun org-babel-detangle-and-save()
  "Detangle the current buffer and then save all buffers."
  (interactive)
  (org-babel-detangle)
  (evil-write-all nil))

(add-hook! 'python-mode-hook
  (map! :localleader
        (:prefix ("b", "buffer")
          :desc "Detangle buffer and save all" "d" #'org-babel-detangle-and-save)))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Truncate lines"          "t" #'toggle-truncate-lines))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Comment/Uncomment lines"          "/" #'comment-line))

(xterm-mouse-mode 1)

(setq blink-cursor-mode 0)

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("☯" "◉" "○" "✿" "◆" "✜" "✸" ))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

(add-hook 'org-mode-hook 'org-fragtog-mode)

(setq org-startup-with-latex-preview t)

(setq org-export-in-background t)

(setq org-image-actual-width (/ (display-pixel-width) 3))

(setq org-table-default-attributes
      (list
       '(:align 'center)
       '(:valign 'center)
       '(:hlines nil)))

;; (after! org
  ;; (setq org-src-window-setup 'current-window))

(load! "~/emacs_modes/abaqus.el")
(add-hook 'abaqus-mode-hook 'turn-on-font-lock)
(autoload 'abaqus-mode "abaqus" "Enter abaqus mode." t)

(after! python
  (setq python-shell-interpreter "python3")
  (setq python-shell-virtualenv-root "/home/kolli/Envs/common/")
)

(after! lsp-mode
  (setq lsp-enable-snippet nil ;; Not using Company Snippets
        lsp-diagnostic-package :auto
        lsp-idle-delay 0.500
        lsp-log-io nil
        lsp-python-ms-auto-install-server t)
  (add-hook 'python-mode-hook #'lsp) ;; Auto-enable LSP
)

(defadvice org-babel-tangle-jump-to-org (after recenter activate)
  (recenter))

(after! org
  ;; disable auto-complete in org-mode buffers
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  ;; disable company too
  (setq company-global-modes '(not org-mode)))
