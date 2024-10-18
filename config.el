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
(setq display-line-numbers-type 'relative)

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

;; Enable true fullscreen mode on startup
(add-hook 'window-setup-hook #'toggle-frame-maximized)

(setq confirm-kill-emacs nil)

(setq-default line-spacing 2)

(beacon-mode 1)

;; (setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 16))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq delete-selection-mode t)

(setq-default flyspell-mode nil)

(setq dired-kill-when-opening-new-dired-buffer t)

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(after! org-noter
  (setq org-noter-always-create-frame nil
        org-noter-highlight-selected t))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Truncate lines"          "t" #'toggle-truncate-lines))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Comment/Uncomment lines"          "/" #'comment-line))

(xterm-mouse-mode 1)

;; Enable cursor blinking indefinitely
(setq blink-cursor-mode t
      blink-cursor-interval 0.5  ; Time (in seconds) between blinks
      blink-cursor-delay 0.0)    ; No delay before blinking starts
(blink-cursor-mode 1)          ; Activate cursor blinking

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("ↂ" "۞" "㈣" "◉" "○" "◆" "✜" "✸" ))
  (org-ellipsis "⤋")
  :hook (org-mode . org-bullets-mode))

(after! org
  (add-hook 'org-mode-hook #'org-num-mode))

(add-hook 'org-mode-hook 'org-fragtog-mode)

(setq org-hide-emphasis-markers t)

(setq org-startup-with-latex-preview t)

(setq org-export-in-background t)

(setq org-image-actual-width (/ (display-pixel-width) 3))

(setq org-table-default-attributes
      (list
       '(:align 'center)
       '(:valign 'center)
       '(:hlines nil)))

;; Set global window splitting preferences to always split vertically
(after! org
  (setq org-src-window-setup 'split-window-right)
  (set-popup-rule! "^\\*Org Src" :ignore t))

(load! "~/.config/doom/emacs_modes/abaqus.el")
(add-hook 'abaqus-mode-hook 'turn-on-font-lock)
(autoload 'abaqus-mode "abaqus" "Enter abaqus mode." t)

(load! "~/.config/doom/emacs_modes/lsdyna.el")
(add-hook 'lsdyna-mode-hook 'turn-on-font-lock)
(autoload 'lsdyna-mode "lsdyna" "Enter lsdyna mode." t)

(defadvice org-babel-tangle-jump-to-org (after recenter activate)
  (recenter))

  (setq org-export-preserve-breaks t)

(after! org
  ;; disable auto-complete in org-mode buffers
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  ;; disable company too
  (setq company-global-modes '(not org-mode)))

(defun toggle-evil-mode ()
  "Toggle evil-mode on and off."
  (interactive) ; Make the function callable via M-x and keybindings
  (if (bound-and-true-p evil-mode)
      (progn
        (evil-mode -1)
        (message "Evil mode disabled"))
    (evil-mode 1)
    (message "Evil mode enabled")))
(global-set-key (kbd "<f2>") #'toggle-evil-mode)

(after! tramp
  (setq vc-handled-backends nil)
  (setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
  (setq tramp-verbose 1)
  (setq remote-file-name-inhibit-locks t)
  (setq tramp-default-method "rsync")
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq projectile--mode-line nil)
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-cache-inodes t)
  (setq tramp-completion-reread-directory-timeout t)
  (setq debug-ignored-errors
      (cons 'remote-file-error debug-ignored-errors))
  )

(after! persp-mode
  (defun display-workspaces-in-minibuffer ()
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (insert (+workspace--tabline))))
  (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
  (+workspace/display))

(defun vasu/revert-and-center-last-line ()
  "Revert the current buffer, go to the last line, and center the view."
  (interactive) ; Makes the function callable through M-x and key bindings
  (revert-buffer :ignore-auto :noconfirm) ; Reverts the buffer without confirmation
  (goto-char (point-max)) ; Moves the cursor to the end of the buffer
  (recenter)) ; Centers the line in the window

(map! "<f5>" #'vasu/revert-and-center-last-line)

(add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
(add-hook 'gnuplot-mode-hook (lambda () (display-line-numbers-mode 1)))

(after! python
  (setq python-shell-interpreter "/home/vasu/.pyenv/versions/3.10.0/envs/common_3_10_0/bin/python3")
  (setq lsp-pyright-python-executable-cmd "/home/vasu/.pyenv/versions/3.10.0/envs/common_3_10_0/bin/python3"))

(after! dap-mode
  (require 'dap-python)
  (setq dap-python-executable "/home/vasu/.pyenv/versions/3.11.0/envs/common_3_11_0/bin/python3")
  (setq dap-python-debugger 'debugpy))

(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "s" #'dap-debug

      ;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e")
  (require 'llm-ollama)
  ;; language you want ellama to translate to
  (setopt ellama-language "English")
  ;; Set the default provider to use llama3.1:latest
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "llama3.1:latest"
           :embedding-model "nomic-embed-text"))
  ;; Add other models (mixtral and codellama) to the provider list
  (setopt ellama-providers
          '(("llama3.1" . (make-llm-ollama
                           :chat-model "llama3.1:latest"
                           :embedding-model "nomic-embed-text"))
            ("mixtral" . (make-llm-ollama
                          :chat-model "mixtral:latest"))
            ("codellama" . (make-llm-ollama
                            :chat-model "codellama:latest")))))
