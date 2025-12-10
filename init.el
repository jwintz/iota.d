;;; -*- no-byte-compile: t; -*-

;; alias iota='emacs -nw --init-directory ~/Development/iota.d'

;;; ============================================================================
;;; Bootstrap & Core Infrastructure
;;; ============================================================================

;; Disable UI chrome immediately (before any frame rendering)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-buffer-choice nil)

;; Optimize Garbage Collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216) ; 16mb
            (setq gc-cons-percentage 0.1)))

;; Suppress startup messages in echo area
(setq inhibit-message t)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq inhibit-message nil)))

;; (setq debug-on-error t)

;; Profiling: Uncomment to enable startup profiling
(require 'profiler)
;; (profiler-start 'cpu)
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (profiler-stop)
;;             (profiler-report)))

;; ============================================================================
;; IOTA Directory Structure & Bootstrap State
;; ============================================================================

;; Define all required directories upfront
(defvar iota--required-dirs
  '("iota-config"
    "iota-packages/elpa"
    "iota-tmp/auto-save-list"
    "iota-tmp/auto-save"
    "iota-data/transient"
    "iota-data/eshell"
    "iota-data/silo"
    "iota-cache/copilot"
    "iota-cache/gptel"
    "iota-cache/copilot-chat")
  "List of directories required by IOTA configuration.")

;; Pre-create all directories silently (no prompts!)
(dolist (dir iota--required-dirs)
  (let ((full-path (locate-user-emacs-file dir)))
    (unless (file-directory-p full-path)
      (make-directory full-path t))))

;; Check if this is a bootstrap run (first time setup)
(defvar iota--bootstrap-needed-p
  (not (file-exists-p (locate-user-emacs-file "iota-packages/elpa/archives/melpa/archive-contents")))
  "Non-nil if this is the first run and packages need to be installed.")

;; Bootstrap splash screen state
(defvar iota--bootstrap-buffer nil "Buffer used for bootstrap splash.")
(defvar iota--bootstrap-message "" "Current installation message.")
(defvar iota--bootstrap-saved-cursor-type nil "Saved cursor type before bootstrap.")
(defvar iota--bootstrap-saved-cursor-in-non-selected nil "Saved cursor in non-selected windows.")
(defvar iota--bootstrap-saved-visible-cursor nil "Saved visible-cursor.")

(defun iota--bootstrap-show-splash ()
  "Display centered bootstrap splash screen."
  (when iota--bootstrap-needed-p
    ;; Save current cursor state globally
    (setq iota--bootstrap-saved-cursor-type cursor-type)
    (setq iota--bootstrap-saved-cursor-in-non-selected cursor-in-non-selected-windows)
    (setq iota--bootstrap-saved-visible-cursor visible-cursor)
    ;; Hide cursor globally
    (setq cursor-type nil)
    (setq cursor-in-non-selected-windows nil)
    (setq visible-cursor nil)
    ;; Create or switch to bootstrap buffer
    (setq iota--bootstrap-buffer (get-buffer-create "*iota-bootstrap*"))
    (switch-to-buffer iota--bootstrap-buffer)
    (buffer-disable-undo)
    (setq-local mode-line-format nil)
    (setq-local header-line-format nil)
    (setq-local cursor-type nil)
    (setq-local cursor-in-non-selected-windows nil)
    (setq-local visible-cursor nil)
    ;; Move point to beginning
    (goto-char (point-min))
    ;; Hide cursor at window level (most reliable method)
    (internal-show-cursor (selected-window) nil)
    (setq buffer-read-only nil)
    (iota--bootstrap-update-splash "Initializing...")))

(defun iota--bootstrap-update-splash (message)
  "Update the bootstrap splash with MESSAGE."
  (setq iota--bootstrap-message message)
  (when (buffer-live-p iota--bootstrap-buffer)
    (with-current-buffer iota--bootstrap-buffer
      (let ((inhibit-read-only t)
            (window (get-buffer-window iota--bootstrap-buffer)))
        (erase-buffer)
        (let* ((width (window-width))
               (height (window-height))
               ;; Main title
               (title "ι • ο • τ • α")
               (title-len (length title))
               ;; Installation line
               (install-prefix "Installing ")
               (install-line (concat install-prefix message))
               (install-len (length install-line))
               ;; Calculate centering
               (vertical-padding (/ (- height 4) 2))
               (title-padding (max 0 (/ (- width title-len) 2)))
               (install-padding (max 0 (/ (- width install-len) 2))))
          ;; Vertical centering
          (dotimes (_ vertical-padding)
            (insert "\n"))
          ;; Title line
          (insert (make-string title-padding ?\s))
          (insert (propertize title 'face '(:weight bold :height 1.3)))
          (insert "\n\n")
          ;; Installation status line
          (insert (make-string install-padding ?\s))
          (insert (propertize install-prefix 'face 'font-lock-comment-face))
          (insert (propertize message 'face '(:slant italic))))
        ;; Ensure cursor remains hidden after redraw
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local visible-cursor nil)
        ;; Move point to beginning
        (goto-char (point-min))
        ;; Hide cursor at window level (most reliable method)
        (when window
          (internal-show-cursor window nil)))
      (redisplay t))))

(defun iota--bootstrap-teardown ()
  "Clean up bootstrap splash after installation completes."
  (when (buffer-live-p iota--bootstrap-buffer)
    (let ((window (get-buffer-window iota--bootstrap-buffer)))
      ;; Restore cursor at window level
      (when window
        (internal-show-cursor window t)))
    (kill-buffer iota--bootstrap-buffer))
  (setq iota--bootstrap-buffer nil)
  ;; Restore all cursor settings
  (when iota--bootstrap-saved-cursor-type
    (setq cursor-type iota--bootstrap-saved-cursor-type))
  (when iota--bootstrap-saved-cursor-in-non-selected
    (setq cursor-in-non-selected-windows iota--bootstrap-saved-cursor-in-non-selected))
  (when iota--bootstrap-saved-visible-cursor
    (setq visible-cursor iota--bootstrap-saved-visible-cursor))
  ;; Ensure cursor is shown in selected window
  (internal-show-cursor (selected-window) t))

;; Show splash immediately if bootstrap needed
(when iota--bootstrap-needed-p
  (iota--bootstrap-show-splash))

;; ============================================================================
;; Path Configuration
;; ============================================================================

;; Use iota- prefix with organized subdirectories (must be set before package-initialize)
(setq custom-file (locate-user-emacs-file "iota-config/custom.el"))
(setq package-user-dir (locate-user-emacs-file "iota-packages/elpa"))
(setq auto-save-list-file-prefix (locate-user-emacs-file "iota-tmp/auto-save-list/.saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,(locate-user-emacs-file "iota-tmp/auto-save/") t)))
(setq savehist-file (locate-user-emacs-file "iota-data/history"))
(setq project-list-file (locate-user-emacs-file "iota-data/projects"))
(setq transient-history-file (locate-user-emacs-file "iota-data/transient/history.el"))
(setq transient-levels-file (locate-user-emacs-file "iota-data/transient/levels.el"))
(setq transient-values-file (locate-user-emacs-file "iota-data/transient/values.el"))
(setq eshell-directory-name (locate-user-emacs-file "iota-data/eshell"))
(setq recentf-save-file (locate-user-emacs-file "iota-data/recentf"))
(setq save-place-file (locate-user-emacs-file "iota-data/places"))
(setq denote-directory (locate-user-emacs-file "iota-data/silo"))
(setopt use-short-answers t)

;; Suppress all interactive prompts during bootstrap
(when iota--bootstrap-needed-p
  ;; Auto-confirm VC package checkouts
  (setq package-vc-allow-side-effects t)
  ;; Named functions for advice (so they can be removed later)
  (defun iota--bootstrap-always-yes (&rest _) "Auto-confirm for bootstrap." t)
  ;; Suppress y-or-n queries during package installation
  (advice-add 'y-or-n-p :override #'iota--bootstrap-always-yes)
  (advice-add 'yes-or-no-p :override #'iota--bootstrap-always-yes))


;; Initialize package system
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-install-upgrade-built-in t)
(package-initialize)

;; Refresh package archives during bootstrap (after package-initialize)
(when iota--bootstrap-needed-p
  (iota--bootstrap-update-splash "package archives...")
  (package-refresh-contents))

;; Setup use-package
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t) ; Show more details for debugging

;; Hook into use-package to update bootstrap splash
(when iota--bootstrap-needed-p
  (defun iota--bootstrap-advice-use-package (orig-fn name &rest args)
    "Advice to update bootstrap splash when processing a package."
    (iota--bootstrap-update-splash (symbol-name name))
    (apply orig-fn name args))
  (advice-add 'use-package :around #'iota--bootstrap-advice-use-package)
  ;; Remove advice after init
  (add-hook 'after-init-hook
            (lambda ()
              (advice-remove 'use-package #'iota--bootstrap-advice-use-package))
            89))

;; Essential keybinding infrastructure
(use-package general
  :ensure t
  :demand t)

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

;;; ============================================================================
;;; Core Emacs Settings
;;; ============================================================================

(use-package emacs
  :ensure nil
  :init
  (when (version< emacs-version "30.1")
    (error "This configuration requires Emacs 30.1 or newer"))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (setq visible-cursor nil
        inhibit-startup-screen t
        make-backup-files nil
        custom-safe-themes t
        confirm-kill-emacs 'y-or-n-p
        show-paren-style 'parenthesis)
  (add-hook 'tty-setup-hook #'xterm-mouse-mode)
  (add-hook 'after-init-hook #'show-paren-mode)

  :config
  (set-face-attribute 'show-paren-match nil
                      :background 'unspecified
                      :foreground 'unspecified
                      :weight 'bold)

  ;; Prevent *Compile-Log* from popping up in windows
  (add-to-list 'display-buffer-alist
               '("\\*Compile-Log\\*"
                 (display-buffer-no-window)
                 (allow-no-window . t)))

  ;; Suppress warnings during startup
  (defun iota/suppress-startup-warnings (orig-fun &rest args)
    "Suppress non-critical warnings during startup."
    (let ((warning-minimum-level :error))
      (apply orig-fun args)))

  (advice-add 'display-warning :around #'iota/suppress-startup-warnings)

  ;; Remove the advice after startup completes
  (add-hook 'emacs-startup-hook
            (lambda ()
              (advice-remove 'display-warning #'iota/suppress-startup-warnings))
            99))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

(use-package treesit-auto
  :disabled t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;; ============================================================================
;;; Help & Documentation
;;; ============================================================================

(use-package helpful
  :general
  (:prefix "C-c h"
   "" '(:ignore t :which-key "help")
   "f" 'helpful-callable
   "v" 'helpful-variable
   "k" 'helpful-key
   "c" 'helpful-command
   "s" 'helpful-symbol
   "p" 'helpful-at-point
   "F" 'helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

(use-package elisp-refs
  :general
  (:prefix "C-c h r"
   "" '(:ignore t :which-key "elisp-refs")
   "f" 'elisp-refs-function
   "m" 'elisp-refs-macro
   "v" 'elisp-refs-variable
   "s" 'elisp-refs-special
   "r" 'elisp-refs-symbol)
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

;;; ============================================================================
;;; Modal Editing & Input
;;; ============================================================================

(use-package god-mode
  :ensure t
  :bind
  (("C-x C-1" . delete-other-windows)
   ("C-x C-2" . split-window-below)
   ("C-x C-3" . split-window-right)
   ("C-x C-0" . delete-window)
   ("C-x C-o" . other-window))
  :config
  (defun iota/god-mode-update-cursor-type ()
    "Change cursor shape based on god-mode state."
    (let ((new-cursor (if god-local-mode
                          'box
                        'bar)))
      (setq cursor-type new-cursor)
      ;; Force redisplay in terminal
      (when (not (display-graphic-p))
        (let ((escape-seq (if (eq new-cursor 'box)
                              "\033[2 q"  ;; steady block cursor
                            "\033[6 q")))  ;; steady bar cursor
          (send-string-to-terminal escape-seq)))))

  (defun iota/god-mode-restore-cursor ()
    "Restore cursor to bar on exit."
    (when (not (display-graphic-p))
      (send-string-to-terminal "\033[6 q")))

  (add-hook 'post-command-hook #'iota/god-mode-update-cursor-type)
  (add-hook 'buffer-list-update-hook #'iota/god-mode-update-cursor-type)
  (add-hook 'window-configuration-change-hook #'iota/god-mode-update-cursor-type)
  (add-hook 'kill-emacs-hook #'iota/god-mode-restore-cursor))

(use-package keycast
  :ensure t
  :config
  ;; Redefine keycast-mode to work with doom-modeline
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
      (progn
        (remove-hook 'pre-command-hook 'keycast--update)
        (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string)))))

  ;; Enable keycast-mode by default
  (keycast-mode 1))

;; Customize keycast faces - must be done after theme loads to override theme settings
(defun iota/setup-keycast-faces ()
  "Configure keycast faces to use semantic face inheritance."
  (set-face-attribute 'keycast-key nil
                      :inherit 'font-lock-keyword-face
                      :weight 'bold
                      :foreground 'unspecified
                      :background 'unspecified
                      :box nil)
  (set-face-attribute 'keycast-command nil
                      :inherit 'font-lock-builtin-face
                      :foreground 'unspecified
                      :background 'unspecified
                      :box nil))

;; Apply after theme loads
(add-hook 'after-init-hook #'iota/setup-keycast-faces 90)

;;; ============================================================================
;;; Window & Buffer Management
;;; ============================================================================

(use-package windmove
  :ensure nil
  :bind*
  (("C-x <left>" . windmove-left)
   ("C-x <right>" . windmove-right)
   ("C-x <up>" . windmove-up)
   ("C-x <down>" . windmove-down)))

(use-package transient
  :ensure nil)

;;; ============================================================================
;;; Completion & Navigation Framework
;;; ============================================================================

(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-count 8
        vertico-resize nil))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :general
  (:prefix "C-c c"
   "" '(:ignore t :which-key "consult")
   "b" 'consult-buffer
   "f" 'consult-find
   "r" 'consult-recent-file
   "p" 'consult-project-buffer
   "g" 'consult-grep
   "s" 'consult-ripgrep
   "l" 'consult-line
   "i" 'consult-imenu
   "I" 'consult-imenu-multi
   "o" 'consult-outline
   "m" 'consult-mark
   "k" 'consult-global-mark
   "y" 'consult-yank-pop
   "t" 'consult-theme)
  :bind
  (("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)
   ("M-y" . consult-yank-pop)
   ("M-g e" . consult-compile-error)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-s d" . consult-find)
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package consult-gh
  :ensure t
  :after (consult embark)
  :config
  (setq consult-gh-default-clone-directory "~/Development")

  (require 'consult-gh-transient)
  (require 'consult-gh-embark)
  (consult-gh-embark-mode +1))

(autoload 'consult-gh-transient "consult-gh-transient" "GitHub interaction via Transient" t)

;;  "g" 'consult-gh-transient)

;;; ============================================================================
;;; System Integration
;;; ============================================================================

(use-package clipetty
  :hook (after-init . global-clipetty-mode))

(use-package xclip
  :init (xclip-mode 1))

(use-package autorevert
  :ensure nil
  :init (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-interval 3)
  (auto-revert-verbose t))

;;; ============================================================================
;;; Terminal Emulation
;;; ============================================================================

(use-package eat
  :ensure t
  :general
  (:prefix "C-c s"
   "" '(:ignore t :which-key "shell")
   "s" 'eat
   "p" 'eat-project
   "e" 'eat-eshell-mode
   "v" 'eat-eshell-visual-command-mode)
  :custom
  ;; For `eat-eshell-mode'
  (eat-enable-directory-tracking t)
  (eat-enable-shell-prompt-annotation t)
  ;; Set the correct terminal type
  (eat-term-name "xterm-256color")
  ;; Kill the terminal process when the buffer is killed
  (eat-kill-process-on-exit t)
  :config
  ;; Disable god-mode in eat buffers to prevent keystroke duplication
  (add-hook 'eat-mode-hook
            (lambda ()
              (when (fboundp 'god-local-mode)
                (god-local-mode -1))))

  ;; Fix backspace in semi-char-mode while keeping Emacs keybindings (C-x, M-x, etc.)
  ;; Semi-char mode should be the default - it reserves C-x, C-c, M-x for Emacs
  (with-eval-after-load 'eat
    ;; Make backspace work in semi-char-mode by binding it explicitly
    (when (boundp 'eat-semi-char-mode-map)
      (define-key eat-semi-char-mode-map (kbd "DEL") 'eat-self-input)
      (define-key eat-semi-char-mode-map (kbd "<backspace>") 'eat-self-input)
      (define-key eat-semi-char-mode-map (kbd "C-?") 'eat-self-input)

      ;; Mode toggle keybindings
      (define-key eat-semi-char-mode-map (kbd "C-c C-k") 'eat-char-mode))

    (when (boundp 'eat-mode-map)
      (define-key eat-mode-map (kbd "C-c C-j") 'eat-semi-char-mode)))

  ;; Enable eat integration with eshell
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

;;; ============================================================================
;;; History & Persistence
;;; ============================================================================

(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-max-saved-items 200)
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :init (save-place-mode 1)
  :custom
  (save-place-limit 400))

;;; ============================================================================
;;; Editing Enhancements
;;; ============================================================================

(use-package completion-preview
  :ensure nil
  :hook (after-init . global-completion-preview-mode)
  :bind
  (:map completion-preview-active-mode-map
        ("M-n" . completion-preview-next-candidate)
        ("M-p" . completion-preview-prev-candidate)))

(use-package move-dup
  :ensure t
  :general
  (:prefix "C-c e"
   "" '(:ignore t :which-key "edit")
   "l" 'move-dup-duplicate-down
   "p" 'move-dup-move-lines-up
   "n" 'move-dup-move-lines-down))

(use-package stripspace
  :hook
  ((prog-mode . stripspace-local-mode)
   (text-mode . stripspace-local-mode)
   (conf-mode . stripspace-local-mode))
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t))

(use-package page-break-lines
  :config
  (add-to-list 'page-break-lines-modes 'special-mode)
  (global-page-break-lines-mode))

;;; ============================================================================
;;; Code Structure & Navigation
;;; ============================================================================

(use-package nerd-icons
  :ensure t)

(use-package outline
  :ensure nil
  :general
  (:prefix "C-c f"
   "" '(:ignore t :which-key "folding")
   "f" 'outline-toggle-children
   "a" 'outline-show-all
   "h" 'outline-hide-body
   "s" 'outline-show-entry
   "d" 'outline-hide-entry
   "n" 'outline-next-visible-heading
   "p" 'outline-previous-visible-heading
   "u" 'outline-up-heading
   "l" 'outline-hide-leaves)
  :hook
  ((prog-mode . outline-minor-mode)
   (outline-minor-mode
    .
    (lambda ()
      (require 'nerd-icons)
      (let* ((display-table (or buffer-display-table (make-display-table)))
             (face-offset (* (face-id 'shadow) (ash 1 22)))
             (chevron (concat " " (nerd-icons-codicon "nf-cod-chevron_down")))
             (value (vconcat (mapcar (lambda (c) (+ face-offset c)) chevron))))
        (set-display-table-slot display-table 'selective-display value)
        (setq buffer-display-table display-table))))))

(use-package outline-indent
  :after nerd-icons
  :custom
  (outline-indent-ellipsis (concat " " (nerd-icons-codicon "nf-cod-chevron_down")))
  :hook
  ((python-mode . outline-indent-minor-mode)
   (python-ts-mode . outline-indent-minor-mode)
   (yaml-mode . outline-indent-minor-mode)
   (yaml-ts-mode . outline-indent-minor-mode)
   (markdown-mode . outline-indent-minor-mode)))

;; (use-package paredit
;;   :hook (emacs-lisp-mode . paredit-mode)
;;   :config
;;   (define-key paredit-mode-map (kbd "RET") nil))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

;;; ============================================================================
;;; Version Control & Projects
;;; ============================================================================

(use-package magit
  :defer t
  :general
  (:prefix "C-c v"
   "" '(:ignore t :which-key "version")
   "v" 'magit-status
   "l" 'magit-log-current
   "b" 'magit-blame
   "d" 'magit-diff
   "s" 'magit-stage-file
   "c" 'magit-commit
   "p" 'magit-push
   "P" 'magit-pull
   "g" 'consult-gh-transient)
  :config
  (setq magit-display-buffer-function
        (lambda (buffer) (display-buffer buffer '(display-buffer-same-window))))

  ;; AI-powered commit message generation
  (defun iota/generate-commit-message ()
    "Generate a commit message using AI based on staged changes.
Uses gptel with the configured backend (Copilot) to analyze the diff
and generate a conventional commit message."
    (interactive)
    (unless (derived-mode-p 'git-commit-mode 'text-mode)
      (user-error "This command should be run in a git commit buffer"))
    ;; Ensure gptel is loaded
    (require 'gptel nil t)
    (let* ((default-directory (or (magit-toplevel) default-directory))
           (diff (shell-command-to-string "git diff --cached --no-color"))
           (prompt (format "Generate a concise git commit message for the following changes.
Follow the Conventional Commits format: <type>(<scope>): <description>

Types: feat, fix, docs, style, refactor, perf, test, build, ci, chore
- Keep the first line under 72 characters
- Use imperative mood (\"add\" not \"added\")
- Be specific but concise
- If there are multiple changes, summarize the main purpose

Diff:
%s

Generate ONLY the commit message, no explanations:" diff)))
      (if (string-empty-p (string-trim diff))
          (message "No staged changes to generate commit message for")
        ;; Show processing message
        (message "Generating commit message...")
        ;; Use gptel to generate the message
        (if (fboundp 'gptel-request)
            (gptel-request prompt
              :callback (lambda (response info)
                          (if (not response)
                              (message "Failed to generate commit message: %s" (plist-get info :status))
                            ;; Clean and insert the response
                            (let ((msg (string-trim response)))
                              ;; Remove markdown code blocks if present
                              (when (string-match "```\\(?:.*\n\\)?\\(\\(?:.\\|\n\\)*?\\)```" msg)
                                (setq msg (match-string 1 msg)))
                              (setq msg (string-trim msg))
                              ;; Insert at point
                              (goto-char (point-min))
                              (insert msg)
                              (message "Commit message generated")))))
          (message "gptel is not available - install and configure gptel first")))))

  ;; Bind in git-commit-mode
  (with-eval-after-load 'git-commit
    (define-key git-commit-mode-map (kbd "C-c C-g") #'iota/generate-commit-message))

  ;; Auto-refresh magit status buffer on file save
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package project
  :demand t
  :general
  ("C-c p" '(:keymap project-prefix-map :which-key "project"))
  :custom
  ;; Automatically remember projects
  (project-vc-merge-submodules nil)
  ;; Use magit instead of vc-dir when switching projects
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (project-find-regexp "Find regexp" ?g)
     (project-find-dir "Find directory" ?d)
     (project-vc-dir "VC-Dir" ?v)
     (project-shell "Shell" ?s)))
  :config
  ;; Replace vc-dir with magit-status
  (defun iota--project-magit-status ()
    "Run magit-status in the current project root."
    (interactive)
    (magit-status (project-root (project-current t))))
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (project-find-regexp "Find regexp" ?g)
          (project-find-dir "Find directory" ?d)
          (iota--project-magit-status "Magit" ?v)
          (project-shell "Shell" ?s)))
  ;; Automatically register projects when opening files
  (defun iota--project-remember-auto ()
    "Automatically remember project when opening a file in a git repo."
    (when-let ((project (project-current)))
      (project-remember-project project)))
  (add-hook 'find-file-hook #'iota--project-remember-auto))

;;; ============================================================================
;;; File Types & Markup
;;; ============================================================================

(use-package markdown-mode
  :mode (("README\\.md\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :general
  (:keymaps 'markdown-mode-map
   :prefix "C-c m"
   "" '(:ignore t :which-key "markdown")
   "l" 'markdown-insert-link
   "u" 'markdown-insert-uri
   "f" 'markdown-insert-footnote
   "w" 'markdown-insert-wiki-link
   "c" 'markdown-insert-code
   "C" 'markdown-insert-gfm-code-block
   "p" 'markdown-insert-pre
   "t" 'markdown-insert-table
   "h" 'markdown-insert-header-dwim
   "b" 'markdown-insert-bold
   "i" 'markdown-insert-italic
   "s" 'markdown-insert-strike-through
   "q" 'markdown-insert-blockquote))

;;; ============================================================================
;;; Knowledge Management
;;; ============================================================================

(use-package denote
  :ensure t
  :general
  (:prefix "C-c n"
   "" '(:ignore t :which-key "denote")
   "n" 'denote
   "N" 'denote-type
   "d" 'denote-date
   "t" 'denote-template
   "s" 'denote-subdirectory
   "S" 'denote-signature
   "f" 'denote-open-or-create
   "F" 'denote-open-or-create-with-command
   "r" 'denote-rename-file
   "R" 'denote-rename-file-using-front-matter
   "l" 'denote-link
   "L" 'denote-find-link
   "i" 'denote-link-or-create
   "b" 'denote-find-backlink
   "B" 'denote-show-backlinks-buffer
   "k" 'denote-keywords-add
   "K" 'denote-keywords-remove
   "g" 'denote-grep
   "D" 'denote-dired)
  :custom
  (denote-link-description-function 'denote-link-description-with-signature-and-title)
  :config
  (denote-rename-buffer-mode 1))

(use-package denote-silo
  :ensure t
  :after denote
  :config
  (defun iota/denote-silo-for-project ()
    "Set up Denote silo for the current project, or use default."
    (let* ((project (project-current nil))
           (silo-dir (if project
                         (expand-file-name "silo/" (project-root project))
                       (locate-user-emacs-file "iota-silo"))))
      (unless (file-directory-p silo-dir)
        (make-directory silo-dir t))
      (setq denote-directory silo-dir)
      silo-dir))

  ;; Only create silos when actually using denote commands, not on every file open
  (advice-add 'denote :before (lambda (&rest _) (iota/denote-silo-for-project)))
  (advice-add 'denote-open-or-create :before (lambda (&rest _) (iota/denote-silo-for-project))))

(use-package denote-markdown
  :ensure t
  :after denote
  :config
  (setq denote-file-type 'markdown-yaml)
  (setq denote-org-store-link-to-heading nil)
  (setq denote-link-button-action 'find-file)
  (setq markdown-enable-wiki-links t)
  (setq markdown-wiki-link-search-type '(project)))

;;; ============================================================================
;;; AI Assistance
;;; ============================================================================

(use-package gptel
  :ensure t
  :general
  (:prefix "C-c g"
   "" '(:ignore t :which-key "copilot")
   "g" 'gptel
   "s" 'gptel-send
   "m" 'gptel-menu)
  :custom
  (gptel-cache-directory (locate-user-emacs-file "iota-cache/gptel"))
  (gptel-gh-github-token-file (locate-user-emacs-file "iota-cache/copilot-chat/github-token"))
  (gptel-gh-token-file (locate-user-emacs-file "iota-cache/copilot-chat/token"))
  (gptel-use-tools t)
  (gptel-display-buffer-action '(display-buffer-same-window))
  :config
  ;; You may need to run M-x gptel-gh-login to authenticate
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  (setq gptel-model 'gpt-5-mini)

  ;; -- genesis backend configuration --
  ;; (setq gptel-backend (gptel-make-openai "Genesis"
  ;;      :host "genesis.inria.fr:6666"
  ;;      :stream t
  ;;      :protocol "http"
  ;;      :models '("mlx-community/Kimi-Dev-72B-4bit-DWQ")))
  ;; (setq gptel-model "mlx-community/Kimi-Dev-72B-4bit-DWQ")

  ;; Auto-scroll to bottom on new messages
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll))

;; llm-tool-collection for gptel
;; Install via package-vc if not available
(unless (package-installed-p 'llm-tool-collection)
  (package-vc-install '(llm-tool-collection :url "https://github.com/skissue/llm-tool-collection"
                                            :branch "main")))

(with-eval-after-load 'gptel
  (require 'llm-tool-collection)

  ;; Define custom tool
  (defvar iota--gptel-project-info-tool
    (gptel-make-tool
     :name "get_project_info"
     :description "Get information about the current project including root directory and current file"
     :function (lambda ()
                 (let ((project (project-current)))
                   (if project
                       (format "Project root: %s\nCurrent file: %s"
                               (project-root project)
                               (or buffer-file-name "no file"))
                     "No project found")))
     :args nil))

  ;; Combine llm-tool-collection tools with custom get_project_info
  (setq gptel-tools
        (append
         ;; All tools from llm-tool-collection
         (mapcar (apply-partially #'apply #'gptel-make-tool)
                 (llm-tool-collection-get-all))
         ;; Add custom get_project_info tool
         (list iota--gptel-project-info-tool))))

;; Copilot configuration with robust error handling
;; Wrapped in with-demoted-errors to prevent bootstrap failures
(with-demoted-errors "Copilot setup error: %S"
  (use-package copilot
    :ensure nil  ; Using :vc instead of :ensure
    :vc (:url "https://github.com/copilot-emacs/copilot.el"
         :rev :newest
         :branch "main")
    :defines (copilot-completion-map copilot-disable-predicates copilot--connection)
    :functions (copilot-mode copilot-installed-version iota/enable-copilot-safely)
    :custom
    (copilot-idle-delay 0.2)
    (copilot-indent-offset-warning-disable t)
    (copilot-install-dir (locate-user-emacs-file "iota-cache/copilot"))
    :hook ((prog-mode markdown-mode) . iota/enable-copilot-safely)
    :config
    ;; Safe wrapper to enable copilot with proper checks
    (defun iota/enable-copilot-safely ()
      "Enable copilot-mode with bootstrap and installation checks.
Only runs after package is loaded, so all copilot functions are available."
      (when (and (not (bound-and-true-p copilot-mode))
                 (not (bound-and-true-p iota--bootstrap-needed-p)))
        (condition-case err
            (when (and (fboundp 'copilot-installed-version)
                       (copilot-installed-version))
              (copilot-mode 1))
          (error nil))))  ; Silently ignore errors during startup
    ;; Suppress copilot warning messages
    (defun iota/copilot-suppress-warnings (orig-fun &rest args)
      "Suppress copilot warning messages about missing language server."
      (let ((inhibit-message t))
        (apply orig-fun args)))

    ;; Advice copilot functions that produce warnings
    (when (fboundp 'copilot--start-agent)
      (advice-add 'copilot--start-agent :around #'iota/copilot-suppress-warnings))

    ;; Keybindings - only set if the map exists
    (when (boundp 'copilot-completion-map)
      (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
      (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
      (define-key copilot-completion-map (kbd "M-f") 'copilot-accept-completion-by-word)
      (define-key copilot-completion-map (kbd "M-<right>") 'copilot-accept-completion-by-word)
      (define-key copilot-completion-map (kbd "C-e") 'copilot-accept-completion-by-line)
      (define-key copilot-completion-map (kbd "<end>") 'copilot-accept-completion-by-line)
      (define-key copilot-completion-map (kbd "M-n") 'copilot-next-completion)
      (define-key copilot-completion-map (kbd "M-p") 'copilot-previous-completion))

    ;; Prevent copilot from interfering with completion-preview-mode
    (when (boundp 'copilot-disable-predicates)
      (add-to-list 'copilot-disable-predicates
                   (lambda () (bound-and-true-p completion-preview-active-mode))))))

;;; ============================================================================
;;; Themes & Visual Appearance
;;; ============================================================================

(use-package standard-themes
  :ensure t
  :defer t)

(use-package doric-themes
  :ensure t
  :demand t)

(use-package ef-themes
  :ensure t
  :demand t)

(use-package modus-themes
  :ensure t
  :demand t
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  ;; Include Ef themes (and other derivatives) in Modus commands
  (modus-themes-include-derivatives-mode 1))

(defun iota/load-random-dark-theme ()
  "Load a random dark theme from Modus, Ef, or Doric collections."
  (interactive)
  (condition-case err
      (let ((choice (random 3)))
        (pcase choice
          (0 (when (fboundp 'modus-themes-load-random-dark)
               (modus-themes-load-random-dark)))
          (1 (when (fboundp 'ef-themes-load-random-dark)
               (ef-themes-load-random-dark)))
          (2 (when (fboundp 'doric-themes-load-random)
               (doric-themes-load-random 'dark)))))
    (error (message "Failed to load random dark theme: %s" err))))

(defun iota/load-random-light-theme ()
  "Load a random light theme from Modus, Ef, or Doric collections."
  (interactive)
  (condition-case err
      (let ((choice (random 3)))
        (pcase choice
          (0 (when (fboundp 'modus-themes-load-random-light)
               (modus-themes-load-random-light)))
          (1 (when (fboundp 'ef-themes-load-random-light)
               (ef-themes-load-random-light)))
          (2 (when (fboundp 'doric-themes-load-random)
               (doric-themes-load-random 'light)))))
    (error (message "Failed to load random light theme: %s" err))))

;; Theme keybindings
(general-define-key
 :prefix "C-c t"
 "" '(:ignore t :which-key "themes")
 "l" 'load-theme
 "d" 'disable-theme
 "m" 'modus-themes-select
 "r" 'iota/load-random-dark-theme
 "R" 'iota/load-random-light-theme)

;; Load random theme at startup (after init completes)
(add-hook 'after-init-hook #'iota/load-random-dark-theme)
;; ... or a specific one
;; (add-hook 'after-init-hook (lambda () (load-theme 'doric-dark t)))
;; ... me-likey themes:
;; - doric-dark
;; - doric-obsidian

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :config
  (doom-modeline-mode 1)
  :custom
  ;; Height and bar
  (doom-modeline-height 25)
  (doom-modeline-bar-width 4)
  (doom-modeline-hud nil)

  ;; Enable nerd-icons for rich visual feedback
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-time-icon t)
  (doom-modeline-buffer-encoding-icon t)

  ;; Modal editing state (god-mode, evil, etc.)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon t)
  (doom-modeline-modal-modern-icon t)

  ;; Buffer display
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)

  ;; Project integration (uses built-in project.el)
  (doom-modeline-project-detection 'project)

  ;; VCS/Git settings (integrates with magit)
  (doom-modeline-vcs-max-length 24)
  (doom-modeline-check-simple-format t)

  ;; Environment display
  (doom-modeline-env-version nil)  ; Don't show Python/Node versions
  (doom-modeline-buffer-encoding nil)  ; Usually utf-8, no need to show
  (doom-modeline-indent-info nil)

  ;; Minor modes
  (doom-modeline-minor-modes nil)  ; Keep modeline clean

  ;; Markdown support (you use markdown-mode)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Unicode fallback for any missing glyphs
  (doom-modeline-unicode-fallback t)

  ;; Performance
  (doom-modeline-checker-simple-format t))

;; Customize god-mode icon in modeline
;; Must be done before doom-modeline-segments loads (defsubst gets inlined)
(with-eval-after-load 'doom-modeline-core
  (defun doom-modeline--god ()
    "The current god state which is enabled by the command `god-mode'."
    (when (bound-and-true-p god-local-mode)
      (doom-modeline--modal-icon
       "<G>" 'doom-modeline-god "God mode"
       "nf-md-keyboard_outline" "🅖"))))

;; Force recompilation of modals segment after our override
(with-eval-after-load 'doom-modeline-segments
  (doom-modeline-def-segment modals
    "Displays modal editing states.

Including `evil', `overwrite', `god', `ryo' and `xha-fly-kyes', etc."
    (when doom-modeline-modal
      (let* ((evil (doom-modeline--evil))
             (ow (doom-modeline--overwrite))
             (god (doom-modeline--god))
             (ryo (doom-modeline--ryo))
             (xf (doom-modeline--xah-fly-keys))
             (boon (doom-modeline--boon))
             (meow (doom-modeline--meow))
             (vsep (doom-modeline-vspc))
             (sep (and (or evil ow god ryo xf boon meow) (doom-modeline-spc))))
        (concat sep
                (and evil (concat evil (and (or ow god ryo xf boon meow) vsep)))
                (and ow (concat ow (and (or god ryo xf boon meow) vsep)))
                (and god (concat god (and (or ryo xf boon meow) vsep)))
                (and ryo (concat ryo (and (or xf boon meow) vsep)))
                (and xf (concat xf (and (or boon meow) vsep)))
                (and boon (concat boon (and meow vsep)))
                meow
                sep)))))

;; Customize Copilot buffer icon
(with-eval-after-load 'nerd-icons
  ;; Add copilot icon for buffers named *Copilot*
  (add-to-list 'nerd-icons-regexp-icon-alist
               '("^\\*Copilot\\*$" nerd-icons-codicon "nf-cod-copilot" :face nerd-icons-blue)))

;; Hook to set copilot icon in modeline for *Copilot* buffer
(add-hook 'markdown-mode-hook
          (lambda ()
            (when (string= (buffer-name) "*Copilot*")
              (setq-local mode-name
                          (concat (nerd-icons-codicon "nf-cod-copilot" :face 'nerd-icons-blue) " Copilot")))))

;;; ============================================================================
;;; Custom/Local Packages
;;; ============================================================================

(use-package header2
  :vc (:url "https://github.com/emacsmirror/header2")
  :general
  (:prefix "C-c e"
   "m" 'iota/make-header
   "c" 'make-box-comment
   "d" 'make-divider)
  :config
  (add-hook 'before-save-hook 'auto-update-file-header)

  (setq make-header-hook '(
                          ;; header-mode-line
                          header-title
                          header-blank
                          ;; header-file-name
                          ;; header-description
                          ;; header-status
                          ;; header-author
                          ;; header-maintainer
                          ;; header-copyright
                          ;; header-creation-date
                          ;; header-rcs-id
                          ;; header-version
                          ;; header-pkg-requires
                          ;; header-sccs
                          ;; header-modification-date
                          ;; header-modification-author
                          ;; header-update-count
                          ;; header-url
                          ;; header-doc-url
                          ;; header-keywords
                          ;; header-compatibility
                          header-blank
                          ;; header-lib-requires
                          header-end-line
                          header-commentary
                          header-blank
                          header-blank
                          header-blank
                          header-end-line
                          header-history
                          header-blank
                          header-blank
                          ;; header-rcs-log
                          header-end-line
                          ;; header-free-software
                          header-code
                          header-eof))

  (defun iota/make-header ()
    "Insert or update file header using header2.
If a header already exists, update it. Otherwise, insert a new one."
    (interactive)
    (let ((header-exists nil))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               "\\(Author:\\|Created:\\|Last-Updated:\\|Modified:\\|Filename:\\|Description:\\)"
               (min (+ (point-min) 1000) (point-max)) t)
          (setq header-exists t)))
      (if header-exists
          (update-file-header)
        (make-header)))))

(use-package iota
  :ensure nil
  :demand t  ; Force immediate loading, not deferred
  :load-path "~/Development/iota/"
  :general
  ("C-c i" 'iota-dispatch)  ; Main dispatch at C-c i
  ;; Optional: direct access to sub-menus
  ;; (:prefix "C-c i"
  ;; 	   "c" 'iota-config-transient
  ;; 	   "s" 'iota-screens-transient
  ;; 	   "m" 'iota-modeline-transient
  ;; 	   "d" 'iota-dimmer-transient
  ;; 	   "t" 'iota-theme-transient
  ;; 	   "p" 'iota-popup-transient
  ;; 	   "w" 'iota-window-transient
  ;;  "?" 'iota-splash-transient)
  :custom
  (iota-footerline-show-for-minibuffer t) ;; default in iota
  (iota-footerline-show-for-which-key t) ;; default in iota
  (iota-footerline-show-for-warnings t) ;; default in iota
  (iota-prevent-line-numbers t) ;; default in iota
  (iota-splash-show-hints t) ;; default in iota
  (iota-window-divider-style 'hidden)

  ;; (iota-dimmer-fraction 0.30)           ; Default: 0.30
  (iota-dimmer-saturation-fraction 0.90)   ; nil = use iota-dimmer-fraction
  (iota-dimmer-luminance-fraction 0.30)    ; nil = use iota-dimmer-fraction

  :config
  (iota-modeline-mode 1) ;; automatic in iota
  (iota-modes-mode 1) 	 ;; automatic in iota
  (iota-popup-mode 1)    ;; automatic in iota
  (iota-dimmer-mode 1)   ;; *not* automatic in iota
  (iota-window-mode 1)   ;; automatic in iota

  ;; Show splash screen at startup (only when not bootstrapping)
  (unless (bound-and-true-p iota--bootstrap-needed-p)
    (add-hook 'emacs-startup-hook #'iota-splash-screen)))

;;; ============================================================================
;;; Bootstrap Finalization (MUST BE LAST)
;;; ============================================================================

(when iota--bootstrap-needed-p
  ;; Remove prompt suppression
  (when (fboundp 'iota--bootstrap-always-yes)
    (advice-remove 'y-or-n-p #'iota--bootstrap-always-yes)
    (advice-remove 'yes-or-no-p #'iota--bootstrap-always-yes))
  ;; Teardown bootstrap splash buffer
  (iota--bootstrap-teardown)
  ;; Reset bootstrap flag
  (setq iota--bootstrap-needed-p nil)
  ;; Schedule splash screen after frame is fully set up
  (add-hook 'window-setup-hook
            (lambda ()
              ;; Force=t bypasses file-buffer check (custom.el may be open)
              (iota-splash-screen t))))

;;; init.el ends here
