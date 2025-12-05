;; alias iota='emacs -nw --init-directory ~/Development/iota.d'

;;; ============================================================================
;;; Bootstrap & Core Infrastructure
;;; ============================================================================

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

;; Use iota- prefix for all generated files/directories (must be set before package-initialize)
(setq custom-file (locate-user-emacs-file "iota-custom.el"))
(setq package-user-dir (locate-user-emacs-file "iota-elpa"))
(setq auto-save-list-file-prefix (locate-user-emacs-file "iota-auto-save-list/.saves-"))
(setq savehist-file (locate-user-emacs-file "iota-history"))
(setq project-list-file (locate-user-emacs-file "iota-projects"))
(setq transient-history-file (locate-user-emacs-file "iota-transient/history.el"))
(setq transient-levels-file (locate-user-emacs-file "iota-transient/levels.el"))
(setq transient-values-file (locate-user-emacs-file "iota-transient/values.el"))
(setq eshell-directory-history (locate-user-emacs-file "iota-eshell/"))
(setq recentf-save-file (locate-user-emacs-file "iota-recentf"))
(setq save-place-file (locate-user-emacs-file "iota-places"))
(setq denote-directory (locate-user-emacs-file "iota-silo"))
(setopt use-short-answers t)

;; Initialize package system
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-install-upgrade-built-in t)
(package-initialize)

;; Setup use-package
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t) ; Show more details for debugging

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

  ;; Custom faces for better visibility
  (custom-set-faces
   '(keycast-command ((t (:inherit font-lock-keyword-face :height 0.9))))
   '(keycast-key ((t (:inherit custom-modified :height 1.1 :weight bold)))))

  ;; Enable keycast-mode by default
  (keycast-mode 1))

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

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

;;; ============================================================================
;;; Version Control & Projects
;;; ============================================================================

(use-package magit
  :defer t
  :general
  (:prefix "C-c v"
   "" '(:ignore t :which-key "magit")
   "v" 'magit-status
   "l" 'magit-log-current
   "b" 'magit-blame
   "d" 'magit-diff
   "s" 'magit-stage-file
   "c" 'magit-commit
   "p" 'magit-push
   "P" 'magit-pull)
  :config
  (setq magit-display-buffer-function
        (lambda (buffer) (display-buffer buffer '(display-buffer-same-window)))))

(use-package project
  :demand t
  :general
  ("C-c p" '(:keymap project-prefix-map :which-key "project")))

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
   "q" 'markdown-insert-blockquote)
  :init
  (setq markdown-add-border t)
  (setq markdown-header-scaling t))

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
  (:prefix "C-c a"
   "" '(:ignore t :which-key "ai")
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
  (setq gptel-model 'claude-sonnet-4.5)

  ;; Tool definitions using gptel-make-tool
  (gptel-make-tool
   :name "edit_buffer"
   :description "Edit the current buffer by replacing text. Use for search-and-replace operations."
   :function (lambda (search replace)
               (let ((case-fold-search nil))  ; Case-sensitive search
                 (message "[gptel-tool] edit_buffer called with search='%s' replace='%s'" search replace)
                 (save-excursion
                   (goto-char (point-min))
                   (if (search-forward search nil t)
                       (progn
                         (replace-match replace t t)  ; literal replacement
                         (message "[gptel-tool] Replacement successful")
                         (format "✓ Replaced text successfully"))
                     (message "[gptel-tool] Text not found")
                     (format "✗ Text not found. Buffer size: %d chars. Search string length: %d"
                             (buffer-size) (length search))))))
   :args (list '(:name "search"
                 :type string
                 :description "Exact text to search for (case-sensitive)")
               '(:name "replace"
                 :type string
                 :description "Text to replace with")))

  (gptel-make-tool
   :name "insert_at_position"
   :description "Insert text at a specific position in the buffer. Use 'start' for beginning, 'end' for end, or a line number."
   :function (lambda (position text)
               (message "[gptel-tool] insert_at_position called with position='%s'" position)
               (save-excursion
                 (cond
                  ((string= position "start")
                   (goto-char (point-min))
                   (insert text)
                   (format "✓ Inserted %d chars at start of buffer" (length text)))
                  ((string= position "end")
                   (goto-char (point-max))
                   (insert text)
                   (format "✓ Inserted %d chars at end of buffer" (length text)))
                  ((string-match "^[0-9]+$" position)
                   (let ((line-num (string-to-number position)))
                     (goto-char (point-min))
                     (forward-line (1- line-num))
                     (insert text)
                     (format "✓ Inserted %d chars at line %d" (length text) line-num)))
                  (t (format "✗ Invalid position: %s. Use 'start', 'end', or a line number" position)))))
   :args (list '(:name "position"
                 :type string
                 :description "Where to insert: 'start', 'end', or a line number")
               '(:name "text"
                 :type string
                 :description "Text to insert")))

  (gptel-make-tool
   :name "read_buffer_region"
   :description "Read a portion of the current buffer. Use to check content before editing."
   :function (lambda (start-pos end-pos)
               (message "[gptel-tool] read_buffer_region called with start='%s' end='%s'" start-pos end-pos)
               (let ((start (cond
                             ((string= start-pos "start") (point-min))
                             ((string-match "^[0-9]+$" start-pos)
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line (1- (string-to-number start-pos)))
                                (point)))
                             (t (point-min))))
                     (end (cond
                           ((string= end-pos "end") (point-max))
                           ((string-match "^[0-9]+$" end-pos)
                            (save-excursion
                              (goto-char (point-min))
                              (forward-line (string-to-number end-pos))
                              (point)))
                           (t (point-max)))))
                 (let ((content (buffer-substring-no-properties start end)))
                   (format "Content (%d chars):\n%s" (length content) content))))
   :args (list '(:name "start_pos"
                 :type string
                 :description "Start position: 'start' or line number")
               '(:name "end_pos"
                 :type string
                 :description "End position: 'end' or line number")))

  (gptel-make-tool
   :name "create_file"
   :description "Create a new file with content, optionally in current project"
   :function (lambda (filename content)
               (let* ((project (project-current))
                      (filepath (if (and project (not (file-name-absolute-p filename)))
                                    (expand-file-name filename (project-root project))
                                  (expand-file-name filename))))
                 (condition-case err
                     (progn
                       (make-directory (file-name-directory filepath) t)
                       (with-temp-file filepath
                         (insert content))
                       (find-file filepath)
                       (format "Created file: %s" filepath))
                   (error (format "Failed to create file: %s" (error-message-string err))))))
   :args (list '(:name "filename"
                 :type string
                 :description "File path relative to project root or absolute")
               '(:name "content"
                 :type string
                 :description "File content")))

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
   :args nil)

  (gptel-make-tool
   :name "list_project_files"
   :description "List files in the current project matching a glob pattern (e.g., '*.el', '**/*.py')"
   :function (lambda (pattern)
               (let ((project (project-current)))
                 (if project
                     (let* ((root (project-root project))
                            (files (project-files project))
                            (matches (seq-filter
                                      (lambda (f)
                                        (string-match-p
                                         (wildcard-to-regexp pattern)
                                         (file-relative-name f root)))
                                      files)))
                       (if matches
                           (mapconcat (lambda (f) (file-relative-name f root))
                                      matches "\n")
                         (format "No files matching '%s'" pattern)))
                   "No project found")))
   :args (list '(:name "pattern"
                 :type string
                 :description "Glob pattern to match files against")))

  ;; Set the tools to use
  (setq gptel-tools (gptel-get-tool "misc")))

;; Copilot configuration with robust error handling
;; Wrapped in with-demoted-errors to prevent bootstrap failures
(with-demoted-errors "Copilot setup error: %S"
  (use-package copilot
    :ensure nil  ; Using :vc instead of :ensure
    :vc (:url "https://github.com/copilot-emacs/copilot.el"
         :rev :newest
         :branch "main")
    :defer t
    :defines (copilot-completion-map copilot-disable-predicates)
    :functions (copilot-mode)
    :custom
    (copilot-idle-delay 0.2)
    (copilot-indent-offset-warning-disable t)
    (copilot-install-dir (locate-user-emacs-file "iota-cache/copilot"))
    :init
    ;; Add hook with error handling to prevent bootstrap failures
    (defun iota/enable-copilot-maybe ()
      "Enable copilot-mode if available, with error handling."
      (when (and (fboundp 'copilot-mode)
                 (not (bound-and-true-p copilot-mode)))
        (condition-case err
            (copilot-mode 1)
          (error (message "Copilot activation failed: %s" err)))))

    (add-hook 'prog-mode-hook #'iota/enable-copilot-maybe)
    :config
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

;; Load a random dark theme at startup (after init completes)
(add-hook 'after-init-hook #'iota/load-random-dark-theme)

;; me-likey themes:
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
  (iota-popup-mode 1)
  (iota-dimmer-mode 1)
  (iota-window-mode 1) ;; automatic in iota

  :hook
  (emacs-startup-hook . iota-splash-screen))
