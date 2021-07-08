;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "kmehtab"
      user-mail-address "kmehtab20@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:

(setq doom-font (font-spec :family "Fira Code" :size 15))
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
; wsl-copy
(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (deactivate-mark))

; wsl-paste
(defun wsl-paste ()
  (interactive)
  (let ((clipboard
     (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove Windows ^M characters
    (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
    (insert clipboard)))

;;To bind them to C-c C-c and C-c C-v

; Bind wsl-copy to C-c C-c
(global-set-key
 (kbd "C-c C-c")
 'wsl-copy)

; Bind wsl-paste to C-c C-v
(global-set-key
 (kbd "C-c C-v")
 'wsl-paste)

;;LSP settings

(defvar +lsp-defer-shutdown 3
  "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.
This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer, or when programmatically
killing and opening many LSP/eglot-powered buffers.")


;;
;;; Common

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)

(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not +lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max
            ;; DEPRECATED Remove check when 26 support is dropped
            (if (boundp 'read-process-output-max)
                (default-value 'read-process-output-max))
            +lsp--default-gcmh-high-cons-threshold
            (default-value 'gcmh-high-cons-threshold))
      ;; `read-process-output-max' is only available on recent development
      ;; builds of Emacs 27 and above.
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
      ;;        native JSON library, so we up the GC threshold to stave off
      ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its
      ;;        GC strategy, so we modify its variables rather than
      ;;        `gc-cons-threshold' directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))


;;
;;; Implementations

(if (featurep! +eglot)
    (load! "+eglot")
  (load! "+lsp"))

;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "package.json")
  (pushnew! projectile-globally-ignored-directories "node_modules" "flow-typed"))


;;
;;; Major modes

(dolist (feature '(rjsx-mode
                   typescript-mode
                   web-mode
                   (nodejs-repl-mode . nodejs-repl)))
  (let ((pkg  (or (cdr-safe feature) feature))
        (mode (or (car-safe feature) feature)))
    (with-eval-after-load pkg
      (set-docsets! mode "JavaScript"
        "AngularJS" "Backbone" "BackboneJS" "Bootstrap" "D3JS" "EmberJS" "Express"
        "ExtJS" "JQuery" "JQuery_Mobile" "JQuery_UI" "KnockoutJS" "Lo-Dash"
        "MarionetteJS" "MomentJS" "NodeJS" "PrototypeJS" "React" "RequireJS"
        "SailsJS" "UnderscoreJS" "VueJS" "ZeptoJS")
      (set-ligatures! mode
        ;; Functional
        :def "function"
        :lambda "() =>"
        :composition "compose"
        ;; Types
        :null "null"
        :true "true" :false "false"
        ;; Flow
        :not "!"
        :and "&&" :or "||"
        :for "for"
        :return "return"
        ;; Other
        :yield "import"))))


(use-package! rjsx-mode
  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"
  :interpreter "node"
  :hook (rjsx-mode . rainbow-delimiters-mode)
  :init
  ;; Parse node stack traces in the compilation buffer
  (after! compilation
    (add-to-list 'compilation-error-regexp-alist 'node)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
                        2 3 4)))
  :config
  (set-repl-handler! 'rjsx-mode #'+javascript/open-repl)
  (set-electric! 'rjsx-mode :chars '(?\} ?\) ?. ?:))

  (setq js-chain-indent t
        ;; These have become standard in the JS community
        js2-basic-offset 2
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-idle-timer-delay 0.15)

  (setq-hook! 'rjsx-mode-hook
    ;; Indent switch-case another step
    js-switch-indent-offset js2-basic-offset)

  (use-package! xref-js2
    :when (featurep! :tools lookup)
    :init
    (setq xref-js2-search-program 'rg)
    (set-lookup-handlers! 'rjsx-mode
      :xref-backend #'xref-js2-xref-backend))

  ;; HACK `rjsx-electric-gt' relies on js2's parser to tell it when the cursor
  ;;      is in a self-closing tag, so that it can insert a matching ending tag
  ;;      at point. The parser doesn't run immediately however, so a fast typist
  ;;      can outrun it, causing tags to stay unclosed, so force it to parse:
  (defadvice! +javascript-reparse-a (n)
    ;; if n != 1, rjsx-electric-gt calls rjsx-maybe-reparse itself
    :before #'rjsx-electric-gt
    (if (= n 1) (rjsx-maybe-reparse))))


(use-package! typescript-mode
  :hook (typescript-mode . rainbow-delimiters-mode)
  :hook (typescript-tsx-mode . rainbow-delimiters-mode)
  :commands typescript-tsx-mode
  :init
  ;; REVIEW We associate TSX files with `typescript-tsx-mode' derived from
  ;;        `web-mode' because `typescript-mode' does not officially support
  ;;        JSX/TSX. See emacs-typescript/typescript.el#4
  (add-to-list 'auto-mode-alist
               (cons "\\.tsx\\'"
                     (if (featurep! :lang web)
                         #'typescript-tsx-mode
                       #'typescript-mode)))

  (when (featurep! :checkers syntax)
    (after! flycheck
      (flycheck-add-mode 'javascript-eslint 'web-mode)
      (flycheck-add-mode 'javascript-eslint 'typescript-mode)
      (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
      (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
      (unless (featurep! +lsp)
        (after! tide
          (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) 'append)
          (flycheck-add-mode 'typescript-tide 'typescript-tsx-mode)))
      (add-hook! 'typescript-tsx-mode-hook
        (defun +javascript-disable-tide-checkers-h ()
          (pushnew! flycheck-disabled-checkers
                    'javascript-jshint
                    'tsx-tide
                    'jsx-tide)))))
  :config
  (when (fboundp 'web-mode)
    (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX"))

  (set-docsets! '(typescript-mode typescript-tsx-mode)
    :add "TypeScript" "AngularTS")
  (set-electric! '(typescript-mode typescript-tsx-mode)
    :chars '(?\} ?\))
    :words '("||" "&&"))
  ;; HACK Fixes comment continuation on newline
  (autoload 'js2-line-break "js2-mode" nil t)
  (setq-hook! 'typescript-mode-hook
    comment-line-break-function #'js2-line-break

    ;; Most projects use either eslint, prettier, .editorconfig, or tsf in order
    ;; to specify indent level and formatting. In the event that no
    ;; project-level config is specified (very rarely these days), the community
    ;; default is 2, not 4. However, respect what is in tsfmt.json if it is
    ;; present in the project
    typescript-indent-level
    (or (and (bound-and-true-p tide-mode)
             (plist-get (tide-tsfmt-options) :indentSize))
        typescript-indent-level)))


;;
;;; Tools

(add-hook! '(typescript-mode-local-vars-hook
             typescript-tsx-mode-local-vars-hook
             web-mode-local-vars-hook
             rjsx-mode-local-vars-hook)
  (defun +javascript-init-lsp-or-tide-maybe-h ()
    "Start `lsp' or `tide' in the current buffer.
LSP will be used if the +lsp flag is enabled for :lang javascript AND if the
current buffer represents a file in a project.
If LSP fails to start (e.g. no available server or project), then we fall back
to tide."
    (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
      (when (derived-mode-p 'js-mode 'typescript-mode 'typescript-tsx-mode)
        (if (null buffer-file-name)
            ;; necessary because `tide-setup' and `lsp' will error if not a
            ;; file-visiting buffer
            (add-hook 'after-save-hook #'+javascript-init-lsp-or-tide-maybe-h
                      nil 'local)
          (or (if (featurep! +lsp) (lsp!))
              ;; fall back to tide
              (if (executable-find "node")
                  (and (require 'tide nil t)
                       (progn (tide-setup) tide-mode))
                (ignore
                 (doom-log "Couldn't start tide because 'node' is missing"))))
          (remove-hook 'after-save-hook #'+javascript-init-lsp-or-tide-maybe-h
                       'local))))))


(use-package! tide
  :hook (tide-mode . tide-hl-identifier-mode)
  :config
  (set-company-backend! 'tide-mode 'company-tide)
  ;; navigation
  (set-lookup-handlers! 'tide-mode :async t
    :xref-backend #'xref-tide-xref-backend
    :documentation #'tide-documentation-at-point)
  (set-popup-rule! "^\\*tide-documentation" :quit t)

  (setq tide-completion-detailed t
        tide-always-show-documentation t
        ;; Fix #1792: by default, tide ignores payloads larger than 100kb. This
        ;; is too small for larger projects that produce long completion lists,
        ;; so we up it to 512kb.
        tide-server-max-response-length 524288
        ;; We'll handle it
        tide-completion-setup-company-backend nil)

  ;; Resolve to `doom-project-root' if `tide-project-root' fails
  (advice-add #'tide-project-root :override #'+javascript-tide-project-root-a)

  ;; Cleanup tsserver when no tide buffers are left
  (add-hook! 'tide-mode-hook
    (add-hook 'kill-buffer-hook #'+javascript-cleanup-tide-processes-h
              nil 'local))

  ;; Eldoc is activated too soon and disables itself, thinking there is no eldoc
  ;; support in the current buffer, so we must re-enable it later once eldoc
  ;; support exists. It is set *after* tide-mode is enabled, so enabling it on
  ;; `tide-mode-hook' is too early, so...
  (advice-add #'tide-setup :after #'eldoc-mode)

  (map! :localleader
        :map tide-mode-map
        "R"   #'tide-restart-server
        "f"   #'tide-format
        "rrs" #'tide-rename-symbol
        "roi" #'tide-organize-imports))


(use-package! js2-refactor
  :hook ((js2-mode rjsx-mode) . js2-refactor-mode)
  :init
  (map! :after js2-mode
        :map js2-mode-map
        :localleader
        (:prefix ("r" . "refactor")
          (:prefix ("a" . "add/arguments"))
          (:prefix ("b" . "barf"))
          (:prefix ("c" . "contract"))
          (:prefix ("d" . "debug"))
          (:prefix ("e" . "expand/extract"))
          (:prefix ("i" . "inject/inline/introduce"))
          (:prefix ("l" . "localize/log"))
          (:prefix ("o" . "organize"))
          (:prefix ("r" . "rename"))
          (:prefix ("s" . "slurp/split/string"))
          (:prefix ("t" . "toggle"))
          (:prefix ("u" . "unwrap"))
          (:prefix ("v" . "var"))
          (:prefix ("w" . "wrap"))
          (:prefix ("3" . "ternary"))))
  :config
  (when (featurep! :editor evil +everywhere)
    (add-hook 'js2-refactor-mode-hook #'evil-normalize-keymaps)
    (let ((js2-refactor-mode-map (evil-get-auxiliary-keymap js2-refactor-mode-map 'normal t t)))
      (js2r-add-keybindings-with-prefix (format "%s r" doom-localleader-key)))))


;;;###package skewer-mode
(map! :localleader
      (:after js2-mode
        :map js2-mode-map
        "S" #'+javascript/skewer-this-buffer
        :prefix ("s" . "skewer"))
      :prefix "s"
      (:after skewer-mode
        :map skewer-mode-map
        "E" #'skewer-eval-last-expression
        "e" #'skewer-eval-defun
        "f" #'skewer-load-buffer)

      (:after skewer-css
        :map skewer-css-mode-map
        "e" #'skewer-css-eval-current-declaration
        "r" #'skewer-css-eval-current-rule
        "b" #'skewer-css-eval-buffer
        "c" #'skewer-css-clear-all)

      (:after skewer-html
        :map skewer-html-mode-map
        "e" #'skewer-html-eval-tag))


;;;###package npm-mode
(use-package! npm-mode
  :hook ((js-mode typescript-mode) . npm-mode)
  :config
  (map! :localleader
        (:map npm-mode-keymap
          "n" npm-mode-command-keymap)
        (:after js2-mode
          :map js2-mode-map
          :prefix ("n" . "npm"))))


;;
;;; Projects


;;; tools/magit/config.el -*- lexical-binding: t; -*-

(defvar +magit-open-windows-in-direction 'right
  "What direction to open new windows from the status buffer.
For example, diffs and log buffers. Accepts `left', `right', `up', and `down'.")

(defvar +magit-fringe-size 14
  "Size of the fringe in magit-mode buffers.
Can be an integer or a cons cell whose CAR and CDR are integer widths for the
left and right fringe.
Only has an effect in GUI Emacs.")


;;
;;; Packages

(use-package! magit
  :commands magit-file-delete
  :defer-incrementally (dash f s with-editor git-commit package eieio lv transient)
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves further down
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  (setq transient-levels-file  (concat doom-etc-dir "transient/levels")
        transient-values-file  (concat doom-etc-dir "transient/values")
        transient-history-file (concat doom-etc-dir "transient/history"))
  :config
  (add-to-list 'doom-debug-variables 'magit-refresh-verbose)

  (setq transient-default-level 5
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)
  (add-hook 'magit-process-mode-hook #'goto-address-mode)

  (defadvice! +magit-revert-repo-buffers-deferred-a (&rest _)
    :after '(magit-checkout magit-branch-and-checkout)
    ;; Since the project likely now contains new files, best we undo the
    ;; projectile cache so it can be regenerated later.
    (projectile-invalidate-cache nil)
    ;; Use a more efficient strategy to auto-revert buffers whose git state has
    ;; changed: refresh the visible buffers immediately...
    (+magit-mark-stale-buffers-h))
  ;; ...then refresh the rest only when we switch to them, not all at once.
  (add-hook 'doom-switch-buffer-hook #'+magit-revert-buffer-maybe-h)

  ;; Center the target file, because it's poor UX to have it at the bottom of
  ;; the window after invoking `magit-status-here'.
  (advice-add #'magit-status-here :after #'doom-recenter-a)

  ;; The default location for git-credential-cache is in
  ;; ~/.cache/git/credential. However, if ~/.git-credential-cache/ exists, then
  ;; it is used instead. Magit seems to be hardcoded to use the latter, so here
  ;; we override it to have more correct behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (setq magit-credential-cache-daemon-socket
          (doom-glob (or (getenv "XDG_CACHE_HOME")
                         "~/.cache/")
                     "git/credential/socket")))

  ;; Prevent sudden window position resets when staging/unstaging/discarding/etc
  ;; hunks in `magit-status-mode' buffers. It's disorienting, especially on
  ;; larger projects.
  (defvar +magit--pos nil)
  (add-hook! 'magit-pre-refresh-hook
    (defun +magit--set-window-state-h ()
      (setq-local +magit--pos (list (current-buffer) (point) (window-start)))))
  (add-hook! 'magit-post-refresh-hook
    (defun +magit--restore-window-state-h ()
      (when (and +magit--pos (eq (current-buffer) (car +magit--pos)))
        (goto-char (cadr +magit--pos))
        (set-window-start nil (caddr +magit--pos) t)
        (kill-local-variable '+magit--pos))))

  ;; Magit uses `magit-display-buffer-traditional' to display windows, by
  ;; default, which is a little primitive. `+magit-display-buffer' marries
  ;; `magit-display-buffer-fullcolumn-most-v1' with
  ;; `magit-display-buffer-same-window-except-diff-v1', except:
  ;;
  ;; 1. Magit sub-buffers (like `magit-log') that aren't spawned from a status
  ;;    screen are opened as popups.
  ;; 2. The status screen isn't buried when viewing diffs or logs from the
  ;;    status screen.
  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'+magit-display-buffer-fn
        magit-bury-buffer-function #'magit-mode-quit-window)
  (set-popup-rule! "^\\(?:\\*magit\\|magit:\\| \\*transient\\*\\)" :ignore t)
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  ;; so magit buffers can be switched to (except for process buffers)
  (add-hook! 'doom-real-buffer-functions
    (defun +magit-buffer-p (buf)
      (with-current-buffer buf
        (and (derived-mode-p 'magit-mode)
             (not (eq major-mode 'magit-process-mode))))))

  ;; Clean up after magit by killing leftover magit buffers and reverting
  ;; affected buffers (or at least marking them as need-to-be-reverted).
  (define-key magit-mode-map "q" #'+magit/quit)
  (define-key magit-mode-map "Q" #'+magit/quit-all)

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one)

  (add-hook! 'magit-mode-hook
    (add-hook! 'window-configuration-change-hook :local
      (defun +magit-enlargen-fringe-h ()
        "Make fringe larger in magit."
        (and (display-graphic-p)
             (derived-mode-p 'magit-mode)
             +magit-fringe-size
             (let ((left  (or (car-safe +magit-fringe-size) +magit-fringe-size))
                   (right (or (cdr-safe +magit-fringe-size) +magit-fringe-size)))
               (set-window-fringes nil left right))))))

  ;; An optimization that particularly affects macOS and Windows users: by
  ;; resolving `magit-git-executable' Emacs does less work to find the
  ;; executable in your PATH, which is great because it is called so frequently.
  ;; However, absolute paths will break magit in TRAMP/remote projects if the
  ;; git executable isn't in the exact same location.
  (add-hook! 'magit-status-mode-hook
    (defun +magit-optimize-process-calls-h ()
      (when-let (path (executable-find magit-git-executable t))
        (setq-local magit-git-executable path))))

  (add-hook! 'magit-diff-visit-file-hook
    (defun +magit-reveal-point-if-invisible-h ()
      "Reveal the point if in an invisible region."
      (if (derived-mode-p 'org-mode)
          (org-reveal '(4))
        (require 'reveal)
        (reveal-post-command)))))


(use-package! forge
  :when (featurep! +forge)
  ;; We defer loading even further because forge's dependencies will try to
  ;; compile emacsql, which is a slow and blocking operation.
  :after-call magit-status
  :commands forge-create-pullreq forge-create-issue
  :preface
  (setq forge-database-file (concat doom-etc-dir "forge/forge-database.sqlite"))
  :config
  ;; All forge list modes are derived from `forge-topic-list-mode'
  (map! :map forge-topic-list-mode-map :n "q" #'kill-current-buffer)
  (set-popup-rule! "^\\*?[0-9]+:\\(?:new-\\|[0-9]+$\\)" :size 0.45 :modeline t :ttl 0 :quit nil)
  (set-popup-rule! "^\\*\\(?:[^/]+/[^ ]+ #[0-9]+\\*$\\|Issues\\|Pull-Requests\\|forge\\)" :ignore t)

  (defadvice! +magit--forge-get-repository-lazily-a (&rest _)
    "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents emacsql getting compiled, which appears to come out of the blue
and blocks Emacs for a short while."
    :before-while #'forge-get-repository
    (file-executable-p emacsql-sqlite-executable))

  (defadvice! +magit--forge-build-binary-lazily-a (&rest _)
    "Make `forge-dispatch' only build emacsql if necessary.
Annoyingly, the binary gets built as soon as Forge is loaded. Since we've
disabled that in `+magit--forge-get-repository-lazily-a', we must manually
ensure it is built when we actually use Forge."
    :before #'forge-dispatch
    (unless (file-executable-p emacsql-sqlite-executable)
      (emacsql-sqlite-compile 2)
      (if (not (file-executable-p emacsql-sqlite-executable))
          (message (concat "Failed to build emacsql; forge may not work correctly.\n"
                           "See *Compile-Log* buffer for details"))
        ;; HACK Due to changes upstream, forge doesn't initialize completely if
        ;;      it doesn't find `emacsql-sqlite-executable', so we have to do it
        ;;      manually after installing it.
        (setq forge--sqlite-available-p t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues   nil t)
        (after! forge-topic
          (dolist (hook forge-bug-reference-hooks)
            (add-hook hook #'forge-bug-reference-setup)))))))


(use-package! github-review
  :after magit
  :config
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" +magit/start-github-review))
  (after! forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" +magit/start-github-review))))


(use-package! magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil))


(use-package! magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))


(use-package! evil-collection-magit
  :when (featurep! :editor evil +everywhere)
  :defer t
  :init (defvar evil-collection-magit-use-z-for-folds t)
  :config
  ;; These numbered keys mask the numerical prefix keys. Since they've already
  ;; been replaced with z1, z2, z3, etc (and 0 with g=), there's no need to keep
  ;; them around:
  (undefine-key! magit-mode-map "M-1" "M-2" "M-3" "M-4" "1" "2" "3" "4" "0")

  ;; q is enough; ESC is way too easy for a vimmer to accidentally press,
  ;; especially when traversing modes in magit buffers.
  (evil-define-key* 'normal magit-status-mode-map [escape] nil)

  ;; Some extra vim-isms I thought were missing from upstream
  (evil-define-key* '(normal visual) magit-mode-map
    "%"  #'magit-gitflow-popup
    "zt" #'evil-scroll-line-to-top
    "zz" #'evil-scroll-line-to-center
    "zb" #'evil-scroll-line-to-bottom
    "g=" #'magit-diff-default-context
    "gi" #'forge-jump-to-issues
    "gm" #'forge-jump-to-pullreqs)

  ;; Fix these keybinds because they are blacklisted
  ;; REVIEW There must be a better way to exclude particular evil-collection
  ;;        modules from the blacklist.
  (map! (:map magit-mode-map
         :nv "q" #'+magit/quit
         :nv "Q" #'+magit/quit-all
         :nv "]" #'magit-section-forward-sibling
         :nv "[" #'magit-section-backward-sibling
         :nv "gr" #'magit-refresh
         :nv "gR" #'magit-refresh-all)
        (:map magit-status-mode-map
         :nv "gz" #'magit-refresh)
        (:map magit-diff-mode-map
         :nv "gd" #'magit-jump-to-diffstat-or-diff))

  ;; A more intuitive behavior for TAB in magit buffers:
  (define-key! 'normal
    (magit-status-mode-map
     magit-stash-mode-map
     magit-revision-mode-map
     magit-process-mode-map
     magit-diff-mode-map)
    [tab] #'magit-section-toggle)

  (after! git-rebase
    (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
      (when-let (desc (assoc (car key) evil-collection-magit-rebase-commands-w-descriptions))
        (setcar desc (cdr key))))
    (evil-define-key* evil-collection-magit-state git-rebase-mode-map
      "gj" #'git-rebase-move-line-down
      "gk" #'git-rebase-move-line-up))

  (after! magit-gitflow
    (transient-replace-suffix 'magit-dispatch 'magit-worktree
      '("%" "Gitflow" magit-gitflow-popup)))

  (transient-append-suffix 'magit-dispatch '(0 -1 -1)
    '("*" "Worktree" magit-worktree)))
(def-project-mode! +javascript-npm-mode
  :modes '(html-mode
           css-mode
           web-mode
           markdown-mode
           js-mode  ; includes js2-mode and rjsx-mode
           json-mode
           typescript-mode
           solidity-mode)
  :when (locate-dominating-file default-directory "package.json")
  :add-hooks '(add-node-modules-path npm-mode))

(def-project-mode! +javascript-gulp-mode
  :when (locate-dominating-file default-directory "gulpfile.js"))
