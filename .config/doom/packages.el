;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! evil-tutor)

;; -*- no-byte-compile: t; -*-
;;; os/macos/packages.el

(package! osx-trash :pin "0f1dc052d0a750b8c75f14530a4897f5d4324b4e")
(package! ns-auto-titlebar :pin "60273e764bf8d95abc40dd2fdc23af87ea9ee33b")


;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; Major modes
(package! coffee-mode :pin "35a41c7d8233eac0b267d9593e67fb8b6235e134")
(package! js2-mode :pin "29979e5f3301796ba606759e39ee0b1b6a2a24f3")
(package! rjsx-mode :pin "b697fe4d92cc84fa99a7bcb476f815935ea0d919")
(package! typescript-mode :pin "1043025d42602d560949955410d3afa2562130ee")

;; Tools
(package! js2-refactor :pin "a0977c4ce1918cc266db9d6cd7a2ab63f3a76b9a")
(package! npm-mode :pin "3ee7c0bad5b7a041d4739ef3aaa06a3dc764e5eb")
(package! add-node-modules-path :pin "7d9be65b3be062842b7ead862dec15d6f25db4a2")

;; Eval
(package! nodejs-repl :pin "3b841055cad00f442e4a9159b1056f59411b6646")
(package! skewer-mode :pin "e5bed351939c92a1f788f78398583c2f83f1bb3c")

;; Programming environment
(package! tide :pin "ad6fa78911d5d7e85c0851c0c1afc01f3cbde7c1")
(when (featurep! :tools lookup)
  (package! xref-js2 :pin "fd6b723e7f1f9793d189a815e1904364dc026b03"))


;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

;; +html.el
(package! emmet-mode :pin "1acb821e0142136344ccf40c1e5fb664d7db2e70")
(package! haml-mode :pin "bf5b6c11b1206759d2b28af48765e04882dd1fc4")
(package! pug-mode :pin "685fd3414d89736bf232f5d1a6bed9e0353b98fe")
(package! slim-mode :pin "3636d18ab1c8b316eea71c4732eb44743e2ded87")
(when (package! web-mode :pin "8ef47935d638902ba35a557cae5edd6ab6ab1346")
  (when (featurep! :completion company)
    (package! company-web :pin "f0cc9187c9c34f72ad71f5649a69c74f996bae9a")))

;; +css.el
(package! css-mode :built-in t)
(package! less-css-mode :built-in t :pin "c7fa3d56d83206b28657f2e56439dc62280a2bf2")

(package! sass-mode :pin "247a0d4b509f10b28e4687cd8763492bca03599b")
(package! stylus-mode :pin "4dbde92542fc7ad61df38776980905a4721d642e")
(package! sws-mode :pin "4dbde92542fc7ad61df38776980905a4721d642e")
(package! rainbow-mode :pin "949166cc0146bc9fabf74ce70c1c4a097f4cffd4")
(when (featurep! :completion ivy)
  (package! counsel-css :pin "f7647b4195b9b4e97f1ee1acede6054ae38df630"))
(when (featurep! :completion helm)
  (package! helm-css-scss :pin "48b996f73af1fef8d6e88a1c545d98f8c50b0cf3"))



;; -*- no-byte-compile: t; -*-
;;; tools/rgb/packages.el

(package! rainbow-mode :pin "949166cc0146bc9fabf74ce70c1c4a097f4cffd4")
(package! kurecolor :pin "3fc84840cbbd75e646cafa2fd3a00004b55e37ec")


;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(if (featurep! +eglot)
    (package! eglot :pin "5cc8df63d86a6c43134dd6e4e3ae26cfae14e66a")
  (package! lsp-mode :pin "9aa22de1b2424a44c8c4a3f9e03b3f9a39636a77")
  (package! lsp-ui :pin "9953a4857227ad83fb18bc295c8c12b1e4d29007")
  (when (featurep! :completion ivy)
    (package! lsp-ivy :pin "bccd86028e669f5a1cad78364775fe7a0741ff93"))
  (when (featurep! :completion helm)
    (package! helm-lsp :pin "c2c6974dadfac459b1a69a1217441283874cea92")))



;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "9b7b25f1e460f79de894f583727635134b7513a2")
  (when (featurep! +forge)
    (package! forge :pin "b4fd0666a4d3987fc41e08eda3f6b1db7b404697"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "78d24cf419138b543460f40509c8c1a168b52ca0")
  (package! github-review :pin "341b7a1352e4ee1f1119756360ac0714abbaf460"))



;; -*- no-byte-compile: t; -*-
;;; tools/pdf/packages.el

(package! pdf-tools :pin "5f77dae43eb8f71e52e10ba8cf994883f74c3fb7")
(package! saveplace-pdf-view :pin "54ed966b842501c3c092dbf57b372e37b033c578")


;; -*- no-byte-compile: t; -*-
;;; ui/treemacs/packages.el

(package! treemacs :pin "a6f9e9f1cea3502b3ead082fd208c4011a55add0")
;; These packages have no :pin because they're in the same repo
(when (featurep! :editor evil +everywhere)
  (package! treemacs-evil))
(package! treemacs-projectile)
(when (featurep! :tools magit)
  (package! treemacs-magit))
(when (featurep! :ui workspaces)
  (package! treemacs-persp))
(when (featurep! +lsp)
  (package! lsp-treemacs :pin "905cc74726438cf06d8ad7cabb2efae75aeb2359"))


;; -*- no-byte-compile: t; -*-
;;; ui/minimap/packages.el

(package! minimap :pin "ed7490652a676c0510ed57d5366c445de20a370b")


;;ORG MODE
;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

(package! org-mode
  :recipe (:host github
           ;; Install cutting-edge version of org-mode, and from a mirror,
           ;; because code.orgmode.org runs on a potato.
           :repo "emacs-straight/org-mode"
           :files ("*.el" "lisp/*.el" "contrib/lisp/*.el" "contrib/scripts")
           ;; HACK A necessary hack because org requires a compilation step
           ;;      after being cloned, and during that compilation a
           ;;      org-version.el is generated with these two functions, which
           ;;      return the output of a 'git describe ...'  call in the repo's
           ;;      root. Of course, this command won't work in a sparse clone,
           ;;      and more than that, initiating these compilation step is a
           ;;      hassle, so...
           :pre-build
           (with-temp-file (doom-path (straight--repos-dir "org-mode") "org-version.el")
             (insert "(fset 'org-release (lambda () \"9.5\"))\n"
                     "(fset 'org-git-version #'ignore)\n"
                     "(provide 'org-version)\n"))
           ;; Prevents built-in Org from sneaking into the byte-compilation of
           ;; `org-plus-contrib', and inform other packages that `org-mode'
           ;; satisfies the `org' dependency: raxod502/straight.el#352
           :includes (org org-plus-contrib))
  :pin "7a62a4d3251a512069aa06b0082529d61d22de26")

(package! avy)
(package! htmlize :pin "49205105898ba8993b5253beec55d8bddd820a70")
(package! org-yt
  :recipe (:host github :repo "TobiasZawada/org-yt")
  :pin "40cc1ac76d741055cbefa13860d9f070a7ade001")
(package! ox-clip :pin "2095537695135c7f1bc19db043925eb7d482907b")
(package! toc-org :pin "c4c61c5a382f94a3a4537e254243006dec2dcca4")
(package! org-cliplink :pin "13e0940b65d22bec34e2de4bc8cba1412a7abfbc")

(when (featurep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :pin "a9706da260c45b98601bcd72b1d2c0a24a017700"))
(when (featurep! :tools pdf)
  (package! org-pdftools :pin "a5b61bca3f8c91b0859bb0df1a929f9a31a57b99"))
(when (featurep! :tools magit)
  (package! orgit :pin "609fd0ccfb5268704b5bc7d7ac1014d4960b9707")
  (when (featurep! :tools magit +forge)
    (package! orgit-forge :pin "ea2a1cf9d337901b413e9df258b8e07af55c00f6")))
(when (featurep! +brain)
  (package! org-brain :pin "e9b9b3e5bb3c63cecb1367df49205c346d9c050a"))
(when (featurep! +dragndrop)
  (package! org-download :pin "947ca223643d28e189480e607df68449c15786cb"))
(when (featurep! +gnuplot)
  (package! gnuplot :pin "116cad8e09024223f97e81b0a4503cef20de9bf5")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (featurep! +jupyter)
  (package! jupyter :pin "6ce8d01e3a550a3268b415bf9d9b635d4dba5940"))
(when (featurep! +journal)
  (package! org-journal :pin "043bb9e26f75066dc1787cdc9265daca7a14dd4e"))
(when (featurep! +noter)
  (package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d"))
(when (featurep! +pomodoro)
  (package! org-pomodoro :pin "aa07c11318f91219336197e62c47bc7a3d090479"))
(when (featurep! +pretty)
  (package! org-superstar :pin "9d64c42e5029910153ec74cb9b5747b074281140")
  (package! org-fancy-priorities :pin "819bb993b71e7253cefef7047306ab4e0f9d0a86"))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "f50859941ab5c7cbeaee410f2d38716252b552ac")
  (package! org-tree-slide :pin "9d2ba1df456d8d7c6372c8c294dbe3ee81540b33")
  (package! org-re-reveal :pin "18a2456befcfda5f681b2b4041f3262f93e52cba")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "cf8e64bd8504737912b39e4153390cffbf443ed7"))
(when (featurep! +roam)
  (package! org-roam :pin "8ad57b121831eda8d226faa14ff2ba7ab652849c"))

;;; Babel
(package! ob-async :pin "de1cd6c93242a4cb8773bbe115b7be3d4dd6b97e")
(when (featurep! :lang crystal)
  (package! ob-crystal :pin "d84c1adee4b269cdba06a97caedb8071561a09af"))
(when (featurep! :lang elixir)
  (package! ob-elixir :pin "8990a8178b2f7bd93504a9ab136622aab6e82e32"))
(when (featurep! :lang go)
  (package! ob-go :pin "2067ed55f4c1d33a43cb3f6948609d240a8915f5"))
(when (featurep! :lang hy)
  (package! ob-hy :pin "a42ecaf440adc03e279afe43ee5ef6093ddd542a"))
(when (featurep! :lang nim)
  (package! ob-nim :pin "bf1642cb93f0a898804dc13fd9408d2964403bd2"))
(when (featurep! :lang racket)
  (package! ob-racket
    :recipe (:host github :repo "DEADB17/ob-racket")
    :pin "d8fd51bddb019b0eb68755255f88fc800cfe03cb"))
(when (featurep! :lang rest)
  (package! ob-restclient :pin "0ebfc7c5ebf96d2fe1a476439831363a5a43b9b6"))
(when (featurep! :lang scala)
  (package! ob-ammonite :pin "39937dff395e70aff76a4224fa49cf2ec6c57cca"))

;;; Export
(when (featurep! +pandoc)
  (package! ox-pandoc :pin "aa37dc7e94213d4ebedb85c384c1ba35007da18e"))
(when (featurep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "02140a294a8d0d15ca42a1956af794fd7ec18140"))
(when (featurep! :lang rst)
  (package! ox-rst :pin "99fa790da55b57a3f2e9aa187493ba434a64250e"))

;; -*- no-byte-compile: t; -*-
;;; ui/tabs/packages.el
(package! centaur-tabs :pin "9c7c936e4e1de6f4f4095d70e43c9ae738d05086")


;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "ee3177cdad47cbe92242eeb52c7bdb9505282db6")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(when (featurep! +childframe)
  (package! company-box :pin "aa5f09a5492344e3cc831f0f169a6a8345dec358"))
(package! mixed-pitch)
