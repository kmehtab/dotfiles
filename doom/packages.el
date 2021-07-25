;; -*- no-byte-compile: t; -*-

(package! rotate :pin "4e9ac3ff800880bd9b705794ef0f7c99d72900a6")

(package! xkcd :pin "66e928706fd660cfdab204c98a347b49c4267bdf")

(package! selectric-mode :pin "1840de71f7414b7cd6ce425747c8e26a413233aa")

(package! wttrin :recipe (:local-repo "lisp/wttrin"))

(package! spray :pin "74d9dcfa2e8b38f96a43de9ab0eb13364300cb46")

(package! elcord :pin "6608e0392b46324fc09a5b5f4457c15ac1394f80")

(package! keycast :pin "a3a0798349adf3e33277091fa8dee63173b68edf")

(package! gif-screencast :pin "fa81e915c256271fa10b807a2935d5eaa4700dff")

(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
                           :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor"))
  :pin "784cf911bc96aac0f47d529e8cee96ebd7cc31c9")

(package! ess-view :pin "925cafd876e2cc37bc756bb7fcf3f34534b457e2")

;; (package! magit-delta :recipe (:host github :repo "dandavison/magit-delta") :pin "1164a6c3e501e944f1a6a2e91f15374a193bb8d3")

(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")

(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c" :disable t)

(package! lexic :recipe (:local-repo "lisp/lexic"))

(package! calibredb :pin "a3b04c0c37b1e8ceff2472e21a3579e64e944528")

(package! nov :pin "b3c7cc28e95fe25ce7b443e5f49e2e45360944a3")

(package! screenshot :recipe (:local-repo "lisp/screenshot"))

(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))

(package! pinentry)

(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets")
  :pin "3076cefea0f6ae9d7757f13c27b5602e007b58ec")
(package! laas :recipe (:local-repo "lisp/LaTeX-auto-activating-snippets"))

(package! auctex :pin "6440ec5964dcbe58155e28f00f84ec0118d8fb7b")

(package! org-mode :recipe (:host github :repo "emacs-straight/org-mode" :files ("*.el" "lisp/*.el") :pre-build (with-temp-file (doom-path (straight--repos-dir "org-mode") "org-version.el") (insert "(fset 'org-release (lambda () \"9.5\"))
" (format "(fset 'org-git-version (lambda () \"%s\"))
" (substring (shell-command-to-string "git rev-parse --short HEAD") 0 -1)) "(provide 'org-version)
")) :includes org) :pin nil)
(unpin! org-mode) ; there be bugs
(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"
           :files ("lisp/*.el"))
  :pin "b18928c973d1de7d05df73f454c7952317919a1c")

(package! org-super-agenda :pin "f5e80e4d0da6b2eeda9ba21e021838fa6a495376")

(package! doct
  :recipe (:host github :repo "progfolio/doct")
  :pin "67fc46c8a68989b932bce879fbaa62c6a2456a1f")

(package! org-pretty-table
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "87772a9469d91770f87bfa788580fca69b9e697a")

(package! org-fragtog :pin "0151cabc7aa9f244f82e682b87713b344d780c23")

(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "6ee49875f8bdefafbde849f5628d673e9740cf8c")

(package! org-pretty-tags :pin "5c7521651b35ae9a7d3add4a66ae8cc176ae1c76")

(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree")
  :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")

(package! engrave-faces :recipe (:local-repo "lisp/engrave-faces"))

(package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")

(package! org-ref :pin "3ca9beb744621f007d932deb8a4197467012c23a")

(package! ob-julia :recipe (:host github :repo "nico202/ob-julia" :files ("*.el" "julia")))

(package! org-transclusion :recipe (:host github :repo "nobiot/org-transclusion")
  :pin "daa18df6de26b74badab0372e8a64fbde6a7be71")

(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view") :pin "13314338d70d2c19511efccc491bed3ca0758170")

(package! org-chef :pin "5b461ed7d458cdcbff0af5013fbdbe88cbfb13a4")

(package! org-pandoc-import :recipe
  (:local-repo "lisp/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))

(package! org-roam-server :pin "2122a61e9e9be205355c7e2c1e4b65986d6985a5")

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")

(package! rjsx-mode :pin "b697fe4d92cc84fa99a7bcb476f815935ea0d919")
(package! typescript-mode :pin "1043025d42602d560949955410d3afa2562130ee")

(package! js2-refactor :pin "a0977c4ce1918cc266db9d6cd7a2ab63f3a76b9a")
(package! npm-mode :pin "3ee7c0bad5b7a041d4739ef3aaa06a3dc764e5eb")
(package! add-node-modules-path :pin "7d9be65b3be062842b7ead862dec15d6f25db4a2")

(package! nodejs-repl :pin "3b841055cad00f442e4a9159b1056f59411b6646")
(package! skewer-mode :pin "e5bed351939c92a1f788f78398583c2f83f1bb3c")

(package! tide :pin "ccff099e94beda9f5378ffc2b412cb4257111e8d")
(when (featurep! :tools lookup)
  (package! xref-js2 :pin "fd6b723e7f1f9793d189a815e1904364dc026b03"))

;; [[file:config.org::*EVIL][EVIL:2]]
(package! evil-escape :disable t)
;; EVIL:2 ends here

(package! org-roam :disable t)
