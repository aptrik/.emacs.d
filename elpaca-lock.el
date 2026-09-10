((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                        "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE"
                                  "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id ace-window
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "77115afc1b0b9f633084cf7479c767988106c196"))
 (aggressive-indent :source "elpaca-menu-lock-file" :recipe
                    (:package "aggressive-indent" :repo
                              "Malabarba/aggressive-indent-mode"
                              :fetcher github :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info"
                               "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                               "docs/dir" "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :id aggressive-indent
                              :type git :protocol https :inherit t
                              :depth treeless :ref
                              "a437a45868f94b77362c6b913c5ee8e67b273c42"))
 (aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
                ("aio.el" "README.md" "UNLICENSE") :source
                "elpaca-menu-lock-file" :id aio :type git :protocol
                https :inherit t :depth treeless :ref
                "0e94a06bb035953cbbb4242568b38ca15443ad4c"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE" "README*"
                           "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id avy :type git
                :protocol https :inherit t :depth treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (blacken :source "elpaca-menu-lock-file" :recipe
          (:package "blacken" :fetcher github :repo
                    "pythonic-emacs/blacken" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id blacken :type
                    git :protocol https :inherit t :depth treeless :ref
                    "52c412e8b14d6a41e80b8bf8fbbc72a5c21b87e7"))
 (bui :source "elpaca-menu-lock-file" :recipe
      (:package "bui" :repo "alezost/bui.el" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE" "README*"
                           "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id bui :type git
                :protocol https :inherit t :depth treeless :ref
                "4319e1bf3ff94ff0568eed280ac1f980a4b68679"))
 (cfrs :source "elpaca-menu-lock-file" :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE" "README*"
                            "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id cfrs :type git
                 :protocol https :inherit t :depth treeless :ref
                 "981bddb3fb9fd9c58aed182e352975bd10ad74c8"))
 (clipetty :source "elpaca-menu-lock-file" :recipe
           (:package "clipetty" :repo "spudlyo/clipetty" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id clipetty :type
                     git :protocol https :inherit t :depth treeless :ref
                     "01b39044b9b65fa4ea7d3166f8b1ffab6f740362"))
 (compat :source "elpaca-menu-lock-file" :recipe
         (:package "compat" :repo
                   ("https://github.com/emacs-compat/compat" . "compat")
                   :tar "31.0.0.2" :host gnu :files
                   ("*" (:exclude ".git")) :source
                   "elpaca-menu-lock-file" :id compat :type git
                   :protocol https :inherit t :depth treeless :ref
                   "f0787bca0f7eae45e51fa752b46e38f49e09137e"))
 (cond-let :source "elpaca-menu-lock-file" :recipe
           (:package "cond-let" :fetcher github :repo "tarsius/cond-let"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id cond-let :type
                     git :protocol https :inherit t :depth treeless :ref
                     "3b88187fe067d4ca3dec3ef8a329b0ce18bdb356"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id consult :type
                    git :protocol https :inherit t :depth treeless :ref
                    "1da8f21ad59d74d7202a463320eb46c1b640271a"))
 (consult-dir :source "elpaca-menu-lock-file" :recipe
              (:package "consult-dir" :fetcher github :repo
                        "karthink/consult-dir" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id consult-dir
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "1497b46d6f48da2d884296a1297e5ace1e050eb5"))
 (consult-lsp :source "elpaca-menu-lock-file" :recipe
              (:package "consult-lsp" :fetcher github :repo
                        "gagbo/consult-lsp" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id consult-lsp
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "f41a3946987a3880068f95f3725bbb7b0d4b0b22"))
 (consult-project-extra :source "elpaca-menu-lock-file" :recipe
                        (:package "consult-project-extra" :fetcher
                                  github :repo
                                  "Qkessler/consult-project-extra"
                                  :files
                                  ("*.el" "*.el.in" "dir" "*.info"
                                   "*.texi" "*.texinfo" "doc/dir"
                                   "doc/*.info" "doc/*.texi"
                                   "doc/*.texinfo" "lisp/*.el"
                                   "docs/dir" "docs/*.info"
                                   "docs/*.texi" "docs/*.texinfo"
                                   (:exclude ".dir-locals.el" "test.el"
                                             "tests.el" "*-test.el"
                                             "*-tests.el" "LICENSE"
                                             "README*" "*-pkg.el"))
                                  :source "elpaca-menu-lock-file" :id
                                  consult-project-extra :type git
                                  :protocol https :inherit t :depth
                                  treeless :ref
                                  "52c453b7f85dea90cc53ff8fba750795606415df"))
 (copilot :source "elpaca-menu-lock-file" :recipe
          (:package "copilot" :fetcher github :repo
                    "copilot-emacs/copilot.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id copilot :type
                    git :protocol https :inherit t :depth treeless :ref
                    "90f429b100418d897b673d6c36c2817364b28e3e"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files
                  (:defaults "extensions/corfu-*.el") :fetcher github
                  :source "elpaca-menu-lock-file" :id corfu :type git
                  :protocol https :inherit t :depth treeless :ref
                  "5869254349a035e16d656eb232a52cf0e163d531"))
 (dap-mode :source "elpaca-menu-lock-file" :recipe
           (:package "dap-mode" :repo "emacs-lsp/dap-mode" :fetcher
                     github :files (:defaults "icons") :source
                     "elpaca-menu-lock-file" :id dap-mode :type git
                     :protocol https :inherit t :depth treeless :ref
                     "7372c429031ad37adb88b42e4a2f4cfff246ce55"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "elpaca-menu-lock-file"
                 :id dash :type git :protocol https :inherit t :depth
                 treeless :ref
                 "d746dd9edcb67a108818beb0cdc78dc1cb466832"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id diff-hl :type git :protocol
                    https :inherit t :depth treeless :ref
                    "0e1d464b48172a1f1e2f36103d812c9fc6387c4e"))
 (diminish :source "elpaca-menu-lock-file" :recipe
           (:package "diminish" :fetcher github :repo
                     "myrjola/diminish.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id diminish :type
                     git :protocol https :inherit t :depth treeless :ref
                     "43d0b9e0d18e4d124e3f7dad9d73275fb678fa50"))
 (direnv :source "elpaca-menu-lock-file" :recipe
         (:package "direnv" :fetcher github :repo
                   "wbolster/emacs-direnv" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id direnv :type git
                   :protocol https :inherit t :depth treeless :ref
                   "c1f38f71184f8aa3130898932f2de638beb5ed33"))
 (docker :source "elpaca-menu-lock-file" :recipe
         (:package "docker" :fetcher github :repo "Silex/docker.el"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id docker :type git
                   :protocol https :inherit t :depth treeless :ref
                   "e476b1bf73e917aaae43daab763696629d864425"))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "dockerfile-mode" :fetcher github :repo
                            "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*"
                                       "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id
                            dockerfile-mode :type git :protocol https
                            :inherit t :depth treeless :ref
                            "97733ce074b1252c1270fd5e8a53d178b66668ed"))
 (dtrt-indent :source "elpaca-menu-lock-file" :recipe
              (:package "dtrt-indent" :fetcher github :repo
                        "jscheid/dtrt-indent" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id dtrt-indent
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "8402da6bcc288709366e0b589fa79e744e877788"))
 (eldoc-box :source "elpaca-menu-lock-file" :recipe
            (:package "eldoc-box" :repo "casouri/eldoc-box" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id eldoc-box
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "89900de72dca291d5914ffc5da701b3170704229"))
 (elisp-docstring-mode :source "elpaca-menu-lock-file" :recipe
                       (:package "elisp-docstring-mode" :fetcher github
                                 :repo "Fuco1/elisp-docstring-mode"
                                 :files
                                 ("*.el" "*.el.in" "dir" "*.info"
                                  "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi"
                                  "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                  "docs/*.info" "docs/*.texi"
                                  "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE"
                                            "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :id
                                 elisp-docstring-mode :type git
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "f512e509dd690f65133e55563ebbfd2dede5034f"))
 (elisp-slime-nav :source "elpaca-menu-lock-file" :recipe
                  (:package "elisp-slime-nav" :repo
                            "purcell/elisp-slime-nav" :fetcher github
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*"
                                       "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id
                            elisp-slime-nav :type git :protocol https
                            :inherit t :depth treeless :ref
                            "0b97339c552cac92788cab7c0724ca7ba160cd0e"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "5b0cbb19421ef20c140b46a7b1fb7d04240b53f6" :depth 1 :inherit
            ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build
            (:not elpaca-activate) :type git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git"
                               :files
                               ("extensions/elpaca-use-package.el")
                               :main "extensions/elpaca-use-package.el"
                               :build
                               (:not elpaca-source elpaca-build-docs)
                               :source "elpaca-menu-lock-file" :id
                               elpaca-use-package :type git :protocol
                               https :inherit t :depth treeless :ref
                               "5b0cbb19421ef20c140b46a7b1fb7d04240b53f6"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher github
                                 :repo "purcell/exec-path-from-shell"
                                 :files
                                 ("*.el" "*.el.in" "dir" "*.info"
                                  "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi"
                                  "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                  "docs/*.info" "docs/*.texi"
                                  "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE"
                                            "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :id
                                 exec-path-from-shell :type git
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "6146fdc16e9882df270be7e58ae8d628032d6bc4"))
 (expand-region :source "elpaca-menu-lock-file" :recipe
                (:package "expand-region" :repo
                          "magnars/expand-region.el" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id
                          expand-region :type git :protocol https
                          :inherit t :depth treeless :ref
                          "351279272330cae6cecea941b0033a8dd8bcc4e8"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id f :type git :protocol
              https :inherit t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id flycheck :type
                     git :protocol https :inherit t :depth treeless :ref
                     "9076d01a8a685bb0f03302e7bf3feaf41e6c34dc"))
 (flycheck-color-mode-line :source "elpaca-menu-lock-file" :recipe
                           (:package "flycheck-color-mode-line" :repo
                                     "flycheck/flycheck-color-mode-line"
                                     :fetcher github :files
                                     ("*.el" "*.el.in" "dir" "*.info"
                                      "*.texi" "*.texinfo" "doc/dir"
                                      "doc/*.info" "doc/*.texi"
                                      "doc/*.texinfo" "lisp/*.el"
                                      "docs/dir" "docs/*.info"
                                      "docs/*.texi" "docs/*.texinfo"
                                      (:exclude ".dir-locals.el"
                                                "test.el" "tests.el"
                                                "*-test.el" "*-tests.el"
                                                "LICENSE" "README*"
                                                "*-pkg.el"))
                                     :source "elpaca-menu-lock-file" :id
                                     flycheck-color-mode-line :build
                                     (:not autoloads) :type git
                                     :protocol https :inherit t :depth
                                     treeless :ref
                                     "df9be4c5bf26c4dc5ddaeed8179c4d66bdaa91f5"))
 (flycheck-golangci-lint :source "elpaca-menu-lock-file" :recipe
                         (:package "flycheck-golangci-lint" :repo
                                   "weijiangan/flycheck-golangci-lint"
                                   :fetcher github :files
                                   ("*.el" "*.el.in" "dir" "*.info"
                                    "*.texi" "*.texinfo" "doc/dir"
                                    "doc/*.info" "doc/*.texi"
                                    "doc/*.texinfo" "lisp/*.el"
                                    "docs/dir" "docs/*.info"
                                    "docs/*.texi" "docs/*.texinfo"
                                    (:exclude ".dir-locals.el" "test.el"
                                              "tests.el" "*-test.el"
                                              "*-tests.el" "LICENSE"
                                              "README*" "*-pkg.el"))
                                   :source "elpaca-menu-lock-file" :id
                                   flycheck-golangci-lint :type git
                                   :protocol https :inherit t :depth
                                   treeless :ref
                                   "51aede797df89eeea5928df05d0f619530339152"))
 (flycheck-yamllint :source "elpaca-menu-lock-file" :recipe
                    (:package "flycheck-yamllint" :repo
                              "krzysztof-magosa/flycheck-yamllint"
                              :fetcher github :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                               "*.texinfo" "doc/dir" "doc/*.info"
                               "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                               "docs/dir" "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file" :id
                              flycheck-yamllint :type git :protocol
                              https :inherit t :depth treeless :ref
                              "1e9fe3b2d3e42d551b94473816a8eeee637b446c"))
 (framemove :source "elpaca-menu-lock-file" :recipe
            (:source "elpaca-menu-lock-file" :package "framemove" :id
                     framemove :host github :repo
                     "emacsmirror/framemove" :type git :protocol https
                     :inherit t :depth treeless :ref
                     "0faa8a4937f398e4971fc877b1c294100506b645"))
 (fullframe :source "elpaca-menu-lock-file" :recipe
            (:package "fullframe" :fetcher sourcehut :repo
                      "tomterl/fullframe" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id fullframe
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "886b831c001b44ec95aec4ff36e8bc1b3003c786"))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo
                      "magit/git-modes" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id git-modes
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "f291a4cc4a8b02a25d5cf93b4ab6af29e6f060d9"))
 (glsl-mode :source "elpaca-menu-lock-file" :recipe
            (:package "glsl-mode" :repo "jimhourihan/glsl-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id glsl-mode
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "515a2ba4dab3ec89c83a962902a123ddf81e3cfe"))
 (go-mode :source "elpaca-menu-lock-file" :recipe
          (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher
                    github :files ("go-mode.el") :source
                    "elpaca-menu-lock-file" :id go-mode :type git
                    :protocol https :inherit t :depth treeless :ref
                    "3a71d28ab47df685e54ca6046a7a3dd3e28b682c"))
 (gradle-mode :source "elpaca-menu-lock-file" :recipe
              (:package "gradle-mode" :fetcher github :repo
                        "scubacabra/emacs-gradle-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id gradle-mode
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "e4d665d5784ecda7ddfba015f07c69be3cfc45f2"))
 (groovy-mode :source "elpaca-menu-lock-file" :recipe
              (:package "groovy-mode" :fetcher github :repo
                        "Groovy-Emacs-Modes/groovy-emacs-modes" :files
                        ("*groovy*.el") :source "elpaca-menu-lock-file"
                        :id groovy-mode :type git :protocol https
                        :inherit t :depth treeless :ref
                        "7b8520b2e2d3ab1d62b35c426e17ac25ed0120bb"))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id hcl-mode :type
                     git :protocol https :inherit t :depth treeless :ref
                     "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (highlight-symbol :source "elpaca-menu-lock-file" :recipe
                   (:package "highlight-symbol" :fetcher github :repo
                             "nschum/highlight-symbol.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*"
                                        "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id
                             highlight-symbol :type git :protocol https
                             :inherit t :depth treeless :ref
                             "7a789c779648c55b16e43278e51be5898c121b3a"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :id ht :type git
               :protocol https :inherit t :depth treeless :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (hydra :source "elpaca-menu-lock-file" :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source
                  "elpaca-menu-lock-file" :id hydra :type git :protocol
                  https :inherit t :depth treeless :ref
                  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (ibuffer-vc :source "elpaca-menu-lock-file" :recipe
             (:package "ibuffer-vc" :repo "purcell/ibuffer-vc" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                        "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE"
                                  "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id ibuffer-vc
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "bcdbbc0ea1b3368850fa71e853bb0806b993e8f0"))
 (idle-highlight-mode :source "elpaca-menu-lock-file" :recipe
                      (:package "idle-highlight-mode" :fetcher codeberg
                                :repo
                                "ideasman42/emacs-idle-highlight-mode"
                                :files
                                ("*.el" "*.el.in" "dir" "*.info"
                                 "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                 "docs/*.info" "docs/*.texi"
                                 "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el"
                                           "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "elpaca-menu-lock-file" :id
                                idle-highlight-mode :type git :protocol
                                https :inherit t :depth treeless :ref
                                "83aa002c837fc6c6e4ce7069c169875beeb1d41e"))
 (jinja2-mode :source "elpaca-menu-lock-file" :recipe
              (:package "jinja2-mode" :fetcher github :repo
                        "paradoxxxzero/jinja2-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id jinja2-mode
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1"))
 (json-mode :source "elpaca-menu-lock-file" :recipe
            (:package "json-mode" :fetcher github :repo
                      "json-emacs/json-mode" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id json-mode
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "466d5b563721bbeffac3f610aefaac15a39d90a9"))
 (json-reformat :source "elpaca-menu-lock-file" :recipe
                (:package "json-reformat" :fetcher github :repo
                          "gongo/json-reformat" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id
                          json-reformat :type git :protocol https
                          :inherit t :depth treeless :ref
                          "9120ab67c5379c44bc7a7a07ca858670cea4f32f"))
 (json-snatcher :source "elpaca-menu-lock-file" :recipe
                (:package "json-snatcher" :fetcher github :repo
                          "Sterlingg/json-snatcher" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id
                          json-snatcher :type git :protocol https
                          :inherit t :depth treeless :ref
                          "b28d1c0670636da6db508d03872d96ffddbc10f2"))
 (jwt :source "elpaca-menu-lock-file" :recipe
      (:package "jwt" :fetcher github :repo "joshbax189/jwt-el" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE" "README*"
                           "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id jwt :type git
                :protocol https :inherit t :depth treeless :ref
                "83bcef37c311645a07c0263007c5b962ca3dab15"))
 (k8s-mode :source "elpaca-menu-lock-file" :recipe
           (:package "k8s-mode" :fetcher github :repo
                     "TxGVNN/emacs-k8s-mode" :files
                     ("*.el" ("snippets/k8s-mode" "snippets/k8s-mode/*"))
                     :source "elpaca-menu-lock-file" :id k8s-mode :type
                     git :protocol https :inherit t :depth treeless :ref
                     "39a189d1e030aa108e90a82fd40f0042b1e69b21"))
 (kkp :source "elpaca-menu-lock-file" :recipe
      (:package "kkp" :fetcher github :repo "benotn/kkp" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE" "README*"
                           "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id kkp :type git
                :protocol https :inherit t :depth treeless :ref
                "82b7443e10a2ba287467b62e90b6adb6dd93dc99"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source
                  "elpaca-menu-lock-file" :id llama :type git :protocol
                  https :inherit t :depth treeless :ref
                  "cfea618f14bc8317f8e4947fe10000b229b9a447"))
 (lsp-docker :source "elpaca-menu-lock-file" :recipe
             (:package "lsp-docker" :repo "emacs-lsp/lsp-docker"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                        "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE"
                                  "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id lsp-docker
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "f666fba72b496c7750bb3f349771b07aa51714f0"))
 (lsp-java :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-java" :repo "emacs-lsp/lsp-java" :fetcher
                     github :files (:defaults "icons") :source
                     "elpaca-menu-lock-file" :id lsp-java :type git
                     :protocol https :inherit t :depth treeless :ref
                     "f17a3808ede477d34c0c64c23a558e95bb439710"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher
                     github :files (:defaults "clients/*.*") :source
                     "elpaca-menu-lock-file" :id lsp-mode :type git
                     :protocol https :inherit t :depth treeless :ref
                     "d0835388732fcd39e951173ac1f6792cdf346706"))
 (lsp-treemacs :source "elpaca-menu-lock-file" :recipe
               (:package "lsp-treemacs" :repo "emacs-lsp/lsp-treemacs"
                         :fetcher github :files (:defaults "icons")
                         :source "elpaca-menu-lock-file" :id
                         lsp-treemacs :type git :protocol https :inherit
                         t :depth treeless :ref
                         "3519ac907ea391e18d9599375b116aeeb6f8a38a"))
 (lsp-ui :source "elpaca-menu-lock-file" :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github
                   :files (:defaults "lsp-ui-doc.html" "resources")
                   :source "elpaca-menu-lock-file" :id lsp-ui :type git
                   :protocol https :inherit t :depth treeless :ref
                   "176eca71d1c5498ed6258b5b27d73293ff7cd7ed"))
 (lua-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github
                     :files (:defaults (:exclude "init-tryout.el"))
                     :source "elpaca-menu-lock-file" :id lua-mode :type
                     git :protocol https :inherit t :depth treeless :ref
                     "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el") :source "elpaca-menu-lock-file" :id lv :type
               git :protocol https :inherit t :depth treeless :ref
               "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("githooks" "githooks/*") ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :id magit :type git
                  :protocol https :inherit t :depth treeless :ref
                  "9cb07d820d2b9ebbe9940e4d493293522d4e21d2"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :id
                          magit-section :type git :protocol https
                          :inherit t :depth treeless :ref
                          "9cb07d820d2b9ebbe9940e4d493293522d4e21d2"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                        "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE"
                                  "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id marginalia
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "c5d0139012d2a84f8040219b9aee17db4e145e5c"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id
                          markdown-mode :type git :protocol https
                          :inherit t :depth treeless :ref
                          "76cb4ffecfdf95ee769e5cb4608e04202c3c1521"))
 (move-text :source "elpaca-menu-lock-file" :recipe
            (:package "move-text" :fetcher github :repo
                      "emacsfodder/move-text" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id move-text
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "142890cfb46d9c374113b4b49021a4202033147b"))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
                   (:package "multiple-cursors" :fetcher github :repo
                             "magnars/multiple-cursors.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*"
                                        "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id
                             multiple-cursors :type git :protocol https
                             :inherit t :depth treeless :ref
                             "94b8b07a4bab87f803123723b68227565429dfa1"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id orderless
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "5806e3f9401606d16962cffae68188c92deb1272"))
 (org-mindmap :source "elpaca-menu-lock-file" :recipe
              (:package "org-mindmap" :fetcher github :repo
                        "krvkir/org-mindmap" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id org-mindmap
                        :host github :type git :protocol https :inherit
                        t :depth treeless :ref
                        "128e8fb853718bb4a04aff29703b927276a8b24c"))
 (ox-pandoc :source "elpaca-menu-lock-file" :recipe
            (:package "ox-pandoc" :repo "emacsorphanage/ox-pandoc"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id ox-pandoc
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "1caeb56a4be26597319e7288edbc2cabada151b4"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
            (:package "pdf-tools" :fetcher github :repo
                      "vedang/pdf-tools" :files
                      (:defaults "README" ("build" "Makefile")
                                 ("build" "server"))
                      :source "elpaca-menu-lock-file" :id pdf-tools
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "365f88238f46f9b1425685562105881800f10386"))
 (pfuture :source "elpaca-menu-lock-file" :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture" :fetcher
                    github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id pfuture :type
                    git :protocol https :inherit t :depth treeless :ref
                    "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id posframe :type
                     git :protocol https :inherit t :depth treeless :ref
                     "435055dd6894fd4e8b21b355d40c0b289211b714"))
 (python-isort :source "elpaca-menu-lock-file" :recipe
               (:package "python-isort" :fetcher github :repo
                         "wyuenho/emacs-python-isort" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id
                         python-isort :type git :protocol https :inherit
                         t :depth treeless :ref
                         "8b4948b7fcad90fc9b72f69f4653260bd21f62c3"))
 (pyvenv :source "elpaca-menu-lock-file" :recipe
         (:package "pyvenv" :fetcher github :repo
                   "jorgenschaefer/pyvenv" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                    "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id pyvenv :type git
                   :protocol https :inherit t :depth treeless :ref
                   "31ea715f2164dd611e7fc77b26390ef3ca93509b"))
 (rainbow-mode :source "elpaca-menu-lock-file" :recipe
               (:package "rainbow-mode" :repo
                         ("https://github.com/emacsmirror/gnu_elpa"
                          . "rainbow-mode")
                         :tar "1.0.7" :host gnu :branch
                         "externals/rainbow-mode" :files
                         ("*" (:exclude ".git")) :source
                         "elpaca-menu-lock-file" :id rainbow-mode :type
                         git :protocol https :inherit t :depth treeless
                         :ref "9d333d3a92132c2a9057d2a39e1ebd8cc575a1bb"))
 (reformatter :source "elpaca-menu-lock-file" :recipe
              (:package "reformatter" :repo "purcell/emacs-reformatter"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id reformatter
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "2bd8818f3f2119a3876e574a437495214c87bc81"))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files
             ("request.el") :source "elpaca-menu-lock-file" :id request
             :type git :protocol https :inherit t :depth treeless :ref
             "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6"))
 (restclient :source "elpaca-menu-lock-file" :recipe
             (:package "restclient" :fetcher github :repo
                       "emacsorphanage/restclient" :files
                       ("restclient.el") :source "elpaca-menu-lock-file"
                       :id restclient :type git :protocol https :inherit
                       t :depth treeless :ref
                       "d280632df39a175dac06037038105e32945624be"))
 (rg :source "elpaca-menu-lock-file" :recipe
     (:package "rg" :fetcher github :repo "dajva/rg.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :id rg :type git
               :protocol https :inherit t :depth treeless :ref
               "6b00e2ae98c47cf7ea04d26636e11b7fa2a540e3"))
 (ruff-format :source "elpaca-menu-lock-file" :recipe
              (:package "ruff-format" :fetcher github :repo
                        "JoshHayes/emacs-ruff-format" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id ruff-format
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "063a5e703b070103f405a4cb090af47396cfb00b"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id s :type git :protocol
              https :inherit t :depth treeless :ref
              "d7c04b84d03481a1ed62ee13dbe595224ccbe57c"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo
                        "Fuco1/smartparens" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id smartparens
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "82d2cf084a19b0c2c3812e0550721f8a61996056"))
 (solarized-theme :source "elpaca-menu-lock-file" :recipe
                  (:package "solarized-theme" :repo
                            "bbatsov/solarized-emacs" :fetcher github
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*"
                                       "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id
                            solarized-theme :type git :protocol https
                            :inherit t :depth treeless :ref
                            "9df935bede27abcaa1b1ca9035ac17f39d2f64c9"))
 (sphinx-doc :source "elpaca-menu-lock-file" :recipe
             (:package "sphinx-doc" :fetcher github :repo
                       "naiquevin/sphinx-doc.el" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                        "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE"
                                  "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id sphinx-doc
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "1eda612a44ef027e5229895daa77db99a21b8801"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el"
                     . "spinner")
                    :tar "1.7.4" :host gnu :files
                    ("*" (:exclude ".git")) :source
                    "elpaca-menu-lock-file" :id spinner :type git
                    :protocol https :inherit t :depth treeless :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (tablist :source "elpaca-menu-lock-file" :recipe
          (:package "tablist" :fetcher github :repo
                    "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id tablist :type
                    git :protocol https :inherit t :depth treeless :ref
                    "01f065e387ffe6b7a41f180f257cd12551c7a9c2"))
 (terraform-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "terraform-mode" :repo
                           "hcl-emacs/terraform-mode" :fetcher github
                           :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :id
                           terraform-mode :type git :protocol https
                           :inherit t :depth treeless :ref
                           "01635df3625c0cec2bb4613a6f920b8569d41009"))
 (toml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "toml-mode" :fetcher github :repo
                      "dryman/toml-mode.el" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id toml-mode
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "f6c61817b00f9c4a3cab1bae9c309e0fc45cdd06"))
 (treemacs :source "elpaca-menu-lock-file" :recipe
           (:package "treemacs" :fetcher github :repo
                     "Alexander-Miller/treemacs" :files
                     (:defaults "Changelog.org" "icons"
                                "src/elisp/treemacs*.el"
                                "src/scripts/treemacs*.py"
                                (:exclude "src/extra/*"))
                     :source "elpaca-menu-lock-file" :id treemacs :type
                     git :protocol https :inherit t :depth treeless :ref
                     "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher
                    github :source "elpaca-menu-lock-file" :id vertico
                    :type git :protocol https :inherit t :depth treeless
                    :ref "8581ed12e9190005ea9afaef19f2a22951aa1bfb"))
 (vimrc-mode :source "elpaca-menu-lock-file" :recipe
             (:package "vimrc-mode" :fetcher github :repo
                       "mcandre/vimrc-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                        "doc/*.texinfo" "lisp/*.el" "docs/dir"
                        "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el"
                                  "*-test.el" "*-tests.el" "LICENSE"
                                  "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id vimrc-mode
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "f594392a0834193a1fe1522d007e1c8ce5b68e43"))
 (vlf :source "elpaca-menu-lock-file" :recipe
      (:package "vlf" :repo
                ("https://github.com/emacsmirror/gnu_elpa" . "vlf") :tar
                "1.7.2" :host gnu :branch "externals/vlf" :files
                ("*" (:exclude ".git")) :source "elpaca-menu-lock-file"
                :id vlf :type git :protocol https :inherit t :depth
                treeless :ref "6192573ee088079bf1f81abc2bf2a370a5a92397"))
 (web-mode :source "elpaca-menu-lock-file" :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id web-mode :type
                     git :protocol https :inherit t :depth treeless :ref
                     "ce24723eb900c455b488d224910519bd36af580a"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo
                  "mhayashi1120/Emacs-wgrep" :files ("wgrep.el") :source
                  "elpaca-menu-lock-file" :id wgrep :type git :protocol
                  https :inherit t :depth treeless :ref
                  "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (with-editor :source "elpaca-menu-lock-file" :recipe
              (:package "with-editor" :fetcher github :repo
                        "magit/with-editor" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                         "doc/*.texinfo" "lisp/*.el" "docs/dir"
                         "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el"
                                   "*-test.el" "*-tests.el" "LICENSE"
                                   "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id with-editor
                        :type git :protocol https :inherit t :depth
                        treeless :ref
                        "5021ef6885381cf5b2852f7a3f67ca8c4be1dca2"))
 (ws-butler :source "elpaca-menu-lock-file" :recipe
            (:package "ws-butler" :fetcher git :url
                      "https://https.git.savannah.gnu.org/git/elpa/nongnu.git"
                      :branch "master" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id ws-butler
                      :host github :repo "lewang/ws-butler" :type git
                      :protocol https :inherit t :depth treeless :ref
                      "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a"))
 (xclip :source "elpaca-menu-lock-file" :recipe
        (:package "xclip" :repo
                  ("https://github.com/emacsmirror/gnu_elpa" . "xclip")
                  :tar "1.11.1" :host gnu :branch "externals/xclip"
                  :files ("*" (:exclude ".git")) :source
                  "elpaca-menu-lock-file" :id xclip :type git :protocol
                  https :inherit t :depth treeless :ref
                  "7febe164de2a881b83b9d604d3c7cf20b69f422d"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE" "README*"
                            "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id yaml :type git
                 :protocol https :inherit t :depth treeless :ref
                 "5546f36bde24a9a8c1934e0f6ce205cd41d72537"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id yaml-mode
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "93dba98c050e9abfc623ec66aa499dbbb46b2fe1"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :fetcher github :repo
                      "joaotavora/yasnippet" :files
                      (:defaults ("doc" "doc/*.org")) :source
                      "elpaca-menu-lock-file" :id yasnippet :type git
                      :protocol https :inherit t :depth treeless :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
                     (:package "yasnippet-snippets" :repo
                               "AndreaCrotti/yasnippet-snippets"
                               :fetcher github :files
                               ("*.el" "snippets" ".nosearch") :source
                               "elpaca-menu-lock-file" :id
                               yasnippet-snippets :type git :protocol
                               https :inherit t :depth treeless :ref
                               "606ee926df6839243098de6d71332a697518cb86"))
 (zig-mode :source "elpaca-menu-lock-file" :recipe
           (:package "zig-mode" :repo "ziglang/zig-mode" :fetcher
                     codeberg :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id zig-mode :type
                     git :protocol https :inherit t :depth treeless :ref
                     "62bfbaced0222e2bfbc086fa8556adf6b3298476")))
