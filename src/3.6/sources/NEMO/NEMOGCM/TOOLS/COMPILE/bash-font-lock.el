; Bash syntax highlighting

(defvar bash-builtins  ;; Unrecognized UNIX commands
  '("awk" "basename" "cat" "cp" "cut" "date" "diff" "dirname" "env" "find"
    "grep" "head" "ls" "make" "mkdir" "mv" "rm" "sed" "sort" "svn" "tail" "tee"
    "touch" "uniq" "xargs"                                                     ))

(defvar bash-keywords  ;; Bash keywords (consistency with 'return' command fontification)
  '("declare" "let" "local" "readonly" "typeset" "set" "unset"                 ))

(defvar bash-functions ;; Sourced or declared fonctions
  '("fake_func"                                                                   ;; 
    "em" "ev" "ge" "heil" "mkcd" "ok"                                             ;; Common
    "nemch" "nemde" "nemid" "nemtr" "nemst"                                       ;; NEMO branchs
    "cmpcf" "if90" "gf90"                                                         ;; NEMO cmp
    "nemo_help" "nemo_cfg"                                                        ;; NEMO cfg
    "append_output_file" "appech_output_file" "super_grep" "check_args_count"     ;; NEMO SETTE
    "set_namelist" "set_xio" "post_test_tidyup"                                )) ;; Id

(defvar bash-font-lock ;; Be careful with order
  '(;; Variables
    ;;; GLOBALS variables|arrays
    ("\\(?:export +\\)?\\$?{?\\(#?[A-Z][A-Z_0-9]+\\)\\(?:\\[\\(?2:[@*]\\)\\]\\)?"
     (1 font-lock-constant-face     ) (2 font-lock-constant-face      nil t))
    ;;; Locals arrays
    ("\\(?1:#?[a-z][a-z_0-9]+\\)\\[\\([@*]\\)?"
     (1 font-lock-variable-name-face) (2 font-lock-variable-name-face nil t))
    ;;; Shell specials variables
    ("\\${?\\([#@?!_-]?[a-z]?[a-z_0-9]*\\)}?" 1 font-lock-variable-name-face)

    ;; Tests  '[ ... ]' '[[ ... ]]' '(( ... ))'
    ("\\((\\{2\\}\\) \\(?:.*\\) \\()\\{2\\}\\)"            
     (1 font-lock-builtin-face) (2 font-lock-builtin-face))
    ("\\(\\[\\{1,2\\}\\) \\(?:[^]]+\\) \\(\\]\\{1,2\\}\\)"
     (1 font-lock-builtin-face) (2 font-lock-builtin-face))

    ;; Operators  ' ! ' ' != ' ' ~= '  ' && ' ' || '
    (" \\!=? \\| ~= " 0 font-lock-negation-char-face)
    (" && \\| || "    0 font-lock-keyword-face      )

    ;; Compound commands
    ("\\\\$"                0 font-lock-warning-face) ; '\'
    ("\\(?:[^;]\\)\\(;\\) " 1 font-lock-keyword-face) ; '; '
     ;;; Function
    ("^\\(?:function +\\)?\\(\\sw+\\)(?)? +\\({\\)"
     (1 font-lock-function-name-face) (2 font-lock-warning-face      ))
    ("^}$"                             0 font-lock-warning-face       )
     ;;; Commands block ' { ...; }'
    ("\\({\\) \\(?:[^}]*\\); +\\(}\\)?" (1 font-lock-warning-face) (2 font-lock-warning-face nil t))
    ("\\(?:.*\\); +\\(}\\)$"          1 font-lock-warning-face                                  )
     ;;; Sub-shell      '( ...  )'
    ("\\((\\) \\(?:[^)]*\\) \\()\\)" (1 font-lock-warning-face) (2 font-lock-warning-face))

    ;; 'case ... in ...) ...;; esac'
    ("\\()\\)\\(?:[^)]*\\)\\(;;\\)" (1 font-lock-keyword-face) (2 font-lock-keyword-face))
    ("^[^(]*\\()\\)$"                1 font-lock-keyword-face) ; Regular syntax
    ("^\\s-+;;$"                     0 font-lock-keyword-face) ;    "      ""
    )
  )

; Configuration loading
(font-lock-add-keywords 'sh-mode `((,(regexp-opt bash-builtins  'words) 0 font-lock-builtin-face      )))
(font-lock-add-keywords 'sh-mode `((,(regexp-opt bash-keywords  'words) 0 font-lock-keyword-face      )))
(font-lock-add-keywords 'sh-mode `((,(regexp-opt bash-functions 'words) 0 font-lock-function-name-face)))
(font-lock-add-keywords 'sh-mode bash-font-lock                                                         )

; Highlighting example for font lock faces
(font-lock-add-keywords 'sh-mode '(
  ("font-lock-warning-face"           . font-lock-warning-face          )
  ("font-lock-function-name-face"     . font-lock-function-name-face    )
  ("font-lock-variable-name-face"     . font-lock-variable-name-face    )
  ("font-lock-keyword-face"           . font-lock-keyword-face          )
  ("font-lock-comment-face"           . font-lock-comment-face          )
  ("font-lock-comment-delimiter-face" . font-lock-comment-delimiter-face)
  ("font-lock-type-face"              . font-lock-type-face             )
  ("font-lock-constant-face"          . font-lock-constant-face         )
  ("font-lock-builtin-face"           . font-lock-builtin-face          )
  ("font-lock-preprocessor-face"      . font-lock-preprocessor-face     )
  ("font-lock-string-face"            . font-lock-string-face           )
  ("font-lock-doc-face"               . font-lock-doc-face              )
  ("font-lock-negation-char-face"     . font-lock-negation-char-face    )))
