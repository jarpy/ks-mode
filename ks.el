;;; ks -- Major mode for Kerboscript.
;;; Commentary:
;;; Code:

(if (featurep 'ks) (unload-feature 'ks))

(defvar ks-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `ks-mode'.")

(defvar ks-keywords
  (list "add" "all" "and" "at" "batch" "break" "clearscreen" "compile"
        "copy" "declare" "delete" "deploy" "do" "edit" "else" "file"
        "for" "from" "function" "global" "if" "in" "is" "list" "local"
        "lock" "log" "not" "off" "or" "on" "parameter" "preserve"
        "print" "reboot" "remove" "rename" "run" "set" "shutdown"
        "step" "switch" "then" "to" "toggle" "unlock" "unset" "until"
        "volume" "wait" "when")
  "List of Kerboscript keywords for ks-mode.")

(defvar ks-types
  (list "sas" "throttle" "ship" "altitude" "apoapsis" "periapsis"
        "steering" )
  "List of Kerboscript structure names for ks-mode.")

(defvar ks-functions
  (list "abs" "arccos" "arcsin" "arctan" "arctan2" "ceiling"
        "constant" "cos" "floor" "ln" "log10" "max" "min" "mod"
        "node" "random" "round" "sin" "sort" "tan")
  "List of Kerboscript built-in functions for ks-mode.")

(defvar ks-variables
  (list "false" "true")
  "List of Kerboscript built-in variables for ks-mode.")

(defun ks-regexp-opt (keywords)
  "Make an optimized regexp from the list of KEYWORDS."
  (regexp-opt keywords 'symbols))

(defvar ks-font-locks
  `(( "//.*" . font-lock-comment-face)
    ( "function \\([^ ]*\\)" . (1 font-lock-function-name-face))
    ( "@lazyglobal off" . font-lock-warning-face)
    ( "\\(\\_<stage\\_>\\):"        . (1 font-lock-type-face))
    ( "\\(\\_<stage\\_>\\)[^:]"     . (1 font-lock-keyword-face))
    ( ,(ks-regexp-opt ks-keywords)  . font-lock-keyword-face)
    ( ,(ks-regexp-opt ks-types)  . font-lock-type-face)
    ( ,(ks-regexp-opt ks-functions)  . font-lock-builtin-face)
    ( ,(ks-regexp-opt ks-variables) . font-lock-constant-face)))

(define-derived-mode ks-mode fundamental-mode "ks"
  "A major mode for editing Kerboscript files."
  :syntax-table ks-mode-syntax-table
  (setq-local font-lock-defaults '(ks-font-locks nil t))
  (if (featurep 'rainbow-delimiters) (rainbow-delimiters-mode-enable))
)

(provide 'ks)
;;; ks.el ends here
