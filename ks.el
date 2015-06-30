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

(defvar ks-keywords (list "@lazyglobal" "add" "all" "and" "at" "batch" "break"
                     "clearscreen" "compile" "copy" "declare" "delete" "deploy"
                     "do" "edit" "else" "file" "for" "from" "global" "if" "in"
                     "list" "local" "lock" "log" "not" "off" "or" "on"
                     "parameter" "preserve" "print" "reboot" "remove" "rename"
                     "run" "set" "shutdown" "stage" "step" "switch" "then" "to"
                     "toggle" "unlock" "unset" "until" "volume" "wait" "when")
  "List of Ks keywords for ks-mode.")

(defvar ks-builtins (list "abs" "arccos" "arcsin" "arctan" "arctan2" "ceiling"
                     "constant" "cos" "floor" "ln" "log10" "max" "min" "mod"
                     "random" "round" "sin" "sin" "sort" "tan")
  "List of Ks built-in functions.")

(defvar ks-variables (list "apoapsis" "false" "periapsis" "ship" "true")
  "List of Ks variables that are always available.")

(defun ks-regexp-opt (keywords)
  "Make an optimized regexp from the list of KEYWORDS."
  (regexp-opt keywords 'words))

(setq ks-font-locks `(
                      ( "declare function \\([^ ]\\)" . font-lock-function-name-face)
                      ( ,(ks-regexp-opt ks-keywords) . font-lock-keyword-face)
                      ( ,(ks-regexp-opt ks-builtins) . font-lock-builtin-face)
                      ( ,(ks-regexp-opt ks-variables) . font-lock-constant-face)))

(define-derived-mode ks-mode fundamental-mode "ks"
  "A major mode for editing Ks files."
  :syntax-table ks-mode-syntax-table
  (setq-local font-lock-defaults '(ks-font-locks nil t))
)

(provide 'ks)
;;; ks.el ends here
