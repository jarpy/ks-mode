;;; ks -- Major mode for Kerboscript.
;;; Commentary:
;;; Code:

(if (featurep 'ks) (unload-feature 'ks))

(defvar ks-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
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
  (list  "sas" "steering" "throttle" )
  "List of Kerboscript structure names for ks-mode.")

(defvar ks-functions
  (list "abs" "arccos" "arcsin" "arctan" "arctan2" "ceiling"
        "constant" "cos" "floor" "ln" "log10" "max" "min" "mod"
        "node" "random" "round" "sin" "sort" "tan")
  "List of Kerboscript built-in functions for ks-mode.")

(defvar ks-constants
  (list "false" "true")
  "List of Kerboscript constants for ks-mode.")

(let
    ((orbitable-suffixes
      (list "altitude" "apoapsis" "body" "direction" "distance"
            "geoposition" "hasbody" "hasobt" "hasorbit" "latitude"
            "longitude" "name" "north" "obt" "patches" "periapsis"
            "position" "prograde" "retrograde" "ship" "srfprograde"
            "srfretrograde" "the" "up" "velocity" ))
     (orbit-suffixes
      (list "apoapsis" "argumentofperiapsis" "body" "eccentricity"
            "hasnextpatch" "inclination" "lan"
            "longitudeofascendingnode" "meananomalyatepoch" "name"
            "nextpatch" "periapsis" "period" "position"
            "semimajoraxis" "semiminoraxis" "transition" "trueanomaly"
            "velocity")))
  (defvar ks-variables (delete-dups (append orbitable-suffixes orbit-suffixes))
    "List of known Kerboscript variables and structure suffixes for ks-mode."))

(defun ks-regexp-opt (keywords)
  "Make an optimized regexp from the list of KEYWORDS."
  (regexp-opt keywords 'symbols))

(defvar ks-font-locks
  `(( "function \\([^ ]*\\)"        . (1 font-lock-function-name-face))
    ( "@lazyglobal off"             . font-lock-warning-face)
    ( "\\(\\_<stage\\_>\\):"        . (1 font-lock-variable-name-face))
    ( "\\(\\_<stage\\_>\\)[^:]"     . (1 font-lock-keyword-face))
    ( ,(ks-regexp-opt ks-functions) . font-lock-builtin-face)
    ( ,(ks-regexp-opt ks-keywords)  . font-lock-keyword-face)
    ( ,(ks-regexp-opt ks-variables) . font-lock-variable-name-face)
    ( ,(ks-regexp-opt ks-types)     . font-lock-type-face)
    ( ,(ks-regexp-opt ks-constants) . font-lock-constant-face)))

(define-derived-mode ks-mode fundamental-mode "ks"
  "A major mode for editing Kerboscript files."
  :syntax-table ks-mode-syntax-table
  (setq-local font-lock-defaults '(ks-font-locks nil t))
  (if (featurep 'rainbow-delimiters) (rainbow-delimiters-mode-enable))
)

(provide 'ks)
;;; ks.el ends here
