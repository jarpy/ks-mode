;;; ks -- Major mode for Kerboscript.  -*- lexical-binding: t; -*-
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
        "print" "reboot" "remove" "rename" "return" "run" "set"
        "shutdown" "step" "switch" "then" "to" "toggle" "unlock"
        "unset" "until" "volume" "wait" "when")
  "List of Kerboscript keywords for ks-mode.")

(defvar ks-types
  (list  "sas" "steering" "throttle")
  "List of special Kerboscript types for ks-mode.")

(defvar ks-functions
  (list "abs" "arccos" "arcsin" "arctan" "arctan2" "ceiling"
        "constant" "cos" "floor" "ln" "log10" "max" "min" "mod"
        "node" "random" "round" "sin" "sort" "tan")
  "List of Kerboscript built-in functions for ks-mode.")

(defvar ks-constants
  (list "false" "true")
  "List of Kerboscript constants for ks-mode.")

(let
    ((orbit-suffixes
      (list "apoapsis" "argumentofperiapsis" "body" "eccentricity"
            "hasnextpatch" "inclination" "lan"
            "longitudeofascendingnode" "meananomalyatepoch" "name"
            "nextpatch" "periapsis" "period" "position"
            "semimajoraxis" "semiminoraxis" "transition" "trueanomaly"
            "velocity"))
     (orbitable-suffixes
      (list "altitude" "apoapsis" "body" "direction" "distance"
            "geoposition" "hasbody" "hasobt" "hasorbit" "latitude"
            "longitude" "name" "north" "obt" "patches" "periapsis"
            "position" "prograde" "retrograde" "ship" "srfprograde"
            "srfretrograde" "the" "up" "velocity")))
  (defvar ks-variables (delete-dups (append orbitable-suffixes orbit-suffixes))
    "List of known Kerboscript variables and structure suffixes for ks-mode."))

(defun ks-regexp-opt (keywords)
  "Make an optimized regexp from the list of KEYWORDS."
  (regexp-opt keywords 'symbols))

(defvar ks-font-locks
  `(( "function \\([^ ]*\\)"        . (1 font-lock-function-name-face))
    ( "@lazyglobal off"             . font-lock-warning-face)
    ( ,(ks-regexp-opt ks-functions) . font-lock-builtin-face)
    ( ,(ks-regexp-opt ks-keywords)  . font-lock-keyword-face)
    ( ,(ks-regexp-opt ks-variables) . font-lock-variable-name-face)
    ( ,(ks-regexp-opt ks-types)     . font-lock-type-face)
    ( ,(ks-regexp-opt ks-constants) . font-lock-constant-face)))

(defvar ks-indent 2
  "Indentation size for ks-mode.")

(defun ks-previous-indentation ()
  "Get the indentation of the previous significant line of Kerboscript."
  (save-excursion
    (ks-backward-significant-line)
    (current-indentation)))

(defun ks-backward-significant-line ()
  "Move backwards to the last non-blank, non-comment line of Kerboscript."
  (forward-line -1)
  (while (and (looking-at "[[:space:]]*\\(//.*\\)?$")
              (not (bobp)))
    (forward-line -1))
  (current-indentation))

(defun ks-unterminated-line-p ()
  "Is the current line of Kerboscript unterminated?"
  (save-excursion
    (beginning-of-line)
    (not (ks-looking-at ".*\\([.{}]\\)"))))

(defun ks-unterminated-previous-line-p ()
  "Is the previous line of Kerboscript unterminated?"
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        nil
      (progn
        (ks-backward-significant-line)
        (ks-unterminated-line-p)))))

(defun ks-indent-buffer ()
  "Indent the current buffer as Kerboscript."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ks-indent-line)
    (while (not (ks-last-line-p))
      (forward-line)
      (ks-indent-line))))

(defun ks-last-line-p ()
  "Is this the last line?"
  (save-excursion
    (end-of-line)
    (= (point) (point-max))))

(defun ks-looking-at (regexp)
  "Look for REGEXP on this line, ignoring traling space and comments."
  (let ((regexp (concat regexp "[[:space:]]*\\(//.*\\)?$")))
    (looking-at regexp)))

(defun ks-indent-line ()
  "Indent a line of Kerboscript."
  (interactive)
  (let* ((indentation (ks-previous-indentation))
         (opening-brace ".*{")
         (closing-brace ".*}.*")
         (blank-line "[[:space:]]*$")
         (indent-more
          (lambda()(setq indentation (+ indentation ks-indent))))
         (indent-less
          (lambda()(setq indentation (- indentation ks-indent)))))
    (save-excursion
      (beginning-of-line)
      (if (or (bobp)
              (looking-at blank-line))
          (setq indentation 0)
        (progn (if (ks-looking-at closing-brace)
                   (funcall indent-less))
               (ks-backward-significant-line)
               (if (ks-looking-at opening-brace)
                   (funcall indent-more))
               ; Hanging indent.
               (if (and (ks-unterminated-line-p)
                        (not (ks-unterminated-previous-line-p)))
                   (funcall indent-more))
               ; Recover from hanging indent.
               (if (and (not (ks-unterminated-line-p))
                        (ks-unterminated-previous-line-p))
                   (funcall indent-less)))))
    (indent-line-to (max indentation 0))))

(define-derived-mode ks-mode fundamental-mode "ks"
  "A major mode for editing Kerboscript files."
  :syntax-table ks-mode-syntax-table
  (setq-local font-lock-defaults '(ks-font-locks nil t))
  (setq-local indent-line-function 'ks-indent-line)
  (if (featurep 'rainbow-delimiters) (rainbow-delimiters-mode-enable)))

(provide 'ks)
;;; ks.el ends here
