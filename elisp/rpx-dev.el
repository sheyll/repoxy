;;;;;;;;;;; THIS FILE IS ONLY FOR DEVELOPMENT PURPOSES
;;;;;;;;;;; IT IS NOT PART OF 'repoxy' AT RUNTIME

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helps me edit stuff
(require 'skeleton)

(abbrev-mode t)

(defun make-maybe-skel(title)
  "Maybe insert a value"

(defconst skel-eieio-slot-src
  '("Slot name: "
    > "(" str " " ("type: " ":type " str \n)
    > ":initarg " str \n
    ("initform: "  > ":initform " str \n)
    > ":documentation " ?\" (skeleton-read "Documenation: ") ?\" ")" \n ))

(defun skel-eieio-slot()
 (interactive)
 "Insert a slot" (skeleton-insert skel-eieio-slot-src))

(define-skeleton skel-eieio-class
  "Insert a class"
  "Class name: "
  "(defclass " str " (eieio-named)" \n
  > "(" skel-eieio-slot-src
  > ") " "\"" (skeleton-read "Documentation: ") "\""  ")" \n)

(define-abbrev global-abbrev-table "classs" "" 'skel-eieio-class)
(define-abbrev global-abbrev-table "slott" "" 'skel-eieio-slot)

(global-set-key [?\C-c ?c] 'skel-eieio-class)
(global-set-key [?\C-c ?s] 'skel-eieio-slot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
