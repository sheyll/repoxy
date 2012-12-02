;;; rpxu.el --- Repoxy's ubiquitous utilities

;; Copyright (C) 2012 Sven Heyll

;; Author: Sven Heyll <sven.heyll@gmail.com>

;; Repoxy is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; Repoxy is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; Repoxy.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These are common functions and utilities used throughout Repoxy's Lisp
;; modules. It does not depend on any other Repoxy file.


;;; Code:

(require 'cl)
(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)

;(require 'speedbar)
;(require 'easymenu)

;; this file can be included in all rpx-*.el files

(setq lexical-binding t)

(defmacro rpxu-list-of(e typ)
  "create a cl type specifier for lists that are empty or contain
only elements of type 't'."
  `(and (listp ,e)
        (or (null ,e) (typep (car ,e) ,typ))))

(provide 'rpxu)

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rpxu.el ends here
