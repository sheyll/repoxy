;;; repoxy.el --- Emacs mode for the integration of Repoxy

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

;; Repoxy is an Erlang program for building erlang IDE's, 'repoxy.el' is such an
;; IDE for Emacs.
;;
;; To enable repoxy, you must have the erlang mode already enabled, e.g.:
;;
;;   (add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.6.8/emacs/")
;;   (setq erlang-root-dir "/usr/lib/erlang")
;;   (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;;   (require 'erlang-start)
;;
;; After that just add the repoxy path to your load path, require it and call
;; 'repoxy-init':
;;
;;  (add-to-list 'load-path "~/tools/repoxy/elisp")
;;  (require 'repoxy)
;;  (repoxy-start)
;;
;; This file contains the global variables that contain the meta information of
;; the erlang projects managed/ignored by repoxy.

;;; Code:

(require 'rpx-pf)
(require 'rpx-main-menu)

(setq lexical-binding t)

;;; Functions

(defun repoxy-start()
  "Start scanning buffers for erlang project files and create
a menu for repoxy access."
  (rpx-pf-start)))


(provide 'repoxy)

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; repoxy.el ends here
