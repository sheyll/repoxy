;;; rpx-main-menu.el --- Repoxy's Main Menu and Customization Group

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

;; Defines the repoxy main customization group and the repoxy menu used to
;; manage a project.

;;; Code:

(provide 'rpx-main-menu)

(setq lexical-binding t)

(require 'easymenu)
(require 'rpx-pf)

(defgroup repoxy nil
  "Customization group for repoxy. All repoxy customizations have
the prefix rpx-cust."
  :version 1
  :prefix "rpx-cust"
  :group 'programming)

(easy-menu-define nil global-map "Doc"
  '("dddBei2" ["gott2" 'was-nen-stress
               :style toggle
               :selected t
               :help "Blahh"]))

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rpx-main-menu.el ends here
