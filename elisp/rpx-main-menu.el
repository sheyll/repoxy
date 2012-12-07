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

(require 'easymenu)
(require 'rpx-pf)

;;; Customization:

(defgroup repoxy nil
  "Customization group for repoxy. All repoxy customizations have
the prefix rpx-cust."
  :version 1
  :prefix "repoxy"
  :group 'programming)

;;; Functions:

(defun rpx-main-menu-start()
  "Create the project independent repoxy menu."
  (add-hook
   'rpx-pf-add-ignore-hooks
   (lambda(dir)
     (lexical-let ((dir-c dir))
       (easy-menu-add-item
        rpx-main-menu '("Ignored Paths")
        (vector dir
                (lambda()
                  (interactive)
                  (rpx-pf-remove-ignore-path dir-c))
                :help "Stop ignoring this path")))))
  (add-hook
   'rpx-pf-remove-ignore-hooks
   (lambda(path)
     (easy-menu-remove-item 'rpx-main-menu '("Ignored Paths") path)))

  (easy-menu-define rpx-main-menu global-map "Repoxy's top level menu"
    '("Repoxy"
      ["Manage project..." rpx-pf-project-discovered
       :help "Select the base directory of an erlang project to manage."]
      ("Ignored Paths"
       ["--" 'ignore :help "Click an item to unignore it..." :active nil]))))

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; lexical-binding: t
;; End:

;;; rpx-main-menu.el ends here
