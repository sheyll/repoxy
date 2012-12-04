;;; rpx-project-finder.el --- Scan directories for erlang projects.

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

;; Functions to find erlang projects in a user customizable way.

;;; Code:

(require 'rpxu)

(defun rpx-project-finder-prompt()
  "Prompt the use for a project base directory of the file in the current buffer,
and suggest a good candidate."
  (read-from-minibuffer
   "Set project directory: "
   (rpx-project-finder-guess-base-dirs (buffer-file-name))))

(defun rpx-project-finder-guess-base-dirs(file)
  "Return a list of guesses for the base directory of the project
file belongs to."
  (let* ((abs-file
          (expand-file-name file))
         (file-dir
          (file-name-directory abs-file))
         (parent-dir
          (expand-file-name
           (concat file-dir (file-name-as-directory "../"))))

         (rebar-dependency-guess
          (when (string-match "/deps/.+" parent-dir)
            (rpx-project-finder-find-rebar-config
               (replace-match "" nil nil parent-dir nil) 0)))

         (rebar-rel-guess
          (when (string-match "/apps/.+" parent-dir)
            (rpx-project-finder-find-rebar-config
               (replace-match "" nil nil parent-dir nil) 0)))

         (rebar-app-guess
          (rpx-project-finder-find-rebar-config parent-dir 0))

         (otp-guess
          (when (or (string= (file-name-directory file-dir) "src")
                    (string= (file-name-directory file-dir) "test")
                    (string= (file-name-directory file-dir) "include"))
            (file-name-directory file-dir))))
    (or
     rebar-dependency-guess
     rebar-rel-guess
     rebar-app-guess
     otp-guess
     file-dir)))

(defun rpx-project-finder-find-rebar-config(&optional start-dir max-depth)
  "Internal function to find a rebar config in the current or parent directory."
  (or (rpxu-find-in-parent-dir "rebar.config" start-dir max-depth)
      (rpxu-find-in-parent-dir "rebar.config.src" start-dir max-depth)
      (rpxu-find-in-parent-dir "rebar" start-dir max-depth)))


(provide 'rpx-project-finder)

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rpx-client.el ends here
