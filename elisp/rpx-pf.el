;;; rpx-pf.el --- The Repoxy Project Finder

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

;; These functions initiate the management of erlang projects in a user
;; customizable way. Contains the hook that checks if an opened file is part of
;; an erlang project.

;;; Code:

(provide 'rpx-pf)

(require 'cl)
(require 'rpxu)
(require 'rpx-prj)

;;; Customizable variables:

(defcustom rpx-cust-autostart-policy :prompt
  "When repoxy detects an erlang project, that is not managed,
what should it do?"
  :type '(radio
          (const :tag "Prompt if the project should be managed." :prompt)
          (const :tag "Manage automatically." :auto)
          (const :tag "Never manage projects automatically." :never))
  :group 'repoxy)

(defcustom rpx-cust-ask-for-project-dir :yes
  "Should the user be asked to the project directory, instead of
using repoxy's best guess?"
  :type '(radio
          (const :tag "Yes." :yes)
          (const :tag "No." :no))
  :group 'repoxy)

;;; Global variables

(defvar rpx-pf-ignored-paths '()
  "A list paths that should be ignored when repoxy searches for
erlang projects. This list contains all open projects as well as
those that the user did not want to manage.")

;;; Functions

(defun rpx-pf-start()
  "Add to find file hook, when an erlang file was opened, try to
manage it."
  (add-hook 'find-file-hook 'rpx-pf-find-file-hook))

(defun rpx-pf-find-file-hook()
  "Depending on the user settings, eiter start a project for a
file, or irgnore the file. Remember which projects shall be ignored."
  (interactive)
;; TODO fix remembering of rejected manage requests
  (when (and
         (not (equal :never rpx-cust-autostart-policy))
         (not (rpx-pf-file-ignored-p (buffer-file-name))))
    (let ((guess (rpx-pf-guess-base-dirs (buffer-file-name))))
      (when (and guess (rpx-pf-sould-manage guess))
        (let ((prj-base-dir
               (if (equal :yes rpx-cust-ask-for-project-dir)
                   (rpx-pf-prompt guess)
                 guess)))
          (rpx-pf-manage prj-base-dir))))))

(defun rpx-pf-sould-manage(path)
  "Find out if the user wants project to be managed by repoxy."
  (or (equal :auto rpx-cust-autostart-policy)
      (when (not (yes-or-no-p "Should the Erlang project be managed by Repoxy?"))
        (rpx-pf-add-ignore-path path)
        nil)))

(defun rpx-pf-manage (prj-base-dir)
  "Initiate management of a project contained in prj-base-dir and
add this directory to the ingore list."
  (interactive "D")
  (rpx-pf-add-ignore-path prj-base-dir)
  (rpx-prj-manage-project prj-base-dir))

(defun rpx-pf-add-ignore-path(prj-base-dir)
  "Add this entry to the ignore-list."
  (rpx-pf-remove-ignore-path prj-base-dir)
  (setq rpx-pf-ignored-paths
        (cons prj-base-dir rpx-pf-ignored-paths)))

(defun rpx-pf-remove-ignore-path (dir)
  "Remove all entries from the ignore list that are prefix of 'dir'"
  (setq rpx-pf-ignored-paths
        (delete-if
         (lambda(ignore-path)
           (rpxu-prefixed-by-p (expand-file-name dir) ignore-path))
         rpx-pf-ignored-paths)))

(defun rpx-pf-file-ignored-p(file)
  "Return non-nil if 'file' is in a subdirectory of an ignored
path."
  (find-if
   (lambda(ignore-path)
     (rpxu-prefixed-by-p (expand-file-name file) ignore-path))
   rpx-pf-ignored-paths))

(ert-deftest rpx-pf-file-ignored-p-test()
  "Onle files in sub-dirs that are ignored, must be ignored..."
  (let ((rpx-pf-ignored-paths '("/a/b/c")))
    (should (rpx-pf-file-ignored-p "/a/b/c/d/e/test.txt"))
    (should (rpx-pf-file-ignored-p "/a/b/c/test.txt"))
    (should (not (rpx-pf-file-ignored-p "/x/b/c/test.txt")))))

(defun rpx-pf-prompt(default-dir)
  "Prompt the use for a project base directory of the file in the current buffer,
and suggest a good candidate."
  (read-from-minibuffer "Set project directory: " default-dir))

(defun rpx-pf-guess-base-dirs(file)
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
            (rpx-pf-find-rebar-config
               (replace-match "" nil nil parent-dir nil) 0)))

         (rebar-rel-guess
          (when (string-match "/apps/.+" parent-dir)
            (rpx-pf-find-rebar-config
               (replace-match "" nil nil parent-dir nil) 0)))

         (rebar-app-guess
          (rpx-pf-find-rebar-config parent-dir 0))

         (otp-guess
          (when (or (string= (file-name-directory file-dir) "src")
                    (string= (file-name-directory file-dir) "test")
                    (string= (file-name-directory file-dir) "include"))
            (file-name-directory file-dir))))
    (or
     rebar-dependency-guess
     rebar-rel-guess
     rebar-app-guess
     otp-guess)))

(defun rpx-pf-find-rebar-config(&optional start-dir max-depth)
  "Internal function to find a rebar config in the current or parent directory."
  (or (rpxu-find-in-parent-dir "rebar.config" start-dir max-depth)
      (rpxu-find-in-parent-dir "rebar.config.src" start-dir max-depth)
      (rpxu-find-in-parent-dir "rebar" start-dir max-depth)))

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rpx-pf.el ends here
