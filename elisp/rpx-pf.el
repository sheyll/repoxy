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
;; Several erlang project relevant hooks are provided.
;; There is a hook that is called whenever a new project is discovered:
;; 'rpx-pf-discovery-hook'

;;; Code:

(provide 'rpx-pf)

(require 'cl)
(require 'rpxu)

;;; Customizable variables:

(defcustom repoxy-autostart-policy :prompt
  "When repoxy detects an erlang project, that is not managed,
what should it do?"
  :type '(radio
          (const :tag "Prompt if the project should be managed." :prompt)
          (const :tag "Manage automatically." :auto)
          (const :tag "Never manage projects automatically." :never))
  :group 'repoxy)

(defcustom repoxy-ask-for-project-dir :yes
  "Should the user be asked to the project directory, instead of
using repoxy's best guess?"
  :type '(radio
          (const :tag "Yes." :yes)
          (const :tag "No." :no))
  :group 'repoxy)

;;; Global variables:

(defvar rpx-pf-discovery-hooks '()
  "A list of functions that will be called when an unmanaged
erlang project is discovered. This is an abnormal hook, the
functions must accept one parameter: the project base directory
of the project a user wants to have managed.")

(defvar rpx-pf-ignored-paths '()
  "A list paths that should be ignored when repoxy searches for
erlang projects. This list contains all open projects as well as
those that the user did not want to manage.")

(defvar rpx-pf-add-ignore-hooks '()
  "A list of functions that will be called when a user ignores a path.")

(defvar rpx-pf-remove-ignore-hooks '()
  "A list of functions that will be called when a path is removed
  from the ignore list.")

;;; Functions:

(defun rpx-pf-start()
  "Add to find file hook, when an erlang file was opened, try to
manage it."
  (add-hook 'find-file-hook 'rpx-pf-find-file-hook))

(defun rpx-pf-project-discovered (prj-base-dir)
  "Initiate management of a project contained in prj-base-dir and
add this directory to the ingore list.  This is a no-op if the
project is already managed. When the project should be managed,
the discovery hooks will be executed."
  (interactive "D")
  (rpx-pf-add-ignore-path prj-base-dir)
  (run-hook-with-args 'rpx-pf-discovery-hooks prj-base-dir))

(defun rpx-pf-add-ignore-path (ignore-path)
  "Add this entry to the ignore-list."
  (rpx-pf-remove-ignore-path ignore-path)
  (setq rpx-pf-ignored-paths
        (cons ignore-path rpx-pf-ignored-paths))
  (run-hook-with-args 'rpx-pf-add-ignore-hooks ignore-path))

(defun rpx-pf-remove-ignore-path (dir)
  "Remove all entries from the ignore list that are prefix of 'dir'"
  (setq rpx-pf-ignored-paths
        (delete-if
         (lambda(ignore-path)
           (when (rpxu-prefixed-by-p (expand-file-name dir) ignore-path)
             (run-hook-with-args 'rpx-pf-remove-ignore-hooks ignore-path)))
         rpx-pf-ignored-paths)))

(defun rpx-pf-find-file-hook()
  "Try to discover an erlang project behind a file just opened.
If such a file is found, the rpx-pf-hook if fired"
  (interactive)
  (let ((file (buffer-file-name)))
    (rpx-pf-discover-project file)))

(defun rpx-pf-discover-project(file)
  "Depending on the user settings, eiter start a project for a
file, or irgnore the file. Remember which projects shall be ignored."
  (when (and (not (equal :never repoxy-autostart-policy))
             (not (rpx-pf-file-ignored-p file)))
    (let ((guess (rpx-pf-guess-base-dirs file)))
      (when (and guess (rpx-pf-should-manage guess))
        (let ((prj-base-dir (if (equal :yes repoxy-ask-for-project-dir)
                                (rpx-pf-prompt guess)
                              guess)))
          (rpx-pf-project-discovered prj-base-dir)
          t)))))

(defun rpx-pf-should-manage(path)
  "Find out if the user wants project to be managed by repoxy."
  (or (equal :auto repoxy-autostart-policy)
      (if (yes-or-no-p "Should the Erlang project be managed by Repoxy?")
          t
        (progn
          (rpx-pf-add-ignore-path path)
          nil))))

(defun rpx-pf-file-ignored-p(file)
  "Return non-nil if 'file' is in a subdirectory of an ignored
path."
  (find-if
   (lambda(ignore-path)
     (rpxu-prefixed-by-p (expand-file-name file) ignore-path))
   rpx-pf-ignored-paths))

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
          (when (or
                 (string-match "/doc/" file)
                 (string-match "/priv/" file)
                 (string-match "/c_src/" file)
                 (string-match "/src/.*\.[eyh]rl$" file)
                 (string-match "/test/.*\.[eyh]rl$" file)
                 (string-match "/include/.*\.[eyh]rl$" file)
                 (string-match "/rebar.config$" file)
                 (string-match "/rebar.config.src$" file)
                 (string-match ".*\.app$" file)
                 (string-match ".*\.app.src$" file)
                 (string-match "/reltool.config$" file))
            parent-dir)))
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

;;; Tests:

(require 'ert)

(ert-deftest rpx-pf-add-ignore-path-hooks-test()
  "Test the add ignore hooks"
  (setq ai-hook-result :fail)
  (setq ri-hook-result :fail)
  (defun ai-test-hook-fun (p) (setq ai-hook-result :pass))
  (defun ri-test-hook-fun (p) (setq ri-hook-result :pass))
  (add-hook 'rpx-pf-add-ignore-hooks 'ai-test-hook-fun)
  (add-hook 'rpx-pf-remove-ignore-hooks 'ri-test-hook-fun)
  (unwind-protect
      (progn
        (rpx-pf-add-ignore-path "/x/y/z")
        (rpx-pf-remove-ignore-path "/x/y/z"))
    (remove-hook 'rpx-pf-add-ignore-hooks 'ai-test-hook-fun)
    (remove-hook 'rpx-pf-remove-ignore-hooks 'ri-test-hook-fun))
  (should (equal ai-hook-result :pass))
  (should (equal ri-hook-result :pass)))

(ert-deftest rpx-pf-remove-ignore-path-hooks-test()
  "Test the remove ignore hooks"
  (setq hook-result :fail)
  (defun test-hook-fun (p) (setq hook-result :pass))
  (add-hook 'rpx-pf-discovery-hooks 'test-hook-fun)
  (unwind-protect
      (rpx-pf-remove-ignore-path "/x/y/z")
    (remove-hook 'rpx-pf-discovery-hooks 'test-hook-fun))
  (should (equal hook-result :pass)))

(ert-deftest rpx-pf-discovery-hooks-test()
  "Test the discovery hook mechanism"
  (setq hook-result :fail)
  (defun discovery-test-hook-fun (p) (setq hook-result :pass))
  (add-hook 'rpx-pf-discovery-hooks 'discovery-test-hook-fun)
  (unwind-protect
      (rpx-pf-project-discovered "/x/y/z")
    (remove-hook 'rpx-pf-discovery-hooks 'discovery-test-hook-fun))
  (should (equal hook-result :pass)))

(ert-deftest rpx-pf-file-ignored-p-test()
  "Only files in sub-dirs that are ignored, must be ignored..."
  (let ((rpx-pf-ignored-paths '("/a/b/c")))
    (should (rpx-pf-file-ignored-p "/a/b/c/d/e/test.txt"))
    (should (rpx-pf-file-ignored-p "/a/b/c/test.txt"))
    (should (not (rpx-pf-file-ignored-p "/x/b/c/test.txt")))))

(ert-deftest rpx-pf-ignore-managed-dir-test()
  "When user agreed to manage a project remember that, and
ignore the path in future."
  (let ((rpx-pf-ignored-paths '())
        (repoxy-ask-for-project-dir :no)
        (repoxy-autostart-policy :auto))
    (should (rpx-pf-discover-project "/a/b/src/d.erl"))
    (should (rpx-pf-file-ignored-p   "/a/b/priv/test.txt"))))

(ert-deftest rpx-pf-remember-user-doesnt-want-to-manage-dir-test()
  "When user disagrees to manage a project remember that, and
ignore the path in future."
  (let ((rpx-pf-ignored-paths '())
        (repoxy-autostart-policy :promtp))
    (should (not (rpx-pf-discover-project "/a/b/src/d.erl")))
    (should (not (rpx-pf-discover-project "/a/b/test/d.erl")))
    ))

(ert-deftest rpx-pf-guess-project-rel-rebar-dir-test()
  "Test the guessing mechanism for rebar relaeases."
  (make-directory "/tmp/test-rel/apps/myapp/src/" t)
  (unwind-protect
      (progn
        (shell-command "touch /tmp/test-rel/rebar.config")
        (shell-command "touch /tmp/test-rel/apps/myapp/rebar.config")
        (should
         (equal "/tmp/test-rel/"
                (rpx-pf-guess-base-dirs "/tmp/test-rel/apps/myapp/src/test.erl"))))
    (delete-directory "/tmp/test-rel" t)))

(ert-deftest rpx-pf-guess-project-app-rebar-dir-test()
  "Test the guessing mechanism for rebar applications."
  (make-directory "/tmp/test-rel/apps/myapp/src/" t)
  (unwind-protect
      (progn
        (shell-command "touch /tmp/test-rel/apps/myapp/rebar.config")
        (should
         (equal "/tmp/test-rel/apps/myapp/"
                (rpx-pf-guess-base-dirs "/tmp/test-rel/apps/myapp/src/test.erl"))))
    (delete-directory "/tmp/test-rel" t)))

(ert-deftest rpx-pf-guess-project-deps-rebar-dir-test()
  "Test the guessing mechanism for dependencies of a rebar project."
  (make-directory "/tmp/test-rel/deps/blah/blub/src/xxx.erl" t)
  (unwind-protect
      (progn
        (shell-command "touch /tmp/test-rel/rebar.config")
        (shell-command "touch /tmp/test-rel/deps/blah/blub/rebar.config")
        (should
         (equal "/tmp/test-rel/"
                (rpx-pf-guess-base-dirs "/tmp/test-rel/deps/blah/blub/rebar.config"))))
    (delete-directory "/tmp/test-rel" t)))

(ert-deftest rpx-pf-guess-project-otp-dir-test()
  "Test the guessing mechanism for project discovery where there is no rebar.config."
  (should (rpx-pf-guess-base-dirs "/balh/blub/priv/abc"))
  (should (rpx-pf-guess-base-dirs "/balh/blub/c_src/abc"))
  (should (rpx-pf-guess-base-dirs "/balh/blub/doc/abc"))

  (should (rpx-pf-guess-base-dirs "/balh/blub/src/eee.erl"))
  (should (rpx-pf-guess-base-dirs "/balh/blub/src/eee.hrl"))
  (should (rpx-pf-guess-base-dirs "/balh/blub/src/eee.yrl"))
  (should (not (rpx-pf-guess-base-dirs "/balh/blub/src/eee.txt")))
  (should (not (rpx-pf-guess-base-dirs "/balh/blub/src/eee.erl.txt")))

  (should (rpx-pf-guess-base-dirs "/balh/blub/test/eee.erl"))
  (should (rpx-pf-guess-base-dirs "/balh/blub/test/eee.hrl"))
  (should (rpx-pf-guess-base-dirs "/balh/blub/test/eee.yrl"))

  (should (rpx-pf-guess-base-dirs "/balh/blub/include/eee.erl"))
  (should (rpx-pf-guess-base-dirs "/balh/blub/include/eee.hrl"))
  (should (rpx-pf-guess-base-dirs "/balh/blub/include/eee.yrl"))

  (should (rpx-pf-guess-base-dirs "x/rebar.config"))
  (should (not (rpx-pf-guess-base-dirs "x/rebar.config.xxx")))
  (should (rpx-pf-guess-base-dirs "x/rebar.config.src"))
  (should (not (rpx-pf-guess-base-dirs "x/rebar.config.src.xxx")))

  (should (rpx-pf-guess-base-dirs "x/blub.app"))
  (should (not (rpx-pf-guess-base-dirs "x/blub.app.xxx")))
  (should (rpx-pf-guess-base-dirs "x/blub.app.src"))
  (should (not (rpx-pf-guess-base-dirs "x/blub.app.src.xxx")))

  (should (rpx-pf-guess-base-dirs "x/reltool.config"))
  (should (not (rpx-pf-guess-base-dirs "x/reltool.config123"))))

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; lexical-binding: t
;; End:

;;; rpx-pf.el ends here
