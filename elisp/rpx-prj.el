;;; rpx-prj.el --- Classes that manage erlang projects.

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

;; These functions and classes hold together information about erlang projects.

;;; Code:

(require 'rpx-pf)

(require 'cl)
(require 'eieio)
(require 'rpx-erl)
(require 'rpx-otp)
;;; Global variables:

(defvar rpx-projects '()
  "A list of rpx-prj objects that are currently managed by repoxy.")

;;; Functions:

(defun rpx-prj-start ()
  "Attach to project discovery hook"
  (add-hook 'rpx-pf-discovery-hooks 'rpx-prj-manage-project))

(defun rpx-prj-manage-project (prj-base-dir)
  "Initiate management of a project contained in prj-base-dir"
  (message "Managing %s" prj-base-dir)
  (when (rpx-prj-can-manage prj-base-dir)
    (rpx-prj-run-manage-hooks
     (rpx-prj-install-hooks
      (rpx-prj-update-project-info
       (rpx-prj-create-menu
        (rpx-prj-start-repoxy-client
         (rpx-prj-start-repoxy-server
          (rpx-prj-create-model
           prj-base-dir)))))))))

(defun rpx-prj-unmanage-project (prj-base-dir)
  "Stop management of a project and clean up everything
associated to the project."
  (message "Un-managing %s" prj-base-dir))

;;; Classes:

(defclass rpx-prj-source ()
  ((source :type rpx-erl-source
           :initarg :source
           :documentation "The definition of this source file.")
   (emacs-buffer :type (or buffer null)
                 :initarg :emacs-buffer
                 :initform nil
                 :documentation "Reference to a buffer holding
this source file, or nil if the source is not open in any
buffer.")
   (is-dirty :type boolean
             :initarg :is-dirty
             :initform t
             :documentation "Indicates that re-computation of the
             fields is required.")
   (info-markers :type (satisfies (lambda(a) (rpxu-list-of a rpx-prj-source-marker)))
                 :initarg :info-markers
                 :initform '()
                 :documentation "A list of
                       'rpx-prj-source-marker' objects
                       concerning informational messages.")
   (warning-markers :type (satisfies (lambda(a) (rpxu-list-of a rpx-prj-source-marker)))
                    :initarg :warning-markers
                    :initform '()
                    :documentation "A list of
                       'rpx-prj-source-marker' objects concerning
                       warnings about this source file.")
   (error-markers :type (satisfies (lambda(a) (rpxu-list-of a rpx-prj-source-marker)))
                  :initarg :error-markers
                  :initform '()
                  :documentation "A list of
                       'rpx-prj-source-marker' objects concerning
                       errors in the this source file.")
   (coverage-markers :type (satisfies (lambda(a) (rpxu-list-of a rpx-prj-source-marker)))
                     :initarg :coverage
                     :initform '()
                     :documentation "A list
             'rpx-prj-coverage-markers' objects concerning this
             source."))
" This class also contains coverage and compilation results. If a
buffer visits the file mentioned here, it will automatically be
pimped with overlays. meta info and what-not.")

(defclass rpx-prj-app ()
  ((descriptor
    :type (or null rpx-otp-app-descriptor)
    :initarg :descriptor
    :initform nil
    :documentation "The descriptor for the app")
   (base-dir
    :type string
    :initarg :base-dir
    :documentation "Top-level application directory.")
   (compilation-failed
    :type boolean
    :initarg :compilation-failed
    :initform t
    :documentation "A flag indicating that the
                       application could was not built, or the
                       last build failed.")
   (main-sources
    :type (satisfies (lambda(a) (rpxu-list-of a rpx-prj-source)))
    :initarg :src
    :initform '()
    :documentation "List of 'rpx-prj-source' objects that
        reside in the app's source folder.")
   (includes
    :type (satisfies (lambda(a) (rpxu-list-of a rpx-prj-source)))
    :initarg :src
    :initform '()
    :documentation "List of 'rpx-prj-source' objects that
        reside in the app's include folder.")
   (test-sources
    :type (satisfies (lambda(a) (rpxu-list-of a rpx-prj-source)))
    :initarg :test-src
    :initform '()
    :documentation "List of 'rpx-prj-source'
        objects that reside in the app's test source folder."))
  "A a complete tree of objects describing an OTP erlang app and
  it's current build status.")

;; method: get included by
;; method: dependencies
;; method: dependent

(defclass rpx-prj-remote-node ()
  ((erlnode :type rpx-erlang-node
            :initarg erlnode
            :documentation "Connection details for an erlang node."))
  "Encapsulates a project associated erlang node.")

(defclass rpx-prj-model()
  ((base-dir  :initarg :base-dir
              :initform ""
              :type string
              :documentation "Top level directory of the erlang project.")
   (server :initform nil
           :type (or null rpx-server))
   (remote-nodes :type (satisfies (lambda (a) (rpxu-list-of a rpx-prj-remote-node)))
                 :initarg remote-nodes
                 :initform nil
                 :documentation "A list of remote nodes to upload
                 code to, to debug on and to open remshells to")
   (rel-descriptor :initargs :rel-descriptor
                   :type (or null rpx-otp-rel-descriptor))
   (apps :type (satisfies (lambda(a) (rpxu-list-of a rpx-prj-app)))
         :initarg :apps
         :initform '()
         :documentation "A list of 'rpx-prj-app' objects that are
         part of the project."))
   "A class defining a repoxy managed erlang project")

;; method: get dirty modules (those that were changed + this that had compiler
;; errors)
;; method set-compilation result for module
;; method set-cover result for module

;;; Methods:

;;; Tests:

(require 'ert)

(ert-deftest rpx-prj-start-test()
  "After rpx-prj-start a project should get started when it is
discovered"
  (rpx-prj-start)
  (should (memq 'rpx-prj-manage-project rpx-pf-discovery-hooks)))


(provide 'rpx-prj)

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; lexical-binding: t
;; End:

;;; rpx-prj.el ends here
