;;; rpx-otp.el --- Model of OTP descriptors

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

;; These classes and methods provide support for modeling OTP applications.


;;; Code:

(provide 'rpx-otp)

(require 'cl)
(require 'eieio)
(require 'rpx-erl)

(defclass rpx-otp-app-id ()
  ((vsn :type string
        :initarg :vsn
        :initform "0.0"
        :documentation "Version"))
  "Name and version of an OTP application. NOTE: the name slot is inherited")

(defclass rpx-otp-rel-descriptor ()
  ((rel-file :type string
            :initarg :rel-file
            :initform ""
            :documentation "An OTP conform .rel file")
  (rel-version :type string
               :initarg :rel-version
               :initform ""
               :documentation "Version of a release.")
  (app-ids :type (satisfies (lambda(a) (rpxu-list-of a rpx-otp-app-id)))
           :initarg :app-ids
           :initform '()
           :documentation "A list of 'rpx-otp-app-id' objects
           that are bundled in the release."))
  "An OTP release descriptor representation. NOTE: the name slot
  is inherited.")

(defclass rpx-otp-app-descriptor ()
  ((id
    :type rpx-otp-app-id
    :initarg :id
    :documentation "Id and version of the application")
   (descriptor-file
    :type string
    :initarg :descriptor-file
    :initform ""
    :documentation "The file name of the OTP
                    conform application descriptor.")
   (description
    :type string
    :initarg :description
    :initform ""
    :documentation "Description of application")
   (modules
    :type (satisfies (lambda(a) (rpxu-list-of a string)))
    :initarg :modules
    :initform '()
    :documentation "List of module names of this
            application.")
   (registered
    :type (satisfies (lambda(a) (rpxu-list-of a string)))
    :initarg :registered
    :initform '()
    :documentation "Registered names of processes in
               the application.")
   (included_applications
    :type (satisfies (lambda(a) (rpxu-list-of a string)))
    :initarg :included_applications
    :writer set-included_applications
    :initform '()
    :documentation "Applications which must
                          be loaded prior to this application.")
   (applications
    :type (satisfies (lambda(a) (rpxu-list-of a string)))
    :initarg :applications
    :initform '()
    :documentation "Applications which must be
                 started before this application.")
   (start-function
    :type (or string null)
    :initarg :start-function
    :initform nil
    :documentation "If not a library application,
                   the name of the module that implements the
                   'applicaiton' behaviour and starts the
                   application."))
  "Describes important properties of an application, as defined
   in the '.app' file.")

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; lexical-binding: t
;; End:

;;; rpx-otp.el ends here
