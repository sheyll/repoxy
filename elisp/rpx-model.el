(provide 'rpx-model)
(require 'cl)
(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)
(require 'speedbar)
(require 'easymenu)


(setq lexical-binding t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGINING OF META
;; END OF META
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rpx-erlang-node (eieio-named)
  ((node :type string
         :initarg :node
         :custom string
         :documentation "Erlang vm node name either, long or short."
         :initform "")
   (cookie :type string
           :initarg :cookie
           :custom string
           :documentation "Optional security cookie used by erlang vm to connect to"
           :initform "")
   (remsh-buffer :type (or buffer null) :initform nil))
  "An erlang node that can be connected to via erl -remsh")


(defclass rpx-server-job (eieio-named)
  ((to-send :type list
            :initarg :to-send
            :documentation "The lisp expression to send."
            :initform '())
   (on-success-f :type function
                 :initarg :on-success-f
                 :initform ignore)
   (on-error-f :type function
               :initar :on-error-f
               :initform ignore))
  "A job for the repoxy server a string to send to the repoxy server.")

(defclass rpx-tcp-client (eieio-named)
 ((host             :type string
                    :initform "localhost"
                    :initarg :host)
  (port             :type number
                    :initform 51979
                    :initarg :port)
  (process          :type process
                    :initarg :process)
  (send-buf         :type buffer
                    :initarg :send-buf)
  (active-job       :type (or rpx-server-job null)
                    :initform nil)
  (job-queue        :type (satisfies (lambda(a) (rpxu-list-of a rpx-server-job)))
                    :initform '())
  (status           :type (member :idle :sending :receiving :error)
                    :initform :idle))
 "A repoxy client that can queue and send commands to a repoxy server.")

(defclass rpx-server (eieio-named)
  ((rpx-process          :type process
                         :initarg :rpx-process)
   (erlang-node          :type rpx-erlang-node
                         :initarg :erlang-node)
   (tcp-client           :type rpx-tcp-client
                         :initarg :rpx-tcp-client))
  "The facade to a repoxy server. Contains the actual server
  process, it's erlang node parameters and a client through which
  commands can be send to the server.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; erlang/otp classes

(defclass rpx-otp-app-id (eieio-named)
  ((vsn :type string
        :initarg :vsn
        :initform "0.0"
        :documentation "Version"))
  "Name and version of an OTP application. NOTE: the name slot is inherited")

(defclass rpx-otp-rel-descriptor (eieio-named)
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

(defclass rpx-otp-app-descriptor (eieio-named)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defclass rpx-prj-app (eieio-named)
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

(defclass rpx-prj ()
  ((base-dir  :initarg :base-dir
              :initform ""
              :type string
              :documentation "Top level directory of the erlang project.")
   (server :initform nil
           :type (or null rpx-server))
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
(add-to-list 'load-path "~/dev/tools/elisp/")
