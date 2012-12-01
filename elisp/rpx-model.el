(provide 'rpx-p)
(require 'cl)
(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)
(require 'speedbar)
(require 'easymenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; META META
(require 'tempo)
(defvar eieio-tempo-tags nil "Tempo tags for eieio")

(tempo-use-tag-list 'eieio-tempo-tags)
;; END OF META META
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq lexical-binding t)


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
                    :initarg :host)
  (port             :type number
                    :initarg :port)
  (process          :type process
                    :initarg :process)
  (send-buf         :type buffer
                    :initarg :send-buf)
  (active-job       :type (or rpx-server-job null)
                    :initform nil)
  (job-queue        :type list
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


(defclass rpx-otp-rel-descriptor
 :rel-file
                  :rel-name
                  :rel-ver
                  :included-apps (list (repoxy-app-id
                                        :name
                                        :version))

(defclass rpx-project ()
  ((directory :initarg :directory
              :initform ""
              :type string
              :documentation "Top level directory of the erlang project.")
   (server :initform nil
           :type (or null rpx-server))
   (rel-descriptor :initargs :rel-descriptor
                   :type (or null rpx-otp-rel-descriptor)
   ) "A class defining a repoxy managed erlang project")

(repoxy-project
 :rel-descriptor )
 :apps (list repoxy-app
             :base-dir
             :app-descriptor (repoxy-app-descriptor
                              :id repoxy-app-id
                              :app-descriptor-file
                              :required-apps
                              :registered-modules
                              :start-function)
              :priv-dir
              :include-dir
              :source-dir
              :c-source-dir
              :test-dir
              :overview-edoc
              :cover-overview
              :erlang-sources (list repoxy-relang-source
                                    :includes (list repoxy-source-file)
                                    :macros (list repoxy-erl-macro-id
                                                  :name
                                                  :arity)
                                    :records (list repoxy-erl-record
                                                   :name)
                                    :type-defs (list repoxy-erl-type-def
                                                     :name
                                                     :arity)
                                    :functions (list repoxy-erl-function-id)
                                    :module (repoxy-erl-module
                                             :id (repoxy-erl-module-id :name)
                                             :edoc
                                             :otp-info (repoxy-erl-otp-info
                                                        :type-of-registration
                                                        :otp-behaviours (list repoxy-erl-module-id)
                                                        :otp-supervisor-children (list repoxy-erl-module-id)
                                                        :otp-gen_event-handlers (list repoxy-erl-module-id))
                                             :behaviours (list repoxy-erl-module-id)
                                             :callbacks (repoxy-erl-spec
                                                         :mod repoxy-erl-module-id
                                                         :name
                                                         :arg-count)
                                             :exported-functions (list repoxy-erl-function-id)
                                             :exported-types (list repoxy-erl-type-def))
                                    :covered-lines (list repoxy-line-coverage
                                                         :file
                                                         :line
                                                         :covered not | full | partial))
              :erlang-test-sources (list repoxy-erlang-source)
              :erlang-include-sources (list repoxy-erlang-source)))

  :deps (list repoxy-rebar-dependency
              :source
              :version)
