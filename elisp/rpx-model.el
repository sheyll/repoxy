(provide 'rpx-model)
(require 'cl)
(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)
(require 'speedbar)
(require 'easymenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGINING OF META
;; END OF META
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rpx-erlang-node ()
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

(defclass rpx-client-msg ()
  ((content)
   (msg-type :type (member :s-exp-msg :text-msg :event-msg)
             :initarg msg-type
             :initform :s-exp-msg)
   (length))
  (:abstract t :documentation "A message with type, length and content fields."))


(defclass rpx-client-request ()
  ((to-send :type rpx-client-msg
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

(defclass rpx-tcp-client ()
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
  (active-request   :type (or rpx-client-request null)
                    :initform nil)
  (request-queue    :type (satisfies
                           (lambda(a) (rpxu-list-of a rpx-client-request)))
                    :initform '())
  (event-hooks      :type list
                    :initarg event-hooks
                    :initform nil
                    :documentation "These functions are called when an event from the server was received")
  (status           :type (member :idle :sending :receiving :error)
                    :initform :idle))
 "A repoxy client that can queue and send commands to a repoxy server.")

(defclass rpx-server ()
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/dev/tools/elisp/")
