
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User defined vairable section.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar repoxy-host "127.0.0.1"
  "The host name or IP that where a repoxy server is running")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-connect()
  "Connect to a repoxy server running on localhost."
  (if (not -repoxy-socket)
      (progn
        (-repoxy-clean-globals)
        (setq process-adaptive-read-buffering 't)
        (if (ignore-errors
              (setq -repoxy-socket
                    (open-network-stream
                     (concat "repoxy-server-" repoxy-host)
                     'nil
                     repoxy-host
                     5678)))
            (progn
              (message (concat "REPOXY connected to " repoxy-host))
              (set-process-filter -repoxy-socket '-repoxy-capture-output)
              (set-process-sentinel -repoxy-socket '-repoxy-socket-sentinel))
          (error (message (concat "REPOXY cannot connect to " repoxy-host)))))
        (error "REPOXY already connected")))

(defun repoxy-disconnect()
  "Disconnect from a repoxy instance. This does not cause repoxy
to reset itself, so a later reconnect does not require recompilation"
  (ignore-errors (delete-process -repoxy-socket))
  (message "REPOXY disconnect from server")
  (-repoxy-clean-globals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-rebar-clean-compile()
  "Flushed the modules and applications currently contained in the node and triggers rebar to compile all"
  (repoxy-do '(reset))
  (repoxy-do '(rebar clean))
  (repoxy-do '(rebar "get-deps")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; low-level custom request API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-do (request)
  "Send the request to repoxy returning the result. Request is an s-expression."
  (setq repoxy-result nil)
  (process-send-string -repoxy-socket (prin1-to-string request))
  (if
      (let ((inhibit-quit 't))
        (while (and (not repoxy-result) (not quit-flag))
          (accept-process-output -repoxy-socket 1 0 'just-this-one))
        (let ((interrupted quit-flag))
          (setq quit-flag nil)
          (not interrupted)))
        (progn
          (message "REPOXY request successful.")
          repoxy-result)
      (progn
        (setq quit-flag 'nil)
        (message "REPOXY interrupted during request. Reconnecting")
        (repoxy-disconnect)
        (repoxy-connect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNAL FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -repoxy-socket-sentinel(process reason)
  "Reset the global variables and close the socket"
  (repoxy-disconnect)
  (message (concat "REPOXY sentinel received: " reason)))

(defun -repoxy-clean-globals()
  "Reset the global variables"
  (setq -repoxy-data "")
  (setq repoxy-result nil)
  (setq -repoxy-socket nil))

(defun -repoxy-capture-output(process output)
  (setq -repoxy-data (concat -repoxy-data output))
  (if (not (ignore-errors (setq repoxy-result (read -repoxy-data))))
      (setq -repoxy-data "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy_simple_test()
  (repoxy-connect)
  (repoxy-rebar-clean-compile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module load section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(-repoxy-clean-globals)
