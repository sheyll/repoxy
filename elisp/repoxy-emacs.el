(provide 'repoxy-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User defined vairable section.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar repoxy-host "127.0.0.1"
  "The host name or IP that where a repoxy server is running")

(defvar repoxy-shell-command "erl -sname repoxy_emacs_shell2 -setcookie repoxy -remsh repoxy@localhost"
  "Shell command that is executed when 'repoxy-shell' is called.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repoxy High-Level API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-run()
  "Find the nearest 'repoxy' executable, run it and connect to it.

Starting with the current working directory, search each parent
recursivly for a file called 'repoxy'.  Connect to a repoxy
server running on localhost, and call rebar clean get-deps
compile. All applications are loaded into the node when rebar has
compiled them."
  (interactive)
  (repoxy-start-server)
  (repoxy-connect)
  (repoxy-rebar-clean-compile))

(defun repoxy-kill()
  "Disconnect from a repoxy server, kill all repoxy shells and kill
repoxy."
  (interactive)
  (ignore-errors (repoxy-disconnect))
  (ignore-errors (repoxy-kill-shell))
  (ignore-errors (repoxy-kill-server)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-Level Build API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-rebar-clean-compile()
  "Flushe the modules and applications currently contained in the
node and triggers rebar to compile all"
  (interactive)
  (repoxy-do '(reset))
  (repoxy-do '(rebar clean))
  (repoxy-do '(rebar "get-deps"))
  (repoxy-do '(rebar compile))
  (repoxy-do '(load_apps_into_node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repoxy invokation API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-start-server()
  "Find the nearest 'repoxy' executable relative the the filename of
the current buffer and run it.

Starting with the current working directory, search each parent
recursivly for a file called 'repoxy'."
  (interactive)
  (if (-repoxy-is-server-running)
      (error "REPOXY already running"))
  (setq -repoxy-server-buf
        (-repoxy-run-in-terminal "./repoxy" 't))
  (if (-repoxy-wait-for-output -repoxy-server-buf "Waiting for client" 10)
      (message "REPOXY repoxy started")
    (error "REPOXY start server failed")))

(defun repoxy-kill-server()
  "Stop the repoxy process."
  (interactive)
  (if (not (-repoxy-is-server-running))
      (error "REPOXY not running"))
  (-repoxy-kill-terminal -repoxy-server-buf)
  (setq -repoxy-server-buf nil)
  (message "REPOXY repoxy stopped"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-shell()
  "Open an erlang remote shell to the repoxy node."
  (interactive)
  (if (-repoxy-is-shell-running)
      (with-current-buffer -repoxy-shell-buf
        (select-window (or (get-buffer-window (current-buffer))
                           (display-buffer (current-buffer)))))
    (setq -repoxy-shell-buf
          (-repoxy-run-in-terminal repoxy-shell-command))))

(defun repoxy-kill-shell()
  "Kill an erlang shell and remove the buffer."
  (interactive)
  (if (not (-repoxy-is-shell-running))
      (error "REPOXY shell not running"))
  (-repoxy-kill-terminal -repoxy-shell-buf)
  (setq -repoxy-shell-buf nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-connect()
  "Connect to a repoxy server running on localhost without
reseting the project and recompiling everything."
  (interactive)
  (if (-repoxy-is-connected)
      (error "REPOXY already connected"))
  (setq -repoxy-receive-buffer nil)
  (setq -repoxy-result nil)
  (let ((process-adaptive-read-buffering 't))
    (if (not (ignore-errors
               (setq -repoxy-socket
                     (open-network-stream
                      (concat "repoxy-server-" repoxy-host)
                      'nil
                      repoxy-host
                      5678))))
        (error "REPOXY cannot connect to %s" repoxy-host))

    (message "REPOXY connected to %s" repoxy-host)
    (set-process-filter -repoxy-socket '-repoxy-capture-output)
    (set-process-sentinel -repoxy-socket '-repoxy-socket-sentinel)))

(defun repoxy-disconnect()
  "Disconnect from a repoxy instance. This does not cause repoxy
to reset itself, so a later reconnect does not require recompilation"
  (interactive)
  (if (not (-repoxy-is-connected))
      (error "REPOXY not connected"))
  (delete-process -repoxy-socket)
  (setq -repoxy-socket nil)
  (setq -repoxy-receive-buffer nil)
  (setq -repoxy-result nil)
  (message "REPOXY disconnected from server"))

(defun repoxy-do (request)
  "Send the request to repoxy returning the result. Request is an s-expression."
  (interactive)
  (if (-repoxy-is-connected)
      (progn
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
              (message "REPOXY request successful")
              repoxy-result)
          (progn
            (setq quit-flag 'nil)
            (message "REPOXY interrupted during request. Reconnecting")
            (repoxy-disconnect)
            (repoxy-connect))))
    (progn
      (error "REPOXY cannot execute request: not connected")
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal Erlang source buffer interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -repoxy-attach-to-buffer()
  "Add a save hook and enable the repoxy minor mode."
  (message "REPOXY attaching to buffer %s" (buffer-name (current-buffer)))
  (repoxy-mode)
  (add-hook 'after-save-hook
	    (function -repoxy-buffer-saved)))

(defun -repoxy-buffer-saved()
  "Compile the buffer if the file is part of the active repoxy project"
  (message "REPOXY compiling buffer %s" (buffer-name (current-buffer)))
  (repoxy-do `(compile_file ,(buffer-file-name (current-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs integration API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-setup()
  "Create all internal global variables and add the compile
function to the save hooks of erlang files."
  (interactive)
  (-repoxy-init-globals)
  (add-hook 'erlang-mode-hook '-repoxy-attach-to-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode and menu definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar -repoxy-mode-map
  (let ((the-map
         '(keymap
           (menu-bar .
                     (keymap
                      (repoxy "Repoxy" .
                               (keymap

                                (sep0 . (menu-item "Repoxy:"))
                                (sep0a . (menu-item "--"))
                                (repoxy-run-skel .
                                                 (menu-item "Run"
                                                            repoxy-run :keys "C-c C-v c"))
                                (repoxy-kill-skel .
                                                 (menu-item "Kill"
                                                            repoxy-kill :keys "C-c C-v d"))
                                (repoxy-shell-skell .
                                                 (menu-item "Shell"
                                                            repoxy-shell :keys "C-c C-v d"))
                                (sep0b . (menu-item "--"))

                                (sep1c . (menu-item "Rebar:"))
                                (sep1d . (menu-item "--"))
                                (refresh-skel .
                                                 (menu-item "Refresh"
                                                            repoxy-rebar-clean-compile :keys "C-c C-v r"))
                                (sep1e . (menu-item "--"))

                                (sep2 . (menu-item "Current Buffer:"))
                                (sep2a . (menu-item "--"))
                                )))))))
    (define-key the-map (kbd "C-c C-v c") 'repoxy-run)
    (define-key the-map (kbd "C-c C-v d") 'repoxy-kill)
    (define-key the-map (kbd "C-c C-v s") 'repoxy-shell)

    (define-key the-map (kbd "C-c C-v r") 'repoxy-rebar-clean-compile)
    the-map)
  "Repoxy minor mode keymap.")

(define-minor-mode repoxy-mode
  "Toggle repoxy integration mode.
With no argument,  this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.
"
  ; initial value
  nil
  ; mode line indicator
  " Repoxy"
  ; mode bindings
  -repoxy-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal global state functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -repoxy-init-globals()
  "Set all global variables to nil"
  (setq -repoxy-receive-buffer "")
  (setq repoxy-result nil)
  (setq -repoxy-socket nil)
  (setq -repoxy-server-buf nil)
  (setq -repoxy-shell-buf nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal repoxy invokation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -repoxy-is-server-running()
  "Return non-nil if the repoxy process is active inside emacs."
  (-repoxy-terminal-live-p -repoxy-server-buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal shell functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -repoxy-is-shell-running()
  "Return non-nil if the shell process is active."
  (-repoxy-terminal-live-p -repoxy-shell-buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal buffer/process functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -repoxy-run-in-terminal(command &optional non-interactive)
  "Execute a command string an emacs terminal window using /bin/bash.
If the optional argument non-interactive is not nil the command
will be run also with bash, and interaction with the
program is not possible.

The program is always executed with the project base dir as
current working directory."
  (let* ((default-directory (-repoxy-find-project-base-dir))
         (result-buf
          (if non-interactive
              (let* ((proc (start-process-shell-command
                            "*repoxy-non-interactive*" "*repoxy-non-interactive*"
                            command))
                     (buf (process-buffer proc)))
                (with-current-buffer buf
                  (select-window (or (get-buffer-window buf)
                                     (display-buffer buf)))
                  buf))
            (let ((buf (term "/bin/bash")))
              (with-current-buffer buf
                (select-window (or (get-buffer-window buf)
                                   (display-buffer buf)))
                (term-send-raw-string (concat "cd " default-directory
                                              " && " command "\n")))
              buf))))
    (if result-buf
        (progn
          (sit-for 1)
          (if (not (-repoxy-terminal-live-p result-buf))
              (error "REPOXY starting \"%s\" in terminal failed" command))
          (set-process-sentinel
           (get-buffer-process result-buf)
           (lambda (proc reason)
             (message "REPOXY sentinel for %s received %s" proc reason)))))
    result-buf))

(defun -repoxy-find-project-base-dir()
  "Return the first parent directory of the directory containing
  the current buffer file, that contains the repoxy executable."
  (let* ((current-depth 3)
         (current-file (buffer-file-name (current-buffer)))
         (current-dir
          (if current-file
              (file-name-directory
               (expand-file-name current-file))
            (expand-file-name default-directory)))
         (path-to-repoxy nil))
    (while
        (null
         (setq path-to-repoxy
               (directory-files current-dir :full-name "^repoxy$")))
      (if (= current-depth 0)
          (error "REPOXY cannot find repoxy executable"))
      (setq current-dir
            (expand-file-name
             (concat current-dir (file-name-as-directory ".."))))
      (setq current-depth (1- current-depth)))
    (file-name-directory (car path-to-repoxy))))

(defun -repoxy-terminal-live-p(buffer)
  "Return non-nil if the buffer and the process associated to the buffer are live."
  (and
   buffer
   (buffer-live-p buffer)
   (get-buffer-process buffer)
   (process-live-p (get-buffer-process buffer))))

(defun -repoxy-kill-terminal(buffer)
  "Kill the process associated to a buffern and kill the buffer."
  (if (get-buffer-process buffer)
      (kill-process buffer))
  (if (get-buffer-process buffer)
      (delete-process buffer))
  (if (bufferp buffer)
      (kill-buffer buffer)))

(defun -repoxy-wait-for-output(buffer regexp timeout)
  "Read the contents of `buffer' and do not return before
`regex' was found or `timeout' seconds have expired.
Returns the char-position of the match or nil"
  (with-current-buffer buffer
    (let ((retries timeout)
          (found nil)
          (failed nil))
      (while (and (not found) (not failed))
        (accept-process-output (get-buffer-process buffer) 1 0 :just-this-one)
        (goto-char 1)
        (if (null (setq found (re-search-forward regexp nil 't)))
            (if (= retries 0)
                (progn
                  (message "REPOXY expected output \"%s\" not received
                   from buffer %s within %s seconds" regexp buffer timeout)
                  (setq failed 't))
              (setq retries (1- retries)))))
      found)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal socket functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -repoxy-is-connected()
  "Return non-nil if a connection to a repoxy server is currently open."
  (and
   -repoxy-socket
   (eq (process-status -repoxy-socket) 'open)))

(defun -repoxy-socket-sentinel(process reason)
  "Reset the global variables and close the socket"
  (repoxy-disconnect)
  (message "REPOXY sentinel received: %s" reason))

(defun -repoxy-capture-output(process output)
  "`read' the s-expression received from the server until 'read'
was able to parse them, then update -repoxy-result with the
s-expression."
  (setq -repoxy-receive-buffer (concat -repoxy-receive-buffer output))
  (if (not (ignore-errors (setq repoxy-result (read -repoxy-receive-buffer))))
      (setq -repoxy-receive-buffer "")))
