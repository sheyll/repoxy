(provide 'repoxy-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User defined vairable section.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar repoxy-host "127.0.0.1"
  "The host name or IP that where a repoxy server is running")

(defvar repoxy-shell-command "erl -sname repoxy_emacs_shell2 -setcookie repoxy -remsh repoxy@localhost"
  "Shell command that is executed when 'repoxy-shell' is called.")

(defvar repoxy-open-non-existant-tests-or-impl nil
  "When set to non-nil repoxy opens a non-existant file when
  toggling between erlang impl-source and test-source file via
  `repoxy-toggle-impl-test'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal global variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar -repoxy-receive-buffer "")
(defvar repoxy-result nil)
(defvar -repoxy-socket nil)
(defvar -repoxy-server-buf nil)
(defvar -repoxy-shell-buf nil)
(defvar -repoxy-app-paths nil)
(defvar -repoxy-dir nil)
(defvar -repoxy-compilation-results nil)

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
  (if (-repoxy-is-connected)
    (with-current-buffer -repoxy-server-buf
      (let ((inhibit-read-only 't)
            (select-window (or (get-buffer-window (current-buffer))
                               (display-buffer (current-buffer)))))
        (setq -repoxy-compilation-results nil)
        (kill-region (point-min) (point-max))
        (repoxy-do '(reset))
        (repoxy-do '(rebar clean))
        (repoxy-do '(rebar "get-deps"))
        (repoxy-do '(rebar compile) 't)
        (repoxy-do '(load_apps_into_node))
        (setq -repoxy-app-paths (repoxy-do '(get_app_paths)))))))

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

  (let ((buf (get-buffer-create "*repoxy-server*")))
    (with-current-buffer buf
      (select-window (or (get-buffer-window (current-buffer))
                         (display-buffer (current-buffer))))
      (setq default-directory (-repoxy-find-project-base-dir))
      (start-process "*repoxy-server*" buf (concat default-directory "repoxy"))
      (if (-repoxy-wait-for-output
           buf
           (regexp-quote "Waiting for client.")
           10)
          (progn
            (setq -repoxy-server-buf buf)
            (setq buffer-read-only 't)
            (setq -repoxy-dir default-directory)
            (message "REPOXY repoxy started"))
        (progn
          (-repoxy-kill-buffer buf)
          (error "REPOXY start server failed"))))))

(defun repoxy-kill-server()
  "Stop the repoxy process."
  (interactive)
  (if (not (-repoxy-is-server-running))
      (error "REPOXY not running"))
  (-repoxy-kill-buffer -repoxy-server-buf)
  (setq -repoxy-server-buf nil)
  (setq -repoxy-dir nil)
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
  (-repoxy-kill-buffer -repoxy-shell-buf)
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
  (setq repoxy-result nil)
  (let ((process-adaptive-read-buffering 't))
    (if (not (ignore-errors
               (setq -repoxy-socket
                     (open-network-stream
                      (concat "repoxy-server-" repoxy-host)
                      nil
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
  (setq -repoxy-receive-buffer nil)
  (setq repoxy-result nil)
  (setq -repoxy-app-paths nil)
  (if (not (-repoxy-is-connected))
      (error "REPOXY not connected"))
  (delete-process -repoxy-socket)
  (setq -repoxy-socket nil)
  (message "REPOXY disconnected from server"))

(defun repoxy-do (request &optional clean-buffer)
  "Send the request to repoxy returning the result. Request is an
s-expression. If the optional clean-buffer parameter is non-nil
the repoxy server output buffer is cleared before running the
request."
  (interactive)
  (if (-repoxy-is-connected)
      (with-current-buffer -repoxy-server-buf
        (let ((inhibit-read-only 't))
          (if clean-buffer
              (kill-region (point-min) (point-max)))
          (setq repoxy-result nil)
          (process-send-string -repoxy-socket (prin1-to-string request))
          (if
              (let ((inhibit-quit 't))
                (while (and (not repoxy-result) (not quit-flag))
                  (accept-process-output -repoxy-socket 1))
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
              (repoxy-connect)))))
    (progn
      (error "REPOXY cannot execute request: not connected")
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDE functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-toggle-impl-test()
 "Toggle between implementation source and test source of an erlang module.
  Rely on the standard directory layout.
  If the current buffer contains a file called A_test.erl or A_tests.erl,
  open ../src/A.erl, otherwise open ../test/A_tests.erl or ../test/A_test.erl.
  Do nothing if the file does not exists."
 (interactive)
 (let ((file-and-dir (-repoxy-buffer-erl-source)))
   (if file-and-dir
       (let* ((dir (file-name-directory file-and-dir))
              (file (file-name-nondirectory file-and-dir))
              (module (file-name-sans-extension file)))
                                        ; find out if we are in source or test folder
         (if (and (string-match-p "test/$" dir)
                  (string-match-p "_tests?$" module))
             (let* ((test-suffix-index (string-match "_tests?$" module))
                    (impl (substring module 0 test-suffix-index))
                    (pot-impl-dir (expand-file-name (concat dir
                                                            (file-name-as-directory "..")
                                                            (file-name-as-directory "src"))))
                    (existing-impl-file (car (directory-files pot-impl-dir 't
                                                              (concat "^" impl ".erl$"))))
                    (impl-file (or existing-impl-file (concat pot-impl-dir impl ".erl"))))
                                        ; in test folder, in a file called impl_tests?,erl
               (if (or (file-readable-p impl-file)
                       repoxy-open-non-existant-tests-or-impl)
                   (find-file impl-file)
                 (message "REPOXY file %s not found, set repoxy-open-non-existant-tests-or-impl to non-nil to open anyway" impl-file)))
                                        ; ELSE not in test/ and not _test(s).erl open. In impl file?
           (if (string-match-p "src/$" dir)
               (let* ((pot-test-dir (expand-file-name (concat dir
                                                              (file-name-as-directory "..")
                                                              (file-name-as-directory "test"))))
                      (existing-test-file (car (directory-files pot-test-dir 't
                                                                (concat "^" module "_tests?.erl$"))))
                      (test-file (or existing-test-file (concat pot-test-dir module "_tests.erl"))))
                                        ; in test folder, in a file called impl_tests?,erl
                 (if (or (file-readable-p test-file)
                         repoxy-open-non-existant-tests-or-impl)
                     (find-file test-file)
                   (message "REPOXY file %s not found, set repoxy-open-non-existant-tests-or-impl to non-nil to open anyway" test-file)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal low-level IDE functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -repoxy-attach-to-buffer()
  "Add a save hook and enable the repoxy minor mode."
  (if (-repoxy-buffer-erl-source)
      (progn
        (message "REPOXY attaching to buffer %s" (buffer-name (current-buffer)))
        (repoxy-mode)
        (add-hook 'after-save-hook
                  (function -repoxy-buffer-saved)))))

(defun -repoxy-buffer-saved()
  "Compile the buffer if the file is part of the active repoxy project"
  (if (-repoxy-is-connected)
      (let ((current-file (-repoxy-buffer-erl-source)))
        (if current-file
            (repoxy-compile-file current-file)))))

;; TODO Move:
(defun repoxy-compile-file(file)
  "Compile a file via repoxy and set the global variable -repoxy-compilation-results to the compilation result."
  (message "REPOXY compiling %s" file)
  (setq -repoxy-compilation-results (repoxy-do `(compile_file ,file) 't)))

(defun -repoxy-buffer-erl-source()
  "Return the expanded buffer file name, if the file opened in
  curren buffer is in a caconical source directory and ends with
  \".erl\". The file must also be part of the current project."
  (let ((file-and-dir-short (buffer-file-name (current-buffer))))
    (if file-and-dir-short
        (let* ((file-and-dir (expand-file-name file-and-dir-short))
               (dir (file-name-directory file-and-dir))
               (file (file-name-nondirectory file-and-dir)))
          (if (and
               (or ; if -repoxy-dir is defined, dir must start with -repoxy-dir
                (null -repoxy-dir)
                (string-prefix-p -repoxy-dir dir)) ; file must be part of the current project
               (or ; either source or test:
                (and
                 (string-match-p "src/$" dir)
                 (string-match-p ".erl$" file))
                (and
                 (string-match-p "test/$" dir)
                 (string-match-p "_tests?.erl$" file))))
              file-and-dir
            (progn
             (message "REPOXY %s not accepted as erlang source." file-and-dir-short)
             nil)))
      nil)))

(defun -repoxy-erl-source-of-loaded-app(file)
  "Determine the type of the file:
`t' - if file is recognized as erlang source file (excluding headers)
`nil' - not erlang source"
 (if (-repoxy-lookup-app-dir file)
     (case (file-name-extension file)
       ("erl" 't)
       ("app.src" 't)
       ("hrl" 't))))

(defun -repoxy-lookup-app-dir(file)
  "Find the application base directory (parent of src/ include/
test/) a file. If the file is not part of an application that has
successfully been compiled be the most recent rebar invokation,
return `nil'."
  (concatenate
   'list
   (mapcar
    (lambda(appname-dir-pair)
      (let ((dir (aref appname-dir-pair 1)))
        (if (string-prefix-p
             (expand-file-name dir)
             (expand-file-name file))
            dir
          '())))
    -repoxy-app-paths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs integration API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repoxy-setup()
  "Create all internal global variables and add the compile
function to the save hooks of erlang files."
  (interactive)
  (add-hook 'find-file-hook '-repoxy-attach-to-buffer))

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
                                (sep1 . (menu-item "Repoxy:"))
                                (sep2 . (menu-item "--"))
                                (repoxy-run-skel .
                                                 (menu-item "Run Repoxy Server"
                                                            repoxy-run :keys "C-c C-v c"))
                                (repoxy-kill-skel .
                                                 (menu-item "Kill Repoxy Server"
                                                            repoxy-kill :keys "C-c C-v d"))
                                (repoxy-shell-skell .
                                                 (menu-item "Open Repoxy RemShell"
                                                            repoxy-shell :keys "C-c C-v d"))
                                (sep3 . (menu-item "--"))

                                (sep4 . (menu-item "Rebar:"))
                                (sep5 . (menu-item "--"))
                                (refresh-skel .
                                                 (menu-item "clean/get-deps/compile"
                                                            repoxy-rebar-clean-compile :keys "C-c C-v r"))
                                (sep6 . (menu-item "--"))
                                (sep7 . (menu-item "Emacs:"))
                                (toggle-impl-test-skel .
                                                 (menu-item "Toggle between test/impl"
                                                            repoxy-toggle-impl-test :keys "<F5>"))
                                )))))))
    (define-key the-map (kbd "C-c C-v c") 'repoxy-run)
    (define-key the-map (kbd "C-c C-v d") 'repoxy-kill)
    (define-key the-map (kbd "C-c C-v s") 'repoxy-shell)
    (define-key the-map (kbd "C-c C-v r") 'repoxy-rebar-clean-compile)
    (define-key the-map (kbd "<f5>") 'repoxy-toggle-impl-test)
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

(defun -repoxy-run-in-terminal(command)
  "Execute a command string an emacs terminal window using /bin/bash.

The program is always executed with the project base dir as
current working directory."
  (let* ((default-directory (-repoxy-find-project-base-dir))
         (result-buf (term "/bin/bash")))
    (with-current-buffer result-buf
      (select-window (or (get-buffer-window result-buf)
                         (display-buffer result-buf)))
      (term-send-raw-string (concat "cd " default-directory
                                    " && " command "\n"))
      result-buf)))

(defun -repoxy-find-project-base-dir()
  "Return the first parent directory of the directory containing
  the current buffer file, that contains the repoxy executable."
  (let* ((current-depth 3)
         (current-file (buffer-file-name (current-buffer)))
         (base-dir nil)
         (path-to-repoxy
          (if current-file
              (file-name-directory (expand-file-name current-file))
            (expand-file-name default-directory))))
    (while (and (>= current-depth 0)
                (null base-dir))
      (if (directory-files path-to-repoxy :full-name "^repoxy$")
          (setq base-dir path-to-repoxy)
        (progn
          (setq path-to-repoxy
                (expand-file-name
                 (concat path-to-repoxy (file-name-as-directory ".."))))
          (setq current-depth (1- current-depth)))))
    base-dir))

(defun -repoxy-terminal-live-p(buffer)
  "Return non-nil if the buffer and the process associated to the buffer are live."
  (and
   buffer
   (buffer-live-p buffer)
   (get-buffer-process buffer)
   (process-live-p (get-buffer-process buffer))))

(defun -repoxy-kill-buffer(buffer)
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
        (goto-char 1)
        (if (null (setq found (re-search-forward regexp nil 't)))
            (if (= retries 0)
                (progn
                  (message "REPOXY expected output \"%s\" not received
                   from buffer %s within %s seconds" regexp buffer timeout)
                  (setq failed 't))
              (progn
                (setq retries (1- retries))
                (sleep-for 1)))))
      (goto-char (point-max))
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
  (message "REPOXY received output %s" output)
  (if (ignore-errors (setq repoxy-result (read -repoxy-receive-buffer)))
      (setq -repoxy-receive-buffer "")))
