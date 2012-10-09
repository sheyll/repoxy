(provide 'repoxy-emacs)
(require 'cl)
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
(defvar -repoxy-buffer-has-repoxy-header-line nil)
(make-variable-buffer-local '-repoxy-buffer-has-repoxy-header-line)

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
        (repoxy-do '(reset))
        (repoxy-do '(rebar clean))
        (repoxy-do '(rebar "get-deps"))
        (repoxy-do '(rebar compile) 't)
        (repoxy-do '(load_apps_into_node))
        (setq -repoxy-app-paths (repoxy-do '(get_app_paths)))
        (compilation-mode)
        (-repoxy-undecorate-buffers)
        (-repoxy-update-erl-buffer-headers)
        (-repoxy-highlight-compiler-results)))))

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
      (start-process "*repoxy-server*" buf (-repoxy-find-repoxy-executable))
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

(defun -repoxy-find-repoxy-executable()
  "Find a repoxy executable either in the `default-directory' or
in the repoxy load path."
  (interactive)
  (let* ((path (find "repoxy" load-path :test 'string-match-p))
         (repoxy-dir (-repoxy-find-in-parent-dir "repoxy"
                                                 (file-name-directory path)))
         (repoxy (concat (file-name-directory repoxy-dir) "repoxy")))
    (and
     (file-readable-p repoxy)
     (file-executable-p repoxy)
     (message "REPOXY using repoxy executable: %s" repoxy)
     repoxy)))

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
  (-repoxy-undecorate-buffers)
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
          (when clean-buffer
            (fundamental-mode)
            (delete-region (point-min) (point-max)))
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
        (when (-repoxy-is-connected)
          (-repoxy-highlight-compiler-results-in-buffer (current-buffer))
          (-repoxy-update-erl-buffer-header (current-buffer)))
        (add-hook 'after-save-hook
                  (function -repoxy-buffer-saved)))))

(defun -repoxy-buffer-saved()
  "Compile the buffer if the file is part of the active repoxy
project"
  (if (-repoxy-is-connected)
      (let ((current-file (-repoxy-buffer-erl-source)))
        (if current-file
            (repoxy-compile-file current-file)))))

(defun repoxy-compile-file(file)
  "Compile a file via repoxy and set the global variable
-repoxy-compilation-results to the compilation result."
  (message "REPOXY compiling %s" file)
  (-repoxy-remove-compilation-results-for-file file)
  (-repoxy-merge-compilation-results (repoxy-do `(compile_file ,file) 't))
  (with-current-buffer -repoxy-server-buf
    (compilation-mode))
  (-repoxy-update-erl-buffer-headers)
  (-repoxy-highlight-compiler-results))

(defun -repoxy-remove-compilation-results-for-file(in-file)
  "Remove all entries for 'file' in -repoxy-compilation-results"
  (let ((file (expand-file-name in-file)))
    (setq -repoxy-compilation-results
          (remove* file -repoxy-compilation-results
                   :key '-repoxy-err-info-file
                   :test 'string=))))

(defun -repoxy-merge-compilation-results(err-infs)
  "Add error infos from a compilation request into
  -repoxy-compilation-results,"
  (setq -repoxy-compilation-results
        (sort
         (append -repoxy-compilation-results err-infs)
         (lambda(i1 i2)
           (let ((file1 (-repoxy-err-info-file i1))
                 (file2 (-repoxy-err-info-file i2))
                 (line1 (-repoxy-err-info-line i1))
                 (line2 (-repoxy-err-info-line i2)))
             (or (string< file1 file2)
                 (and (string= file1 file2) (< line1 line2))))))))

(defun -repoxy-err-info-type(err-info)
  "Get the type, either warning or error from an err-info"
  (aref err-info 0))

(defun -repoxy-err-info-file(err-info)
  "Get the file name from an err-info"
  (aref err-info 1))

(defun -repoxy-err-info-line(err-info)
  "Get the line number from an err-info"
  (aref err-info 2))

(defun -repoxy-err-info-msg(err-info)
  "Get the message from an err-info"
  (aref err-info 3))

(defun -repoxy-highlight-compiler-results()
  "Highlight errors and warnings that resulted from a previous
call to repoxy-compile-file in all buffers currently open."
  (interactive)
  (mapcar
   (lambda(buf)
     (when (buffer-file-name buf)
       (-repoxy-highlight-compiler-results-in-buffer buf)))
   (buffer-list)))

(defun -repoxy-highlight-compiler-results-in-buffer(buf)
  "Take a look at -repoxy-compilation-results and highlight all
  warnings and error concerning the file opened by 'buf'."
  (let ((current-file (buffer-file-name buf)))
    (when current-file
      (save-excursion
        (with-current-buffer buf
          (remove-overlays 'nil 'nil 'compilation-error-overlay 't)
          (mapcar
           '-repoxy-add-overlay-compiler-result-current-buffer
           (-repoxy-compilation-results-for-file current-file)))))))

(defun -repoxy-add-overlay-compiler-result-current-buffer(err_info)
  "Put a highlight and tooltip for a single compilation result
into the current buffers overlay."
  (goto-char 1)
  (let* ((type (-repoxy-err-info-type err_info))
         (line-number (-repoxy-err-info-line err_info))
         (msg (-repoxy-err-info-msg err_info))
         (start-pos (line-beginning-position line-number))
         (end-pos (line-end-position line-number))
         (ov (make-overlay start-pos end-pos)))
    (overlay-put ov 'compilation-error-overlay 't)
    (overlay-put ov 'face type)
    (overlay-put ov 'help-echo  (format "%s: %s" type msg))))

(defun -repoxy-compilation-results-for-file(current-file)
  "Get a list of compilation warnings/error for a file by
filtering -repoxy-compilation-results."
  (let ((current-file (expand-file-name current-file)))
    (remove* current-file -repoxy-compilation-results
             :key '-repoxy-err-info-file
             :test-not 'string=)))

(defun repoxy-goto-prev-error()
  "Goto to the nearest compiler message before (point)."
  (interactive)
  :todo)

(defun repoxy-goto-next-error()
  "Goto to the nearest compiler message after (point)."
  (interactive)
  (let* ((c-file (expand-file-name (buffer-file-name)))
         (c-line (1+ (count-lines (point-min) (point)))))
    (-repoxy-visit-err-info
     (or (find-if (lambda(i)
                    (let ((i-file (-repoxy-err-info-file i))
                          (i-line (-repoxy-err-info-line i)))
                      (or
                       (and (string= i-file c-file)
                            (> i-line c-line))
                       (and (string< c-file i-file)))))
                  -repoxy-compilation-results)
         ;; nothing after, wrap around:
         (car -repoxy-compilation-results)))))

(defun -repoxy-visit-err-info(err-info)
  "Open the file and go to the line where an error/warning was found"
    (when err-info
      (let ((e-file (-repoxy-err-info-file err-info))
            (e-line (-repoxy-err-info-line err-info)))
        (and (find-file e-file)
             (goto-line e-line)))))

(defun -repoxy-update-erl-buffer-headers()
  "Update the header-lines of in all erlang buffers currently open."
  (interactive)
  (mapcar
   (lambda(buf)
     (when (buffer-file-name buf)
       (-repoxy-update-erl-buffer-header buf)))
   (buffer-list)))

(defun -repoxy-update-erl-buffer-header(buf)
  "Show a nice info header line in the current buffer if it is managed by repoxy"
  (save-excursion
    (with-current-buffer buf
      (when (and (-repoxy-is-connected)
                 (-repoxy-buffer-erl-source))
        (let* ((erl-file (-repoxy-buffer-erl-source))
               (app-name (car (-repoxy-lookup-app erl-file)))
               (compilation-errors (length
                                    (-repoxy-compilation-results-for-file erl-file))))
          (setq -repoxy-buffer-has-repoxy-header-line 't)
          (setq header-line-format
                (concat
                 (propertize " *REPOXY CONNECTED*   " 'face 'bold)
                 (format "%s" app-name)
                 (when (> compilation-errors 0)
                   (propertize (format "  Errors/Warnings: %d" compilation-errors) 'face 'error))))))
      (force-mode-line-update))))

(defun -repoxy-undecorate-buffers()
  "Remove a headerline and compilation result overlays from all
buffers modified by repoxy."
  (interactive)
  (mapcar
   (lambda(buf)
     (when (buffer-file-name buf)
       (-repoxy-undecorate-buffer buf)))
   (buffer-list)))

(defun -repoxy-undecorate-buffer(buf)
  "Remove a headerline and compilation result overlays from a
buffer modified by repoxy."
  (save-excursion
    (with-current-buffer buf
      (when -repoxy-buffer-has-repoxy-header-line
        (setq -repoxy-buffer-has-repoxy-header-line nil)
        (setq header-line-format nil))
      (when (buffer-file-name)
        (remove-overlays 'nil 'nil 'compilation-error-overlay 't)))))

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
                 file-and-dir)))))

(defun -repoxy-lookup-app(file)
  "Find the application name and base directory (parent of src/
include/ test/) of a file, or nil if the file is not part of an
application currently loaded by repoxy. If the file is not part
of an application that has successfully been compiled be the most
recent rebar invokation, return `nil'."
  (concatenate
   'list
   (find file -repoxy-app-paths
         :test (lambda(file appname-dir-pair)
                 (let ((dir (aref appname-dir-pair 1)))
                   (string-prefix-p
                        (expand-file-name dir)
                        (expand-file-name file)))))))

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

                                (sep4 . (menu-item "Erlang:"))
                                (sep5 . (menu-item "--"))
                                (refresh-skel .
                                                 (menu-item "clean + get_deps + compile"
                                                            repoxy-rebar-clean-compile :keys "C-c C-v r"))
                                (toggle-impl-test-skel .
                                                 (menu-item "Toggle between test/impl"
                                                            repoxy-toggle-impl-test :keys "<F5>"))
                                (prev-error-skel .
                                                 (menu-item "Goto previous error"
                                                            repoxy-goto-prev-error :keys "<F3>"))
                                (prev-error-skel .
                                                 (menu-item "Goto next error"
                                                            repoxy-goto-next-error :keys "<F4>"))
                                )))))))
    (define-key the-map (kbd "C-c C-v c") 'repoxy-run)
    (define-key the-map (kbd "C-c C-v d") 'repoxy-kill)
    (define-key the-map (kbd "C-c C-v s") 'repoxy-shell)
    (define-key the-map (kbd "C-c C-v r") 'repoxy-rebar-clean-compile)
    (define-key the-map (kbd "<f3>") 'repoxy-goto-prev-error)
    (define-key the-map (kbd "<f4>") 'repoxy-goto-next-error)
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
  "Return a working directory for the current project. First
search for a executable called 'repoxy' in the parent
directories, when this failes, try to find a rebar.config, if that fails too
assume \"../\" as project dir."
  (or (-repoxy-find-in-parent-dir "repoxy")
      (-repoxy-find-in-parent-dir "rebar.config")
      (-repoxy-find-in-parent-dir "rebar.config.src")
      (-repoxy-find-in-parent-dir "rebar")
      (expand-file-name (file-name-as-directory ".."))))

(defun -repoxy-find-in-parent-dir(file &optional start-dir)
  "Find a parent directory containing 'file' starting from the
directory of the file in the current buffer. Alternately if
'start-dir' is non-nil start from there."
  (let* ((current-depth 3)
         (current-file (buffer-file-name (current-buffer)))
         (base-dir nil)
         (path (or start-dir
                   (if current-file
                       (file-name-directory (expand-file-name current-file))
                     (expand-file-name default-directory)))))
    (while (and (>= current-depth 0) (null base-dir))
      (if (file-regular-p (concat path file))
          (setq base-dir path)
        (progn
          (setq path
                (expand-file-name
                 (concat path (file-name-as-directory ".."))))
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
