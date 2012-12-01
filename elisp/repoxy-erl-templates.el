;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Templates for erlan programming.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'repoxy-erl-templates)
(require 'tempo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialisation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar repoxy-templates-file-header
  '("%%%=============================================================================" n
    "%%% @author " (p "Author: " author) "<" (p "email: ") ">" n
    "%%% @copyright (C) " (format-time-string "%Y") ", " (s author)  n
    "%%%============================================================================="
    n )
  "Erlang source header template. NOTE: setting this variable
    after calling repoxy-templates-generate has no effects.")

(defvar repoxy-templates-eunit-module-extra-header
  '()
  "Erlang unit test template with additional definitons to
    include after the export section at the top of a test source
    file. NOTE: setting this variable after calling
    repoxy-templates-generate has no effects.")

(defconst repoxy-templates-menu
  '(repoxy-templates
    "Repoxy Templates"
    keymap
    (e00 . (menu-item "Private function"    tempo-template-rpx-fun-priv))
    (e01 . (menu-item "Documented function" tempo-template-rpx-fun-doc))
    (e02 . (menu-item "EUnit module header" tempo-template-rpx-test-header))
    (e03 . (menu-item "Test Case w/ Mock"   tempo-template-rpx-test-case-em))
    (e04 . (menu-item "HRL frame"           tempo-template-rpx-rpx-hrl-header)))
  "Menu/Keymap containing all repoxy templates - adapt as you see fit.")

(defun repoxy-templates-generate()
  "Initialise the templates."
  (setq tempo-interactive t)
  (tempo-define-template
   "rpx-fun-doc"
   '("%%------------------------------------------------------------------------------" n
     "%% @doc" n
     "%% " (p "Description: " descr) n
     "%% @end" n
     "%%------------------------------------------------------------------------------" n
     "-spec " (p "Name: " name)"(" (p "Args:|" args) ") ->" n
     > (p "return type: ") "." n
     (s name) "(" (s args) ") ->" n
     > "{error, not_yet_implemented}."n
     n)
   nil
   "Insert a Documented/Spec-ed erlang function.")

  (tempo-define-template
   "rpx-fun-priv"
   '("%%------------------------------------------------------------------------------" n
     "%% @private" n
     "%%------------------------------------------------------------------------------" n
     (p "Name: " name)"(" (p "Args:|" args) ") ->" n
     > r "." n
     n)
   nil
   "Insert a private erlang function.")

  (tempo-define-template
   "rpx-test-header"
   `(,@repoxy-templates-file-header
     n
     "-module(" (repoxy-templates-buffer_module_name) ")." n
     n
     "-include_lib(\"eunit/include/eunit.hrl\")." n
     ,@repoxy-templates-eunit-module-extra-header
     n
     "%%%=============================================================================" n
     "%%% TESTS" n
     "%%%=============================================================================" n
     n
     r n
     n
     "%%%=============================================================================" n
     "%%% Internal Functions" n
     "%%%=============================================================================" n
     n
     )
   nil
   "Create an empty eunit test module.")

  (tempo-define-template
   "rpx-test-case-em"
   '((p "Function to test: " fun)"_test() ->" n
     > "process_flag(trap_exit, true)," n
     > "M = em:new()," n
     > "em:replay(M)," n
     n
     > "Res = " (repoxy-templates-buffer_module_name_sans_test) ":" (s fun) "(" r ")," n
     > "?assertMatch({ok, _}, Res)," n
     n
     > "em:verify(M)."
     n)
   nil
   "Create eunit test case with some erlymock setup.")

  (tempo-define-template
   "rpx-hrl-header"
   `(,@repoxy-templates-file-header
     n
     "-ifndef(" (repoxy-templates-buffer_module_name) ")." n
     "-define(" (repoxy-templates-buffer_module_name) ", true)." n
     n
     r n
     n
     "-endif. %% " (repoxy-templates-buffer_module_name) n)
   nil
   "Create an erlang header file frame."))

(defun repoxy-templates-buffer_module_name()
  ""
  (concat
   (mapcar (lambda(c)
             (case c
               (?- ?_)
               (t c)))
           (downcase
            (file-name-sans-extension
             (file-name-nondirectory (buffer-file-name)))))))

(defun repoxy-templates-buffer_module_name_sans_test()
  (replace-regexp-in-string
   "_test$" ""
   (repoxy-templates-buffer_module_name)))
