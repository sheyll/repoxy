(provide 'repoxy-erl-templates)
(require 'tempo)
(setq tempo-interactive t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User config.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Templates for erlan programming.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tempo-define-template
 "public-function"
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
 "Insert a Documented/Speced erlang function.")

(tempo-define-template
 "lbm-header"
 '("%%%=============================================================================" n
   "%%%" n
   "%%%               |  o __   _|  _  __  |_   _       _ _   (TM)" n
   "%%%               |_ | | | (_| (/_ | | |_) (_| |_| | | |" n
   "%%%" n
   "%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>" n
   "%%% @author Timo Koepke <timo.koepke@lindenbaum.eu>" n
   "%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>" n
   "%%% @author Olle Toernstroem  <olle.toernstroem@lindenbaum.eu>" n
   "%%% @copyright (C) 2012, Lindenbaum GmbH" n
   "%%%" n
   "%%%=============================================================================" n
   )
 nil "Lindenbaum source header")

(tempo-define-template
 "eunit"
 '(n
   "-module(" (buffer_module_name) ")." n
   n
   "-include_lib(\"eunit/include/eunit.hrl\")." n
   "-include_lib(\"lbm_test_lib/include/lbm_test_lib.hrl\")." n
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
 "test-erlymock"
 '((p "Function to test: " fun)"_test() ->" n
   > "process_flag(trap_exit, true)," n
   > "M = em:new()," n
   > "em:replay(M)," n
   n
   > "Res = " (buffer_module_name_sans_test) ":" (s fun) "(" r ")," n
   > "?assertMatch({ok, _}, Res)," n
   n
   > "em:verify(M)."
   n)
 nil
 "Create eunit test case with some erlymock setup.")

(tempo-define-template
 "hrl"
 '("-ifndef(" (uppercase_file_name) ")." n
   "-define(" (uppercase_file_name) ", true)." n
   n
   r n
   n
   "-endif. %% " (uppercase_file_name) n)
 nil
 "Create an erlang header file frame.")

(defun buffer_module_name()
  (concat
   (mapcar (lambda(c)
             (case c
               (?- ?_)
               (t c)))
           (downcase
            (file-name-sans-extension
             (file-name-nondirectory (buffer-file-name)))))))

(defun buffer_module_name_sans_test()
  (replace-regexp-in-string "_test$" "" (buffer_module_name)))

(defun uppercase_file_name()
  (concat
   (mapcar (lambda(c)
             (case c
               (?- ?_)
               (t c)))
           (upcase
            (file-name-sans-extension
             (file-name-nondirectory (buffer-file-name)))))))
