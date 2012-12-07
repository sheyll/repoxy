;;; rpx-erl.el --- Repoxy's Simplified Erlang Syntax Tree

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

;; These classes represent a simplified version of Erlang forms and source
;; files, and to a very small extend functions. They enable showing high-level
;; project structure, as well as in editor help on items(like remote function
;; calls) not contained in a source buffer.


;;; Code:

(provide 'rpx-erl)

(require 'cl)
(require 'eieio)
(require 'rpxu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common attributes

(defclass rpx-erl-location ()
  ((file :type string
         :initarg :file
         :documentation "The filename part of the location.")
   (line :type number
         :initarg :line
         :initform 0
         :documentation "Optional linenumber")
   (line-count :type number
               :initarg :line-count
               :initform 1
               :documentation "Number of lines that a location
               contains."))
  "Represents objects assoctiated with specific file and
optionally line and column.")

(defclass rpx-erl-has-location ()
  ((loc :type rpx-erl-location
        :initarg :loc
        :documentation "Location relative to a source."))
  (:abstract t :documentation "Base class for elements that are
  located in a sourcefile"))

(defclass rpx-erl-has-arity ()
  ((artiy :type number
          :initarg :arity
          :documentation "Arity of something."))
  (:abstract t :documentation "Base class for everything that has
  an arity."))

(defclass rpx-erl-has-name ()
  ((name :type string
         :initarg :name
         :documentation "Arity of something."))
  (:abstract t :documentation "Base class for everything that has
  an arity."))

(defclass rpx-erl-has-module ()
  ((module :type (or string null)
           :initarg :module
           :initform nil
           :documentation "Name of the module something belongs to"))
  (:abstract t :documentation "Base class for elements that might
  be associated with module."))

(defclass rpx-erl-has-comment ()
  ((comment :type (or null rpx-erl-comment)
            :initarg :comment
            :initform nil
            :documentation "The optional comment associated with an rpx-syn."))
  (:abstract t :documentation "Base class for elements that might
  be associated with commentary."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplified erlang expressions

(defclass rpx-erl-name-arity (rpx-erl-has-name
                              rpx-erl-has-arity)
  ()
  "A class for Name/Arity elements.")

(defclass rpx-erl-MFA (rpx-erl-has-module
                       rpx-erl-name-arity)
  ()
  "A class for Module:Function/Arity elements.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplified erlang forms

(defclass rpx-erl-comment (rpx-erl-has-location)
  ((has-@doc :type boolean
             :initarg :has-@doc
             :initform nil
             :documentation "Indicates the presence of a '@doc'
   tag in the comment body."))
  "A possible multi line comment in a source file.")

(defclass rpx-erl-text (rpx-erl-has-location)
  ()
  "A part of a source file that erl_dodger classifies as
  'text'.")

(defclass rpx-erl-behaviour (rpx-erl-has-module
                             rpx-erl-has-location
                             rpx-erl-has-comment)
  () "A behaviour definition")

(defclass rpx-erl-export (rpx-erl-has-location
                          rpx-erl-has-comment)
  ((functions :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-name-arity)))
              :initarg :functions
              :initform nil
              :documentation "List of functions exported by an
              export attribute."))
  "An export(_type) attribute definition")

(defclass rpx-erl-macro (rpx-erl-has-name
                         rpx-erl-has-arity
                         rpx-erl-has-location
                         rpx-erl-has-comment)
  () "An erlang macro.")

(defclass rpx-erl-type-def (rpx-erl-has-name
                            rpx-erl-has-arity
                            rpx-erl-has-location
                            rpx-erl-has-comment)
  () "An erlang type definition.")

(defclass rpx-erl-record-field (rpx-erl-has-name
                                rpx-erl-has-location
                                rpx-erl-has-comment)
  () "An erlang record field")

(defclass rpx-erl-record (rpx-erl-has-name
                          rpx-erl-has-location
                          rpx-erl-has-comment)
  ((fields :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-record-field)))
           :initarg :fields
           :initform nil
           :documentation "List of field of the record."))
  "An erlang record.")

(defclass rpx-erl-callback (rpx-erl-has-name
                            rpx-erl-has-location
                            rpx-erl-has-comment)
  () "An erlang behaviour callbak.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; erlang function form with function clause

(defclass rpx-erl-function-spec (rpx-erl-has-location)
  () "An erlang function type specification.")

(defclass rpx-erl-function-clause (rpx-erl-has-location)
  () "An erlang clause.")

(defclass rpx-erl-function (rpx-erl-has-name
                            rpx-erl-has-arity
                            rpx-erl-has-location
                            rpx-erl-has-comment)
  ((spec :type (or null rpx-erl-function-spec)
         :initarg :spec
         :documentation "An optional erlang type spec for the function.")
   (clauses :type (satisfies (lambda(a)
                               (and (not (null a))
                                    (rpxu-list-of a rpx-erl-function-clause))))
            :initarg :clauses
            :documentation "List of function clauses of the function."))
  "An erlang function is all it's glory.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complete erlang source file

(defclass rpx-erl-source (rpx-erl-has-location
                          rpx-erl-has-module
                          rpx-erl-has-comment)
  ((includes :type (satisfies (lambda(a) (rpxu-list-of a string)))
             :initarg :includes
             :initform '()
             :documentation "A list of files included by this
             source file. All entries are absolute paths.")
   (unkown-syntax :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-text)))
                  :initarg :unkown-syntax
                  :initform nil
                  :documentation "All parts of the source code
                  that could not be interpreted as erlang
                  syntax.")
   (behaviours :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-behaviour)))
               :initarg :behaviours
               :initform '()
               :documentation "A list of 'rpx-erl-behaviour' objects.")
   (export_types :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-export)))
                 :initarg :export_types
                 :initform '()
                 :documentation "A list of 'rpx-erl-export' objects.")
   (exports :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-export)))
                 :initarg :exports
                 :initform '()
                 :documentation "A list of 'rpx-erl-export' objects.")
   (macros :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-macro)))
           :initarg :macros
           :initform '()
           :documentation "A list of 'rpx-erl-macro' objects.")
   (types  :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-type-def)))
           :initarg :type-defs
           :initform '()
           :documentation "A list of 'rpx-erl-type-def' objects.")
   (records :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-record)))
            :initarg :records
            :initform '()
            :documentation "A list of 'rpx-erl-record' objects.")
   (callbacks :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-callback)))
           :initarg :callbacks
           :initform '()
           :documentation "A list of 'rpx-erl-callback' objects.")
   (functions :type (satisfies (lambda(a) (rpxu-list-of a rpx-erl-function)))
           :initarg :functions
           :initform '()
           :documentation "A list of 'rpx-erl-function' objects."))
  "An erlang source, that might contain a module, macros, types,
callbacks or functions.")

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not cl-functions)
;; lexical-binding: t
;; End:

;;; rpx-erl.el ends here
