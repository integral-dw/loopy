;;; loopy.el --- Interactively building (cl-)loop macros -*- lexical-binding: t; -*-

;; Copyright (C) 2021  D. Williams

;; Author: D. Williams <d.williams@posteo.net>
;; Keywords: lisp, convenience, tools
;; URL: https://github.com/integral-dw/PLACEHOLDER-TEXT
;; Version: 0.4.0
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements interactive completion for the Loop
;; facility in Common Lisp and Elisp, and adds an appropriate menu
;; entry to ‘emacs-lisp-mode’ and ‘lisp-mode’.

;; This package requires no additional setup to work.  The primary
;; interactive function (‘loopy-insert’) is autoloaded.  By default it
;; is NOT bound to any key, but automatically added to the menus
;; <emacs-lisp> (in emacs-lisp-mode) and <lisp> (in lisp-mode) once
;; loaded.

;; NOTE: This package is still in beta.  Breaking changes may occur
;; without warning until the release of version 1.0.0.

;; loopy defines the following insertion commands:
;;   ‘loopy-insert’ The primary command defined by this package.
;;   ‘loopy-add-clause’ Adds a single clause to an existing loop.

;; It also offers the following custom variables (which see):
;;   ‘loopy-offer-type-specifiers’
;;   ‘loopy-allow-destructuring’
;;   ‘loopy-enter-recursive-edit’
;; Each variable may also be toggled interactively by corresponding commands.
;;   ‘loopy-toggle-type-specifiers’
;;   ‘loopy-toggle-recursive-editing’
;;   ‘loopy-toggle-destructuring’

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(declare-function seq-intersection "seq" (sequence1 sequence2 &optional testfn))
(declare-function seq-difference "seq" (sequence1 sequence2 &optional testfn))
(declare-function looking-back "subr" (regexp limit &optional greedy))
(declare-function string-trim "subr-x" (string &optional trim-left trim-right))
(defvar loopy-offer-type-specifiers)
(defvar loopy-allow-destructuring)
(defvar loopy-enter-recursive-edit)

(defgroup loopy nil
  "Interactively insert CL-style LOOP macros."
  :group 'lisp)

(define-key-after emacs-lisp-mode-map
  [menu-bar emacs-lisp loopy-insert-macro]
  '(menu-item "Insert CL Loop Macro"
              loopy-insert
              :help "Insert a cl-loop macro")
  'comment-region)

(define-key-after lisp-mode-map
  [menu-bar lisp loopy-insert-macro]
  '(menu-item "Insert Loop Macro"
              loopy-insert
              :help "Insert a LOOP macro"))


;;;; User-Level Variables

(defvar loopy-default-cl-syntax-alist nil
 "Alist holding the syntax of the Loop facility in Common Lisp.

This alist is automatically updated when custom settings are
changed.  You can also manually update it using
‘loopy-update-syntax-alists’.

See ‘loopy-syntax-alist’ for more information about how syntax is
encoded.")

(defvar loopy-default-el-syntax-alist nil
 "Alist holding the syntax of the Loop facility in Emacs Lisp.

This alist is automatically updated when custom settings are
changed.  You can also manually update it using
‘loopy-update-syntax-alists’.

See ‘loopy-syntax-alist’ for more information about how syntax is
encoded.")


;;;; The Syntax Alist
;; This section defines the massive data structures holding the syntax
;; rules for the loop facility.

(defvar-local loopy-syntax-alist nil
  "Alist holding the syntax of the Loop facility in the given language.

Each key is a symbol representing a (sub)clause, or form.  Each
value in turn is a list of subclauses and forms said clause is
composed of.  In the simplest case, each element would be a
symbol which in turn serves as another key in the alist.
However, elements may take other forms for different purposes.

The following forms have special meaning:

\(symbol SYMBOL)
  A symbol to be inserted literally.

\(list ELEMENTS...)
  A literal list.

\(newline)
  Inserts a newline.

\(choose DEFAULT OTHER-ELEMENTS...)
  A set of mutually exclusive choices.  Each element must be a symbol.
  The first element serves as the default choice.

\(choose-symbol DEFAULT OTHER-SYMBOLS...)
  A set of mutually exclusive choices.  Each element is a symbol
  to be inserted literally.  The first element serves as the
  default choice.

\(* ELEMENTS...)
  A sequence of elements that may be repeated 0 or more times.

\(optional FIRST ELEMENTS...)
  An optional sequence of elements.  The first element must either
  be a symbol or of the form (symbol SYMBOL).

Additionally to the forms specified above, there are a couple of
extended forms requiring more user interaction than choosing from
a predetermined set of possible clauses, possibly requiring
recursive editing.

\(enter TYPE)
  A simple extended form, invoking a recursive editing level.  If
  the skeleton wraps around words or regions, this form consumes
  a word or region instead (see Info node ‘(autotype) Wrapping Skeletons)’).
  TYPE should specify the name of the structure the user is
  supposed to enter.  Examples are form, compound-form and
  vector.  This should only be used for complex expressions
  potentially spanning multiple lines.  For small expressions,
  use prompt (see below) instead.

  Invoking a recursive editing level for this purpose may be
  deemed unnecessarily disruptive of one's work flow.  Should this
  be the case, consider setting the customizable variable
  ‘loopy-enter-recursive-edit’ to nil.

\(prompt TYPE &optional DEFAULT)
  A simple extended form, prompting for a small Lisp object or
  expression in the minibuffer.  TYPE should specify the name of
  the structure the user is supposed to enter.  An example is
  symbol.  If DEFAULT is given, use DEFAULT on empty input.
  Otherwise, use TYPE, converted to uppercase.  Do not use this
  for expressions that could span multiple lines.  User
  enter (see above) instead.

\(arithmetic-clause CHOSEN-CLAUSES...)
  A special extended form expanding to a valid arithmetic
  from..to..[by..] clause.  The arguments are a collection of all
  previously selected forms.

Symbols in choose and choose-symbol forms will be exposed to the
user in completing prompts, as well as those used in extended
forms.")


;;; Common Lisp LOOP
;; This alist is largely a translated version of LOOP's BFN from the
;; CLHS, see http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm.
;;
;; I made a few modifications, in particular for the sake of
;; streamlining the user experience (i.e. not having to sift through
;; layers of abstract clause names to obtain a recognizable keyword).

(defun loopy--create-cl-syntax-alist ()
  "Return a dynamically generated syntax alist for current settings.

This function is for the Common Lisp variant of LOOP only."
  ;; loop [name-clause] {variable-clause}* {main-clause}*
  `((loop (list (symbol loop) (optional name-clause)
                (* variable-clause) (* main-clause)))
    ;; For adding a clause after the fact.
    (extra-clause (newline)
                  (choose name-clause variable-clause main-clause))
    ;; Name Clause
    ;; name-clause::= named name
    (name-clause (symbol named) (prompt symbol NAME))

    ;; Variable Clauses
    ;; variable-clause::= with-clause | initial-final | for-as-clause
    (variable-clause (choose for as with
                             initially finally)
                     (newline))
    ;; with-clause::= with var1 [type-spec] [= form1]
    (with (symbol with) variable type-spec (optional assignment)
                 ;; {and var2 [type-spec] [= form2]}*
                 (* (symbol and) variable type-spec (optional assignment)))
    (assignment (symbol =) (enter form))
    ;; initial-final::= initially compound-form+ | finally compound-form+
    (initially (symbol initially) (enter compound-form))
    (finally (symbol finally) (enter compound-form))
    ;; for-as-clause::= {for | as} for-as-subclause {and for-as-subclause}*
    (as  (symbol as)  for-as-subclause (* (symbol and) for-as-subclause))
    (for (symbol for) for-as-subclause (* (symbol and) for-as-subclause))
    ;; for-as-subclause::= for-as-arithmetic | for-as-in-list | for-as-on-list |
    (for-as-subclause
     ;; all start with var [type-spec] anyway.
     variable type-spec
     ;; TODO: arithmetic-clause could/should be expanded here.
     (choose in-list on-list arithmetic
             ;; for-as-equals-then | for-as-across |
             across-vector equals-then
             ;; for-as-hash | for-as-package
             being)
     (newline))
    ;; for-as-in-list::= var [type-spec] in form1 [by step-fun]
    (in-list (symbol in) (enter form)
             (optional (symbol by) (enter step-function)))
    ;; for-as-on-list::= var [type-spec] on form1 [by step-fun]
    (on-list (symbol on) (enter form)
             (optional (symbol by) (enter step-function)))
    ;; for-as-across::= var [type-spec] across vector
    (across-vector (symbol across) (enter vector))
    ;; for-as-equals-then::= var [type-spec] = form1 [then form2]
    (equals-then (symbol =) (enter form)
                 (optional (symbol then) (enter form)))
    ;; The entire arithmetic section is better implemented as a
    ;; function than as a data structure.  However, choosing between
    ;; many real keywords or "arithmetic" as some kind of special case
    ;; is not particularly helpful either.
    (arithmetic (arithmetic-clause))
    ;; for-as-hash::=    var [type-spec] being {each | the} {...}
    ;; for-as-package::= var [type-spec] being {each | the} {...}
    ;;                   [{in | of} package]
    (being (symbol being) (choose each the))
    (the (symbol the) (choose hash-keys hash-values symbols
                              present-symbols external-symbols))
    (each (symbol each) (choose hash-key hash-value symbol
                                present-symbol external-symbol))
    ;; The actual rules are a little stricter depending on the
    ;; implementation, so I will only allow the "most correct"
    ;; combinations (the + plural, each + singular)
    ;; {hash-key | hash-keys} {in | of} hash-table
    (hash-key  (symbol hash-key)  (choose-symbol in of) (enter hash-table)
               ;; [using (hash-value other-var)] | ...
               (optional (symbol using) (list (symbol hash-value) variable)))
    (hash-keys (symbol hash-keys) (choose-symbol in of) (enter hash-table)
               (optional (symbol using) (list (symbol hash-values) variable)))
    ;; ... | {hash-value | hash-values} {in | of} hash-table
    (hash-value  (symbol hash-value)  (choose-symbol in of) (enter hash-table)
                 ;; [using (hash-key other-var)]}
                 (optional (symbol using) (list (symbol hash-key) variable)))
    (hash-values (symbol hash-values) (choose-symbol in of) (enter hash-table)
                 (optional (symbol using) (list (symbol hash-keys) variable)))
    ;; {symbol | symbols |
    (symbol  (symbol symbol)  (choose-symbol in of) (enter package))
    (symbols (symbol symbols) (choose-symbol in of) (enter package))
    ;; present-symbol | present-symbols |
    (present-symbol  (symbol present-symbol)  (choose-symbol in of)
                     (enter package))
    (present-symbols (symbol present-symbols) (choose-symbol in of)
                     (enter package))
    ;; external-symbol | external-symbols}
    (external-symbol  (symbol external-symbol)  (choose-symbol in of)
                      (enter package))
    (external-symbols (symbol external-symbols) (choose-symbol in of)
                      (enter package))

    ;; Main Clauses
    ;; main-clause::= unconditional | accumulation | conditional |
    ;;                termination-test | initial-final
    (main-clause (choose do doing return
                         collect collecting append appending
                         nconc nconcing count counting
                         sum summing maximize maximizing
                         minimize minimizing
                         if when unless
                         while until repeat always never thereis
                         initially finally)
                 (newline))
    ;; unconditional::= {do | doing} compound-form+ | return {form | it}
    (do    (symbol do)    (enter compound-form))
    (doing (symbol doing) (enter compound-form))
    ;; I skipped the {form | it} part since "it" seems to only apply
    ;; to a conditional execution clause.
    (return (symbol return) (enter form))
    ;; There's no point to distinguish between different accumulation
    ;; clauses from a user perspective.
    ,@(loopy--create-cl-accumulation-syntax)
    ;; conditional::= {if | when | unless} form selectable-clause
    ;;                {and selectable-clause}*
    ,@(mapcar (lambda (s) `(,s (symbol ,s) conditional-rest))
              (list 'if 'when 'unless))
    (conditional-rest (enter form) (newline)
                      selectable-clause
                      (* (symbol and) selectable-clause)
                      ;; [else selectable-clause {and selectable-clause}*]
                      (optional (symbol else) (newline) selectable-clause
                                (* (symbol and) selectable-clause))
                      ;; [end]
                      (optional (symbol end))
                      (newline))
    ;; selectable-clause::= unconditional | accumulation | conditional
    (selectable-clause (choose do doing
                               collect collecting append appending
                               nconc nconcing count counting
                               sum summing maximize maximizing
                               minimize minimizing
                               if when unless)
                       (newline))
    ;; termination-test::= while form | until form | repeat form |
    ;;                     always form | never form | thereis form

    ,@(mapcar (lambda (s) `(,s (symbol ,s) (enter form) (newline)))
              '(thereis while until repeat always never))

    ;; Misc. Clauses (Variables and Type Specs)
    ;; d-var-spec::= simple-var | nil | (d-var-spec . d-var-spec)
    ;; REVIEW: what does nil do here except for destructuring?
    ;; currently, only supports simple variable names, no destructuring.
    (variable ,(if loopy-allow-destructuring
                   '(enter destructuring-argument)
                 'simple-variable))

    (simple-variable (prompt symbol VARIABLE-NAME))
    ;; type-spec::= simple-type-spec | destructured-type-spec
    ;; again, no destructuring for now.
    (type-spec ,@(when loopy-offer-type-specifiers
                   '((optional type-specifier))))
    ;; destructured-type-spec::= of-type d-type-spec
    ;; d-type-spec::= type-specifier | (d-type-spec . d-type-spec)
    (type-specifier  ,@(if loopy-allow-destructuring
                           '((symbol of-type) (enter destructuring-typespec))
                         '(simple-type-spec)))
    ;; simple-type-spec::= fixnum | float | t | nil
    ;; I don't think completion is helpful here.
    (simple-type-spec (prompt type TYPE))))

(defun loopy--create-cl-accumulation-syntax ()
  "Return a list of all variations of the accumulation clause.

This function only applies to the Common Lisp LOOP macro.

The benefit of collecting all these keywords into independent,
repetitive entries is that the user does not have to choose
between numeric or list accumulation, saving an unnecessary
dialog."
  (let ((list-symbols
         '(collect collecting append appending nconc nconcing))
        (num-symbols
         '(count counting sum summing maximize maximizing minimize minimizing))
        syntax-entries)
    ;; accumulation::= list-accumulation | numeric-accumulation
    ;; list-accumulation::= {collect | collecting | append | appending |
    ;;                       nconc | nconcing}
    (dolist (entry-name list-symbols)
      (push `(,entry-name (symbol ,entry-name) acc-list-rest) syntax-entries))
    ;; {form | it} [into simple-var]
    (push '(acc-list-rest (enter form) (optional (symbol into) simple-variable))
          syntax-entries)
    ;; numeric-accumulation::= {count | counting | sum | summing |
    ;;                          maximize | maximizing | minimize | minimizing}
    (dolist (entry-name num-symbols)
      (push `(,entry-name (symbol ,entry-name) acc-num-rest) syntax-entries))
    ;; {form | it} [into simple-var] [type-spec]
    (push '(acc-num-rest (enter form)
                         (optional (symbol into) simple-variable type-spec))
          syntax-entries)
    (nreverse syntax-entries)))


;;; Emacs Lisp (Elisp)
;; This alist is a modified version of the above, with additions and
;; changes made according to the Info node ‘(cl) Loop Facility’

(defun loopy--create-el-syntax-alist ()
  "Return a dynamically generated syntax alist for current settings.

This function is for the Emacs Lisp variant of LOOP (‘cl-loop’) only."
  `((loop (list (symbol cl-loop) (optional name-clause)
                (* variable-clause) (* main-clause)))

    ;; For adding a clause after the fact.
    (extra-clause (newline)
                  (choose name-clause variable-clause main-clause))

    ;; Name Clause
    (name-clause (symbol named) (prompt symbol NAME))

    ;; Variable Clauses
    (variable-clause (choose for as with
                             repeat initially finally)
                     (newline))
    (initially (symbol initially) (choose do doing compound-form))
    (finally (symbol finally) (choose do doing return compound-form))
    (repeat (symbol repeat) (enter form))
    (with (symbol with) variable (optional assignment)
          (* (symbol and) variable (optional assignment)))
    (assignment (symbol =) (enter form))
    (as  (symbol as)  for-as-subclause (* (symbol and) for-as-subclause))
    (for (symbol for) for-as-subclause (* (symbol and) for-as-subclause))
    (for-as-subclause
     variable
     (choose in-list in-ref-list on-list arithmetic across-array
             across-ref-array equals-then being)
     (newline))
    ;; The entire arithmetic section is better implemented as a
    ;; function than as a data structure.  However, choosing between
    ;; many real keywords or "arithmetic" as some kind of special case
    ;; is not particularly helpful either.
    (arithmetic (arithmetic-clause))
    (in-list (symbol in) (enter form)
             (optional (symbol by) (enter step-function)))
    (on-list (symbol on) (enter form)
             (optional (symbol by) (enter step-function)))
    (in-ref-list (symbol in-ref) (enter form)
                 (optional (symbol by) (enter step-function)))
    (across-array (symbol across) (enter array))
    (across-ref-array (symbol across-ref) (enter array))
    (equals-then (symbol =) (enter form)
                 (optional (symbol then) (enter form)))
    ;;‘being’ is actually optional.  But is it worth asking the user?
    (being (symbol being) (choose each the))
    (the (optional (symbol the))
         (choose elements symbols hash-keys hash-values
                 key-codes key-bindings key-seqs
                 overlays extents intervals
                 frames screens windows buffers))
    (each (optional (symbol each))
          (choose element symbol hash-key hash-value
                  key-code key-binding key-seq
                  overlay extent interval
                  frame screen window buffer))
    ;; As in the CL case I will only allow some
    ;; combinations (the + plural, each + singular)
    (element  (symbol element)  (choose-symbol of of-ref) (enter sequence))
    (elements (symbol elements) (choose-symbol of of-ref) (enter sequence))
    (symbol  (symbol symbol)  (optional (symbol of) (enter obarray)))
    (symbols (symbol symbols) (optional (symbol of) (enter obarray)))
    (hash-key  (symbol hash-key)  (symbol of) (enter hash-table)
               (optional (symbol using) (list (symbol hash-value) variable)))
    (hash-keys (symbol hash-keys) (symbol of) (enter hash-table)
               (optional (symbol using) (list (symbol hash-values) variable)))
    (hash-value  (symbol hash-value)  (symbol of) (enter hash-table)
                 (optional (symbol using) (list (symbol hash-key) variable)))
    (hash-values (symbol hash-values) (symbol of) (enter hash-table)
                 (optional (symbol using) (list (symbol hash-keys) variable)))
    (key-code  (symbol key-code)  (symbol of) (enter keymap)
               (optional (symbol using) (list (symbol key-binding) variable)))
    (key-codes (symbol key-codes) (symbol of) (enter keymap)
               (optional (symbol using) (list (symbol key-bindings) variable)))
    (key-binding  (symbol key-binding)  (symbol of) (enter keymap)
                  (optional (symbol using) (list (symbol key-code) variable)))
    (key-bindings (symbol key-bindings) (symbol of) (enter keymap)
                  (optional (symbol using) (list (symbol key-codes) variable)))
    (key-seq  (symbol key-seq)  (symbol of) (enter keymap)
              (optional (symbol using) (list (symbol key-binding) variable)))
    (key-seqs (symbol key-seqs) (symbol of) (enter keymap)
              (optional (symbol using) (list (symbol key-bindings) variable)))
    (overlay  (symbol overlay)  (optional (symbol of) (enter buffer))
              (optional (symbol from) (enter pos))
              (optional (symbol to) (enter pos)))
    (overlays (symbol overlays) (optional (symbol of) (enter buffer))
              (optional (symbol from) (enter pos))
              (optional (symbol to) (enter pos)))
    (extent  (symbol extent)  (optional (symbol of) (enter buffer))
             (optional (symbol from) (enter pos))
             (optional (symbol to) (enter pos)))
    (extents (symbol extents) (optional (symbol of) (enter buffer))
             (optional (symbol from) (enter pos))
             (optional (symbol to) (enter pos)))
    (interval (symbol interval)
              (optional (symbol of) (enter buffer-or-string))
              (optional (symbol from) (enter pos))
              (optional (symbol property) (enter property)))
    (intervals (symbol intervals)
               (optional (symbol of) (enter buffer-or-string))
               (optional (symbol from) (enter pos))
               (optional (symbol to) (enter pos))
               (optional (symbol property) (enter property)))
    (window  (symbol window)  (optional (symbol of) (enter frame)))
    (windows (symbol windows) (optional (symbol of) (enter frame)))
    ,@(mapcar (lambda (s) `(,s (symbol ,s)))
              '(frame frames screen screens buffer buffers))

    ;; Main Clauses
    (main-clause (choose do doing return iter-by
                         while until always never thereis
                         collect collecting append appending
                         concat concating vconcat vconcating
                         nconc nconcing count counting
                         sum summing maximize maximizing
                         minimize minimizing
                         if when unless
                         initially finally)
                 (newline))
    (return (symbol return) (enter form))
    (do    (symbol do)    (enter compound-form))
    (doing (symbol doing) (enter compound-form))
    ,@(mapcar (lambda (s) `(,s (symbol ,s) (enter condition) (newline)))
              '(thereis while until always never))
    (iter-by (symbol iter-by) (enter iterator))
    ,@(loopy--create-el-accumulation-syntax)
    ,@(mapcar (lambda (s) `(,s (symbol ,s) conditional-rest))
              (list 'if 'when 'unless))
    (conditional-rest (enter form) (newline)
                      main-clause
                      (* (symbol and) main-clause)
                      (optional (symbol else) (newline) main-clause
                                (* (symbol and) main-clause))
                      (optional (symbol end))
                      (newline))

    ;; Other Clauses
    (compound-form (enter compound-form))
    (variable ,(if loopy-allow-destructuring
                   '(enter destructuring-argument)
                 'simple-variable))
    (simple-variable (prompt symbol VARIABLE-NAME))))

(defun loopy--create-el-accumulation-syntax ()
  "Return a list of all variations of the accumulation clause.

This function is for the Emacs Lisp variant of LOOP (‘cl-loop’) only.

The benefit of collecting all these keywords into independent,
repetitive entries is that the user does not have to choose
between numeric or list accumulation, saving an unnecessary
dialog."
  (let ((symbols
         '(collect collecting append appending
                   concat concating vconcat vconcating
                   nconc nconcing count counting
                   sum summing maximize maximizing
                   minimize minimizing))
        syntax-entries)
    (push `(accumulation (choose ,@symbols))
          syntax-entries)
    (dolist (entry-name symbols)
      (push `(,entry-name (symbol ,entry-name) acc-rest) syntax-entries))
    ;; {form | it} [into simple-var]
    (push '(acc-rest (enter form) (optional (symbol into) simple-variable))
          syntax-entries)
    (nreverse syntax-entries)))


;;; General

(defun loopy--init-alist ()
  "Initialize ‘loopy-syntax-alist’ in current buffer."
  (setq loopy-syntax-alist
        (cond
         ((derived-mode-p 'emacs-lisp-mode)
          loopy-default-el-syntax-alist)
         ((derived-mode-p 'lisp-mode)
          loopy-default-cl-syntax-alist)
         (t (error "Unknown mode: %S" major-mode)))))


;;;; Public Functions

(defun loopy-update-syntax-alists ()
  "Re-evaluate default syntax alists."
  (setq loopy-default-cl-syntax-alist
        (loopy--create-cl-syntax-alist)
        loopy-default-el-syntax-alist
        (loopy--create-el-syntax-alist)))


;;;; Utility Functions

(defun loopy--format-keyword (keyword)
  "Return a nicely formatted version of KEYWORD.

KEYWORD should be a symbol or a list of the form
\(symbol SYMBOL)."
  (pcase keyword
    ((pred symbolp))
    (`(symbol ,kw) (setq keyword kw))
    (_ (error "Unrecognized keyword format: %S" keyword)))
  (symbol-name keyword))

(defun loopy--space-maybe ()
  "Add a space if the last char before point is not whitespace.

Returns a skeleton element (the string \"\\s\" or nil)."
  (unless (looking-back "\\s-" (line-beginning-position)) " "))

(defun loopy--delete-trailing-whitespace ()
  "Deletes trailing whitespace in the current line."
  (delete-trailing-whitespace (line-beginning-position)
                              (line-end-position))
  (when (= (point) (line-beginning-position))
    (delete-char -1)))



;;;; Custom Variables

(defun loopy--set (symbol value)
  "Set SYMBOL's default value to VALUE and update syntax alists."
  (prog1 (set-default symbol value)
    ;; Do NOT update while loading the package!
    (when (featurep 'loopy)
      (loopy-update-syntax-alists))))

(defcustom loopy-offer-type-specifiers nil
  "If non-nil, offer type specifiers where appropriate.

This affects clauses such as ‘with’ and ‘for’ clauses, where a
variable can be specified together with a type specification.

See also ‘loopy-toggle-type-specifiers’."
  :type 'boolean
  :set #'loopy--set
  :group 'loopy)

(defcustom loopy-allow-destructuring nil
  "If non-nil, suppose variable names to be destructuring arguments.

This affects all forms in a loop clause where destructuring is
possible, such as the iteration variable of a ‘for’ clause.

If ‘loopy-enter-recursive-edit’ is non-nil, this will cause the
skeleton insertion to enter recursive editing instead of
prompting for a variable name in the minibuffer.

See also ‘loopy-toggle-destructuring’ and
‘loopy-enter-recursive-edit’."
  :type 'boolean
  :set #'loopy--set
  :group 'loopy)

(defcustom loopy-enter-recursive-edit t
  "If non-nil, enter recursive editing for complex forms within loops.

If nil, prompt for expressions in the minibuffer instead.

See also ‘loopy-toggle-recursive-editing’."
  :type 'boolean
  :set #'loopy--set
  :group 'loopy)


;;;; Expansion Functions
;; These functions essentially implement parser-like structures to
;; dynamically expand a skeleton to fit every possible LOOP construct.
;; This is possible because the way skeletons evaluate Lisp
;; expressions to produce subskeletons essentially allows you to just
;; implement LOOP's Backus-Naur form (BNF) directly as a skeleton.

;;; Individual Syntax Elements

;; (symbol SYMBOL)
(defun loopy--expand-symbol (symbol)
  "Expand a SYMBOL for literal insertion.

Returns a skeleton element."
  `(nil ,(format "%S " symbol) >))

;; (list ELEMENTS...)
(defun loopy--expand-list (elements)
  "Expand to a list of ELEMENTS for literal insertion.

Returns a subskeleton."
  `(nil
    "(" ,@(loopy--wrap elements t)
    '(loopy--delete-trailing-whitespace)
    ") " -))

;; (optional FIRST ELEMENTS...)
(defun loopy--expand-optional-clause (first-element &optional other-elements)
  "Prompt for optional clause, named after FIRST-ELEMENT, and expand.

Returns a subskeleton.

If OTHER-ELEMENTS is non-nil, suggest the general of the clause
structure with an ellipsis."
  (let ((name-string (loopy--format-keyword first-element)))
    (when other-elements
      (setq name-string (concat name-string ".. clause")))
    (when (y-or-n-p (format "[optional] Insert %s? " name-string))
      (loopy--wrap (cons first-element other-elements)))))

;; (choose DEFAULT OTHER-ELEMENTS...)
(defun loopy--expand-choice (clauses)
  "Prompt for one of several CLAUSES to expand to.

CLAUSES should be a list of symbols."
  (let* ((default (car clauses))
         (prompt (format "Choose clause (default %s): "
                         (loopy--format-keyword default)))
         chosen-clause)
    ;; In principle, this could be pulled into an interactor.
    ;; However, I don't think that would make it any more readable.
    (setq chosen-clause
          (read (completing-read prompt clauses
                                 nil t nil nil
                                 (symbol-name default))))
    (loopy--wrap chosen-clause)))

;; (choose-symbol DEFAULT OTHER-SYMBOLS...)
(defun loopy--expand-symbol-choice (symbols)
  "Prompt for one of several SYMBOLS to expand to.

SYMBOLS should be a list of symbols."
  (let* ((default (car symbols))
         (prompt (format "Choose clause (default %s): "
                         (loopy--format-keyword default)))
         chosen-clause)
    (setq chosen-clause
          (read (completing-read prompt symbols
                                 nil t nil nil
                                 (symbol-name default))))
    (loopy--expand-symbol chosen-clause)))

;; (* ELEMENTS...)
(defun loopy--expand-*-clause (first-element &optional other-elements)
  "Prompt for optional clause and expand, appending a copy of the original.

Returns a subskeleton.

The arguments FIRST-ELEMENT and OTHER-ELEMENTS work the same as
in ‘loopy--expand-optional-clause’."
  (let ((name-string (loopy--format-keyword first-element))
        (skeleton (loopy--wrap `(,first-element ,@other-elements) t))
        prompt-string)
    (when other-elements
      (setq name-string (concat name-string ".. clause")))
    (setq prompt-string (format "[optional] Insert %s? " name-string))
    `((if (y-or-n-p ,prompt-string) " " "")
      str -1
      ,@skeleton)))


;;; Arithmetic clause(s)
;; The arithmetic clause is a bit tricky.  It's composed of a set of 9
;; different clauses of 3 different kinds.  Each arithmetic clause can
;; be composed of *at most* one of each kind, and requires *at least*
;; one of any kind.  To make matters worse, clauses that begin with
;; "up" cannot be combined with those beginning with "down" and the
;; three kinds can appear in any order.  Additionally, "above"/"below"
;; serve as exclusive versions of "downto"/"upto".  Finally, a
;; "downto" clause requires a "from" clause.  As a consequence, which
;; clauses are allowed depends on the history of previous clauses.
;; This also means there are dozens of different valid arrangements
;; with one clause from each kind.

(defun loopy--cl-arithmetic-choices (choice-history)
  "Return a list of valid clauses left.

This function only applies to the Common Lisp LOOP macro.

CHOICE-HISTORY is a list of clauses already chosen for the
current arithmetic clause."
  (let ((kw-lists '((by)
                    (from upfrom downfrom)
                    (to downto upto above below)))
        (up-kws '(upfrom upto below))
        (down-kws '(downfrom downto above))
        choices)
    ;; First, filter exhausted kinds of clauses.
    (cond
     ((null choice-history)
      (setq choices (apply #'append kw-lists)))
     ((< (length choice-history) 3)
      (cl-loop for kw-list in kw-lists
               unless (seq-intersection
                       choice-history  kw-list #'eq)
               do (setq choices (append choices kw-list)))))
    ;; Next, remove contradictory ones.
    (cond
     ((seq-intersection up-kws choice-history)
      (seq-difference choices down-kws #'eq))
     ((seq-intersection down-kws choice-history)
      (seq-difference choices up-kws #'eq))
     (t choices))))

(defun loopy--expand-cl-arithmetic (choice-history)
  "Programmatically expand (part of) an arithmetic clause.

Returns a subskeleton.

This function only applies to the Common Lisp LOOP macro.

The argument CHOICE-HISTORY, if non-nil, excludes subclauses that
would clash with elements of it."
  (let* ((choices (loopy--cl-arithmetic-choices choice-history))
         (default (if choice-history "nil" "from"))
         prompt
         chosen-clause)
    ;; The downto clause implies the presence of a from clause.
    (when (and (or (memq 'downto choice-history)
                   (memq 'above choice-history))
               (not (memq 'from choice-history)))
      (setq default "from"))
    (when choices
      (setq prompt
            (if (string= default "nil")
                "[optional] Additional clause: "
              (format "Choose clause (default %s): " default))
            chosen-clause
            (read (completing-read prompt choices nil t nil nil default))))
    (when chosen-clause
      (push chosen-clause choice-history)
      (loopy--wrap `((symbol ,chosen-clause)
                     (enter form)
                     (arithmetic-clause ,@choice-history))))))

(defun loopy--expand-el-arithmetic (&optional choice-history)
  "Programmatically expand (part of) an arithmetic clause.

Returns a subskeleton.

This function is for the Emacs Lisp variant of LOOP (‘cl-loop’) only.
In this case, ‘from’, ‘to’ and ‘by’ have a fixed order.

The argument CHOICE-HISTORY, if non-nil, excludes subclauses that
would clash with elements of it.  The first and second element,
if present, correspond to the chosen from- and to-clause,
respectively.  The special value ‘none’ signifies that the clause
was skipped."
  (let (choices
        chosen-clause)
    (pcase choice-history
      ;; First clause: from
      ('nil
       (setq choices '(from upfrom downfrom)))
      ;; Second clause: to
      ;; downto and above imply a from clause
      ('(none)
       (setq choices '(to upto below)))
      (`(,_)
       (setq choices '(to upto below downto above)))
      ;; Last clause: by
      ;; At least one clause must be chosen!
      ('(none none)
       (setq chosen-clause 'by))
      (`(,_ ,_)
       (setq choices '(by))))
    (when choices
      (setq chosen-clause
            (read (completing-read (format "[optional] Choose %S.. clause: "
                                           (elt choices 0))
                                   choices nil t nil nil "none"))))
    (push chosen-clause choice-history)
    (pcase chosen-clause
      ('nil nil)
      ('none
       (loopy--wrap `((arithmetic-clause ,@choice-history))))
      (_
       (loopy--wrap
        `((symbol ,chosen-clause)
          (enter form)
          (arithmetic-clause ,@choice-history)))))))


;; (arithmetic-clause CHOSEN-CLAUSES...)
(defun loopy--expand-arithmetic (choice-history)
  "Programmatically expand (part of) an arithmetic clause.

Returns a subskeleton.

This mode chooses what arithmetic expansion to use based on the
current major mode.

The argument CHOICE-HISTORY, if non-nil, excludes subclauses that
would clash with elements of it."
  (cond
   ((derived-mode-p 'emacs-lisp-mode)
    (loopy--expand-el-arithmetic choice-history))
   ((derived-mode-p 'lisp-mode)
    (loopy--expand-cl-arithmetic choice-history))))


;;; The Enter Form
;; This extended form is probably the most involved.  Right now it
;; uses recursive editing to generate the "meat" of the LOOP macro,
;; which may or may not be the best idea.

(defconst loopy-help-buffer-name "*Loopy Help*"
  "Name of the help buffer displayed during recursive edits.")

(defun loopy--format-bindings-of (func-def)
  "Format a readable representation of the invocation sequence for FUNC-DEF.

It would either be a key or \\[FUNC-DEF].

FUNC-DEF should be the name of an interactive function.

This function is a modified version of ‘ediff-format-bindings-of’."
  (let ((desc
         (car (where-is-internal func-def overriding-local-map
                                 nil nil)))
        M-x)
    (if desc
        (key-description desc)
      (setq M-x ;; This begs the question: Did anyone ever remap M-x?..
            (car (last (where-is-internal 'execute-extended-command))))
      (format "%s %s"
              (key-description M-x)
              func-def))))

;; This is heavily inspired by ediff's help window.
(defun loopy--format-message (type)
  "Format a message for the recursive edit help window.

TYPE, a symbol, is the name of the type of expression the user is
expected to enter."
  (concat
   (format "\n**** Enter an expression of type %s ****\n"
           (upcase (symbol-name type)))
   (format "When done, type %s     Use %s to abort"
           (loopy--format-bindings-of 'exit-recursive-edit)
           (loopy--format-bindings-of 'abort-recursive-edit))))

(defun loopy--display-message (type)
  "Display a help message below the current buffer.

Return the newly created window.

TYPE, a symbol, is the name of the type of expression the user is
expected to enter."
  (let* ((new-window (split-window-vertically -5))
         (fill-column (window-width new-window)))
    (with-selected-window new-window
      (switch-to-buffer loopy-help-buffer-name t t)
      (set-window-dedicated-p new-window t)
      (shrink-window-if-larger-than-buffer new-window))
    (with-current-buffer loopy-help-buffer-name
      (erase-buffer)
      (insert (loopy--format-message type))
      (center-region (point-min) (point-max)))
    new-window))

(defun loopy--recursive-edit (type)
  "Invokes a recursive editing level with a small message.

Returns a string or nil.

TYPE, a symbol, should describe the type of expression the user
is expected to insert."
  (loopy--display-message type)
  ;; No matter whether the user quit or not, the help window must die.
  (unwind-protect
      (condition-case nil (recursive-edit)
        (quit (signal 'quit nil)))
    (kill-buffer loopy-help-buffer-name))
  ;; Add a space if the user didn't
  (loopy--space-maybe))

;; (enter TYPE)
(defun loopy--expand-enter (type)
  "Pause expansion to allow the user to edit a complex form.

Returns a subskeleton.

TYPE, a symbol, is a descriptor telling the user what kind of
expression is expected.

By default, this command invokes a recursive editing level to
accomplish this.  To disable this behavior, set
‘loopy-enter-recursive-edit’ to nil.  If the skeleton wraps
around words or regions, this form consumes a word or region
instead (see Info node ‘(autotype) Wrapping Skeletons)’)."
  (let* ((default-placeholder (upcase (symbol-name type)))
         (prompt (format "Enter %s (default %s): "
                         type
                         default-placeholder)))
    `(nil
      resume: (nil > @ _ (loopy--space-maybe))
      | (nil
         ,(if loopy-enter-recursive-edit
              `(loopy--recursive-edit (quote ,type))
            `(string-trim (read-string ,prompt)))
         | ,default-placeholder & " ")
      resume:)))


;;; Other Extended Forms

;; (prompt TYPE &optional DEFAULT)
(defun loopy--expand-prompt (type default)
  "Prompt for a small expression and expand to it.

Returns a subskeleton.

TYPE (a symbol) describes the type of expression expected from
the user, such as a symbol.  DEFAULT, if non-nil, should be a
list of the form (DEFAULT-SYMBOL), where DEFAULT-SYMBOL is the
default to be inserted on empty input."
  (let* ((default (car default))
         (default (if default (symbol-name default)
                    (upcase (symbol-name type))))
         (prompt (format "Enter %S (default %s): " type default)))
    `(nil
      @ ,(concat (string-trim (read-string prompt nil nil default)) " "))))

;;; Expansion Boilerplate
(defun loopy--wrap (syntax-elements &optional body-only)
  "Wrap SYNTAX-ELEMENTS into a subskeleton for future expansion.

If SYNTAX-ELEMENTS is a list, wrap each element individually.
Otherwise, wrap the single element, omitting the interactor.

BODY-ONLY, if non-nil, does not add an interactor to the
subskeleton, even if SYNTAX-ELEMENTS is a list."
  (let (skeleton)
    (cond
     ((null syntax-elements)
      nil)
     ((consp syntax-elements)
      (setq skeleton
            (mapcar (lambda (x) `(loopy--expand (quote ,x)))
                    syntax-elements))
      (if body-only skeleton (cons nil skeleton)))
     (t
      `(loopy--expand (quote ,syntax-elements))))))


;;; Core Expansion Function

(defun loopy--expand (syntax-element)
  "Expand SYNTAX-ELEMENT into a subskeleton, recursively."
  (pcase syntax-element
    ((and (pred symbolp) keyword)
     (loopy--wrap (cdr (or (assq keyword loopy-syntax-alist)
                           (error "Keyword ‘%S’ not found in syntax alist"
                                  keyword)))))
    (`(symbol ,the-symbol) (loopy--expand-symbol the-symbol))
    (`(list . ,rest) (loopy--expand-list rest))
    (`(optional ,first . ,rest)
     (loopy--expand-optional-clause first rest))
    (`(choose . ,choices)
     (loopy--expand-choice choices))
    (`(choose-symbol . ,choices)
     (loopy--expand-symbol-choice choices))
    (`(* ,first . ,rest)
     (loopy--expand-*-clause first rest))
    (`(arithmetic-clause . ,rest)
     (loopy--expand-arithmetic rest))
    (`(prompt ,type . ,default)
     (loopy--expand-prompt type default))
    (`(enter ,type)
     (loopy--expand-enter type))
    (`(newline)
     '(nil (loopy--delete-trailing-whitespace) "\n"))
    (something-else
     (error "Unexpected syntax element in syntax alist: %S"
            something-else))))


;;;; Interactive Functions
(defun loopy-toggle-type-specifiers ()
  "Toggle destructuring arguments for interactive loop insertion.

See also ‘loopy-offer-type-specifiers’."
  (declare
   (interactive-only "set ‘loopy-offer-type-specifiers’ directly instead."))
  (interactive)
  (setq loopy-offer-type-specifiers (not loopy-offer-type-specifiers))
  (loopy-update-syntax-alists)
  (loopy--init-alist)
  (message "loopy: Destructuring arguments %s"
           (if loopy-offer-type-specifiers "enabled" "disabled")))

(defun loopy-toggle-destructuring ()
  "Toggle destructuring arguments for interactive loop insertion.

See also ‘loopy-allow-destructuring’."
  (declare
   (interactive-only "set ‘loopy-allow-destructuring’ directly instead."))
  (interactive)
  (setq loopy-allow-destructuring (not loopy-allow-destructuring))
  (loopy-update-syntax-alists)
  (loopy--init-alist)
  (message "loopy: Destructuring arguments %s"
           (if loopy-allow-destructuring "enabled" "disabled")))

(defun loopy-toggle-recursive-editing ()
  "Toggle recursive editing for interactive loop insertion.

See also ‘loopy-enter-recursive-edit’."
  (declare
   (interactive-only "set ‘loopy-enter-recursive-edit’ directly instead."))
  (interactive)
  (setq loopy-enter-recursive-edit (not loopy-enter-recursive-edit))
  (loopy-update-syntax-alists)
  (loopy--init-alist)
  (message "loopy: Recursive editing %s"
           (if loopy-enter-recursive-edit "enabled" "disabled")))

;;;###autoload
(define-skeleton loopy-insert
  "Insert a LOOP macro interactively.

Depending on the major mode, interactively insert a (cl-)loop
macro in the given host language (Emacs Lisp for modes derived
from ‘emacs-lisp-mode’, Common Lisp for other modes derived from
‘lisp-mode’).

This command is (in principle) capable of building nigh
arbitrary (syntactically valid) loop macro expressions on the
fly.  This is accomplished by repeated sequences asking for a
valid loop clause to be added (with completion), asking the user
to enter some Lisp expression, or asking whether some optional
(part of a) clause should be added using ‘y-or-n-p’.

After choosing whether the loop should have a name clause, the
user is prompted for an arbitrary number of variable clauses,
followed by an arbitrary number of main clauses.  The insertion
command terminates when the user declines to add another main
clause.

By default, a recursive editing level is invoked whenever a
complex Lisp expression may be required (for example a form,
list, or vector).  This can be disabled via
‘loopy-enter-recursive-edit’, or toggled interactively with
‘loopy-toggle-recursive-editing’.  In contrast, \"simple\" Lisp
expressions (such as symbols for variable names) are prompted for
in the minibuffer.  If you want to invoke recursive editing for
variables (which may be useful for destructuring) either set
‘loopy-allow-destructuring’ to t or toggle the feature
interactively using ‘loopy-toggle-destructuring’.

Common Lisp allows the use of type declarations for variables.
Prompts handling the insertion of type declarations are disabled
by default, which can be changed by setting
‘loopy-offer-type-specifiers’ to t.

A prefix argument of -N says to successively wrap the macro
around the last N regions instead of asking for user input.
Since the loop macro is built on-the-fly, the last N regions are
consumed as needed.  The most recent region is consumed last."

  nil
  '(loopy--init-alist)
  (loopy--expand 'loop)
  -1)

(define-skeleton loopy-add-clause
  "Add a single clause to an existing loop macro."
  nil
  '(loopy--init-alist)
  (loopy--expand 'extra-clause)
  (loopy--delete-trailing-whitespace))

;;;; Initialization
(unless loopy-default-cl-syntax-alist
  (setq-default loopy-default-cl-syntax-alist
                (loopy--create-cl-syntax-alist)))

(unless loopy-default-el-syntax-alist
  (setq-default loopy-default-el-syntax-alist
                (loopy--create-el-syntax-alist)))


(provide 'loopy)
;;; loopy.el ends here
