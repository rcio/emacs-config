;;; ecb-method-browser.el --- the method-browser of Emacs

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2000

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-method-browser.el,v 1.75 2005/06/27 17:03:04 berndl Exp $

;;; Commentary:

;; This file contains the code for the method-browser of ECB


(require 'tree-buffer)
(require 'ecb-util)
(require 'ecb-layout)
(require 'ecb-mode-line)
(require 'ecb-navigate)
(require 'ecb-face)
(require 'ecb-speedbar)
(require 'ecb-common-browser)

(require 'ecb-semantic-wrapper)
;; This loads the semantic-setups for the major-modes.
(require 'semantic-load)

;; various loads
(require 'assoc)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun hs-minor-mode)
(silentcomp-defun hs-show-block)
(silentcomp-defun hs-hide-block)
(silentcomp-defvar hs-minor-mode)
(silentcomp-defvar hs-block-start-regexp)
(silentcomp-defvar imenu--index-alist)

(silentcomp-defun ecb-get-tags-for-non-semantic-files)
(silentcomp-defun ecb-create-non-semantic-tree)

(defvar ecb-selected-tag nil
  "The currently selected Semantic tag.")
(make-variable-buffer-local 'ecb-selected-tag)

(defvar ecb-methods-root-node nil
  "Path to currently selected source.")

(defconst ecb-methods-nodetype-tag 0)
(defconst ecb-methods-nodetype-bucket 1)
(defconst ecb-methods-nodetype-externtag 2)

(defun ecb-method-browser-initialize-caches ()
  "Initialize the caches of the method-browser of ECB."
  (ecb-clear-tag-tree-cache))

(defun ecb-method-browser-initialize (&optional no-caches)
  "Initialize the method-browser of ECB. If optional arg NO-CACHES is not nil
then the caches used by the method-browser will not be initialized."
  (setq ecb-selected-tag nil)
  (setq ecb-methods-root-node nil)
  (setq ecb-methods-user-filter-alist nil)
  (setq ecb-current-post-processed-tag-table nil)
  (unless no-caches
    (ecb-method-browser-initialize-caches)))

;;====================================================
;; Customization
;;====================================================

(defgroup ecb-methods nil
  "Settings for the methods-buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")


(defgroup ecb-non-semantic nil
  "Settings for parsing and displaying non-semantic files."
  :group 'ecb
  :prefix "ecb-")


(defcustom ecb-methods-buffer-name " *ECB Methods*"
  "*Name of the ECB methods buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Methods*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-methods-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-methods
  :type 'string)


(defcustom ecb-auto-expand-tag-tree 'expand-spec
  "*Expand the methods-tag-tree automatically if node invisible.
This option has only an effect if option `ecb-highlight-tag-with-point' is
switched on too. There are three possible choices:
- nil: No auto. expanding of the method buffer.
- expand-spec: Auto expand the method-buffer nodes if the node belonging to
  current tag under point is invisible because its parent-node is collapsed.
  But expanding is only done if the type of the tag under point in the
  edit-buffer is contained in `ecb-methods-nodes-expand-spec'.
- all: Like expand-spec but expands all tags regardless of the setting in
  `ecb-methods-nodes-expand-spec'.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "No auto. expand" :value nil)
                (const :tag "Expand as specified" :value expand-spec)
                (const :tag "Expand all" :value all)))


(defcustom ecb-auto-expand-tag-tree-collapse-other nil
  "*Auto. expanding the tag-tree collapses all not related nodes.
There are several choices:
- Only if on tag: This means collapsing all nodes which have no relevance for
  the currently highlighted node will be collapsed, because they are not
  necessary to make the highlighted node visible. But do this only if point
  stays onto a tag in the selected edit-window.
- Always: Same as before but collapse also when point doesn't stays on a tag
  \(e.g. between two defuns in elisp) in the selected edit-window. This means
  in such a situation a full collapsing of the methods-buffer.
- Never: Do not automatically collapse the methods-buffer."
  :group 'ecb-methods
  :type '(radio (const :tag "Collapse only when point stays on a tag"
                       :value only-if-on-tag)
                (const :tag "Collapse always" :value always)
                (const :tag "Never" :value nil)))

(defcustom ecb-expand-methods-switch-off-auto-expand t
  "*Switch off auto expanding in the ECB-method buffer.
If on then auto expanding is switched off after explicit expanding or
collapsing by `ecb-expand-methods-nodes'.

This is done with `ecb-toggle-auto-expand-tag-tree' so after the switch off
the auto expanding feature can again switched on quickly.

But after explicitly expanding/collapsing the methods-buffer to a certain
level the auto. expanding could undo this when the node belonging to current
tag under point in the edit-window is invisible after
`ecb-expand-methods-nodes' - then the auto. expand feature would make this
node immediately visible and destroys the explicitly set expand-level."
  :group 'ecb-methods
  :type 'boolean)


(defcustom ecb-auto-update-methods-after-save t
  "*Automatically updating the ECB method buffer after saving a source."
  :group 'ecb-methods
  :type 'boolean)


(defcustom ecb-font-lock-tags t
  "*Adds font-locking \(means highlighting) to the ECB-method buffer.
This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type 'boolean
  :initialize 'custom-initialize-default)


(defcustom ecb-tag-jump-sets-mark t
  "*Set the mark after jumping to a tag from the ECB-method buffer.
If set the user can easily jump back."
  :group 'ecb-methods
  :type 'boolean)

(defconst ecb-tag->text-functions
  (mapcar (lambda (fkt-elem)
            (cons (intern
                   (concat "ecb-"
                           (mapconcat 'identity
                                      (cdr (split-string (symbol-name
                                                          (cdr fkt-elem)) "-"))
                                      "-")))
                  (intern
                   (concat "ecb--" (symbol-name (cdr fkt-elem))))))
          ecb--semantic-format-function-alist)
  "Alist containing one element for every member of 
`ecb--semantic-format-function-alist'")

(defcustom ecb-tag-display-function '((default . ecb-format-tag-uml-prototype))
  "*Function to use for displaying tags in the methods buffer.
This functionality is set on major-mode base, i.e. for every major-mode a
different function can be used. The value of this option is a list of
cons-cells:
- The car is either a major-mode symbol or the special symbol 'default which
  means if no function for a certain major-mode is defined then the cdr of
  the 'default cons-cell is used.
- The cdr is the function used for displaying a tag in the related
  major-mode.
Every function is called with 3 arguments:
1. The tag
2. The parent-tag of tag \(can be nil)
3. The value of `ecb-font-lock-tags'.
Every function must return the display of the tag as string, colorized if
the third argument is not nil.

The following functions are predefined:
- For each element E of `ecb--semantic-format-function-alist' exists a
  function with name \"ecb--<\(cdr E)>\". These functions are just aliase to
  the builtin format-functions of semantic. See the docstring of these
  functions to see what they do.
  Example: \(semantic-name-nonterminal . semantic-format-tag-name) is an
  element of `ecb--semantic-format-function-alist'. Therefore the
  alias-function for this element is named `ecb--semantic-format-tag-name'.
- For every cdr in `ecb--semantic-format-function-alist' with name
  \"semantic-XYZ\" a function with name \"ecb-XYC\" is predefined. The
  differences between the semantic- and the ECB-version are:
  + The ECB-version displays for type tags only the type-name and nothing
    else \(exceptions: In c++-mode a template specifier is appended to the
    type-name if a template instead a normal class. If the tag-name of a
    type-tag is the empty-string \(tag has no name) then always the
    type-specifier is displayed - see `ecb-type-tag-display'.).
  + The ECB-version displays type-tags according to the setting in
    `ecb-type-tag-display'. This is useful for better recognizing
    different classes, structs etc. in the ECB-method window.
  For all tags which are not types the display of the ECB-version is
  identical to the semantic version. Example: For
  `ecb--semantic-format-tag-name' \(the builtin semantic formatter) the
  pendant is `ecb-format-tag-name'.

This functionality also allows the user to display tags as UML. To enable
this functionality set the function for a major-mode \(e.g. `jde-mode') to
`ecb--semantic-format-tag-uml-concise-prototype',
`ecb--semantic-format-tag-uml-prototype', or
`ecb--semantic-format-tag-uml-abbreviate' the ECB-versions of these functions.

If the value is nil, i.e. neither a function for a major-mode is defined nor
the special 'default, then `ecb--semantic-format-tag-prototype' is used for
displaying the tags.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :group 'ecb-most-important
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type (list 'repeat ':tag "Display functions per mode"
              (list 'cons ':tag "Mode tag display"
                    '(symbol :tag "Major mode")
                    (nconc (list 'choice ':tag "Display function"
                                 ':menu-tag '"Display function")
                           (append
                            (mapcar (lambda (f)
                                      (list 'const ':tag
                                            (symbol-name (car f)) (car f)))
                                    ecb-tag->text-functions)
                            (mapcar (lambda (f)
                                      (list 'const ':tag
                                            (symbol-name (cdr f)) (cdr f)))
                                    ecb-tag->text-functions)
                            (list '(function :tag "Function"))))))
  :initialize 'custom-initialize-default)

(defun ecb-get-tag-display-function ()
  (let ((mode-display-fkt (cdr (assoc major-mode ecb-tag-display-function)))
        (default-fkt (cdr (assoc 'default ecb-tag-display-function))))
    (or (and (fboundp mode-display-fkt) mode-display-fkt)
        (and (fboundp default-fkt) default-fkt)
        'ecb--semantic-format-tag-prototype)))
  

(defcustom ecb-type-tag-display nil
  "*How to display semantic type-tags in the methods buffer.
Normally all tag displaying, colorizing and facing is done by semantic
according to the value of `ecb--semantic-format-face-alist' and the semantic
display-function \(e.g. one from `ecb--semantic-format-function-alist'). But
sometimes a finer distinction in displaying the different type specifiers of
type-tags can be useful. For a description when this option is evaluated look
at `ecb-tag-display-function'!

This functionality is set on a major-mode base, i.e. for every major-mode a
different setting can be used. The value of this option is a list of
cons-cells:
- The car is either a major-mode symbol or the special symbol 'default which
  means if no setting for a certain major-mode is defined then the cdr of
  the 'default cons-cell is used.
- The cdr is a list of 3-element-lists:
  1. First entry is a semantic type specifier in string-form. Current
     available type specifiers are for example \"class\", \"interface\",
     \"struct\", \"typedef\", \"union\" and \"enum\". In addition to these
     ones there is also a special ECB type specifier \"group\" which is
     related to grouping tags \(see `ecb-post-process-semantic-taglist' and
     `ecb-group-function-tags-with-parents'). Any arbitrary specifier can be
     set here but if it is not \"group\" or not known by semantic it will be
     useless.
  2. Second entry is a flag which indicates if the type-specifier string from
     \(1.) itself should be removed \(if there is any) from the display.
  3. Third entry is the face which is used in the ECB-method window to display
     type-tags with this specifier. ECB has some predefined faces for this
     \(`ecb-type-tag-class-face', `ecb-type-tag-interface-face',
     `ecb-type-tag-struct-face', `ecb-type-tag-typedef-face',
     `ecb-type-tag-union-face', `ecb-type-tag-enum-face' and
     `ecb-type-tag-group-face') but any arbitrary face can be set here. This
     face is merged with the faces semantic already uses to display a tag,
     i.e. the result is a display where all face-attributes of the ECB-face
     take effect plus all face-attributes of the semantic-faces which are not
     set in the ECB-face \(with XEmacs this merge doesn't work so here the
     ECB-face replaces the semantic-faces; this may be fixed in future
     versions).

The default value is nil means there is no special ECB-displaying of
type-tags in addition to the displaying and colorizing semantic does. But a
value like the following could be a useful setting:

  \(\(default
     \(\"class\" t ecb-type-tag-class-face)
     \(\"group\" nil ecb-type-tag-group-face))
    \(c-mode
     \(\"struct\" nil ecb-type-tag-struct-face)
     \(\"typedef\" nil ecb-type-tag-typedef-face)))

This means that in `c-mode' only \"struct\"s and \"typedef\"s are displayed
with special faces \(the specifiers itself are not removed) and in all other
modes \"class\"es and grouping-tags \(see `ecb-tag-display-function',
`ecb-group-function-tags-with-parents') have special faces and the \"class\"
specifier-string is removed from the display.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :group 'ecb-most-important
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (repeat :tag "Display of type specifiers"
                               (list (choice :tag "Specifier list"
                                             :menu-tag "Specifier list"
                                             (const :tag "class"
                                                    :value "class")
                                             (const :tag "interface"
                                                    :value "interface")
                                             (const :tag "struct"
                                                    :value "struct")
                                             (const :tag "typedef"
                                                    :value "typedef")
                                             (const :tag "union"
                                                    :value "union")
                                             (const :tag "enum"
                                                    :value "enum")
                                             (const :tag "group"
                                                    :value "group")
                                             (string :tag "Any specifier"))
                                     (boolean :tag "Remove the type-specifier" t)
                                     (face :tag "Any face"
                                           :value ecb-type-tag-class-face)))))
  :initialize 'custom-initialize-default)

(defun ecb-get-face-for-type-tag (type-specifier)
  "Return the face set in `ecb-type-tag-display' for current major-mode and
TYPE-SPECIFIER or nil."
  (let ((mode-display (cdr (assoc major-mode ecb-type-tag-display)))
        (default-display (cdr (assoc 'default ecb-type-tag-display))))
    (or (nth 2 (assoc type-specifier mode-display))
        (and (null mode-display)
             (nth 2 (assoc type-specifier default-display))))))


(defun ecb-get-remove-specifier-flag-for-type-tag (type-specifier)
  "Return the remove-specifier-flag set in `ecb-type-tag-display' for
current major-mode and TYPE-SPECIFIER or nil."
  (let ((mode-display (cdr (assoc major-mode ecb-type-tag-display)))
        (default-display (cdr (assoc 'default ecb-type-tag-display))))
    (or (nth 1 (assoc type-specifier mode-display))
        (and (null mode-display)
             (nth 1 (assoc type-specifier default-display))))))

(defcustom ecb-type-tag-expansion
  '((default . ("class" "interface" "group" "namespace"))
    (c-mode .  ("struct")))
  "*Default expansion of semantic type-tags.
Semantic groups type-tags into different type-specifiers. Current available
type specifiers are for example \"class\", \"interface\", \"struct\",
\"typedef\", \"union\" and \"enum\". In addition to these ones there is also a
special ECB type specifier \"group\" which is related to grouping tags \(see
`ecb-post-process-semantic-taglist').

This option defines which type-specifiers should be expanded at
file-open-time. Any arbitrary specifier can be set here but if it is not
\"group\" or not known by semantic it will be useless.

This functionality is set on a major-mode base, i.e. for every major-mode a
different setting can be used. The value of this option is a list of
cons-cells:
- The car is either a major-mode symbol or the special symbol 'default which
  means if no setting for a certain major-mode is defined then the cdr of
  the 'default cons-cell is used.
- The cdr is either a list of type-specifiers which should be expanded at
  file-open-time or the symbol 'all-specifiers \(then a type-tag is always
  expanded regardless of its type-specifier).

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :group 'ecb-most-important
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (radio (const :tag "Expand all type-specifiers"
                                     :value all-specifiers)
                              (repeat :tag "Expand type specifiers"
                                      (choice :tag "Specifier"
                                              :menu-tag "Specifier"
                                              (const :tag "class"
                                                     :value "class")
                                              (const :tag "interface"
                                                     :value "interface")
                                              (const :tag "struct"
                                                     :value "struct")
                                              (const :tag "typedef"
                                                     :value "typedef")
                                              (const :tag "union"
                                                     :value "union")
                                              (const :tag "enum"
                                                     :value "enum")
                                              (const :tag "group"
                                                     :value "group")
                                              (string :tag "Any specifier"))))))
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :initialize 'custom-initialize-default)
  
(defun ecb-type-tag-expansion (type-specifier)
  "Return the default expansion-state of TYPE-SPECIFIER for current major-mode
as specified in `ecb-type-tag-expansion'"
  (let ((mode-expansion (cdr (assoc major-mode ecb-type-tag-expansion)))
        (default-expansion (cdr (assoc 'default ecb-type-tag-expansion))))
    (or (equal mode-expansion 'all-specifiers)
        (member type-specifier mode-expansion)
        (and (null mode-expansion)
             (or (equal default-expansion 'all-specifiers)
                 (member type-specifier default-expansion))))))

(defsubst ecb-faux-group-tag-p (tag)
  "Returns not nil if TAG is a \"virtual\" faux-group token which has no
position but groups some external members having the same parent-tag."
  (or (ecb--semantic--tag-get-property tag 'ecb-group-tag)
      (ecb--semantic--tag-get-property tag 'faux)))

(defun ecb-get-type-specifier (tag)
  (if (ecb-faux-group-tag-p tag)
      "group"
    (ecb--semantic-tag-type tag)))
  

(dolist (elem ecb-tag->text-functions)
  (fset (car elem)
        `(lambda (tag &optional parent-tag colorize)
           (if (eq 'type (ecb--semantic-tag-class tag))
               (let* (;; we must here distinguish between UML- and
                      ;; not-UML-semantic functions because for UML we must
                      ;; preserve some semantic facing added by semantic (e.g.
                      ;; italic for abstract classes)!
                      (text (funcall (if (string-match "-uml-" (symbol-name (quote ,(car elem))))
                                         'ecb--semantic-format-tag-uml-abbreviate
                                       'ecb--semantic-format-tag-name)
                                     tag parent-tag colorize))
                      (type-specifier (ecb-get-type-specifier tag))
                      (face (ecb-get-face-for-type-tag type-specifier))
                      (remove-flag (ecb-get-remove-specifier-flag-for-type-tag
                                    type-specifier)))
                 (save-match-data
                   ;; the following is done to replace the "struct" from
                   ;; grouping tags (see
                   ;; ecb-group-function-tags-with-parents) with "group".
                   ;; This code can be removed (or changed) if semantic allows
                   ;; correct protection display for function-tags with
                   ;; parent-tag.
                   (when (ecb-faux-group-tag-p tag)
                     (if (string-match (concat "^\\(.+"
                                               (ecb--semantic-uml-colon-string)
                                               "\\)\\("
                                               (if (ecb--semantic--tag-get-property tag 'faux)
                                                   (ecb--semantic-orphaned-member-metaparent-type)
                                                 "struct")
                                               "\\)") text)
                         (let ((type-spec-text "group"))
                           (put-text-property 0 (length type-spec-text)
                                              'face
                                              (get-text-property
                                               0 'face
                                               (match-string 2 text))
                                              type-spec-text)
                           (setq text (concat (match-string 1 text)
                                              type-spec-text)))))
                   ;; Now we must maybe add a template-spec in c++-mode and
                   ;; maybe remove the type-specifier string.
                   (let (col-type-name col-type-spec template-text)
                     (if (string-match (concat "^\\(.+\\)\\("
                                               (ecb--semantic-uml-colon-string)
                                               type-specifier "\\)")
                                       text)
                         (setq col-type-name (match-string 1 text)
                               col-type-spec (if (not remove-flag)
                                                 (match-string 2 text)))
                       ;; necessary for anonymous types like unnamed enums etc...
                       (setq col-type-spec (if (= (length text) 0)
                                               type-specifier
                                             nil))
                       (setq col-type-name text))
                     (when (and (equal major-mode 'c++-mode)
                                (fboundp 'semantic-c-template-string))
                       (setq template-text (semantic-c-template-string
                                            tag parent-tag colorize))
                       ;; Removing {...} from within the template-text.
                       ;; Normally the semantic-formatters should not add this
                       ;; ugly stuff.
                       (if (string-match "^\\(.+\\){.*}\\(.+\\)$" template-text)
                           (setq template-text
                                 (concat (match-string 1 template-text)
                                         (match-string 2 template-text))))
                       (put-text-property 0 (length template-text)
                                          'face
                                          (get-text-property
                                           (1- (length col-type-name)) 'face
                                           col-type-name)
                                          template-text))
                     (setq text (concat col-type-name template-text
                                        col-type-spec))))
                 ;; now we add some own colorizing if necessary
                 (if face
                     (ecb-merge-face-into-text text face))
                 text)
             (funcall (quote ,(cdr elem)) tag parent-tag colorize)))))

(defcustom ecb-display-image-icons-for-semantic-tags ecb-images-can-be-used
  "*Display nice and pretty icons for semantic-tags in the Methods-buffer.
This option takes only effect if Emacs can display images and if
`ecb-tree-buffer-style' is set to 'image."
  :group 'ecb-methods
  :type 'boolean)

(defsubst ecb-use-images-for-semantic-tags ()
  (and ecb-display-image-icons-for-semantic-tags
       ecb-images-can-be-used
       (equal ecb-tree-buffer-style 'image)))

(defcustom ecb-post-process-semantic-taglist
  '((c++-mode . (ecb-group-function-tags-with-parents))
    (emacs-lisp-mode . (ecb-group-function-tags-with-parents))
    (c-mode . (ecb-filter-c-prototype-tags)))
  "*Define mode-dependent post-processing for the semantic-taglist.
This is an alist where the car is a major-mode symbol and the cdr is a list of
function-symbols of functions which should be used for post-processing the
taglist \(returned by `ecb--semantic-fetch-tags') for a buffer in this
major-mode. The first function in the list is called with current semantic
taglist of current buffer and must return a valid taglist again. All other
functions are called with the result-taglist of its preceding function and
have to return a new taglist again.

For oo-programming languages where the methods of a class can be defined
outside the class-definition \(e.g. C++, Eieio) the function
`ecb-group-function-tags-with-parents' can be used to get a much better
method-display in the methods-window of ECB, because all method
implementations of a class are grouped together.

Another senseful usage is to filter out certain tags, e.g. prototype tags in
`c-mode'. For this you can set `ecb-filter-c-prototype-tags'.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (repeat (function :tag "Post-process function")))))

(defcustom ecb-default-tag-filter nil
  "*Default tag-filters for certain files.
This option allow to define default tag-filters for certain files which are
applied automatically after loading such a file into a buffer. The possible
filters are the same as offered by the command `ecb-methods-filter' and they
are applied in the same manner - the only difference is they are applied
automatically. Please be aware that symbol-filters \(e.g. protection-symbols
like public or private) must not be inserted with quotes whereas a
filter-regexp has to be inserted with surrounding double-quotes! In addition
backslashes in a regexp have to be doubled!

For each file-spec \(a major-mode plus a file-regexp which both specify a
file for which filters should be applied) there can be as much filters as
needed - they are layered like with `ecb-methods-filter' too.

Tag-classes which are completely hidden or excluded by the option
`ecb-show-tags' will never being displayed in the Methods-buffer regardless of
the filters of this option!"
  :group 'ecb-methods
  :type '(repeat (cons :tag "Default tag filter"
                       (cons :tag "Filespec"
                             (symbol :tag "Major-mode")
                             (regexp :tag "Filename-regexp"))
                       (repeat :tag "Default filters"
                               (list :tag "Filterspec"
                                     (choice :tag "Filter-type"
                                             :menu-tag "Filtertype"
                                             (const :tag "Regexp" :value regexp)
                                             (const :tag "Protection" :value protection)
                                             (const :tag "Tag-class" :value tag-class)
                                             (const :tag "Funtion" :value function))
                                     (sexp :tag "Filter-value")
                                     (boolean :tag "inverse"))))))

  
(defcustom ecb-show-only-positioned-tags t
  "*Show only nodes in the method-buffer which are \"jump-able\".
If not nil then ECB displays in the method-buffer only nodes which are
\"jump-able\", i.e. after selecting it by clicking or with RET then ECB jumps
to the corresponding location in the edit-window.
Example: With CLOS or Eieio source-code there can exist some position-less
nodes like variable-attributes in a `defclass' form which are only displayed
if this option is nil. Displaying such nodes can be senseful even if they can
not be jumped.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type 'boolean)


(defcustom ecb-show-tags
  '((default . ((include collapsed nil)
                (parent collapsed nil)
                (type flattened nil)
                (variable collapsed access)
                (function flattened access)
                (label hidden nil)
                (t collapsed nil)))
    (c++-mode . ((include collapsed nil)
                 (parent collapsed nil)
                 (type flattened nil)
                 (variable collapsed access)
                 (function flattened access) ;; for Methods
                 (function collapsed access) ;; for Method-prototypes
                 (label hidden nil)
                 (t collapsed nil)))
    (c-mode . ((include collapsed nil)
               (parent collapsed nil)
               (type flattened nil)
               (variable collapsed access)
               (function flattened access) ;; for Functions
               (function collapsed access) ;; for Function-prototypes
               (label hidden nil)
               (t collapsed nil)))
    (bovine-grammar-mode . ((keyword collapsed name)
                            (token collapsed name)
                            (nonterminal flattened name)
                            (rule flattened name)
                            (t collapsed nil)))
    (wisent-grammar-mode . ((keyword collapsed name)
                            (token collapsed name)
                            (nonterminal flattened name)
                            (rule flattened name)
                            (t collapsed nil)))
    (texinfo-mode . ((section flattened nil)
                     (def collapsed name)
                     (t collapsed nil))))
  "*How to show tags in the methods buffer first time after find-file.
This functionality is set on a major-mode base, i.e. for every major-mode a
different setting can be used. The value of this option is a list of
cons-cells:

The car is either a major-mode symbol or the special symbol 'default which
means if no setting for a certain major-mode is defined then the cdr of
the 'default cons-cell is used. This option should always contain a
default-setting!

The cdr is a list where each element represents a type of tags:

\(<tag type> <display type> <sort method>)

There can be more than 1 element for a certain <tag type>. This is for example
useful for C++ and C because these languages distinct between a
method-prototype \(rsp. function-prototype for C) and the method \(rsp.
function for C) itself. The default value of these option contains two entries
for <tag type> is 'function whereas the first one is responsible for the
\"real\" methods \(rsp. functions) and the second one for the prototypes. So
if the methods should be flattened and the prototypes collapsed the
show-tags-list for C++ and C must contain two entries for <tag type>
'function, the first one defined as 'flattened and the second one defined as
'collapsed. See also `ecb-methods-separate-prototypes'.

The tags in the methods buffer are displayed in the order as they appear in
this list.

Tag Type
----------

A Semantic tag type symbol \(for all possible type symbols see documentation
of semantic):
- include
- type
- variable
- function
- rule
- section \(chapters and sections in `info-mode')
- def \(definitions in `info-mode')

or one of the following:

- t:      All tag types not specified anywhere else in the list.
- parent: The parents of a type.

Display Type
------------

A symbol which describes how the tags of this type shall be shown:

- expanded:  The tags are shown in an expanded node.
- collapsed: The tags are shown in a collapsed node.
- flattened: The tags are added to the parent node.
- hidden:    The tags are not shown.

Sort Method
-----------

A symbol describing how to sort the tags of this type:

- name:   Sort by the tag name.
- access: Sort by tag access (public, protected, private) and then by name.
- nil:    Don't sort tags. They appear in the same order as in the source
          buffer.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :group 'ecb-most-important
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (repeat (list (symbol :tag "Tag symbol")
                                     (choice :tag "Display type" :value collapsed
                                             (const :tag "Expanded" expanded)
                                             (const :tag "Collapsed" collapsed)
                                             (const :tag "Flattened" flattened)
                                             (const :tag "Hidden" hidden))
                                     (choice :tag "Sort by" :value nil
                                             (const :tag "Name" name)
                                             (const :tag "Access then name" access)
                                             (const :tag "No sort" nil))))))
  :initialize 'custom-initialize-default)

(defun ecb-get-show-tags-list ()
  "Return the show-tags-list of `ecb-show-tags' for current major-mode."
  (let ((mode-show-tag-list (cdr (assoc major-mode ecb-show-tags)))
        (default-show-tag-list (cdr (assoc 'default ecb-show-tags))))
    (or mode-show-tag-list
        (and (null mode-show-tag-list)
             default-show-tag-list))))

(defcustom ecb-methods-separate-prototypes t
  "*Separate function-prototypes from the real functions.
This is for example useful for C++ and C because these languages distinct
between a method-prototype \(rsp. function-prototype for C) and the method
\(rsp. function for C) itself. If this option is not nil then ECB separates
the prototypes from the real function/methods. Then with `ecb-show-tags' the
user can define different display-settings for each of them. If this option is
nil then the prototypes and the real functions are filled in the same bucket
and displayed plain and there is no sorting between prototypes and functions
possible. If this option is switched on then it is senseful that
`ecb-show-tags' contains for all modes which distinct between prototypes and
real functions/methods two entries for the tag-type 'function - see the
documentation of this option."
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-methods-filter-replace-existing 'never
  "*How the methods-filter should be applied to existing filters.
There are three different choices:
- 'never: This is the default and means that calling `ecb-methods-filter'
  always adds the new filter on top of already existing filters. So you can
  combine several filter to one combined like this example: 'Display only all
  public methods having the string \"test\" in its name.' With this setting
  the filters can only be cleared by calling `ecb-methods-filter' and then
  choosing \"nothing\".
- 'always: This means that `ecb-methods-filter' always clears a previous
  filter before applying the new one.
- 'ask: ECB asks if the new filter should replace the existing ones."
  :group 'ecb-methods
  :type '(radio (const :tag "Do not replace" :value never)
                (const :tag "Always replace" :value always)
                (const :tag "Ask if to replace" :value ask)))

(defcustom ecb-methods-nodes-expand-spec '(type variable function section
                                                nonterminal keyword token)
  "*Semantic tag-types expanded by `ecb-expand-methods-nodes'.
The value of this option is either the symbol 'all \(all tags are expanded
regardless of their type) or a list of symbols where each symbol is a valid
semantic tag-type. For a description of semantic tag types see option
`ecb-show-tags'.

But this option also defines if bucket-nodes in the ECB-method-buffer \(e.g.
\"\[Variables\]\") should be expanded. Therefore valid symbols for this list
are also all cars of the variable `semantic-symbol->name-assoc-list'.

If there is a bucket-name \(the node-name stripped of the settings in
`ecb-bucket-node-display') which is not contained as cdr in
`semantic-symbol->name-assoc-list' then the symbol with this bucket-name as
name is also a valid symbol for this list. Example: In ECB there are buckets
\"\[Parents\]\". The bucket-name is \"Parents\" and the valid symbol-name is
then 'Parents.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "All node-types" :value all)
                (repeat :tag "Node-type list"
                        (symbol :tag "Node-type"))))


(defcustom ecb-methods-nodes-collapse-spec 'all
  "*Semantic tag-types collapsed by `ecb-expand-methods-nodes'.
For valid values of this option see `ecb-methods-nodes-expand-spec'!

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "All node-types" :value all)
                (repeat :tag "Node-type list"
                        (symbol :tag "Node-type"))))

(defcustom ecb-methods-show-node-info '(if-too-long . name+type)
  "*When to display which node-info in the methods-buffer.
Define which node info should displayed after moving the mouse over a node
\(or after a shift click onto the node) in the methods-buffer.

You can define \"when\" a node-info should be displayed:
See `ecb-directories-show-node-info' for the possible choices.

You can define what info should be displayed:
- name: Only the full node name is displayed.
- name+type: The full name + the type of the node \(function, class,
  variable) is displayed.

Do NOT set this option directly via setq but use always customize!"
  :group 'ecb-methods
  :type '(cons :tag "* Method-buffer"
               (choice :tag "When"
                       (const :tag "Always" :value always)
                       (const :tag "If too long" :value if-too-long)
                       (const :tag "After shift click" :value shift-click)
                       (const :tag "Never" :value never))
               (choice :tag "What"
                       (const :tag "Node-name" :value name)
                       (const :tag "Node-name + type" :value name+type))))


(defcustom ecb-exclude-parents-regexps nil
  "*Regexps which parent classes should not be shown in the methods buffer.
If nil then all parents will be shown if `ecb-show-parents' is not nil.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type '(repeat (regexp :tag "Parents-regexp to exclude"))
  :initialize 'custom-initialize-default)

(defsubst ecb-check-parent-for-exclude (parent-name)
  (ecb-match-regexp-list parent-name ecb-exclude-parents-regexps))

(defcustom ecb-highlight-tag-with-point 'highlight-scroll
  "*How to highlight the method or variable under the cursor.
- highlight-scroll: Always scroll the method buffer, so the current method of the
  edit-window is highlighted in the method-window.
- highlight: Only highlight the current method of the edit window in the
  method window if the method is visible in the method-window.
- nil: No highlighting is done.
See also `ecb-highlight-tag-with-point-delay'.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "Highlight and scroll window"
                       :value highlight-scroll)
                (const :tag "Just highlight"
                       :value highlight)
                (const :tag "Do not highlight"
                       :value nil)))


(defcustom ecb-highlight-tag-with-point-delay 0.25
  "*Time Emacs must be idle before current tag is highlighted.
If nil then there is no delay, means current tag is highlighted immediately.
A small value of about 0.25 seconds saves CPU resources and you get even
though almost the same effect as if you set no delay. But such a delay
prevents also \"jumping backward/forward\" during scrolling within
java-classes if point goes out of method-definition into class-definition.
Therefore the default value is a delay of 0.25 seconds.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "No highlighting delay"
                       :value nil)
                (number :tag "Idle time before highlighting"
                        :value 0.25))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode) ecb-minor-mode)
                       (ecb-activate-ecb-autocontrol-functions value
                                                               'ecb-tag-sync))))
  :initialize 'custom-initialize-default)


(defvar ecb-method-overlay (ecb-make-overlay 1 1)
  "Internal overlay used for the first line of a method.")
(ecb-overlay-put ecb-method-overlay 'face ecb-tag-header-face)


(defcustom ecb-tag-visit-post-actions '((default . (ecb-tag-visit-smart-tag-start
                                                    ecb-tag-visit-highlight-tag-header))
                                        (java-mode . (ecb-tag-visit-goto-doc-start))
                                        (jde-mode . (ecb-tag-visit-goto-doc-start)))
  "*Actions to perform after visiting a tag from the Method-buffer.
With this option actions can be added which will be performed after visiting
the start of the tag in the source-buffer.

This functionality is set on a `major-mode' base, i.e. for every `major-mode' a
different setting can be used. The value of this option is a list of
cons-cells:
- The car is either a `major-mode' symbol or the special symbol 'default.
- The cdr is a list of action-functions or nil.

ECB first performs all actions defined for the special symbol 'default \(if
any) and then all actions defined for current `major-mode' \(if any).

ECB offers some predefined senseful action-functions. Currently there are:
- `ecb-tag-visit-highlight-tag-header'
- `ecb-tag-visit-smart-tag-start'
- `ecb-tag-visit-recenter'
- `ecb-tag-visit-recenter-top'
- `ecb-tag-visit-goto-doc-start'
- `ecb-tag-visit-narrow-tag'
See the documentation of these function for details what they do.

But you can add any arbitrary function if the following conditions are
fulfilled:
- The function gets the semantic tag as argument and
- the function returns the \(new) point after finishing its job.
- The function must not put the point outside the tag-boundaries of the
  tag-argument."
  :group 'ecb-methods
  :type '(repeat (cons :value (nil . (ecb-tag-visit-recenter))
                       (symbol :tag "Major-mode or default")
                       (repeat (choice :tag "Post action" :menu-tag "Post action"
                                       (const :tag "ecb-tag-visit-smart-tag-start"
                                              :value ecb-tag-visit-smart-tag-start)
                                       (const :tag "ecb-tag-visit-highlight-tag-header"
                                              :value ecb-tag-visit-highlight-tag-header)
                                       (const :tag "ecb-tag-visit-goto-doc-start"
                                              :value ecb-tag-visit-goto-doc-start)
                                       (const :tag "ecb-tag-visit-narrow-tag"
                                              :value ecb-tag-visit-narrow-tag)
                                       (const :tag "ecb-tag-visit-recenter-top"
                                              :value ecb-tag-visit-recenter-top)
                                       (const :tag "ecb-tag-visit-recenter"
                                              :value ecb-tag-visit-recenter)
                                       (function :tag "Function"))))))


(defun ecb-tag-visit-function-member-p (fnc)
  (or (member fnc (cdr (assoc 'default ecb-tag-visit-post-actions)))
      (member fnc (cdr (assoc major-mode ecb-tag-visit-post-actions)))))

(defcustom ecb-methods-menu-user-extension nil
  "*Static user extensions for the popup-menu of the methods buffer.
For further explanations see `ecb-directories-menu-user-extension'.

The node-argument of a menu-function contains as data the semantic-tag of
the method/variable/tag for which the popup-menu has been opened.

Per default the static user-extensions are added at the beginning of the
built-in menu-entries of `ecb-methods-menu' but the whole menu can be
re-arranged with `ecb-methods-menu-sorter'."
  :group 'ecb-methods
  :type (ecb-create-menu-user-ext-type 1 ecb-max-submenu-depth))


(defcustom ecb-methods-menu-user-extension-function 'ignore
  "*Dynamic user extensions for the popup-menu of the methods buffer.
A function which has to return a list in the same format like the option
`ecb-methods-menu-user-extension'. This function is called when the user opens
the popup-menu for the methods buffer. For an example how such a function can
be programmed see `ecb-methods-menu-editwin-entries'.

If no dynamically evaluated menu-extensions should be added to the
methods-buffer the function has to return nil. Therefore the default-value
of this option is `ignore'.

Per default the dynamic user-extensions are added in front of the static
extensions of `ecb-methods-menu-user-extension' but the whole menu can be
re-arranged with `ecb-methods-menu-sorter'."
  :group 'ecb-methods
  :type 'function)

(defcustom ecb-methods-menu-sorter nil
  "*Function which re-sorts the menu-entries of the directories buffer.
If a function then this function is called to sort the menu-entries of the
combined menu-entries of the user-menu-extensions of
`ecb-methods-menu-user-extension' and the built-in-menu
`ecb-methods-menu'. If nil then no special sorting will be done and the
user-extensions are placed in front of the built-in-entries.

For the guidelines for such a sorter-function see
`ecb-directories-menu-sorter'."
  :group 'ecb-methods
  :type '(choice :tag "Menu-sorter" :menu-tag "Menu-sorter"
                 (const :tag "No special sorting" :value nil)
                 (function :tag "Sort-function" :value identity)))


(defcustom ecb-methods-buffer-after-create-hook nil
  "*Local hook running after the creation of the methods-buffer.
Every function of this hook is called once without arguments direct after
creating the methods-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the methods-buffer of ECB."
  :group 'ecb-methods
  :type 'hook)


(defcustom ecb-process-non-semantic-files (if (locate-library "speedbar")
                                              t)
  "*Display contents of non-semantic-files in the ECB-methods-buffer.
See also `ecb-non-semantic-parsing-function'."
  :group 'ecb-general
  :group 'ecb-non-semantic
  :group 'ecb-most-important
  :type 'boolean)


(defcustom ecb-non-semantic-parsing-function nil
  "*Define mode-dependent parsing functions for non-semantic files.
This is an alist where the car is a major-mode symbol and the cdr is a
function-symbol of a function which should be used for parsing a non-semantic
buffer, i.h. a buffer for which no semantic grammar exists. Such a function
gets one argument - the filename of current buffer - and has to generate and
return a tag/tag list which is understandable by
`speedbar-insert-generic-list'. speedbar has already included two functions
`speedbar-fetch-dynamic-imenu' and `speedbar-fetch-dynamic-etags' which can be
used for parsing buffers with imenu rsp. etags.

This option takes only effect if `ecb-process-non-semantic-files' is not nil:
Then ECB checks for non-semantic buffers if current `major-mode' is contained
in this option and if yes, then the specified parsing function is called;
if not then the cars of the elements of `speedbar-dynamic-tags-function-list'
are called in that sequence they are listed in this variable. See option
`speedbar-dynamic-tags-function-list' for further details.

In most cases imenu-parsing is preferable over etags-parsing because imenu
operates on Emacs-buffers and needs no external tool and therefore parsing
works also if current contents of a buffer are not saved to disk. But maybe
sometimes etags may return better parsing results.

IMPORTANT: if imenu-parsing should be used then the option
`speedbar-use-imenu-flag' must be set to not nil!"
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (function :tag "Parsing function"))))


(defcustom ecb-non-semantic-methods-initial-expand nil
  "*Initially expand all tags for not by semantic supported sources.
This option can be customized on a major-mode basis, i.e. if a `major-mode' is
contained in this option then all tags for this modes will be initially
expanded - otherwise not."
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type '(repeat :tag "Expand this modes"
                 (symbol :tag "major mode")))


(defcustom ecb-auto-save-before-etags-methods-rebuild t
  "*Automatic saving of current buffer before rebuilding its methods.
This option is only relevant for sources which are supported and parsed by
etags \(see `ecb-process-non-semantic-files'). Because etags is an external
tool a source-buffer can only be reparsed if the buffer is saved to disk. So
the command `ecb-rebuild-methods-buffer' checks for sources which are not
supported by semantic or imenu if either this option is t or if the major-mode
of the source-buffer is contained in this list: In both cases ECB saves the
current source-buffer before it re-runs etags for reparsing the source.
If nil or if the major-mode is not contained then no automatic saving will be
done!

For all source supported by semantic or by imenu this option takes no effect."
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type '(radio (const :tag "For all etags modes" :value t)
                (repeat :tag "For these modes" (symbol :tag "Major-mode"))))


(defcustom ecb-non-semantic-exclude-modes '(sh-mode fundamental-mode text-mode)
  "*Exclude modes from parsing with imenu or etags.
Per default, ECB tries to parse all file-types not supported by semantic with
imenu or etags or some other method \(for details see the option
`ecb-non-semantic-parsing-function'). If a file-type can not be parsed by
semantic, imenu or etags than this simply results in an empty method-buffer
for this file. But nevertheless you will get a message \"Sorry, no support for
a file of that extension\" which comes from the speedbar-library and can not
switched off. Therefore if a `major-mode' is known as not parse-able by
semantic, imenu or etags it can be added to this option and then it will be
excluded from being tried to parsed."
  :group 'ecb-non-semantic
  :type '(repeat :tag "Modes to exclude"
                 (symbol :tag "Major-mode")))


(defcustom ecb-rebuild-non-semantic-methods-before-hook nil
  "*Hook at beginning of `ecb-rebuild-methods-buffer-for-non-semantic'.
So this function is always called by the command `ecb-rebuild-methods-buffer'
for not semantic supported source-types.

Every function of this hook gets one argument: The complete filename of the
current source-buffer in the edit-window. The Method-buffer is only rebuild by
`ecb-rebuild-methods-buffer-for-non-semantic' if either the hook contains no
function \(the default) or if no function of this hook returns nil! See
`run-hook-with-args-until-failure' for description how these function are
processed."
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type 'hook)

;;====================================================
;; Internals
;;====================================================


(defun ecb-enter-debugger (&rest error-args)
  "If `ecb-debug-mode' is not nil then enter the Emacs-debugger and signal an
error with ERROR-ARGS."
  (when ecb-debug-mode
    (let ((debug-on-error t))
      (apply 'error error-args))))

;; encapsulation all semantic-functions ECB uses if they operate with the
;; semantic-overlays, so we can handle an error if these overlays (extends for
;; XEmacs) are destroyed and invalid cause of some mysterious circumstances.

(defun ecb-semantic-assert-valid-tag (tag &optional no-reparse)
  "Assert that TAG is a valid tag. If not valid then `ecb-enter-debugger'
is called. If NO-REPARSE is not nil then the buffer is not autom. reparsed. It
returns nil if the assertion fails otherwise not nil. So the caller can even
check the result if `ecb-debug-mode' is nil in which case the function
`ecb-enter-debugger' is a no-op."
  (if (ecb--semantic-tag-p tag)
      (if (ecb--semantic-tag-with-position-p tag)
          (let ((o  (ecb--semantic-tag-overlay tag)))
            (if (and (ecb--semantic-overlay-p o)
                     (not (ecb--semantic-overlay-live-p o)))
                (progn
                  (when (not no-reparse)
                    ;; we need this because:
                    ;; 1. After every jump to a tag X via the method-buffer of
                    ;;    ECB this tag X is added to the navigation history list
                    ;;    as new ecb-nav-tag-history-item.
                    ;; 2. Before every select of a source in the sources- or
                    ;;    history-buffer or of a node in the method-buffer
                    ;;    `ecb-nav-save-current' is called which operates onto
                    ;;    the last saved history-item which is often a
                    ;;    tag-history-item (see 1.): `ecb-nav-save-current'
                    ;;    saves for tag-history-items current-position and
                    ;;    window-start relative to the tag position of the
                    ;;    last saved tag-history-item which is tag X from
                    ;;    1.
                    ;; Now suppose that after 1. and before 2. the overlay of
                    ;; tag X has been destroyed cause of some reason. Then
                    ;; the tag-history-item of 1. contains now a tag with
                    ;; a destroyed overlay. Now step 2. is performed and now
                    ;; we see why from this moment every click onto a node in
                    ;; the source-, history- or method-buffer must fail:
                    ;; During step 2. `ecb-nav-save-current' gets the tag
                    ;; from the last tag-history-item and calls for this
                    ;; tag `ecb--semantic-tag-start' which fails now because
                    ;; the contained overlay of this tag is destroyed in the
                    ;; meanwhile. Therefore we must throw away this last
                    ;; tag-history-item containing the tag with the
                    ;; destroyed overlay. Then after a complete reparse of the
                    ;; source-buffer and following rebuild of the
                    ;; ECB-method-buffer ECB is in correct state again!
                    (ecb-nav-initialize)
                    (ecb--semantic-clear-toplevel-cache)
                    (ecb-update-methods-buffer--internal))
                  (ecb-enter-debugger "Tag %S is invalid!" tag)
                  nil)
              ;; else, tag is OK.
              t))
        ;; Position-less tags are also OK.
        t)
    ;; For no semantic-tags a reparse makes no sense!
    (ecb-enter-debugger "Not a semantic tag: %S" tag)
    nil))


(defun ecb-semantic-tag-buffer (tag)
  (ecb-semantic-assert-valid-tag tag)
  ;; if ecb-debug-mode is not nil then the TAG is valid if we pass the
  ;; assert. If ecb-debug-mode is nil then we call simply the semantic
  ;; function and see what happens.
  (ecb--semantic-tag-buffer tag))


(defun ecb-semantic-tag-start (tag)
  (ecb-semantic-assert-valid-tag tag)
  ;; if ecb-debug-mode is not nil then the TAG is valid if we pass the
  ;; assert. If ecb-debug-mode is nil then we call simply the semantic
  ;; function and see what happens.
  (ecb--semantic-tag-start tag))


(defun ecb-semantic-tag-end (tag)
  (ecb-semantic-assert-valid-tag tag)
  ;; if ecb-debug-mode is not nil then the TAG is valid if we pass the
  ;; assert. If ecb-debug-mode is nil then we call simply the semantic
  ;; function and see what happens.
  (ecb--semantic-tag-end tag))

;; Klaus: We must not reparse the buffer if `ecb--semantic-current-tag'
;; returns nil because here this is no error but nil is always returned for
;; example if point stays within a comment. Therefore here we only catch real
;; errors!
(defun ecb-semantic-current-nonterminal ()
  (condition-case nil
      (ecb--semantic-current-tag)
    (error (message "ecb--semantic-current-tag has problems --> reparsed is performed!")
           (when (ecb-point-in-edit-window)
             (ecb--semantic-clear-toplevel-cache)
             (ecb-update-methods-buffer--internal)
             (ecb--semantic-current-tag)))))


(defun ecb-goto-window-methods ()
  "Make the ECB-methods window the current window.
If `ecb-use-speedbar-instead-native-tree-buffer' is 'method then goto to the
speedbar-window."
  (interactive)
  (or (ecb-goto-ecb-window ecb-methods-buffer-name)
      (and (equal ecb-use-speedbar-instead-native-tree-buffer 'method)
           (ecb-goto-window-speedbar))))

(defun ecb-maximize-window-methods ()
  "Maximize the ECB-methods-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-methods-window is not visible in current layout."
  (interactive)
  (if (equal ecb-use-speedbar-instead-native-tree-buffer 'method)
      (ecb-maximize-window-speedbar)
    (ecb-maximize-ecb-buffer ecb-methods-buffer-name t)))

(defecb-window-dedicator ecb-set-methods-buffer ecb-methods-buffer-name
  "Display in current window the methods-buffer and make window dedicated."
  (let ((set-methods-buffer
         (not (equal ecb-use-speedbar-instead-native-tree-buffer 'method))))
    ;; first we act depending on the value of
    ;; ecb-use-speedbar-instead-native-tree-buffer
    (when (not set-methods-buffer)
      (condition-case error-data
          (ecb-set-speedbar-buffer)
        ;; setting the speedbar buffer has failed so we set
        ;; set-method-buffer to t ==> standard-methods-buffer is set!
        (error (message "%s" error-data)
               (setq set-methods-buffer t))))
    ;; maybe we need to set the standard methods buffer:
    ;; - if ecb-use-speedbar-instead-native-tree-buffer is not 'method or
    ;; - if setting the speedbar buffer has failed.
    (when set-methods-buffer
      (if (null ecb-use-speedbar-instead-native-tree-buffer)
          (ignore-errors (ecb-speedbar-deactivate)))
      (switch-to-buffer ecb-methods-buffer-name))))


(defun ecb-create-node (parent-node display name data type)
  (if (eq 'hidden display)
      nil
    (if (eq 'flattened display)
	parent-node
      (let ((node (tree-node-new name type data nil parent-node
				 (if ecb-truncate-long-names 'end))))
	(when (eq 'expanded display)
	  (setf (tree-node->expanded node) t))
        node))))


(defun ecb-get-tag-type-display (tag-type)
  (let* ((show-tags-list (ecb-get-show-tags-list))
         (display (ecb-find-assoc tag-type show-tags-list)))
    (if display
	display
      (setq display (ecb-find-assoc t show-tags-list))
      (if display
	  display
	'(t hidden nil)))))


(defun ecb-get-tag-parent-names (parents)
  (when parents
    (let* ((parent (car parents))
	   (name (cond
		  ((ecb--semantic-tag-p parent)
		   (ecb--semantic-format-tag-name parent nil ecb-font-lock-tags))
		  ((stringp parent)
		   (ecb--semantic--format-colorize-text parent 'type)))))
      (if name
	  (if (ecb-check-parent-for-exclude name)
	      (ecb-get-tag-parent-names (cdr parents))
	    (cons name (ecb-get-tag-parent-names (cdr parents))))
	(if (listp parent)
	    (append (ecb-get-tag-parent-names parent)
		    (ecb-get-tag-parent-names (cdr parents))))))))

(defun ecb-get-tag-parents (tag)
  "Return a list of parent-names already colorized by semantic. Currently
there is no distinction between superclasses and interfaces."
  (ecb-get-tag-parent-names
   (append (ecb--semantic-tag-type-superclass tag)
           (ecb--semantic-tag-type-interfaces tag))))
;;    (ecb--semantic-tag-type-parent tag)))


(defun ecb-get-tag-name (tag &optional parent-tag)
  "Get the name of TAG with the appropriate fcn from
`ecb-tag-display-function'."
  (condition-case nil
      (funcall (ecb-get-tag-display-function)
               tag parent-tag ecb-font-lock-tags)
    (error (ecb--semantic-format-tag-prototype tag parent-tag
                                               ecb-font-lock-tags))))


(defun ecb-find-add-tag-bucket (node type display sort-method buckets
                                       &optional parent-tag no-bucketize)
  "Finds a bucket containing tags of the given TYPE, creates nodes for them
and adds them to the given NODE. The bucket is removed from the BUCKETS list.
PARENT-TAG is only propagated to `ecb-add-tag-bucket'."
  (when (cdr buckets)
    (let ((bucket (cadr buckets)))
      (if (eq type (ecb--semantic-tag-class (cadr bucket)))
	  (progn
	    (ecb-add-tag-bucket node bucket display sort-method parent-tag
                                  no-bucketize)
	    (setcdr buckets (cddr buckets)))
	(ecb-find-add-tag-bucket node type display sort-method
				   (cdr buckets) parent-tag no-bucketize)))))

(defsubst ecb-forbid-tag-display (tag)
  (ecb--semantic--tag-put-property tag 'hide-tag t))
  
(defsubst ecb-allow-tag-display (tag)
  (ecb--semantic--tag-put-property tag 'hide-tag nil))

(defsubst ecb-tag-forbidden-display-p (tag)
  (ecb--semantic--tag-get-property tag 'hide-tag))

(defsubst ecb-show-at-least-one-tag-p (taglist)
  "Not nil if at least one of the tags in TAGLIST should be displayed in the
Methods-buffer."
  (catch 'found
    (dolist (tag taglist)
      (if (not (ecb-tag-forbidden-display-p tag))
          (throw 'found t)))
    nil))


;; The function requires that TAGLIST is a subset of the tag-table returned by
;; semantic for the current-buffer.
(defun ecb-apply-user-filter-to-tags (taglist)
  "Applies to the tags of TAGLIST the related filter of
`ecb-methods-user-filter-alist' - if there is any."
  (save-match-data
    (let ((filters (cdr (assoc (current-buffer) ecb-methods-user-filter-alist)))
          (filter-type nil)
          (filter nil)
          (inverse nil))
      (when filters
        (dolist (tag taglist)
          (dolist (filter-spec filters)
            (setq filter-type (nth 0 filter-spec))
            (setq filter (car (nth 1 filter-spec))) ;; ignore the attached fcn
            (setq inverse (nth 2 filter-spec))
            ;; we forbid some tags to be displayed when they do not match the
            ;; filter. Currently we do not apply a filter to tags of class 'type
            (unless (equal (ecb--semantic-tag-class tag) 'type)
              (cond ((equal filter-type 'regexp)
                     (if (funcall inverse
                                  (not (string-match filter
                                                     (ecb--semantic-tag-name tag))))
                         (ecb-forbid-tag-display tag)))
                    ((and (member filter '(private protected public))
                          (equal filter-type 'protection))
                     (if (funcall inverse
                                  (not (or (null (ecb--semantic-tag-protection tag))
                                           (equal (ecb--semantic-tag-protection tag)
                                                  filter))))
                         (ecb-forbid-tag-display tag)))
                    ((and (symbolp filter)
                          (equal filter-type 'tag-class))
                     (if (funcall inverse
                                  (not (equal (ecb--semantic-tag-class tag) filter)))
                         (ecb-forbid-tag-display tag)))
                    ((and (functionp filter)
                          (equal filter-type 'function))
                     (if (funcall inverse
                                  (not (funcall filter tag (current-buffer))))
                         (ecb-forbid-tag-display tag)))
                    (t nil)))))))))


(defun ecb-tag-generate-node-name (text-name first-chars icon-name)
  "Generate an suitable node name. Add needed image-icons if possible and
necessary. For the arguments TEXT-NAME, FIRST-CHARS and ICON-NAME see
`ecb-generate-node-name'."
  (if (ecb-use-images-for-semantic-tags)
      (ecb-generate-node-name text-name first-chars icon-name
                              ecb-methods-buffer-name)
    text-name))


(defun ecb-add-tag-bucket (node bucket display sort-method
                                &optional parent-tag no-bucketize)
  "Adds a tag bucket to a node unless DISPLAY equals 'hidden."
  (when bucket
    (let* ((name-bucket (ecb-format-bucket-name (car bucket)))
           (image-name (format "%s-bucket" (ecb--semantic-tag-class (cadr bucket))))
           (name (ecb-tag-generate-node-name name-bucket -1 image-name))
           ;;(type (ecb--semantic-tag-class (cadr bucket)))
           (bucket-node node))
      (unless (eq 'hidden display)
        (ecb-apply-user-filter-to-tags (cdr bucket))
	(unless (or (eq 'flattened display)
                    ;; we must not create a bucket-node when each tag in the
                    ;; bucket is forbidden to be displayed
                    (not (ecb-show-at-least-one-tag-p (cdr bucket))))
	  (setq bucket-node
                (tree-node-new name ecb-methods-nodetype-bucket
                               (list 'ecb-bucket-node
                                     (car bucket)
                                     (ecb--semantic-tag-class (car (cdr bucket))))
                               nil node
                               (if ecb-truncate-long-names 'end)))
	  (setf (tree-node->expanded bucket-node) (eq 'expanded display)))
        (dolist (tag (ecb-sort-tags sort-method (cdr bucket)))
          ;; we create only a new node for a tag of the bucket when the tag is
          ;; not forbidden to be displayed.
          (if (not (ecb-tag-forbidden-display-p tag))
              (ecb-update-tag-node tag
                                   (tree-node-new "" ecb-methods-nodetype-tag
                                                  tag t bucket-node
                                                  (if ecb-truncate-long-names 'end))
                                   parent-tag no-bucketize))
          ;; now we allow each tag to be displayed. This can be done because
          ;; here we already excluded the tag from being added as a node to
          ;; the tree-buffer and therefore from being displayed. So we can
          ;; reset all tags to be shown by default. So we can apply a complete
          ;; new filter (or no filter) without resetting the old filter before.
          (ecb-allow-tag-display tag))))))



(defconst ecb-tag-image-name-alias-alist
  '((abstract . ((static . ((struct . ((nil . "abstract-class-unknown")
                                       (unknown . "abstract-class-unknown")
                                       (private . "abstract-class-private")
                                       (protected . "abstract-class-protected")
                                       (public . "abstract-class-public")))
                            (class . ((nil . "abstract-class-unknown")
                                      (unknown . "abstract-class-unknown")
                                      (private . "abstract-class-private")
                                      (protected . "abstract-class-protected")
                                      (public . "abstract-class-public")))
                            ;; currently we have no special icon for
                            ;; interfaces - we use the icon for abstract classes
                            (interface . ((nil . "abstract-class-unknown")
                                          (unknown . "abstract-class-unknown")
                                          (private . "abstract-class-private")
                                          (protected . "abstract-class-protected")
                                          (public . "abstract-class-public")))
                            ;; we have no static and no abstract enum-icon
                            (enum . ((nil . "enum-unknown")
                                     (unknown . "enum-unknown")
                                     (private . "enum-private")
                                     (protected . "enum-protected")
                                     (public . "enum-public")))
                            ;; we have no icon for static constructors
                            (constructor . ((nil . "abstract-constructor-unknown")
                                            (unknown . "abstract-constructor-unknown")
                                            (private . "abstract-constructor-private")
                                            (protected . "abstract-constructor-protected")
                                            (public . "abstract-constructor-public")))
                            (function . ((nil . "abstract-function-unknown-static")
                                         (unknown . "abstract-function-unknown-static")
                                         (private . "abstract-function-private-static")
                                         (protected . "abstract-function-protected-static")
                                         (public . "abstract-function-public-static")))
                            (variable . ((nil . "abstract-variable-unknown-static")
                                         (unknown . "abstract-variable-unknown-static")
                                         (private . "abstract-variable-private-static")
                                         (protected . "abstract-variable-protected-static")
                                         (public . "abstract-variable-public-static")))))
                 (not-static . ((struct . ((nil . "abstract-class-unknown")
                                           (unknown . "abstract-class-unknown")
                                           (private . "abstract-class-private")
                                           (protected . "abstract-class-protected")
                                           (public . "abstract-class-public")))
                                (class . ((nil . "abstract-class-unknown")
                                          (unknown . "abstract-class-unknown")
                                          (private . "abstract-class-private")
                                          (protected . "abstract-class-protected")
                                          (public . "abstract-class-public")))
                                ;; we have currently no special icon for interfaces
                                (interface . ((nil . "abstract-class-unknown")
                                              (unknown . "abstract-class-unknown")
                                              (private . "abstract-class-private")
                                              (protected . "abstract-class-protected")
                                              (public . "abstract-class-public")))
                                ;; we have no abstract enum-icon
                                (enum . ((nil . "enum-unknown")
                                         (unknown . "enum-unknown")
                                         (private . "enum-private")
                                         (protected . "enum-protected")
                                         (public . "enum-public")))
                                (constructor . ((nil . "abstract-constructor-unknown")
                                                (unknown . "abstract-constructor-unknown")
                                                (private . "abstract-constructor-private")
                                                (protected . "abstract-constructor-protected")
                                                (public . "abstract-constructor-public")))
                                (function . ((nil . "abstract-function-unknown")
                                             (unknown . "abstract-function-unknown")
                                             (private . "abstract-function-private")
                                             (protected . "abstract-function-protected")
                                             (public . "abstract-function-public")))
                                (variable . ((nil . "abstract-variable-unknown")
                                             (unknown . "abstract-variable-unknown")
                                             (private . "abstract-variable-private")
                                             (protected . "abstract-variable-protected")
                                             (public . "abstract-variable-public")))))))
    (not-abstract . ((static . ((struct . ((nil . "class-unknown")
                                           (unknown . "class-unknown")
                                           (private . "class-private")
                                           (protected . "class-protected")
                                           (public . "class-public")))
                                (class . ((nil . "class-unknown")
                                          (unknown . "class-unknown")
                                          (private . "class-private")
                                          (protected . "class-protected")
                                          (public . "class-public")))
                                ;; we use the icon for abstract classes for interfaces
                                (interface . ((nil . "abstract-class-unknown")
                                              (unknown . "abstract-class-unknown")
                                              (private . "abstract-class-private")
                                              (protected . "abstract-class-protected")
                                              (public . "abstract-class-public")))
                                ;; we have no static enum-icon
                                (enum . ((nil . "enum-unknown")
                                         (unknown . "enum-unknown")
                                         (private . "enum-private")
                                         (protected . "enum-protected")
                                         (public . "enum-public")))
                                (constructor . ((nil . "constructor-unknown")
                                                (unknown . "constructor-unknown")
                                                (private . "constructor-private")
                                                (protected . "constructor-protected")
                                                (public . "constructor-public")))
                                (function . ((nil . "function-unknown-static")
                                             (unknown . "function-unknown-static")
                                             (private . "function-private-static")
                                             (protected . "function-protected-static")
                                             (public . "function-public-static")))
                                (variable . ((nil . "variable-unknown-static")
                                             (unknown . "variable-unknown-static")
                                             (private . "variable-private-static")
                                             (protected . "variable-protected-static")
                                             (public . "variable-public-static")))))
                     (not-static . ((struct . ((nil . "class-unknown")
                                               (unknown . "class-unknown")
                                               (private . "class-private")
                                               (protected . "class-protected")
                                               (public . "class-public")))
                                    (class . ((nil . "class-unknown")
                                              (unknown . "class-unknown")
                                              (private . "class-private")
                                              (protected . "class-protected")
                                              (public . "class-public")))
                                    (interface . ((nil . "abstract-class-unknown")
                                                  (unknown . "abstract-class-unknown")
                                                  (private . "abstract-class-private")
                                                  (protected . "abstract-class-protected")
                                                  (public . "abstract-class-public")))
                                    (enum . ((nil . "enum-unknown")
                                             (unknown . "enum-unknown")
                                             (private . "enum-private")
                                             (protected . "enum-protected")
                                             (public . "enum-public")))
                                    (constructor . ((nil . "constructor-unknown")
                                                    (unknown . "constructor-unknown")
                                                    (private . "constructor-private")
                                                    (protected . "constructor-protected")
                                                    (public . "constructor-public")))
                                    (function . ((nil . "function-unknown")
                                                 (unknown . "function-unknown")
                                                 (private . "function-private")
                                                 (protected . "function-protected")
                                                 (public . "function-public")))
                                    (variable . ((nil . "variable-unknown")
                                                 (unknown . "variable-unknown")
                                                 (private . "variable-private")
                                                 (protected . "variable-protected")
                                                 (public . "variable-public"))))))))
  "This alist defines the mapping from the combination
abstract-static-tag-protection to an existing icon-file-name.")


(defsubst ecb-get-icon-for-tag (abstract-p static-p type protection)
  (cdr (assq protection
              (cdr (assq type
                          (cdr (assq static-p
                                      (cdr (assq abstract-p
                                                  ecb-tag-image-name-alias-alist)))))))))


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: All this tag-icon-display-stuff
;; should be done by semantic - but for now we let do it by ECB because so we
;; can test the whole stuff. If Eric has added such icon-display to semantic
;; then we can throw away all this stuff and just using plain-tag-name as
;; node-name without any modification.

(defun ecb-displayed-tag-name (tag &optional parent-tag)
  "Return the tag-name of TAG as it will be displayed in the methods-buffer."
  (let* ((plain-tag-name (ecb-get-tag-name tag parent-tag))
         (has-protection (if (= 0 (length plain-tag-name))
                             nil
                           (member (ecb-first plain-tag-name)
                                   '(?- ?# ?+))))
         (icon-name (ecb-get-icon-for-tag
                     (if (ecb--semantic-tag-abstract-p tag parent-tag)
                         'abstract
                       'not-abstract)
                     (if (ecb--semantic-tag-static-p tag parent-tag)
                         'static
                       'not-static)
                     (or (and (equal (ecb--semantic-tag-class tag)
                                     'type)
                              (intern (ecb--semantic-tag-type tag)))
                         (and (ecb--semantic-tag-function-constructor-p tag)
                              'constructor)
                         (ecb--semantic-tag-class tag))
                     (or (and (ecb--semantic--tag-get-property tag 'adopted)
                              'unknown)
                         (and (not (member (ecb--semantic-tag-class tag)
                                           '(type function variable)))
                              'unknown)
                         (ecb--semantic-tag-protection tag parent-tag)))))
    (ecb-tag-generate-node-name plain-tag-name
                                (if has-protection 1 -1)
                                icon-name)))

(defun ecb-children-tags (parent-tag)
  "Return a list of children-tags of PARENT-TAG. If a child is not a
semantic-tag \(but a plain string) then it will be converted to a positionless
tag of class 'variable."
  (mapcar (function (lambda (c)
                      (typecase c
                        (ecb--semantic-tag
                         c)
                        (string
                         (ecb--semantic-tag-new-variable c nil nil nil))
                        (otherwise
                         (ecb-error "Tag with name %s contains invalid childrens"
                                    (ecb--semantic-tag-name parent-tag))))))
          (ecb--semantic-tag-children-compatibility
           parent-tag ecb-show-only-positioned-tags)))
                        

(defun ecb-update-tag-node (tag node &optional parent-tag no-bucketize)
  "Updates a node containing a tag."
  (let ((children (ecb-children-tags tag))
        (tag-name (ecb-displayed-tag-name tag parent-tag)))
    (setf (tree-node->name node) tag-name)
    (unless (eq 'function (ecb--semantic-tag-class tag))
      (ecb-add-tags node children tag no-bucketize)
      (setf (tree-node->expandable node)
            (not (= 0 (length (tree-node->children node)))))
      ;; Always expand types, maybe this should be customizable and more
      ;; flexible
      (if (not (eq 'type (ecb--semantic-tag-class tag)))
          (setf (tree-node->expanded node) nil)
        (let ((type-specifier (ecb-get-type-specifier tag)))
          (setf (tree-node->expanded node)
                (and (tree-node->expandable node)
                     (ecb-type-tag-expansion type-specifier))))))))

;; (ecb-tag-generate-node-name "klaus" 1 "function-public")

(defun ecb-post-process-taglist (taglist)
  "If for current major-mode post-process functions are found in
`ecb-post-process-semantic-taglist' then these functions are called with
TAGLIST otherwise TAGLIST is returned."
  (let ((fcn-list (cdr (assoc major-mode ecb-post-process-semantic-taglist))))
    (dolist (fcn fcn-list)
      (if (fboundp fcn)
          (setq taglist (funcall fcn taglist)))))
  (ecb-set-current-tag-table taglist)
  ;; now we apply that tag-filters which must operate onto the whole
  ;; tag-table of
  (ecb-apply-tag-table-filters taglist))

(defun ecb-apply-tag-table-filters (taglist)
  "Perform all tag-filters which must be applied to the whole tag-table."
  (let ((filters (cdr (assoc (current-buffer) ecb-methods-user-filter-alist)))
        (filter nil))
    (dolist (filter-type '(current-type))
      (setq filter (car (cdr (assoc filter-type filters))))
      (if filter
          (setq taglist (funcall (cdr filter) (car filter) taglist)))))
  taglist)


(defun ecb-methods-filter-perform-current-type (filter taglist)
  "Perform a current-type filter on TAGLIST. FILTER is a type-name-hierarchy
for a certain type. If this hierarchy can be found in TAGLIST a new tag-list
is returned which contains only the leaf-type in the hierarchy."
  (let ((curr-type-filter (reverse filter))
        (new-tag-list taglist)
        (found-type-tag nil))
    (if (null curr-type-filter)
        taglist
      (catch 'not-found
        (dolist (type-name curr-type-filter)
          (setq found-type-tag
                (car (ecb--semantic-find-tags-by-name
                      type-name
                      (ecb--semantic-find-tags-by-class 'type new-tag-list))))
          (if (null found-type-tag)
              (progn
                ;; remove here the filters for current source because the
                ;; current-type filter is no longer useable! TODO: Klaus
                ;; Berndl <klaus.berndl@sdm.de>: Maybe we should be smarter
                ;; and only remove the current-type-filter instead of all
                ;; filters. This could be done with
                ;; `ecb-replace-first-occurence' (replace the curr filter with
                ;; nil and then do (delq nil filters)
                (ecb-methods-filter-apply nil nil nil "" "" (current-buffer))
                (ecb-info-message
                 "ECB has removed all filters cause of changes in the type-hierarchy for the current-type!")
                ;; whenever we can not found any type in our filter type-hierarchy
                ;; then we can not apply this current-type filter so we have to
                ;; return the original tag-list
                (throw 'not-found taglist))
            (setq new-tag-list (ecb-children-tags found-type-tag))))
        ;; when we reach this point we can be sure that the whole type-hierarchy
        ;; has been found and so we return just our current-type as new taglist.
        (list found-type-tag)))))

(defun ecb-group-function-tags-with-parents (taglist)
  "Return a new taglist based on TAGLIST where all function-tags in
TAGLIST having a parent tag are grouped together under a new faux tag
for this parent-tag. The new taglist contains first all parent-less tags
and then all grouped tags.

This is useful for oo-programming languages where the methods of a class can
be defined outside the class-definition, e.g. C++, Eieio."
  (ecb--semantic-adopt-external-members taglist))

(defun ecb-filter-c-prototype-tags (taglist)
  "Filter out all prototypes.
Beginning with version 2.24 of ECB this function does nothing when
`ecb-methods-separate-prototypes' is set to not nil \(default).

For example this is useful for editing C files which have the function
prototypes defined at the top of the file and the implementations at the
bottom. This means that everything appears twice in the methods buffer, but
probably nobody wants to jump to the prototypes, they are only wasting space
in the methods buffer.
For C-header-files prototypes are never filtered out!"
;;   ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Is there a better way to
;;   ;; recognize a C-Header-file?
  (if ecb-methods-separate-prototypes
      taglist
    (let ((header-extensions '("\\.h\\'" "\\.H\\'" "\\.HH\\'" "\\.hxx\\'" "\\.hh\\'")))
      (or (and (catch 'found
                 (dolist (ext header-extensions)
                   (if (save-match-data
                         (string-match ext (buffer-file-name (current-buffer))))
                       (throw 'found t)))
                 nil)
               taglist)
          (ecb-filter taglist
                      (function (lambda (x)
                                  (not (ecb--semantic-tag-prototype-p x)))))))))

;; Filtering the Methods-buffer by the user ----------------

(defvar ecb-methods-user-filter-alist nil
  "The filter currently applied to the methods-buffer by the user. This cache
is an alist where the key is the buffer-object of that buffer the filter
belongs and the value is the applied filter to that buffer.

Filters which can work onto single tags are applied by
`ecb-apply-user-filter-to-tags' whereas tag-filters which have to be applied
onto the whole tag-table are performed by `ecb-apply-tag-table-filters'.")


(defun ecb-methods-filter-by-prot (inverse source-buffer &optional prot)
  "Filter the Methods-buffer by protection."
  (let ((choice (or prot
                    (ecb-query-string "Protection filter:"
                                      '("private" "protected" "public")))))
    (ecb-methods-filter-apply 'protection
                              (cons (intern choice) nil)
                              inverse
                              (concat (and inverse "^") "Prot")
                              choice
                              source-buffer)))

(defun ecb-methods-filter-by-tag-class (inverse source-buffer
                                                &optional tag-class)
  "Filter the Methods-buffer by a tag-class."
  (let* ((curr-semantic-symbol->name-assoc-list
          (save-excursion
            (set-buffer source-buffer)
            (ecb--semantic-symbol->name-assoc-list)))
         (choice (or tag-class
                     (ecb-query-string "Tag-class filter:"
                                       (mapcar 'cdr
                                               curr-semantic-symbol->name-assoc-list))))
         (class (or tag-class
                    (symbol-name
                     (car (delq nil (mapcar (function (lambda (e)
                                                        (if (ecb-string= (cdr e) choice)
                                                            (car e))))
                                            curr-semantic-symbol->name-assoc-list)))))))
    (ecb-methods-filter-apply 'tag-class
                              (cons (intern class) nil)
                              inverse
                              (concat (and inverse "^") "Tagclass")
                              (cdr (assoc (intern class)
                                          curr-semantic-symbol->name-assoc-list))
                              source-buffer)))


;; The popup-menu commands for protection- and tag-class-filters are generated
;; dynamically - see `ecb-methods-menu-tagfilter-entries'.

(defun ecb-methods-filter-by-regexp (inverse source-buffer &optional regexp)
  "Filter the Methods-buffer by a regular expression."
  (let ((regexp-str (or regexp (read-string "Filter-regexp: "))))
    (ecb-methods-filter-apply 'regexp
                              (if (> (length regexp-str) 0)
                                  (cons regexp-str nil)
                                nil)
                              inverse
                              (concat (and inverse "^") "Regexp")
                              (if (> (length regexp-str) 0) regexp-str nil)
                              source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-by-regexp-popup
  "Filter the Methods-buffer by regexp from popup."
  (ecb-methods-filter-by-regexp nil (ecb-methods-get-data-store 'source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-by-regexp-popup-inverse
  "Filter the Methods-buffer by inverse regexp from popup."
  (ecb-methods-filter-by-regexp t (ecb-methods-get-data-store 'source-buffer)))

(defun ecb-methods-filter-by-function (inverse source-buffer &optional fcn-name)
  "Filter the Methods-buffer by a filter-function."
  (let ((filter-fcn-name (or fcn-name
                             (completing-read "Tag-filter-function: "
                                              obarray 'fboundp t))))
    (ecb-methods-filter-apply 'function
                              (cons (intern filter-fcn-name)
                                    nil)
                              inverse
                              (concat (and inverse "^") "Function")
                              filter-fcn-name
                              source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-by-function-popup
  "Filter the Methods-buffer by function-filter from popup."
  (ecb-methods-filter-by-function nil (ecb-methods-get-data-store 'source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-by-function-popup-inverse
  "Filter the Methods-buffer by inverse function-filter from popup."
  (ecb-methods-filter-by-function t (ecb-methods-get-data-store 'source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-by-nothing-popup
  "Remove any filter from the Methods-buffer from popup."
  (ecb-methods-filter-apply nil nil nil "" ""
                            (ecb-methods-get-data-store 'source-buffer)))

(tree-buffer-defpopup-command ecb-methods-filter-delete-last-popup
  "Remove the last added filter from the Methods-buffer from popup."
  (ecb-methods-filter-apply nil nil nil "" "" (ecb-methods-get-data-store 'source-buffer) t))


(defun ecb-get-type-node-of-node (node)
  "Returns that node which data-tag is of class 'type the tag of the node NODE
of the Methods-buffer belongs to. If the tag of NODE do not belong to a type
then nil is returned."
  (let ((parent (tree-node->parent node)))
    (catch 'found
      (while (not (eq (tree-buffer-get-root) parent))
        (if (equal (and (= (tree-node->type parent) ecb-methods-nodetype-tag)
                        (ecb--semantic-tag-class (tree-node->data parent)))
                   'type)
            (throw 'found parent)
          (setq parent (tree-node->parent parent))))
      nil)))


(defun ecb-get-type-name-hierarchy-of-current-node ()
  "Return the type-name-hierarchy of current node in form of a list whereas the
first element is the name of the tag of the current node itself, the second
element is the name of the type the current node belongs to, the third element
is the name of the parent-type of that type and so on. The last element in
this list is the topmost type-parent of the tag of the current node. If the
current node has no tag as data then nil is returned. If the tag of the
current node does not belong to a type-tag \(e.g. a toplevel function) then
the returned list contains just the name of the tag of the current node."
  (let ((type-hierarchy nil)
        (curr-node (tree-buffer-get-node-at-point)))
    (when (and curr-node
               (= (tree-node->type curr-node) ecb-methods-nodetype-tag))
      (while (progn
               (setq type-hierarchy (cons (ecb--semantic-tag-name
                                           (tree-node->data curr-node))
                                          type-hierarchy))
               (setq curr-node (ecb-get-type-node-of-node curr-node)))))
    (nreverse type-hierarchy)))


(defun ecb-get-type-tag-of-tag (&optional tag table always-parent-type)
  "Returns that tag of class 'type the tag TAG belongs to. If TAG does not
belong to a type then nil is returned. If TAG is already of class 'type then
the behavior depends on the optional argument ALWAYS-PARENT-TYPE: If nil then
the current tag is returned otherwise the next parent-tag of class 'type is
returned.

If TAG is nil the tag returned by `ecb-get-real-curr-tag' is used. If TABLE is
nil then the tag-table of the current buffer is used; otherwise the tag-table
TABLE is used."
  (let* ((table (or table (ecb-get-current-tag-table)))
         (curr-tag (or tag (ecb-get-real-curr-tag)))
         (function-parent (ecb--semantic-tag-function-parent curr-tag)))
    (cond ((ecb-faux-group-tag-p curr-tag)
           (and (not always-parent-type) curr-tag))
          ((and (not always-parent-type)
                (equal (ecb--semantic-tag-class curr-tag) 'type))
           curr-tag)
          (t (if function-parent
                 ;; we have an external member and we search the type this
                 ;; external member belongs to. This can either be a type-tag
                 ;; in the current file (which is then contained in table) or
                 ;; a faux-tag (created by semantic-adopt-external-members)
                 ;; when the parent-type of this external member is defined
                 ;; outside the current source - but this faux-type is
                 ;; contained in table too.
                 (catch 'found
                   (dolist (tag (ecb--semantic-flatten-tags-table table))
                     (if (and (equal (ecb--semantic-tag-class tag) 'type)
                              (ecb-string= (ecb--semantic-tag-name tag)
                                           function-parent)
                              (delq nil
                                    (mapcar (lambda (child)
                                              (if (ecb--semantic-equivalent-tag-p
                                                   child curr-tag)
                                                  curr-tag))
                                            (ecb-children-tags tag))))
                         (throw 'found tag)))
                   nil)
               ;; we are already inside the parent-type - if there is any, so
               ;; we simply search the nearest tag of class 'type in the
               ;; reversed overlay-stack
               (catch 'found
                 (dolist (tag (cdr (reverse
                                    (ecb--semantic-find-tag-by-overlay
                                     (ecb--semantic-tag-start curr-tag)
                                     (ecb--semantic-tag-buffer curr-tag)))))
                   (if (equal (ecb--semantic-tag-class tag) 'type)
                       (throw 'found tag)))
                 nil))))))


(defun ecb-get-type-name-hierarchy-of-current-tag (&optional tag)
  "Return the type-name-hierarchy of TAG in form of a list whereas the
first element is the name of the TAG itself, the second element is the name of
the type the TAG belongs to, the third element is the name of the parent-type
of that type and so on. The last element in this list is the topmost
type-parent of the TAG. If the TAG does not belong to a type-tag \(e.g. a
toplevel function) then the returned list contains just the name of the
TAG. If TAG is nil then the current tag returned by `ecb-get-real-curr-tag' is
used; if point does not stay on a tag then nil is returned."
  (let ((type-hierarchy nil)
        (curr-tag (or tag (ecb-get-real-curr-tag))))
    (when curr-tag
      (while (progn
               (setq type-hierarchy (cons (ecb--semantic-tag-name curr-tag)
                                          type-hierarchy))
               (setq curr-tag (ecb-get-type-tag-of-tag curr-tag nil t)))))
    (nreverse type-hierarchy)))

(defun ecb-methods-filter-by-current-type (inverse source-buffer &optional
                                                   tag)
  "Display only the current-type and its contents in the methods-buffer. The
argument INVERSE is ignored here."
  (let* ((curr-type-tag (or (and (ecb--semantic-tag-p tag)
                                 (save-excursion
                                   (set-buffer source-buffer)
                                   (ecb-get-type-tag-of-tag tag)))
                            (cond ((ecb-point-in-edit-window)
                                   (if (ecb--semantic-active-p)
                                       (save-excursion
                                         (set-buffer source-buffer)
                                         (ecb-get-type-tag-of-tag (ecb-get-real-curr-tag)))))
                                  ((equal (current-buffer)
                                          (get-buffer ecb-methods-buffer-name))
                                   (let ((node (tree-buffer-get-node-at-point)))
                                     (and node
                                          (tree-node->data (ecb-get-type-node-of-node node)))))
                                  (t (ecb-error "ECB can not identify the current-type-tag!")))))
         (curr-tag-type-name-hierachy (and curr-type-tag
                                           (save-excursion
                                             (set-buffer source-buffer)
                                             (ecb-get-type-name-hierarchy-of-current-tag
                                              curr-type-tag)))))
    (if (and curr-type-tag curr-tag-type-name-hierachy)
        (ecb-methods-filter-apply 'current-type
                                  (cons curr-tag-type-name-hierachy
                                        'ecb-methods-filter-perform-current-type)
                                  nil
                                  "Type"
                                  (ecb--semantic-tag-name curr-type-tag)
                                  source-buffer)
      (ecb-error "ECB can not identify the current-type!"))))
                          
(tree-buffer-defpopup-command ecb-methods-filter-by-current-type-popup
  "Display only the current-type from popup."
  (ecb-methods-filter-by-current-type nil
                                      (ecb-methods-get-data-store 'source-buffer)
                                      (tree-node->data node)))


(defun ecb-get-source-buffer-for-tag-filter ()
  "Return the source-buffer of the tag-list which should be filtered."
  (cond ((ecb-point-in-edit-window)
         (current-buffer))
        ((equal (current-buffer)
                (get-buffer ecb-methods-buffer-name))
         (ecb-methods-get-data-store 'source-buffer))
        (t (or (and ecb-last-source-buffer
                    (buffer-live-p ecb-last-source-buffer)
                    ecb-last-source-buffer)
               (ecb-error "There is no source-file to filter!")))))
  
(defun ecb-methods-filter-inverse ()
  "Apply an inverse filter to the Methods-buffer. This is the same as calling
`ecb-methods-filter' with a prefix arg."
  (interactive)
  (ecb-methods-filter-internal t))

(defun ecb-methods-filter-protection (&optional inverse)
  "Filter the methods-buffer by protection. If INVERSE is not nil \(called
with a prefix arg) then an inverse filter is applied. For further details see
`ecb-methods-filter'."
  (interactive "P")
  (ecb-methods-filter-internal inverse 'protection))

(defun ecb-methods-filter-tagclass (&optional inverse)
  "Filter the methods-buffer by tag-class. If INVERSE is not nil \(called
with a prefix arg) then an inverse filter is applied. For further details see
`ecb-methods-filter'."
  (interactive "P")
  (ecb-methods-filter-internal inverse 'tag-class))

(defun ecb-methods-filter-current-type ()
  "Display in the Methods-buffer only the current type and its members. For
further details see `ecb-methods-filter'."
  (interactive)
  (ecb-methods-filter-internal nil 'curr-type))

(defun ecb-methods-filter-regexp (&optional inverse)
  "Filter the methods-buffer by a regexp. If INVERSE is not nil \(called
with a prefix arg) then an inverse filter is applied. For further details see
`ecb-methods-filter'."
  (interactive "P")
  (ecb-methods-filter-internal inverse 'regexp))

(defun ecb-methods-filter-function (&optional inverse)
  "Filter the methods-buffer by a function. If INVERSE is not nil \(called
with a prefix arg) then an inverse filter is applied. For further details see
`ecb-methods-filter'."
  (interactive "P")
  (ecb-methods-filter-internal inverse 'function))

(defun ecb-methods-filter-nofilter ()
  "Remove any filter from the Methods-buffer. For further details see
`ecb-methods-filter'."
  (interactive)
  (ecb-methods-filter-internal nil 'no-filter))

(defun ecb-methods-filter-delete-last ()
  "Remove the most recent filter from the Methods-buffer. For further details see
`ecb-methods-filter'."
  (interactive)
  (ecb-methods-filter-internal nil 'delete-last))

(defun ecb-methods-filter (&optional inverse)
  "Apply a filter to the Methods-buffer to reduce the number of entries.
So you get a better overlooking. There are six choices:
- Filter by protection: Just insert the protection you want the Methods-buffer
  being filtered: private, protected or public!
- Filter by regexp: Insert the filter as regular expression.
- Filter by tag-class: You can filter by the tag-classes of current
  major-mode. The available tag-classes come from the variable
  `semantic--symbol->name-assoc-list'. The are normally methods, variables
  etc.
- Filter by current type: In languages which have types like Java or C++ this
  filter displays only the current type and all its members \(e.g. attributes
  and methods). If ECB can not identify the current type in the source-buffer
  or in the methods-window then nothing will be done.
- Filter by a filter-function: Such a function gets two arguments: a tag and
  the source-buffer of this tag. If the tag should be displayed \(i.e. not
  being filtered out) then the function has to return not nil otherwise nil.
- No special filter: This means to display all tags specified with the option
  `ecb-show-tokens'. If currently some of the above filters are applied they
  will be all removed.
- Delete the last added: This removes only the topmost filter-layer, means
  that filter added last.

The protection-, the current-type and the tag-class-filter are only available
for semantic-supported sources.

Be aware that the tag-list specified by the option `ecb-show-tags' is the
basis of all filters, i.e. tags which are excluded by that option will never
be shown regardless of the filter type here!

All tags which match the applied filter\(s) will be displayed in the
Methods-buffer.

If called with a prefix-argument or when optional arg INVERSE is not nil then 
an inverse filter is applied to the Methods-buffer, i.e. all tags which
do NOT match the choosen filter will be displayed in the Methods-buffer!

Per default the choosen filter will be applied on top of already existing
filters. This means that filters applied before are combined with the new
filter. This behavior can changed via the option
`ecb-methods-filter-replace-existing'. But regardless of the setting in
`ecb-methods-filter-replace-existing' applying one of the not-inverse filters
protection, tag-class or current-type always replaces exactly already existing
filters of that type. On the other hand applying more than one inverse
tag-class- or protection-filter can make sense.

Such a filter is only applied to the current source-buffer, i.e. each
source-buffer can have its own tag-filters.

The current active filter will be displayed in the modeline of the
Methods-buffer \[regexp, prot \(= protection), tag-class, function \(=
filter-function)]. If an inverse filter has been applied then this is
signalized by a preceding caret ^. If currently more than 1 filter is applied
then always the top-most filter is displayed in the modeline but the fact of
more than 1 filter is visualized by the number of the filters - included in
parens. You can see all currently applied filters by moving the mouse over the
filter-string in modeline of the Methods-buffer: They will displayed as
help-echo.

See the option `ecb-default-tag-filter' if you search for automatically
applied default-tag-filters."
  (interactive "P")
  (ecb-methods-filter-internal inverse))

(defun ecb-methods-filter-internal (inverse &optional filter-type)
  "FILTER-TYPE has to be one of the symbols 'regexp, 'protection,
'tag-class, 'curr-type, 'function, 'no-filter or 'delete-last."
  (if (save-excursion
        (set-buffer ecb-methods-buffer-name)
        (tree-buffer-empty-p))
      (message "There is nothing to filter in an empty Methods-buffer!")
    (let* ((source-buffer (ecb-get-source-buffer-for-tag-filter))
           (semantic-source-p (save-excursion
                                (set-buffer source-buffer)
                                (ecb--semantic-active-p)))
           (choice (intern (or filter-type
                               (ecb-query-string
                                (format "Apply %sfilter:"
                                        (if inverse "inverse " ""))
                                (delq nil (list "regexp"
                                                (if semantic-source-p "protection")
                                                (if semantic-source-p "tag-class")
                                                (if semantic-source-p "curr-type")
                                                "function" "no-filter" "delete-last")))
                               "no-filter-specified"))))
      (case choice
        (protection
         (ecb-methods-filter-by-prot inverse source-buffer))
        (tag-class
         (ecb-methods-filter-by-tag-class inverse source-buffer))
        (regexp
         (ecb-methods-filter-by-regexp inverse source-buffer))
        (curr-type
         (ecb-methods-filter-by-current-type inverse source-buffer))
        (function
         (ecb-methods-filter-by-function inverse source-buffer))
        (delete-last
         (ecb-methods-filter-apply nil nil nil "" "" source-buffer t))
        (no-filter
         (ecb-methods-filter-apply nil nil nil "" "" source-buffer))
        (otherwise
         (ecb-methods-filter-apply nil nil nil "" "" source-buffer))))))

(defun ecb-methods-filter-apply (filtertype filter inverse filter-type-display
                                            filter-display
                                            source-buffer &optional remove-last)
  "Apply the FILTER of type FILTERTYPE to the buffer SOURCEBUFFER. If INVERSE
is not nil then this filter will be applied inverse. FILTER-TYPE-DISPLAY and
FILTER-DISPLAY are strings and specify how the FILTER of type FILTERTYPE
should be displayed in the modeline of the methods-buffer. If REMOVE-LAST is
not nil then the topmost filter will be removed and all other arguments unless
SOURCE-BUFFER arguments are ignored."
  (save-excursion
    (set-buffer source-buffer)
    (if (and (not remove-last)
             (member filtertype '(protection tag-class curr-type))
             (not (ecb--semantic-active-p)))
        (ecb-error "A %s-filter '%s' can only applied to semantic-supported sources!"
                   filtertype filter)))
  (let* ((filter-elem (assoc source-buffer ecb-methods-user-filter-alist))
         (new-filter-spec (and filtertype
                               (list filtertype filter (if inverse 'not 'identity)
                                     filter-type-display filter-display)))
         (replace-all (and (not remove-last)
                           (not (equal ecb-methods-filter-replace-existing 'never))
                           (or (equal ecb-methods-filter-replace-existing 'always)
                               (y-or-n-p "Should the new filter replace existing ones? "))))
         (replace-filter-type (and (not inverse)
                                   (not replace-all)
                                   (not remove-last)
                                   (assoc filtertype (cdr filter-elem))
                                   (member filtertype '(protection tag-class current-type))))
         (filters (or (and replace-filter-type
                           (progn
                             (setcdr filter-elem
                                     (ecb-remove-assoc filtertype (cdr filter-elem)))
                             (append (cdr filter-elem) (list new-filter-spec))))
                      (and remove-last
                           (nreverse (cdr (reverse (cdr filter-elem)))))
                      (and new-filter-spec ;; if nil there should be no filter anymore
                           (if replace-all
                               new-filter-spec ;; just the new filter-spec
                             (append (cdr filter-elem) (list new-filter-spec)))))))
    (if filter-elem
        (setcdr filter-elem filters)
      (if filters
          (setq ecb-methods-user-filter-alist
                (cons (cons source-buffer filters) ecb-methods-user-filter-alist)))))
  (when (buffer-live-p source-buffer)
    (save-excursion
      (set-buffer source-buffer)
      (if (ecb--semantic-active-p)
          ;; For semantic-sources we do not use `ecb-rebuild-methods-buffer)'
          ;; because this would always reparse the source-buffer even if not
          ;; necessary.
          (save-restriction
            (widen)
            (ecb-rebuild-methods-buffer-with-tagcache
             (ecb--semantic-fetch-tags t)))
        (ecb-rebuild-methods-buffer)))
    (when (save-excursion
            (set-buffer ecb-methods-buffer-name)
            (tree-buffer-empty-p))
      (ecb-methods-filter-apply nil nil nil "" "" source-buffer t)
      (message "ECB has not applied this filter because it would filter out all nodes!"))))
        
  
(defun ecb-methods-filter-modeline-prefix (buffer-name sel-dir sel-source)
  "Compute a mode-line prefix for the Methods-buffer so the current filter
applied to the displayed tags is displayed. This function is only for using by
the option `ecb-mode-line-prefixes'."
  (let* ((filters (and sel-source
                       (cdr (assoc (get-file-buffer sel-source)
                                   ecb-methods-user-filter-alist))))
         (top-filter-spec (ecb-last filters))
         (filter-type-str (nth 3 top-filter-spec))
         (filter-str (nth 4 top-filter-spec)))
    (if (null top-filter-spec)
        nil ;; no prefix if no filter
      (let ((str (format "[%s%s: %s]"
                         filter-type-str
                         (if (> (length filters) 1)
                             (format "(%d)" (length filters))
                           "")
                         filter-str)))
        (put-text-property 0 (length str) 'help-echo
                           (mapconcat (function
                                       (lambda (f-elem)
                                         (let ((f-type-str (nth 3 f-elem) )
                                               (f-str (nth 4 f-elem)))
                                           (format "[%s: %s]"
                                                   f-type-str f-str))))
                                      filters
                                      ", ")
                           str)
        str))))

(defun ecb-default-tag-filter-for-current-file ()
  "Check if for the file of the current buffer a default-tag-filter should be
applied. If yes, then the filters-list of `ecb-default-tag-filter' is returned
otherwise nil."
  (catch 'found
    (dolist (spec ecb-default-tag-filter)
      (let ((m-mode (caar spec))
            (regexp (cdar spec)))
        (if (and (equal major-mode m-mode)
                 (save-match-data
                   (string-match regexp (buffer-file-name (current-buffer)))))
            (throw 'found (cdr spec)))))
    nil))

(defun ecb-apply-default-tag-filter ()
  "Applies all default-tag-filters specified in `ecb-default-tag-filter' for
the current file."
  (remove-hook 'post-command-hook 'ecb-apply-default-tag-filter)
  (ignore-errors
    (let ((tag-filter-list (ecb-default-tag-filter-for-current-file)))
      (dolist (filter-spec tag-filter-list)
        (let ((filter-apply-fcn
               (case (nth 0 filter-spec)
                 (protection 'ecb-methods-filter-by-prot)
                 (tag-class  'ecb-methods-filter-by-tag-class)
                 (regexp 'ecb-methods-filter-by-regexp)
                 (function 'ecb-methods-filter-by-function)))
              (filter
               (case (nth 0 filter-spec)
                 (protection
                  (typecase (nth 1 filter-spec)
                    (symbol (symbol-name (nth 1 filter-spec)))
                    (string (nth 1 filter-spec))
                    (otherwise
                     (ecb-error "Not a valid tag-filter: %s" (nth 1 filter-spec)))))
                 (tag-class
                  (typecase (nth 1 filter-spec)
                    (symbol (symbol-name (nth 1 filter-spec)))
                    (string (nth 1 filter-spec))
                    (otherwise
                     (ecb-error "Not a valid tag-filter: %s" (nth 1 filter-spec)))))
                 (regexp
                  (typecase (nth 1 filter-spec)
                    (string (nth 1 filter-spec))
                    (otherwise
                     (ecb-error "Not a valid tag-filter: %s" (nth 1 filter-spec)))))
                 (function
                  (typecase (nth 1 filter-spec)
                    (symbol (symbol-name (nth 1 filter-spec)))
                    (string (nth 1 filter-spec))
                    (otherwise
                     (ecb-error "Not a valid tag-filter: %s" (nth 1 filter-spec))))))))
          (funcall filter-apply-fcn
                   (nth 2 filter-spec) (current-buffer) filter))))))

(defun ecb-find-file-hook ()
  "Adds `ecb-apply-default-tag-filter' to `post-command-hook'. This function
removes itself from the `post-command-hook'."
  (add-hook 'post-command-hook 'ecb-apply-default-tag-filter))

;; adding tags to the Methods-buffer 

(defun ecb-add-tags (node tags &optional parent-tag no-bucketize)
  "Add TAGS to the node NODE.
If NO-BUCKETIZE is not nil then TAGS will not bucketized by
`ecb--semantic-bucketize' but must already been bucketized! If not nil
PARENT-TAG is the parent of TAGS."
  (ecb-add-tag-buckets
   node parent-tag
   (if no-bucketize
       tags
     (ecb--semantic-bucketize tags
                              (and parent-tag
                                   (ecb--semantic-symbol->name-assoc-list-for-type-parts)
                                   (equal (ecb--semantic-tag-class parent-tag)
                                          'type))))
   no-bucketize))


(defun ecb-access-order (access)
  "Map ACCESS to a integer-value.
'public     --> 0
'protected  --> 1
'private    --> 3
<all other> --> 2"
  (cond
   ((eq 'public access) 0)
   ((eq 'protected access) 1)
   ((eq 'private access) 3)
   (t  2)))


(defun ecb-sort-tags (sort-method tags)
  (if sort-method
      (let ((tags-by-name
	     (sort tags (function (lambda (a b)
				      (ecb-string< (ecb--semantic-tag-name a)
                                                   (ecb--semantic-tag-name b)))))))
	(if (eq 'access sort-method)
	    (sort tags-by-name
		  (function
		   (lambda (a b)
		     (< (ecb-access-order (ecb--semantic-tag-protection a))
			(ecb-access-order (ecb--semantic-tag-protection b))))))
	  tags-by-name))
    tags))


(defun ecb-add-tag-buckets (node parent-tag buckets &optional no-bucketize)
  "Creates and adds tag nodes to the given node.
The PARENT-TAG is propagated to the functions `ecb-add-tag-bucket' and
`ecb-find-add-tag-bucket'."
  (setq buckets (cons nil buckets))
  (dolist (tag-display (ecb-get-show-tags-list))
    (let* ((type (car tag-display))
           (display (cadr tag-display))
           (sort-method (caddr tag-display)))
      (cond
       ((eq 'parent type)
 	(when (and parent-tag
 		   (eq 'type (ecb--semantic-tag-class parent-tag)))
 	  (let ((parents (ecb-get-tag-parents parent-tag)))
	    (when parents
	      (let* ((name-bucket (ecb-format-bucket-name "Parents"))
                     (name (ecb-tag-generate-node-name name-bucket -1
                                                       "parent-bucket"))
                     (parent-node nil))
                (setq parent-node (ecb-create-node node display
                                                   name
                                                   (list 'ecb-bucket-node
                                                         "Parents"
                                                         'parent)
                                                   ecb-methods-nodetype-bucket))
                (when node
		  (dolist (parent (if sort-method
				      (sort parents 'ecb-string<) parents))
                    (let* ((plain-parent-name
                            (if ecb-font-lock-tags
                                (ecb--semantic--format-colorize-text parent 'type)
                              parent))
                           ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: When
                           ;; the next version of the semantic-parsers offer
                           ;; the protection of the inheritance (like possible
                           ;; in C++) then we have to adjust this code and
                           ;; compute the correct icon-name.
                           (parent-name (ecb-tag-generate-node-name plain-parent-name
                                                                    -1
                                                                    "parent-unknown")))
                      (tree-node-new parent-name
                                     ecb-methods-nodetype-externtag
                                     parent t parent-node
                                     (if ecb-truncate-long-names 'end))))))))))
       (t (ecb-find-add-tag-bucket node type display sort-method buckets
                                   parent-tag no-bucketize)))))
  (let ((type-display (ecb-get-tag-type-display t)))
    (dolist (bucket buckets)
      (ecb-add-tag-bucket node bucket (cadr type-display)
                            (caddr type-display) parent-tag no-bucketize))))


(defun ecb-update-after-partial-reparse (updated-tags)
  "Updates the method buffer and all internal ECB-caches after a partial
semantic-reparse. This function is added to the hook
`semantic-after-partial-cache-change-hook'."
  ;; TODO: Currently we get simply the whole cache from semantic (already up
  ;; to date at this time!) and then we rebuild the whole tree-buffer with
  ;; this cache-contents. This is for great sources slow. We should implement
  ;; a mechanism where only the UPDATED-TAGS are used and only this ones are
  ;; updated. But for this we need also a tree-buffer-update which can update
  ;; single nodes without refreshing the whole tree-buffer like now.

  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: here we could check if
  ;; UPDATED-TAGS contains only one tag and if this tag contains no childrens
  ;; then we could use the new function `tree-buffer-update-node' to simply
  ;; updating the associated node instead of a full reparse and then full
  ;; tree-buffer-update.
  (if (and (= 1 (length updated-tags))
           (null (ecb-children-tags (car updated-tags))))
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: 
      ;; we could update this single node if we can find this node. But this
      ;; could be difficult (or impossible?) because here we only know the new
      ;; semantic-tag but our nodes contain only outdated semantic-tags as
      ;; data so how to find the associated node??!!
      ;; Maybe we could search the node which contaisn the parent-tag of the
      ;; updated tag and then we compute the position p of this tag in the list
      ;; of the children of its parent-tag and then we update that node which
      ;; comes on the same position p in the list of childrens of the
      ;; associated parent-node - hmm, but can we be sure that the sequence of
      ;; children-tags and children-nodes is the same?? probably not because
      ;; the nodes are ordered alphabetically and the tags are are ordered in
      ;; that sequence they are code in the source-buffer! Hmmm...........
      ;; Until this question is solved we must use the full reparse/rebuild
      ;; :-( One possible solution: tempor. ordering the
      ;; semantic-tag-childrens by name and getting the position p of the
      ;; updated tag in that ordered tag-sequence...
      (ecb-rebuild-methods-buffer-with-tagcache (ecb--semantic-fetch-tags t))
    (ecb-rebuild-methods-buffer-with-tagcache (ecb--semantic-fetch-tags t))))


(defun ecb-semantic-active-for-file (filename)
  "Return not nil if FILENAME is already displayed in a buffer and if semantic
is active for this buffer."
  (and (get-file-buffer filename)
       (save-excursion
         (set-buffer (get-file-buffer filename))
         (ecb--semantic-active-p))))


(defun ecb-update-methods-after-saving ()
  "Updates the methods-buffer after saving if this option is turned on and if
current-buffer is saved."
  (when (and (equal (selected-frame) ecb-frame)
             ecb-auto-update-methods-after-save
             ecb-last-edit-window-with-point
             ;; this prevents updating the method buffer after saving a not
             ;; current buffer (e.g. with `save-some-buffers'), because this
             ;; would result in displaying a method-buffer not belonging to the
             ;; current source-buffer.
             (equal (current-buffer)
                    (window-buffer ecb-last-edit-window-with-point)))
    (ecb-select-source-file ecb-path-selected-source)
    (ecb-rebuild-methods-buffer)))


(defvar ecb-method-buffer-needs-rebuild t
  "This variable is only set and evaluated by the functions
`ecb-update-methods-buffer--internal' and
`ecb-rebuild-methods-buffer-with-tagcache'!")


(defun ecb-update-methods-buffer--internal (&optional scroll-to-top
                                                      rebuild-non-semantic)
  "Updates the methods buffer with the current buffer. The only thing what
must be done is to start the toplevel parsing of semantic, because the rest is
done by `ecb-rebuild-methods-buffer-with-tagcache' because this function is in
the `semantic-after-toplevel-cache-change-hook'.
If optional argument SCROLL-TO-TOP is non nil then the method-buffer is
displayed with window-start and point at beginning of buffer.

If second optional argument REBUILD-NON-SEMANTIC is not nil then non-semantic
sources are forced to be rescanned and reparsed by
`ecb-rebuild-methods-buffer-with-tagcache'. The function
`ecb-rebuild-methods-buffer-for-non-semantic' is the only one settings this
argument to not nil!"
  (when (and (equal (selected-frame) ecb-frame)
             (get-buffer-window ecb-methods-buffer-name))
    ;; Set here `ecb-method-buffer-needs-rebuild' to t so we can see below if
    ;; `ecb-rebuild-methods-buffer-with-tagcache' was called auto. after
    ;; `ecb--semantic-fetch-tags'.
    (setq ecb-method-buffer-needs-rebuild t)

    (let ((current-tagcache (and (ecb--semantic-active-p)
                                   ;; if we manually bovinate the buffer we
                                   ;; must widen the source to get all tags.
                                   (save-excursion
                                     (save-restriction
                                       (widen)
                                       (ecb--semantic-fetch-tags t))))))
      ;; If the `ecb--semantic-fetch-tags' has done no reparsing but only
