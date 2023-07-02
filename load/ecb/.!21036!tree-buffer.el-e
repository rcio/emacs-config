;;; tree-buffer.el --- functions for tree buffers

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools, tree
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

;; $Id: tree-buffer.el,v 1.170 2005/06/27 17:02:29 berndl Exp $

;;; Commentary:

;; Functions for tree buffers.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;; XEmacs stuff
(silentcomp-defun button-release-event-p)
(silentcomp-defun button-press-event-p)
(silentcomp-defun event-key)
(silentcomp-defun extent-end-position)
(silentcomp-defun event-glyph-extent)
(silentcomp-defun event-over-glyph-p)
(silentcomp-defun display-message)
(silentcomp-defun clear-message)
(silentcomp-defun locate-data-directory)
(silentcomp-defun make-image-specifier)
(silentcomp-defun make-glyph)
(silentcomp-defun popup-menu-and-execute-in-window)
(silentcomp-defun valid-image-instantiator-format-p)
(silentcomp-defvar modeline-map)
;; Emacs
(silentcomp-defvar message-log-max)
(silentcomp-defvar message-truncate-lines)
(silentcomp-defun posn-window)
(silentcomp-defun event-start)
(silentcomp-defun posn-point)
(silentcomp-defun event-basic-type)
(silentcomp-defun display-images-p)
(silentcomp-defun image-type-available-p)
(silentcomp-defun count-screen-lines)
(silentcomp-defun tmm-prompt)
(silentcomp-defun font-lock-add-keywords)
;; timer stuff for XEmacs
(silentcomp-defun delete-itimer)
(silentcomp-defun start-itimer)

(defconst tree-buffer-running-xemacs
  (string-match "XEmacs\\|Lucid" emacs-version))

;; miscellaneous differences

(if tree-buffer-running-xemacs
    ;; XEmacs
    (progn
      (defun tree-buffer-facep (face)
        (memq face (face-list)))
      (defalias 'tree-buffer-line-beginning-pos 'point-at-bol)
      (defalias 'tree-buffer-line-end-pos 'point-at-eol)
      (defalias 'tree-buffer-window-display-height 'window-displayed-height)
      (defun tree-buffer-event-to-key (event)
        (typecase event
          (button-release-event 'mouse-release)
          (button-press-event 'mouse-press)
          (otherwise
           ;; the ignore-errors is a little hack because i don't know all
           ;; events of XEmacs so sometimes event-key produces a
           ;; wrong-type-argument error.
           (ignore-errors (event-key event)))))
      (defalias 'tree-buffer-event-window 'event-window)
      (defalias 'tree-buffer-event-point 'event-point)
      ;; stolen from dframe.el of the speedbar-library.
      (defun tree-buffer-mouse-set-point (e)
        "Set POINT based on event E. Handles clicking on images in XEmacs."
        (mouse-set-point e)
        (if (and (fboundp 'event-over-glyph-p) (event-over-glyph-p e))
            ;; We are in XEmacs, and clicked on a picture
            (let ((ext (event-glyph-extent e)))
              ;; This position is back inside the extent where the
              ;; junk we pushed into the property list lives.
              (if (extent-end-position ext)
                  (goto-char (1- (extent-end-position ext))))))))

  ;; GNU Emacs
  (defalias 'tree-buffer-facep 'facep)
  (defalias 'tree-buffer-line-beginning-pos 'line-beginning-position)
  (defalias 'tree-buffer-line-end-pos 'line-end-position)
  ;; Klaus Berndl <klaus.berndl@sdm.de>: Is not really the same as
  ;; `window-displayed-height' of XEmacs, because if the buffer-end is before
  ;; the window-end (i.e. there are "empty" lines between window-end and last
  ;; char of the buffer) then these empty-lines are not counted. But in the
  ;; situations this function is used (only in tree-buffer-recenter) this
  ;; doesn't matter.
  (defalias 'tree-buffer-window-display-height 'window-text-height)
  (defun tree-buffer-event-window (event)
    (posn-window (event-start event)))
  (defun tree-buffer-event-point (event)
    (posn-point (event-start event)))
  (defalias 'tree-buffer-mouse-set-point 'mouse-set-point)
  (defun tree-buffer-event-to-key (event)
    (let ((type (event-basic-type event)))
      (case type
        ((mouse-1 mouse-2 mouse-3) 'mouse-release)
        ((down-mouse-1 down-mouse-2 down-mouse-3) 'mouse-press)
        (otherwise (event-basic-type event)))))
  )

;; overlay/extend stuff

(if (not tree-buffer-running-xemacs)
    (progn
      (defalias 'tree-buffer-make-overlay   'make-overlay)
      (defalias 'tree-buffer-overlay-put    'overlay-put)
      (defalias 'tree-buffer-overlay-move   'move-overlay)
      (defalias 'tree-buffer-overlay-delete 'delete-overlay)
      (defalias 'tree-buffer-overlay-kill   'delete-overlay))
  ;; XEmacs
  (defalias 'tree-buffer-make-overlay   'make-extent)
  (defalias 'tree-buffer-overlay-put    'set-extent-property)
  (defalias 'tree-buffer-overlay-move   'set-extent-endpoints)
  (defalias 'tree-buffer-overlay-delete 'detach-extent)
  (defalias 'tree-buffer-overlay-kill   'delete-extent))


;; timer stuff

(if (not tree-buffer-running-xemacs)
    (progn
      (defalias 'tree-buffer-run-with-idle-timer 'run-with-idle-timer)
      (defalias 'tree-buffer-cancel-timer 'cancel-timer))
  ;; XEmacs
  (if (fboundp 'run-with-idle-timer)
      (defalias 'tree-buffer-run-with-idle-timer 'run-with-idle-timer)
    (defun tree-buffer-run-with-idle-timer (secs repeat function &rest args)
      "Perform an action the next time Emacs is idle for SECS seconds.
If REPEAT is non-nil, do this each time Emacs is idle for SECS seconds.
SECS may be an integer or a floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in
`tree-buffer-cancel-timer'."
      (start-itimer "tree-buffer-idle-timer"
                    function secs (if repeat secs nil)
                    t (if args t nil) args)))

  (if (fboundp 'cancel-timer)
      (defalias 'tree-buffer-cancel-timer 'cancel-timer)
    (defun tree-buffer-cancel-timer (timer)
      "Remove TIMER from the list of active timers."
      (delete-itimer timer))))  


;; basic utilities

(defun tree-buffer-copy-list (list)
  "Return a copy of a LIST, which may be a dotted list.
The elements of the list are not copied, just the list structure itself."
  (if (fboundp 'copy-sequence)
      (copy-sequence list)
    (if (consp list)
        (let ((res nil))
          (while (consp list) (push (pop list) res))
          (prog1 (nreverse res) (setcdr res list)))
      (car list))))

(defun tree-buffer-member (item list &optional test-fcn)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM. Comparison is done with `equal'
unless TEST-FCN is not nil: In this case TEST-FCN will be used to compare ITEM
with the elements of LIST. If TEST-FCN is `eq' then `memq' is called for
optimization."
  (if test-fcn
      (if (eq test-fcn 'eq)
          ;; some optimization
          (memq item list)
        (progn
          (while (and list (not (funcall test-fcn item (car list))))
            (setq list (cdr list)))
          list))
    (member item list)))

(defun tree-buffer-position (seq elem &optional test-fcn)
  "Return the position of ELEM within SEQ counting from 0. Comparison is done
with `equal' unless TEST-FCN is not nil: In this case TEST-FCN will be used to
compare ITEM with the elements of SEQ."
  (if (listp seq)
      (let ((pos (- (length seq) (length (tree-buffer-member elem seq test-fcn)))))
        (if (= pos (length seq))
            nil
          pos))
    (catch 'found
      (dotimes (i (length seq))
        (if (funcall (or test-fcn 'equal) elem (aref seq i))
            (throw 'found i)))
      nil)))

(defun tree-buffer-set-elt (seq n val)
  "Set VAL as new N-th element of SEQ. SEQ can be any sequence. SEQ will be
changed because this is desctructive function. SEQ is returned."
  (if (listp seq)
      (setcar (nthcdr n seq) val)
    (aset seq n val))
  seq)

(defun tree-buffer-remove-elt (seq n)
  "Remove N-th element from SEQ. SEQ can be any sequence. SEQ will be
changed because this is desctructive function. SEQ is returned."
  (delq 'tree-buffer-remove-marker
        (tree-buffer-set-elt seq n 'tree-buffer-remove-marker)))

(defsubst tree-buffer-aset (array idx newelt)
  "Same as `aset' but returns changed ARRAY."
  (aset array idx newelt)
  array)

(defun tree-buffer-nolog-message (&rest args)
  "Works exactly like `message' but does not log the message"
  (let ((msg (cond ((or (null args)
                        (null (car args)))
                    nil)
                   ((null (cdr args))
                    (car args))
                   (t
                    (apply 'format args)))))
    ;; Now message is either nil or the formated string.
    (if tree-buffer-running-xemacs
        ;; XEmacs way of preventing log messages.
        (if msg
            (display-message 'no-log msg)
          (clear-message 'no-log))
      ;; Emacs way of preventing log messages.
      (let ((message-log-max nil)
            (message-truncate-lines nil))
        (if msg
            (message "%s" msg)
          (message nil))))
    msg))

(defsubst tree-buffer-current-line ()
  "Return the current line-number - the first line in a buffer has number 1."
  (+ (count-lines 1 (point)) (if (= (current-column) 0) 1 0)))

;; debugging

(defvar tree-buffer-debug-mode nil
  "If not nil then all functions of tree-buffer which are debug-able write
debug-messages to the message-log of Emacs. Ensure that this variable is opnlx
not nil if you want find or report an error!")

(defun tree-buffer-debug-error (&rest args)
  "Run ARGS through `format' and write it to the *Messages*-buffer.
Do nothing if `tree-buffer-debug-mode' is nil!"
  (when tree-buffer-debug-mode
    (message (concat (format "Tree-buffer-debug: [%s] "
                             (format-time-string "%H:%M:%S"))
                     (apply 'format args)))))

;; tree-node

(defstruct (tree-node
            (:constructor -tree-node-new)
            (:copier nil)
            (:conc-name tree-node->))
  name
  type
  data
  children
  parent
  shrink-name
  expandable
  expanded
  displayed-name
  indentstr)

(defun tree-node-new (name type data &optional not-expandable parent
                           shrink-name)
  "Create a new tree-node which can be displayed in a tree-buffer.
A tree-node can have the following slots:

  NAME: The name of the node. Regardless how the node is displayed; see
  SHRINK-NAME and DISPLAYED-NAME.

  TYPE: The type of the node; must currently be an interger!

  DATA: The data of the node; can be arbitrary lisp-structures.

  EXPANDED: If not nil then the node is currently expanded, means its children
  are visible.

  PARENT: The parent tree-node.

  SHRINK-NAME: Decides if the NAME can be shortened when displayed in a
  narrow tree buffer window. The following values are valid:
  - beginning: The NAME is truncated at the beginning so the end is always
    visible.
  - end: The NAME is truncated at the end. If the tree-node is EXPANDABLE the
    name is truncated so that the expand symbol is visible.
  - nil: The NAME is never truncated. In this case DISPLAYED-NAME is equal to
    NAME.

  CHILDREN: List of children tree-nodes.

  EXPANDABLE: If not nil then the node is expandable means has children.

  INDENTSTR: Containes the full indentation-string for the node. So a single
  node can easily be redrawn.

  DISPLAYED-NAME: Contains the current displayed name of the node. The
  displayed name can be different from the NAME according to the value of
  SHRINK-NAME.

For all parameters except NOT-EXPANDABLE the description is available in the
slot-list above. If NOT-EXPANDABLE is set to not nil then the slot EXPANDABLE
will be set to nil; otherwise to t.

See Info node `(ecb)tree-buffer' for all details of using tree-nodes."
  (let ((n (-tree-node-new :name name
                           :type type
                           :data data
                           :expandable (not not-expandable)
                           :parent parent
                           :shrink-name shrink-name
                           :children nil
                           :expanded nil
                           :displayed-name nil
                           :indentstr nil)))
    (when (and parent (tree-node-p parent))
      (tree-node-add-children parent n))
    n))

(defsubst tree-node-indentlength (node)
  "Return the length of slot INDENTSTR."
  (length (tree-node->indentstr node)))

(defsubst tree-node-linelength (node)
  "Return the length of the full node display in current tree-buffer.
This is the length of the indentation \(slot INDENTSTR) plus the length of the
slot DISPLAYED-NAME of NODE."
  (+ (length (tree-node->displayed-name node))
     (tree-node-indentlength node)))

(defsubst tree-node-toggle-expanded (node)
  "Toggle the value of slot EXPANDED."
  (setf (tree-node->expanded node) (not (tree-node->expanded node))))

(defun tree-node-indent-level (node)
  "Return indentation-level of NODE.
Top-level nodes \(children of the root-node) have level 0."
  (let ((parent (tree-node->parent node)))
    (if (eq parent (tree-buffer-get-root))
        0
      (1+ (tree-node-indent-level parent)))))

(defun tree-node-new-root ()
  "Creates a new root node.
The root node has always NAME=\"root\", TYPE=-1 and DATA=nil. The root node
will not be displayed. Only the root-node is allowed to have as TYPE -1!"
  (tree-node-new "root" -1 nil))

(defun tree-node-update (node name type data expandable shrink-name)
  "Update NODE with setable datas.
Each of the arguments NAME, SHRINK-NAME, TYPE, DATA and EXPANDABLE can have
the special value 'use-old-value\; this means that this attribute/slot of NODE
will not be updated."
  (unless (eq name 'use-old-value)
    (setf (tree-node->name node) name))
  (unless (eq shrink-name 'use-old-value)
    (setf (tree-node->shrink-name node) shrink-name))
  (unless (eq type 'use-old-value)
    (setf (tree-node->type node) type))
  (unless (eq data 'use-old-value)
    (setf (tree-node->data node) data))
  (unless (eq expandable 'use-old-value)
    (setf (tree-node->expandable node) expandable)))

(defun tree-node-add-children (node children &optional at-beginning)
  "Add new CHILDREN to the already existing children of NODE.
If the optional arg AT_BEGINNING is not nil then the new CHILDREN will be
added to the beginning of the existing children of NODE otherwise to the end
\(default). CHILDREN must be either a single tree-node object or a list of
tree-nodes."
  (let ((c-list (typecase children
                  (tree-node (list children))
                  (list children)
                  (otherwise
                   (error "Children must be either a single tree-node or a list of tree-nodes.")))))
    ;; set NODE as parent of all new CHILDREN
    (dolist (c c-list)
      (setf (tree-node->parent c) node))
    ;; add the new CHILDREN to the existing ones
    (setf (tree-node->children node)
          (if at-beginning
              (append c-list (tree-node->children node))
            (append (tree-node->children node) c-list)))))

(defsubst tree-node-sort-children (node sortfn)
  "Run `sort' for the children of NODE with SORTFN as sorting-function.
SORTFN must be a function acceptable by `sort'. The sorted children-list
become the new children of NODE."
  (setf (tree-node->children node)
        (sort (tree-node->children node) sortfn)))

(defsubst tree-node-remove-child (node child)
  "Removes the CHILD from the childrens of NODE."
  (setf (tree-node->parent child) nil)
  (setf (tree-node->children node)
        (delq child (tree-node->children node))))

(defun tree-node-find-child-by-data (node child-data)
  "Finds the first child with the given CHILD-DATA.
CHILD-DATA will be compared with the data of each children of NODE by calling
`tree-buffer-node-data-equal-p'."
  (catch 'exit
    (dolist (child (tree-node->children node))
      (when (tree-buffer-node-data-equal-p (tree-node->data child)
                                           child-data)
        (throw 'exit child)))))

(defun tree-node-remove-child-by-data (node child-data)
  "Removes the first child with the given CHILD-DATA.
Returns the removed child. CHILD-DATA will be compared with the data of each
children of NODE by calling `tree-buffer-node-data-equal-p'."
  (catch 'exit
    (let ((last-cell nil)
	  (cell (tree-node->children node)))
      (while cell
	(when (tree-buffer-node-data-equal-p (tree-node->data (car cell))
                                             child-data)
	  (if last-cell
	      (setcdr last-cell (cdr cell))
	    (setf (tree-node->children node) (cdr cell)))
	  (setcdr cell nil)
	  (setf (tree-node->parent (car cell)) nil)
	  (throw 'exit cell))
	(setq last-cell cell)
	(setq cell (cdr cell))))))

(defun tree-node-find-child-by-name (node child-name)
  "Return the first child of NODE with name CHILD-NAME."
  (catch 'exit
    (dolist (child (tree-node->children node))
      (when (equal (tree-node->name child) child-name)
        (throw 'exit child)))))

(defun tree-node-search-subtree-by-data (node data)
  "Search the full subtree of NODE for the first \(sub-)node with DATA.
The \"full subtree\" means the NODE itself, its children, their grandchildren
etc. The search is done by a depth-first-search. Data-comparison is performed
with `tree-buffer-node-data-equal-p'."
  (if (tree-buffer-node-data-equal-p data (tree-node->data node))
      node
    (catch 'exit
      (dolist (child (tree-node->children node))
	(let ((n (tree-node-search-subtree-by-data child data)))
	  (when n
	    (throw 'exit n)))))))

;; ------- tree-buffer local variables ----------------------------------

(defvar tree-buffer-root nil
  "The \(not displayed) root-node of each tree-buffer.
The value is buffer-local in each tree-buffer.")

(defvar tree-buffer-displayed-nodes nil
  "Contains all the current visible nodes of current tree-buffer in
top-to-bottom order. This variable is buffer-local in each tree-buffer!")

(defsubst tree-buffer-initialize-displayed-nodes ()
  "Initialize the `tree-buffer-displayed-nodes' with nil."
  (setq tree-buffer-displayed-nodes nil))

(defsubst tree-buffer-number-of-displayed-nodes ()
  "Return the number of current displayed nodes."
  (length tree-buffer-displayed-nodes))

(defsubst tree-buffer-nth-displayed-node (n)
  "Return the N-th displayed node of current tree-buffer.
Counts from 0 whereas the 0-th node is the topmost displayed node."
  (nth n tree-buffer-displayed-nodes))

(defun tree-buffer-find-displayed-node-by-data (node-data &optional start-node)
  "Find the first displayed node in current tree-buffer having data NODA-DATA.
When START-NODE is nil then all currently visible nodes are searched beginning
with the first one otherwise START-NODE is the startpoint for the search.

If the search has success then the found node is returend."
  (catch 'exit
    (let ((node-list (if (or (null start-node)
                             (eq start-node (tree-buffer-get-root)))
                         tree-buffer-displayed-nodes
                       ;; we need that sub-list of tree-buffer-displayed-nodes
                       ;; which has the start-node as first elem. But we can
                       ;; not calling `member' for this search because this
                       ;; can result in a stack-overflow in equal for large
                       ;; node-lists especially with complex-data (e.g.
                       ;; semantic tags). Therefore we use `memq'.
                       (or (tree-buffer-member start-node
                                               tree-buffer-displayed-nodes
                                               'eq)
                           tree-buffer-displayed-nodes))))
      (dolist (node node-list)
        (when (tree-buffer-node-data-equal-p (tree-node->data node) node-data)
          (throw 'exit node))))))

(defun tree-buffer-search-displayed-node-list (search-fcn)
  "Call SEARCH-FCN for each currently visible node in current tree-buffer.
Return the first node for which SEARCH-FCN returns not nil."
  (catch 'exit
    (dolist (node tree-buffer-displayed-nodes)
      (when (funcall search-fcn node)
        (throw 'exit node)))))

(defun tree-buffer-displayed-node-nr (node)
  "Return the number of NODE in the node-sequence of current tree-buffer.
Nodes are compared by `eq'! Number is counted from 0 whereas the topmost
displayed node ha number 0."
  (tree-buffer-position tree-buffer-displayed-nodes node 'eq))

(defun tree-buffer-displayed-node-linenr (node)
  "Return the line-number of NODE in current tree-buffer.
Nodes are compared by `eq'."
  (1+ (tree-buffer-displayed-node-nr node)))

(defsubst tree-buffer-add-to-displayed-nodes (node)
  "Add NODE at the end of the displayed nodes `tree-buffer-displayed-nodes'."
  (setq tree-buffer-displayed-nodes
        (append tree-buffer-displayed-nodes (list node))))

(defsubst tree-buffer-displayed-nodes-remove-nth (n)
  "Remove the N-th node from the displayed nodes `tree-buffer-displayed-nodes'."
  (tree-buffer-remove-elt tree-buffer-displayed-nodes n))

(defsubst tree-buffer-displayed-nodes-remove-node (node)
  "Remove NODE from the displayed nodes `tree-buffer-displayed-nodes'."
  (setq tree-buffer-displayed-nodes
        (delq node tree-buffer-displayed-nodes)))

(defsubst tree-buffer-displayed-nodes-replace-nth (n new-node)
  "Replace the N-th node with NEW-NODE in `tree-buffer-displayed-nodes'.
Return the updated list."
  (tree-buffer-set-elt tree-buffer-displayed-nodes n new-node))

(defun tree-buffer-displayed-nodes-replace-node (node new-node)
  "Replace NODE with NEW-NODE in `tree-buffer-displayed-nodes'.
Return the updated list."
  (let ((memq-list (tree-buffer-member node tree-buffer-displayed-nodes 'eq)))
    (if memq-list
        (setcar memq-list new-node)))
  tree-buffer-displayed-nodes)

(defsubst tree-buffer-set-displayed-nodes (displayed-nodes)
  "Set `tree-buffer-displayed-nodes' to DISPLAYED-NODES.
DISPLAYED-NODES which has to be a list of node-objects. Replaces the old list
of displayed-nodes."
  (setq tree-buffer-displayed-nodes displayed-nodes))

(defsubst tree-buffer-displayed-nodes-copy ()
  "Return a copy of the displayed-nodes-list `tree-buffer-displayed-nodes'.
Only the list-structure is copied not the elements itself."
  (tree-buffer-copy-list tree-buffer-displayed-nodes))

(defsubst tree-buffer-map-displayed-nodes (function)
  "Apply function to each node of `tree-buffer-displayed-nodes'.
Make a list of the results. The result is a list just as long as
`tree-buffer-displayed-nodes'."
  (mapcar (function (lambda (n)
                      (funcall function n)))
          tree-buffer-displayed-nodes))

;; rest of tree-buffer local variables

(defvar tree-buffer-frame nil
  "The frame the tree-buffer lives in.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-key-map nil
  "The local keymap of current tree-buffer.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-highlighted-node-data nil
  "The data of that node which is currently highlighted.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-highlight-overlay nil
  "Overlay \(rsp. extent for XEmacs) used for highlighting current node.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-general-overlay nil
  "Overlay \(rsp. extent for XEmacs) used for displaying the whole content.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-spec nil
  "A Buffer local object of type tree-buffer-spec.")

(defvar tree-buffer-hscroll-number 0
  "Current value of horizontal tree-buffer-scrolling'.
The value is buffer-local in current tree-buffer.")


;; tree-buffer specification

(defstruct (tree-buffer-spec
            (:constructor -tree-buffer-spec-new)
            (:copier nil)
            (:conc-name tree-buffer-spec->))
  (tree-indent nil :read-only t)
  (menu-creator nil :read-only t)
  (menu-titles nil :read-only t)
  (modeline-menu-creator :read-only t)
  (type-facer nil :read-only t)
  (expand-symbol-before-p nil :read-only t)
  (mouse-action-trigger nil :read-only t)
  (is-click-valid-fn nil :read-only t)
  (node-selected-fn nil :read-only t)
  (node-expanded-fn nil :read-only t)
  (node-collapsed-fn nil :read-only t)
  (node-mouse-over-fn nil :read-only t)
  (mouse-highlight-fn nil :read-only t)
  (node-data-equal-fn nil :read-only t)
  (after-update-hook nil :read-only t)
  (maybe-empty-node-types nil :read-only t)
  (leaf-node-types nil :read-only t)
  (general-face nil :read-only t)
  (incr-search-additional-pattern nil :read-only t)
  (incr-search-p nil :read-only t)
  (hor-scroll-step nil :read-only t)
  (default-images-dir nil :read-only t)
  (additional-images-dir nil :read-only t)
  (image-file-prefix nil :read-only t)
  (style nil :read-only t)
  (ascii-guide-face nil :read-only t))

(defun* tree-buffer-spec-new (&key
                              tree-indent
                              menu-creator
                              menu-titles
                              modeline-menu-creator
                              type-facer
                              expand-symbol-before-p
                              mouse-action-trigger
                              is-click-valid-fn
                              node-selected-fn
                              node-expanded-fn
                              node-collapsed-fn
                              node-mouse-over-fn
                              mouse-highlight-fn
                              node-data-equal-fn
                              after-update-hook
                              maybe-empty-node-types
                              leaf-node-types
                              general-face
                              incr-search-additional-pattern
                              incr-search-p
                              hor-scroll-step
                              default-images-dir
                              additional-images-dir
                              image-file-prefix
                              style
                              ascii-guide-face)
  "Creates and returns a new specification object for current tree-buffer.

The arguments are key-arguments of the form :arg-name arg-value, so for
example a call looks like \(tree-buffer-spec-new :menu-creator 'creator...)
The key-arguments can be arranged in any arbitrary order but all of them are
not-optional! The key-arg-name is always a : followed by the lowercase version
of the mentioned argument \(e.g. MENU-CREATOR --> :menu-creator)

See `tree-buffer-create' for a description of the arguments."
  (let ((my-style (tree-buffer-real-style style)))
    (-tree-buffer-spec-new :menu-creator menu-creator
                           :menu-titles menu-titles
                           :modeline-menu-creator modeline-menu-creator
                           :type-facer type-facer
                           :mouse-action-trigger mouse-action-trigger
                           :is-click-valid-fn is-click-valid-fn
                           :node-selected-fn node-selected-fn
                           :node-expanded-fn node-expanded-fn
                           :node-collapsed-fn node-collapsed-fn
                           :node-mouse-over-fn node-mouse-over-fn
                           :mouse-highlight-fn mouse-highlight-fn
                           :node-data-equal-fn node-data-equal-fn
                           :after-update-hook
                           (if (functionp after-update-hook)
                               (list after-update-hook)
                             after-update-hook)
                           :maybe-empty-node-types maybe-empty-node-types
                           :leaf-node-types leaf-node-types
                           :general-face general-face
                           :incr-search-additional-pattern incr-search-additional-pattern
                           :incr-search-p incr-search-p
                           :hor-scroll-step hor-scroll-step
                           :default-images-dir default-images-dir
                           :additional-images-dir additional-images-dir
                           :image-file-prefix image-file-prefix
                           :style my-style
                           :expand-symbol-before-p (if (equal 'image my-style)
                                                       t
                                                     expand-symbol-before-p)
                           :tree-indent
                           (cond ((equal 'image my-style)
                                  tree-buffer-indent-with-images)
                                 (expand-symbol-before-p
                                  (if (< tree-indent
                                         tree-buffer-indent-w/o-images-before-min)
                                      tree-buffer-indent-w/o-images-before-min
                                    tree-indent))
                                 (t ;; after
                                  (if (< tree-indent
                                         tree-buffer-indent-w/o-images-after-min)
                                      tree-buffer-indent-w/o-images-after-min
                                    tree-indent)))
                           :ascii-guide-face ascii-guide-face)))

;; incremental search in a tree-buffer 

(defconst tree-buffer-incr-searchpattern-expand-prefix
  "\\(\\[[^][]+\\] ?\\)?\\[?"
  "The prefix ignores all expand/collapse-buttons: \[+], \[x], rsp. \[-]")

(defvar tree-buffer-incr-searchpattern nil
  "Current search pattern when a inremental search is active.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-last-incr-searchpattern nil
  "Most recent used search-pattern for incremental search.
Used to compared with the value of `tree-buffer-incr-searchpattern'.
The value is buffer-local in current tree-buffer.")

(defvar tree-buffer-incr-searchpattern-indent-prefix nil
  "Prefix-pattern which ignores all not interesting basic stuff of a displayed
tag at incr. search. The following contents of a displayed tag are ignored
by this pattern:
- beginning spaces and guide characters \(|`-)
This prefix is computed by `tree-buffer-gen-searchpattern-indent-prefix'!
The value is buffer-local in current tree-buffer.")

;; --- tree-buffer-local data-storage with get- and set-function --------

(defvar tree-buffer-data-store nil
  "Arbitrary data-storage which is buffer-local for each tree-buffer.
Use `tree-buffer-set-data-store' and `tree-buffer-get-data-store' to set and
get the data.")

(defsubst tree-buffer-set-data-store (data)
  "Set `tree-buffer-data-store' to DATA."
  (setq tree-buffer-data-store data))

(defsubst tree-buffer-get-data-store ()
  "Return the current value of `tree-buffer-data-store'."
  tree-buffer-data-store)

;; ------- image support ------------------------------------------------

(defvar tree-buffer-local-image-cache nil
  "Alist with car is one of the names in
`tree-buffer-tree-image-names' and cdr is an associated image-object.")

(defconst tree-buffer-images-can-be-used
  (and (or (fboundp 'defimage)
           (fboundp 'make-image-specifier))
       (if (fboundp 'display-images-p)
           (display-images-p)
         window-system)))

(defvar tree-buffer-image-properties-emacs
  '(:ascent center :mask (heuristic t))
  "Properties of GNU Emacs images.")

(defvar tree-buffer-image-properties-xemacs
  nil
  "Properties of XEmacs images.")

(defvar tree-buffer-enable-xemacs-image-bug-hack
  tree-buffer-running-xemacs
  "If true then tree-buffer tries to deal best with the XEmacs-bug to display
adjacent images not correctly. Set this to nil if your XEmacs-version has fixed
this bug.")

(defconst tree-buffer-image-formats
  '((xpm ".xpm") (png ".png") (gif ".gif") (jpeg ".jpg" ".jpeg")
    (xbm ".xbm")))

(defconst tree-buffer-expand-symbol-length 3)
(defconst tree-buffer-indent-with-images 3)
(defconst tree-buffer-indent-w/o-images-before-min 3)
(defconst tree-buffer-indent-w/o-images-after-min 2)

(defconst tree-buffer-tree-image-names
  '(("open"      . ((after . "[-]") (before . "[-]")))
    ("close"     . ((after . "[+]") (before . "[+]")))
    ("empty"     . ((after . "[x]") (before . "[x]")))
    ("leaf"      . ((after . "*")   (before . "*")))
    ("guide"     . ((after . "|")   (before . " |")))
    ("no-guide"  . ((after . " ")   (before . "  ")))
    ("end-guide" . ((after . "`")   (before . " `")))
    ("handle"    . ((after . "-")   (before . "-")))
    ("no-handle" . ((after . " ")   (before . " "))))
  "This alist contains all allowed tree-image-names and their corresponding
ascii-representation. Currently allowed names for tree-images and current
ascii-symbols are: open, close, empty, leaf, guide, noguide, end-guide,
handle, no-handle. See the value of this constant for the ascii-symbols
related to the names.")

(if tree-buffer-running-xemacs
    (progn
      (defsubst tree-buffer-create-image (file type)
        "Create an image of type TYPE from FILE. Return the new image."
        (apply 'make-glyph
               `([,type :file ,file
                        ,@tree-buffer-image-properties-xemacs])))
      (defsubst tree-buffer-image-type-available-p (type)
        "Return non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
      (valid-image-instantiator-format-p type)))
  (defsubst tree-buffer-create-image (file type)
    (apply 'create-image
           `(,file ,type nil
                   ,@tree-buffer-image-properties-emacs)))
  (defsubst tree-buffer-image-type-available-p (type)
    "Return non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
    (image-type-available-p type)))

(defun tree-buffer-real-style (&optional style)
  "Return the currently used style of the tree-buffer. If \X)Emacs allows
displaying images then this is simply the value of the slot STYLE of
`tree-buffer-spec'. Otherwise the style 'image is replaced by 'ascii-guides.
If the optional arg STYLE is not nil then this is used instead of the slot
STYLE of `tree-buffer-spec'. Allowed values of STYLE are nil, 'image,
'ascii-guides, ascii-no-guides."
  (let ((my-style (or style (tree-buffer-spec->style tree-buffer-spec))))
    (if tree-buffer-images-can-be-used
        my-style
      (if (equal my-style 'image)
          'ascii-guides
        my-style))))

(defsubst tree-buffer-ascii-symbol-4-image-name (name)
  "Return the ascii-symbol which displays the tree-image NAME. This is done
according to the value of the slot EXPAND-SYMBOL-BEFORE-P of
`tree-buffer-spec'. It always returns a copy of the registered string in
`tree-buffer-tree-image-names'!"
  (let ((sym (if (tree-buffer-spec->expand-symbol-before-p tree-buffer-spec)
                 'before
               'after)))
    ;; Klaus Berndl <klaus.berndl@sdm.de>: If there are performance issues
    ;; concerning the tree-buffer-redisplay then maybe this copy-sequence is
    ;; the reason. But must be profiled! If yes, then an alternative could be
    ;; not to return copies but references and write a function which removes
    ;; all text-properties from the strings in `tree-buffer-tree-image-names'.
    ;; This function has either to be called once or within
    ;; `tree-buffer-create'. But for the moment we use copies.
    ;; Background: Without copies or without removing the text-properties from
    ;; the strings in `tree-buffer-tree-image-names' before using tree-buffers
    ;; we also get some images if we switch from image- to ascii-display
    ;; without restarting emacs.
    (copy-sequence
     (cdr (assoc sym (cdr (assoc name tree-buffer-tree-image-names)))))))


(defun tree-buffer-add-image-icon-maybe (start len str image-icon)
  "Add IMAGE-ICON to STR between START \(incl.) and START+LEN \(excl.). If
IMAGE-ICON is not nil \(which must be an image-object in the sense of
\(X)Emacs) then add this image to STR otherwise do nothing. Normally
IMAGE-ICON should be either nil or an image-object returned by
`tree-buffer-find-image'. Always return STR. If IMAGE-ICON is nil or
`tree-buffer-real-style' returns not 'image then START and LEN are ignored!
If an image is added then two text-properties are added to the full length of
STR: 'tree-buffer-image-start which holds START as value and
'tree-buffer-image-length which holds LEN as value."
  (when (equal 'image (tree-buffer-real-style))
    ;; Regular images (created with `insert-image' are intangible
    ;; which (I suppose) make them more compatible with XEmacs 21.
    ;; Unfortunately, there is a giant pile of code dependent on the
    ;; underlying text.  This means if we leave it tangible, then I
    ;; don't have to change said giant piles of code.
    (when image-icon
      (if tree-buffer-running-xemacs
          (add-text-properties (+ start len) start
                               (list 'end-glyph image-icon
                                     'rear-nonsticky (list 'display)
                                     'invisible t
                                     'detachable t)
                               str)
        (add-text-properties start (+ start len)
                             (list 'display image-icon
                                   'rear-nonsticky (list 'display))
                             str))
      (add-text-properties 0 (length str)
                           (list 'tree-buffer-image-start start
                                 'tree-buffer-image-length len)
                           str)))
  str)

(defsubst tree-buffer-image-cache-get (tree-image-name)
  (cdr (assoc tree-image-name
              tree-buffer-local-image-cache)))

(defsubst tree-buffer-image-cache-put (tree-image-name image)
  (setq tree-buffer-local-image-cache
        (cons (cons tree-image-name image)
              tree-buffer-local-image-cache)))

(defun tree-buffer-find-image (tree-image-name)
  "Return an image-object for the TREE-IMAGE-NAME. The needed image-file with
name \"<prefix><TREE-IMAGE-NAME>.<a supported image-file-extension>\" is first
searched in the dir of slot ADDITIONAL-IMAGES-DIR of `tree-buffer-spec' \(if
not nil) and then - if there is no image found for this name - in the dir of
slot DEFAULT-IMAGES-DIR of `tree-buffer-spec'. <prefix> is the value of the
slot IMAGE-FILE-PREFIX of `tree-buffer-spec'. All found and created
image-objectes will be cached so every image is only created once! Returns the
image-object for TREE-IMAGE-NAME."
  (and (equal 'image (tree-buffer-real-style))
       ;; Klaus Berndl <klaus.berndl@sdm.de>: This comes from the XEmacs-bug
       ;; not able to display adjacent images.
       (or (not tree-buffer-enable-xemacs-image-bug-hack)
           (not (member tree-image-name
                        '("handle" "no-handle"))))
       (or (tree-buffer-image-cache-get tree-image-name)
           (let ((dirs (mapcar 'expand-file-name
                               (if (tree-buffer-spec->additional-images-dir
                                    tree-buffer-spec)
                                   (list (tree-buffer-spec->additional-images-dir
                                          tree-buffer-spec)
                                         (tree-buffer-spec->default-images-dir
                                          tree-buffer-spec))
                                 (list (tree-buffer-spec->default-images-dir
                                        tree-buffer-spec)))))
                 (fmt-specs tree-buffer-image-formats)
                 fmt fmt-exts file file-name image loc-dirs)
             (while (and fmt-specs (not file))
               (setq fmt (car (car fmt-specs))
                     fmt-exts (cdr (car fmt-specs))
                     fmt-specs (cdr fmt-specs))
               (when (tree-buffer-image-type-available-p fmt)
                 (while (and fmt-exts (not file))
                   (setq loc-dirs dirs)
                   (while (and loc-dirs (not file))
                     (setq file-name (concat (car loc-dirs) "/"
                                             (tree-buffer-spec->image-file-prefix
                                              tree-buffer-spec)
                                             tree-image-name
                                             (car fmt-exts)))
                     (when (file-readable-p file-name)
                       (setq file file-name))
                     (setq loc-dirs (cdr loc-dirs)))
                   (setq fmt-exts (cdr fmt-exts)))))
             (when file
               (setq image (tree-buffer-create-image file fmt))
               (tree-buffer-image-cache-put tree-image-name
                                            image)
               image)))))

;; ------ tree-buffer global variables -----------------------------------

(defvar tree-buffers nil)

(defvar tree-buffer-syntax-table nil
  "Syntax-table used in a tree-buffer.")

(if tree-buffer-syntax-table
    nil
  (setq tree-buffer-syntax-table (make-syntax-table))
  ;; turn off paren matching around here.
  (modify-syntax-entry ?\' " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\" " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\( " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\) " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\{ " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\} " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\[ " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\] " " tree-buffer-syntax-table))

;; ------- utilities --------------------------------------------------

(defun tree-buffer-get-node-name-start-column (node)
  "Returns the buffer column where the name of the node starts."
  (+ (tree-node-indentlength node)
     (if (and (tree-buffer-spec->expand-symbol-before-p
               tree-buffer-spec)
              (or (tree-node->expandable node)
                  (member (tree-node->type node)
                          (tree-buffer-spec->maybe-empty-node-types
                           tree-buffer-spec))))
         (if (or tree-buffer-enable-xemacs-image-bug-hack
                 (not (equal 'image (tree-buffer-real-style))))
             4 3)
       0)
     (if (and (tree-buffer-spec->expand-symbol-before-p
               tree-buffer-spec)
              (not (tree-node->expandable node))
              (member (tree-node->type node)
                      (tree-buffer-spec->leaf-node-types tree-buffer-spec)))
         (if (or tree-buffer-enable-xemacs-image-bug-hack
                 (not (equal 'image (tree-buffer-real-style))))
             2 1)
       0)))
     

(defun tree-buffer-get-node-name-start-point (node)
  "Returns the buffer point where the name of the NODE starts."
  (let ((linenr (tree-buffer-displayed-node-linenr node)))
    (tree-buffer-debug-error "tree-buffer-get-node-name-start-point: Cur-buf: %s, linenr: %d"
                             (current-buffer) linenr)
    (when linenr
      (save-excursion
        (goto-line linenr)
        (beginning-of-line)
        (+ (point) (tree-buffer-get-node-name-start-column node))))))

(defun tree-buffer-get-node-name-end-point (node)
  "Returns the buffer point where the name of the NODE ends."
  (tree-buffer-debug-error "tree-buffer-get-node-name-end-point: Cur-buf: %s"
                           (current-buffer))
  (+ (tree-buffer-get-node-name-start-point node)
     (length (tree-node->displayed-name node))))

(defun tree-buffer-point-at-expand-symbol-p (node p)
  "Return not nil if point P is located at the expand-symbol of NODE."
  (tree-buffer-debug-error "tree-buffer-point-at-expand-symbol-p: Cur-buf: %s, p: %d, exp-sym-before: %s"
                           (current-buffer)
                           p (tree-buffer-spec->expand-symbol-before-p tree-buffer-spec))
  (when (or (tree-node->expandable node)
            ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe it would be
            ;; better to handle such nodes as if point can not stay at the
            ;; empty-symbol... 
            (member (tree-node->type node)
                    (tree-buffer-spec->maybe-empty-node-types tree-buffer-spec)))
    (let ((start-point (tree-buffer-get-node-name-start-point node))
          (end-point (tree-buffer-get-node-name-end-point node)))
      (if (tree-buffer-spec->expand-symbol-before-p tree-buffer-spec)
          (and (>= p (- start-point 4))
               (< p (1- start-point)))
        (and (> p end-point)
             (<= p (+ end-point 4)))))))

(defun tree-buffer-point-at-node-name-p (node p)
  "Return not nil if point P is located at the displayed-name of NODE."
  (tree-buffer-debug-error "tree-buffer-point-at-node-name-p: Cur-buf: %s, p: %d"
                           (current-buffer) p)
  (and (>= p (tree-buffer-get-node-name-start-point node))
       (< p (tree-buffer-get-node-name-end-point node))))

(defun tree-buffer-get-node-at-point (&optional p)
  "Returns the node at point P. If p is nil the current point is used."
  (save-excursion
    (if p (goto-char p))
    (tree-buffer-nth-displayed-node (1- (tree-buffer-current-line)))))

(defun tree-buffer-select (mouse-button shift-pressed control-pressed meta-pressed)
  "If the callback-function in slot IS-CLICK-VALID-FN of `tree-buffer-spec'
returns nil then nothing is done. Otherwise: If either the MOUSE-BUTTON is 0
or point is as the node-name then the callback-function in slot
NODE-SELECTED-FN is called with the needed arguments \(see
`tree-buffer-create'). If point is at the expand/collape-button depending of
the expansion-state either the callback in slot NODE-EXPANDED-FN or
NODE-COLLAPSED-FN is called \(for parameters see again `tree-buffer-create').
None of these callbacks must modify the slot EXPANDED of the passed node
because this is done automatically by this function."
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (when (and (tree-buffer-spec->is-click-valid-fn tree-buffer-spec)
               (funcall (tree-buffer-spec->is-click-valid-fn tree-buffer-spec)
                        mouse-button shift-pressed control-pressed meta-pressed
                        (buffer-name)))
      (tree-buffer-debug-error "tree-buffer-select-1: Cur-buf: %s"
                               (current-buffer))
      (let ((node (tree-buffer-get-node-at-point)))
        (when node
          (tree-buffer-debug-error "tree-buffer-select-2: Cur-buf: %s"
                                   (current-buffer))
          ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Is this the right place
          ;; for this? probably it can cause some erros...... Yep - it causes
          ;; serious XEmacs-sideeffects: clicking into tree-buffer doesn't
          ;; work anymore when doing this during an active isearch! Seems that
          ;; isearch-exit switches the current buffer so the buffer after the
          ;; isearch-exit is not the same as before!! So we comment this out!!
;;           (ignore-errors
;;             (let ((search-nonincremental-instead nil))
;;               (isearch-exit)))
          (tree-buffer-debug-error "tree-buffer-select-3: Cur-buf: %s"
                                   (current-buffer))
          (cond ((or (= mouse-button 0)
                     (tree-buffer-point-at-node-name-p node (point)))
                 (setq tree-buffer-incr-searchpattern "")
                 (when (tree-buffer-spec->node-selected-fn tree-buffer-spec)
                   (funcall (tree-buffer-spec->node-selected-fn tree-buffer-spec)
                            node mouse-button shift-pressed control-pressed meta-pressed
                            (buffer-name))))
                ((tree-buffer-point-at-expand-symbol-p node (point))
                 (when (and (not (tree-node->expanded node))
                            (tree-buffer-spec->node-expanded-fn tree-buffer-spec))
                   (funcall (tree-buffer-spec->node-expanded-fn tree-buffer-spec)
                            node mouse-button
                            shift-pressed control-pressed meta-pressed
                            (buffer-name)))
                 (when (tree-node->expandable node)
                   (when (and (tree-node->expanded node)
                              (tree-buffer-spec->node-collapsed-fn tree-buffer-spec))
                     (funcall (tree-buffer-spec->node-collapsed-fn tree-buffer-spec)
                              node mouse-button
                              shift-pressed control-pressed meta-pressed
                              (buffer-name)))
                   (tree-node-toggle-expanded node))
                 ;; Update the tree-buffer with optimized display of NODE
                 (tree-buffer-update node))))))))


(defun tree-buffer-node-data-equal-p (node-data-1 node-data-2)
  "Calls the function stored in slot NODE-DATA-EQUAL-FN of `tree-buffer-spec'
to test NODE-DATA-1 and NODE-DATA-2 for equality."
  (and node-data-1 node-data-2
       ;; if this comparison-function runs into an error we handle this as
       ;; non-equality!
       (ignore-errors
         (funcall (tree-buffer-spec->node-data-equal-fn tree-buffer-spec)
                  node-data-1 node-data-2))))

(defun tree-buffer-get-node-facer (node)
  (let ((facer (cdr (assoc (tree-node->type node)
                           (tree-buffer-spec->type-facer tree-buffer-spec)))))
    (if facer
        facer
      nil)))

(defun tree-buffer-pos-hor-visible-p (pos window)
  "Return non nil if POS is horizontal visible in WINDOW otherwise nil."
  (save-excursion
    (goto-char pos)
    (and (>= (- (current-column) (window-hscroll window)) 0)
         (< (- (current-column) (window-hscroll window))
            (window-width window)))))

(defun tree-buffer-hscroll (amount)
  (ignore-errors
    (let ((current-prefix-arg amount))
      (call-interactively 'scroll-left))))

;; Stolen from dframe.el from the speedbar-library
;; XEmacs: this can be implemented using modeline key-maps, but there
;; is no use, as we have horizontal scrollbar (as the docstring
;; hints.)
(defun tree-buffer-mouse-hscroll (e)
  "Read a mouse event E from the mode line and scroll horizontally.
If the mouse is being clicked on the far left, or far right of the
mode-line.  This is only useful for non-XEmacs"
  (interactive "e")
  (let* ((x-point (car (nth 2 (car (cdr e)))))
	 (pixels-per-10-col (/ (* 10 (frame-pixel-width))
			       (frame-width)))
	 (click-col (1+ (/ (* 10 x-point) pixels-per-10-col)))
	 )
    (cond ((< click-col 3)
	   (tree-buffer-hscroll (- (tree-buffer-spec->hor-scroll-step tree-buffer-spec))))
	  ((> click-col (- (window-width) 4))
	   (tree-buffer-hscroll (tree-buffer-spec->hor-scroll-step tree-buffer-spec)))
          (t (tree-buffer-nolog-message
	      "Click on the edge of the modeline to scroll left/right")))
    ))

(defun tree-buffer-count-subnodes-to-display (node)
  "Returns the number of ALL subnodes of NODE which will currently be displayed
if NODE is expanded, means the number of all the children of NODE \(if NODE is
expanded) plus recursive the number of the children of each expanded child.
Example:
\[-] NODE
    \[+] child 1
    \[-] child 2
        \[+] child 2.1
        \[-] child 2.2
            \[+] child 2.2.1
            \[+] child 2.2.2
        \[+] child 2.3
    \[-] child 3
        \[+] child 3.1
    \[+] child 4
The result for NODE here is 10"
  (let ((result 0))
    (when (and (tree-node->expandable node)
               (tree-node->expanded node))
      (setq result (+ result (length (tree-node->children node))))
      (dolist (child (tree-node->children node))
        (setq result (+ result (tree-buffer-count-subnodes-to-display child)))))
    result))

(defun tree-buffer-recenter (node window)
  "If NODE is not visible then first recenter the window WINDOW so NODE is
best visible, means NODE is displayed in the middle of the window if possible.
If NODE is expanded then recenter the WINDOW so as much as possible subnodes
of NODE will be visible. If NODE is not expandable then WINDOW is always
displayed without empty-lines at the end, means WINDOW is always best filled."
  (let* ((node-points (save-excursion
                        (goto-line (tree-buffer-displayed-node-linenr node))
                        (cons (tree-buffer-line-beginning-pos)
                              (tree-buffer-line-end-pos))))
         (node-point (car node-points))
         (point-lines-before (count-lines (point-min) node-point))
         (point-lines-after (1- (count-lines node-point (point-max)))))
    ;; first make point best visible, means display node in the middle of the
    ;; window if possible (if there are enough lines before/after the node).
    (when (not (pos-visible-in-window-p node-point window))
      (if (< node-point (window-start window))
          (set-window-start
           window
           (save-excursion
             (goto-char node-point)
             (forward-line
              (* -1 (min point-lines-before
                         (/ (tree-buffer-window-display-height window) 2))))
             (tree-buffer-line-beginning-pos)))
        (set-window-start window
                          (save-excursion
                            (goto-char (window-start window))
                            (forward-line
                             (- (+ 1
                                   (count-lines (window-start window) node-point)
                                   (min point-lines-after
                                        (/ (tree-buffer-window-display-height window) 2)))
                                (tree-buffer-window-display-height window)))
                            (tree-buffer-line-beginning-pos)))
        ))
    ;; now optimize the window display for displaying as much possible
    ;; subnodes of node.
    (if (tree-node->expanded node)
        (let ((exp-node-children-count (1+ (tree-buffer-count-subnodes-to-display node)))
              (point-window-line (count-lines (window-start window) node-point)))
          ;; if the current node is not already displayed in the first line of
