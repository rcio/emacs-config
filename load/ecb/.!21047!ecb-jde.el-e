;;; ecb-jde.el --- ECB JDE integrations

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2003

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

;; $Id: ecb-jde.el,v 1.15 2005/04/19 15:18:59 berndl Exp $

;;; Commentary:
;;
;; Contains code for JDEE integrations into ECB or vice versa
;; JDEE is available at http://jdee.sunsite.dk/

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun jde-show-class-source)
(silentcomp-defvar jde-open-class-at-point-find-file-function)
(silentcomp-defun jde-open-functions-exist)
(silentcomp-defun jde-parse-java-variable-at-point)
(silentcomp-defun jde-open-get-class-to-open)
(silentcomp-defun jde-open-get-path-prefix-list)
(silentcomp-defun jde-open-find-java-file-name)
(silentcomp-defun jde-gen-class-buffer)
(silentcomp-defvar jde-sourcepath)

(require 'ecb-util)
(require 'ecb-layout)
(require 'ecb-file-browser)
(require 'ecb-method-browser)


(defgroup ecb-jde-integration nil
  "Settings for the JDEE-integration in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-jde-")


(defcustom ecb-jde-set-directories-buffer-to-jde-sourcepath nil
  "*THIS FEATURE IS NOT YET FINISHED"
  :group 'ecb-jde-integration
  :type '(radio (const :tag "No" :value nil)
                (const :tag "Add" :value add)
                (const :tag "Replace" :value replace)))

(defun ecb-jde-display-class-at-point ()
  "Displays in the ECB-methods-buffer contents of class under point.
This means displays the contents \(methods, attributes etc...) of the class
which contains the definition of the \"thing\" under point \(this can be a
variablename, classname, methodname, attributename). This function needs the
same requirements to work as the method-completion feature of JDE \(see
`jde-complete-at-point')!. The source-file is searched first in
`jde-sourcepath', then in `jde-global-classpath', then in $CLASSPATH, then in
current-directory.

Works only for classes where the source-code \(i.e. the *.java-file) is
available."
  (interactive)
  (when (and ecb-minor-mode
             (ecb-point-in-edit-window)
             (equal major-mode 'jde-mode))
    (if (jde-open-functions-exist)
        (let* (
               (thing-of-interest (ecb-thing-at-point 'symbol))
               (pair (save-excursion (ecb-end-of-thing 'symbol)
                                     (jde-parse-java-variable-at-point)))
               (class-to-open (jde-open-get-class-to-open
                               pair thing-of-interest))
               (source-path-prefix-list (jde-open-get-path-prefix-list)) 
               (java-file-name nil)
               )
          (if (and class-to-open (stringp class-to-open))
              (progn
                (setq java-file-name (jde-open-find-java-file-name
                                      class-to-open source-path-prefix-list))
                (if (not java-file-name)
                    (ecb-error "Can not find the sourcecode-file for \"%s\""
                               thing-of-interest)

                  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: 
                  ;; The following two lines of code are the only difference
                  ;; between this function and `jde-open-class-at-point'.
                  ;; Therefore it would be nice if the whole stuff necessary
                  ;; for finding the source-file of `thing-of-interest' would
                  ;; be extracted in an own function of JDE.
                  
