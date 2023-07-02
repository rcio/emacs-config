;;; ecb-layout.el --- layout for ECB

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

;; $Id: ecb-layout.el,v 1.253 2005/06/20 14:34:20 berndl Exp $

;;; Commentary:
;;
;; Contains functions for settings the ECB layout.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

;; This file has been re-implemented by Klaus Berndl <klaus.berndl@sdm.de>.
;; What has been done:
;; Completely rewritten the layout mechanism for better customizing, adding
;; new layouts, better redrawing and more straightforward code.
;; 1. Now all user-layouting is done by customizing the new option
;;    `ecb-layout-name' or by the command `ecb-change-layout'. The function
;;    `ecb-redraw-layout' (formally known as 'ecb-set-layout) can still be
;;    called interactively. But it just redraws the layout specified in
;;    `ecb-layout-name'. All changes to the layout must be made by customizing
;;    this new option. Please read the very detailed comment of
;;    `ecb-layout-name'!
;; 2. Adding new layouts is now much easier and more straightforward: We have
;;    now a main core-layout function (`ecb-redraw-layout-full') which is the
;;    "environment" for the specific "layout-functions". The core function
;;    does first some layout independent actions, then calls the
;;    "layout-function" for the name which has been set in `ecb-layout-name'
;;    and after that it does some layout independent actions again (see the
;;    comments in this function). See the macro `ecb-layout-define' and the
;;    command `ecb-create-new-layout'!
;;
;; Background-info: For each layout-type (ecb-windows left, right, top and
;; left-right) there is one function:
;; 'ecb-delete-window-ecb-windows-[left|right|top|leftright]'.
;; These functions follow these guide-lines:
;; - Preconditions for these functions:
;;   + the edit-area is splitted - at least in two edit-windows
;;   + The function gets two arguments: The window to delete (if nil then the
;;     current window has to be deleted) and the list of all current
;;     edit-windows.
;;   + These functions are always(!) called with deactivated advices of
;;     `delete-window' function.
;;   + These functions can only use `delete-window' of the set of maybe
;;     adviced window functions, because of a bug in advice.el only one
