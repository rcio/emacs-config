;;; wisent.el --- GNU Bison for Emacs - Runtime

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 30 January 2002
;; Keywords: syntax
;; X-RCS: $Id: wisent.el,v 1.37 2007/02/19 13:39:11 zappo Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Parser engine and runtime of Wisent.
;;
;; Wisent (the European Bison ;-) is an Elisp implementation of the
;; GNU Compiler Compiler Bison.  The Elisp code is a port of the C
;; code of GNU Bison 1.28 & 1.31.
;;
;; For more details on the basic concepts for understanding Wisent,
;; read the Bison manual ;)
;;
;; For more details on Wisent itself read the Wisent manual.

;;; History:
;;

;;; Code:
(provide 'wisent)

(defgroup wisent nil
  "
           /\\_.-^^^-._/\\     The GNU
           \\_         _/
            (     `o  `      (European ;-) Bison
             \\      ` /
