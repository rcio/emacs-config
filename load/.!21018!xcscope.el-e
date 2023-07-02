; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xcscope.el
; RCS:          $RCSfile: xcscope.el,v $ $Revision: 1.14 $ $Date: 2002/04/10 16:59:00 $ $Author: darrylo $
; Description:  cscope interface for (X)Emacs
; Author:       Darryl Okahata
; Created:      Wed Apr 19 17:03:38 2000
; Modified:     Thu Apr  4 17:22:22 2002 (Darryl Okahata) darrylo@soco.agilent.com
; Language:     Emacs-Lisp
; Package:      N/A
; Status:       Experimental
;
; (C) Copyright 2000, 2001, 2002, Darryl Okahata <darrylo@sonic.net>,
;     all rights reserved.
; GNU Emacs enhancements (C) Copyright 2001,
;         Triet H. Lai <thlai@mail.usyd.edu.au>
; Fuzzy matching and navigation code (C) Copyright 2001,
;         Steven Elliott <selliott4@austin.rr.com>
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALPHA VERSION 0.96
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is a cscope interface for (X)Emacs.
;; It currently runs under Unix only.
;;
;; Using cscope, you can easily search for where symbols are used and defined.
;; Cscope is designed to answer questions like:
;;
;;         Where is this variable used?
;;         What is the value of this preprocessor symbol?
;;         Where is this function in the source files?
;;         What functions call this function?
;;         What functions are called by this function?
;;         Where does the message "out of space" come from?
;;         Where is this source file in the directory structure?
;;         What files include this header file?
;;
;; Send comments to one of:     darrylo@soco.agilent.com
;;                              darryl_okahata@agilent.com
;;                              darrylo@sonic.net
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ***** INSTALLATION *****
;;
;; * NOTE: this interface currently runs under Unix only.
;;
;; This module needs a shell script called "cscope-indexer", which
;; should have been supplied along with this emacs-lisp file.  The
;; purpose of "cscope-indexer" is to create and optionally maintain
;; the cscope databases.  If all of your source files are in one
;; directory, you don't need this script; it's very nice to have,
;; though, as it handles recursive subdirectory indexing, and can be
;; used in a nightly or weekly cron job to index very large source
;; repositories.  See the beginning of the file, "cscope-indexer", for
;; usage information.
;;
;; Installation steps:
;;
;; 0. (It is, of course, assumed that cscope is already properly
;;    installed on the current system.)
;;
;; 1. Install the "cscope-indexer" script into some convenient
;;    directory in $PATH.  The only real constraint is that (X)Emacs
;;    must be able to find and execute it.  You may also have to edit
;;    the value of PATH in the script, although this is unlikely; the
;;    majority of people should be able to use the script, "as-is".
;;
;; 2. Make sure that the "cscope-indexer" script is executable.  In
;;    particular, if you had to ftp this file, it is probably no
;;    longer executable.
;;
;; 3. Put this emacs-lisp file somewhere where (X)Emacs can find it.  It
;;    basically has to be in some directory listed in "load-path".
;;
;; 4. Edit your ~/.emacs file to add the line:
;;
;;      (require 'xcscope)
;;
;; 5. If you intend to use xcscope.el often you can optionally edit your
;;    ~/.emacs file to add keybindings that reduce the number of keystrokes
;;    required.  For example, the following will add "C-f#" keybindings, which
;;    are easier to type than the usual "C-c s" prefixed keybindings.  Note
;;    that specifying "global-map" instead of "cscope:map" makes the
;;    keybindings available in all buffers:
;;
;;	(define-key global-map [(control f3)]  'cscope-set-initial-directory)
;;	(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
;;	(define-key global-map [(control f5)]  'cscope-find-this-symbol)
;;	(define-key global-map [(control f6)]  'cscope-find-global-definition)
;;	(define-key global-map [(control f7)]
;;	  'cscope-find-global-definition-no-prompting)
;;	(define-key global-map [(control f8)]  'cscope-pop-mark)
;;	(define-key global-map [(control f9)]  'cscope-next-symbol)
;;	(define-key global-map [(control f10)] 'cscope-next-file)
;;	(define-key global-map [(control f11)] 'cscope-prev-symbol)
;;	(define-key global-map [(control f12)] 'cscope-prev-file)
;;      (define-key global-map [(meta f9)]  'cscope-display-buffer)
;;      (defin-ekey global-map [(meta f10)] 'cscope-display-buffer-toggle)
;;
;; 6. Restart (X)Emacs.  That's it.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ***** USING THIS MODULE *****
;;
;; * Basic usage:
;;
;; If all of your C/C++/lex/yacc source files are in the same
;; directory, you can just start using this module.  If your files are
;; spread out over multiple directories, see "Advanced usage", below.
;;
;; Just edit a source file, and use the pull-down or pop-up (button 3)
;; menus to select one of:
;;
;;         Find symbol
;;         Find global definition
;;         Find called functions
;;         Find functions calling a function
;;         Find text string
;;         Find egrep pattern
;;         Find a file
;;         Find files #including a file
;;
;; The cscope database will be automatically created in the same
;; directory as the source files (assuming that you've never used
;; cscope before), and a buffer will pop-up displaying the results.
;; You can then use button 2 (the middle button) on the mouse to edit
;; the selected file, or you can move the text cursor over a selection
;; and press [Enter].
;;
;; Hopefully, the interface should be fairly intuitive.
;;
;;
;; * Locating the cscope databases:
;;
;; This module will first use the variable, `cscope-database-regexps',
;; to search for a suitable database directory.  If a database location
;; cannot be found using this variable then a search is begun at the
;; variable, `cscope-initial-directory', if set, or the current
;; directory otherwise.  If the directory is not a cscope database
;; directory then the directory's parent, parent's parent, etc. is
;; searched until a cscope database directory is found, or the root
;; directory is reached.  If the root directory is reached, the current
;; directory will be used.
;;
;; A cscope database directory is one in which EITHER a cscope database
;; file (e.g., "cscope.out") OR a cscope file list (e.g.,
;; "cscope.files") exists.  If only "cscope.files" exists, the
;; corresponding "cscope.out" will be automatically created by cscope
;; when a search is done.  By default, the cscope database file is called
;; "cscope.out", but this can be changed (on a global basis) via the
;; variable, `cscope-database-file'.  There is limited support for cscope
;; databases that are named differently than that given by
;; `cscope-database-file', using the variable, `cscope-database-regexps'.
;;
;; Note that the variable, `cscope-database-regexps', is generally not
;; needed, as the normal hierarchical database search is sufficient
;; for placing and/or locating the cscope databases.  However, there
;; may be cases where it makes sense to place the cscope databases
;; away from where the source files are kept; in this case, this
;; variable is used to determine the mapping.  One use for this
;; variable is when you want to share the database file with other
;; users; in this case, the database may be located in a directory
;; separate from the source files.  
;;
;; Setting the variable, `cscope-initial-directory', is useful when a
;; search is to be expanded by specifying a cscope database directory
;; that is a parent of the directory that this module would otherwise
;; use.  For example, consider a project that contains the following
;; cscope database directories:
;;
;;     /users/jdoe/sources
;;     /users/jdoe/sources/proj1
;;     /users/jdoe/sources/proj2
;;
;; If a search is initiated from a .c file in /users/jdoe/sources/proj1
;; then (assuming the variable, `cscope-database-regexps', is not set)
;; /users/jdoe/sources/proj1 will be used as the cscope data base directory.
;; Only matches in files in /users/jdoe/sources/proj1 will be found.  This
;; can be remedied by typing "C-c s a" and then "M-del" to remove single
;; path element in order to use a cscope database directory of
;; /users/jdoe/sources.  Normal searching can be restored by typing "C-c s A".
;;
;;
;; * Keybindings:
;;
;; All keybindings use the "C-c s" prefix, but are usable only while
;; editing a source file, or in the cscope results buffer:
;;
;;      C-c s s         Find symbol.
;;      C-c s d         Find global definition.
;;      C-c s g         Find global definition (alternate binding).
;;      C-c s G         Find global definition without prompting.
;;      C-c s c         Find functions calling a function.
;;      C-c s C         Find called functions (list functions called
;;                      from a function).
;;      C-c s t         Find text string.
;;      C-c s e         Find egrep pattern.
;;      C-c s f         Find a file.
;;      C-c s i         Find files #including a file.
;;
;; These pertain to navigation through the search results:
;;
;;      C-c s b         Display *cscope* buffer.
;;      C-c s B         Auto display *cscope* buffer toggle.
;;      C-c s n         Next symbol.
;;      C-c s N         Next file.
;;      C-c s p         Previous symbol.
;;      C-c s P         Previous file.
;;      C-c s u         Pop mark.
;;
;; These pertain to setting and unsetting the variable,
;; `cscope-initial-directory', (location searched for the cscope database
;;  directory):
;;
;;      C-c s a         Set initial directory.
;;      C-c s A         Unset initial directory.
;;
;; These pertain to cscope database maintenance:
;;
;;      C-c s L         Create list of files to index.
;;      C-c s I         Create list and index.
;;      C-c s E         Edit list of files to index.
;;      C-c s W         Locate this buffer's cscope directory
;;                      ("W" --> "where").
;;      C-c s S         Locate this buffer's cscope directory.
;;                      (alternate binding: "S" --> "show").
;;      C-c s T         Locate this buffer's cscope directory.
;;                      (alternate binding: "T" --> "tell").
;;      C-c s D         Dired this buffer's directory.
;;
;;
;; * Advanced usage:
;;
;; If the source files are spread out over multiple directories,
;; you've got a few choices:
;;
;; [ NOTE: you will need to have the script, "cscope-indexer",
;;   properly installed in order for the following to work.  ]
;;
;; 1. If all of the directories exist below a common directory
;;    (without any extraneous, unrelated subdirectories), you can tell
;;    this module to place the cscope database into the top-level,
;;    common directory.  This assumes that you do not have any cscope
;;    databases in any of the subdirectories.  If you do, you should
;;    delete them; otherwise, they will take precedence over the
;;    top-level database.
;;
;;    If you do have cscope databases in any subdirectory, the
;;    following instructions may not work right.
;;
;;    It's pretty easy to tell this module to use a top-level, common
;;    directory:
;;
;;    a. Make sure that the menu pick, "Cscope/Index recursively", is
;;       checked (the default value).
;;
;;    b. Select the menu pick, "Cscope/Create list and index", and
;;       specify the top-level directory.  This will run the script,
;;       "cscope-indexer", in the background, so you can do other
;;       things if indexing takes a long time.  A list of files to
;;       index will be created in "cscope.files", and the cscope
;;       database will be created in "cscope.out".
;;
;;    Once this has been done, you can then use the menu picks
;;    (described in "Basic usage", above) to search for symbols.
;;
;;    Note, however, that, if you add or delete source files, you'll
;;    have to either rebuild the database using the above procedure,
;;    or edit the file, "cscope.files" to add/delete the names of the
;;    source files.  To edit this file, you can use the menu pick,
;;    "Cscope/Edit list of files to index".
;;
;;
;; 2. If most of the files exist below a common directory, but a few
;;    are outside, you can use the menu pick, "Cscope/Create list of
;;    files to index", and specify the top-level directory.  Make sure
;;    that "Cscope/Index recursively", is checked before you do so,
;;    though.  You can then edit the list of files to index using the
;;    menu pick, "Cscope/Edit list of files to index".  Just edit the
;;    list to include any additional source files not already listed.
;;
;;    Once you've created, edited, and saved the list, you can then
;;    use the menu picks described under "Basic usage", above, to
;;    search for symbols.  The first time you search, you will have to
;;    wait a while for cscope to fully index the source files, though.
;;    If you have a lot of source files, you may want to manually run
;;    cscope to build the database:
;;
;;            cd top-level-directory    # or wherever
;;            rm -f cscope.out          # not always necessary
;;            cscope -b
;;
;;
;; 3. If the source files are scattered in many different, unrelated
;;    places, you'll have to manually create cscope.files and put a
;;    list of all pathnames into it.  Then build the database using:
;;
;;            cd some-directory         # wherever cscope.files exists
;;            rm -f cscope.out          # not always necessary
;;            cscope -b
;;
;;    Next, read the documentation for the variable,
;;    "cscope-database-regexps", and set it appropriately, such that
;;    the above-created cscope database will be referenced when you
;;    edit a related source file.
;;
;;    Once this has been done, you can then use the menu picks
;;    described under "Basic usage", above, to search for symbols.
;;
;;
;; * Interesting configuration variables:
;;
;; "cscope-truncate-lines"
;;      This is the value of `truncate-lines' to use in cscope
;;      buffers; the default is the current setting of
;;      `truncate-lines'.  This variable exists because it can be
;;      easier to read cscope buffers with truncated lines, while
;;      other buffers do not have truncated lines.
;;
;; "cscope-use-relative-paths"
;;      If non-nil, use relative paths when creating the list of files
;;      to index.  The path is relative to the directory in which the
;;      cscope database will be created.  If nil, absolute paths will
;;      be used.  Absolute paths are good if you plan on moving the
;;      database to some other directory (if you do so, you'll
;;      probably also have to modify `cscope-database-regexps').
;;      Absolute paths may also be good if you share the database file
;;      with other users (you'll probably want to specify some
;;      automounted network path for this).
;;
;; "cscope-index-recursively"
;;      If non-nil, index files in the current directory and all
;;      subdirectories.  If nil, only files in the current directory
;;      are indexed.  This variable is only used when creating the
;;      list of files to index, or when creating the list of files and
;;      the corresponding cscope database.
;;
;; "cscope-name-line-width"
;;      The width of the combined "function name:line number" field in
;;      the cscope results buffer.  If negative, the field is
;;      left-justified.
;;
;; "cscope-do-not-update-database"
;;      If non-nil, never check and/or update the cscope database when
;;      searching.  Beware of setting this to non-nil, as this will
;;      disable automatic database creation, updating, and
;;      maintenance.
;;
;; "cscope-display-cscope-buffer" 
;;      If non-nil, display the *cscope* buffer after each search
;;      (default).  This variable can be set in order to reduce the
;;      number of keystrokes required to navigate through the matches.
;;
;; "cscope-database-regexps"
;; 	List to force directory-to-cscope-database mappings.
;; 	This is a list of `(REGEXP DBLIST [ DBLIST ... ])', where:
;;
;; 	REGEXP is a regular expression matched against the current buffer's
;; 	current directory.  The current buffer is typically some source file,
;; 	and you're probably searching for some symbol in or related to this
;; 	file.  Basically, this regexp is used to relate the current directory
;; 	to a cscope database.  You need to start REGEXP with "^" if you want
;; 	to match from the beginning of the current directory.
;;
;; 	DBLIST is a list that contains one or more of:
;;
;; 	    ( DBDIR )
;; 	    ( DBDIR ( OPTIONS ) )
;; 	    ( t )
;; 	    t
;;
;; 	Here, DBDIR is a directory (or a file) that contains a cscope
;; 	database.  If DBDIR is a directory, then it is expected that the
;; 	cscope database, if present, has the filename given by the variable,
;; 	`cscope-database-file'; if DBDIR is a file, then DBDIR is the path
;; 	name to a cscope database file (which does not have to be the same as
;; 	that given by `cscope-database-file').  If only DBDIR is specified,
;; 	then that cscope database will be searched without any additional
;; 	cscope command-line options.  If OPTIONS is given, then OPTIONS is a
;; 	list of strings, where each string is a separate cscope command-line
;; 	option.
;;
;; 	In the case of "( t )", this specifies that the search is to use the
;; 	normal hierarchical database search.  This option is used to
;; 	explicitly search using the hierarchical database search either before
;; 	or after other cscope database directories.
;;
;; 	If "t" is specified (not inside a list), this tells the searching
;; 	mechanism to stop searching if a match has been found (at the point
;; 	where "t" is encountered).  This is useful for those projects that
;; 	consist of many subprojects.  You can specify the most-used
;; 	subprojects first, followed by a "t", and then followed by a master
;; 	cscope database directory that covers all subprojects.  This will
;; 	cause the most-used subprojects to be searched first (hopefully
;; 	quickly), and the search will then stop if a match was found.  If not,
;; 	the search will continue using the master cscope database directory.
;;
;; 	Here, `cscope-database-regexps' is generally not used, as the normal
;; 	hierarchical database search is sufficient for placing and/or locating
;; 	the cscope databases.  However, there may be cases where it makes
;; 	sense to place the cscope databases away from where the source files
;; 	are kept; in this case, this variable is used to determine the
;; 	mapping.
;;
;; 	This module searches for the cscope databases by first using this
;; 	variable; if a database location cannot be found using this variable,
;; 	then the current directory is searched, then the parent, then the
;; 	parent's parent, until a cscope database directory is found, or the
;; 	root directory is reached.  If the root directory is reached, the
;; 	current directory will be used.
;;
;; 	A cscope database directory is one in which EITHER a cscope database
;; 	file (e.g., "cscope.out") OR a cscope file list (e.g.,
;; 	"cscope.files") exists.  If only "cscope.files" exists, the
;; 	corresponding "cscope.out" will be automatically created by cscope
;; 	when a search is done.  By default, the cscope database file is called
;; 	"cscope.out", but this can be changed (on a global basis) via the
;; 	variable, `cscope-database-file'.  There is limited support for cscope
;; 	databases that are named differently than that given by
;; 	`cscope-database-file', using the variable, `cscope-database-regexps'.
;;
;; 	Here is an example of `cscope-database-regexps':
;;
;;		(setq cscope-database-regexps
;;		      '(
;;			( "^/users/jdoe/sources/proj1"
;;			  ( t )
;;			  ( "/users/jdoe/sources/proj2")
;;			  ( "/users/jdoe/sources/proj3/mycscope.out")
;;			  ( "/users/jdoe/sources/proj4")
;;			  t
;;			  ( "/some/master/directory" ("-d" "-I/usr/local/include") )
;;			  )
;;			( "^/users/jdoe/sources/gnome/"
;;			  ( "/master/gnome/database" ("-d") )
;;			  )
;;			))
;;
;; 	If the current buffer's directory matches the regexp,
;; 	"^/users/jdoe/sources/proj1", then the following search will be
;; 	done:
;;
;; 	    1. First, the normal hierarchical database search will be used to
;;	       locate a cscope database.
;;
;; 	    2. Next, searches will be done using the cscope database
;;	       directories, "/users/jdoe/sources/proj2",
;;	       "/users/jdoe/sources/proj3/mycscope.out", and
;;	       "/users/jdoe/sources/proj4".  Note that, instead of the file,
;;	       "cscope.out", the file, "mycscope.out", will be used in the
;;	       directory "/users/jdoe/sources/proj3".
;;
;; 	    3. If a match was found, searching will stop.
;;
;; 	    4. If a match was not found, searching will be done using
;;	       "/some/master/directory", and the command-line options "-d"
;;	       and "-I/usr/local/include" will be passed to cscope.
;;
;; 	If the current buffer's directory matches the regexp,
;; 	"^/users/jdoe/sources/gnome", then the following search will be
;; 	done:
;;
;; 	    The search will be done only using the directory,
;; 	    "/master/gnome/database".  The "-d" option will be passed to
;; 	    cscope.
;;
;; 	If the current buffer's directory does not match any of the above
;; 	regexps, then only the normal hierarchical database search will be
;; 	done.
;;
;;
;; * Other notes:
;;
;; 1. The script, "cscope-indexer", uses a sed command to determine
;;    what is and is not a C/C++/lex/yacc source file.  It's idea of a
;;    source file may not correspond to yours.
;;
;; 2. This module is called, "xcscope", because someone else has
;;    already written a "cscope.el" (although it's quite old).
;;
;;
;; * KNOWN BUGS:
;;
;; 1. Cannot handle whitespace in directory or file names.
;;
;; 2. By default, colored faces are used to display results.  If you happen
;;    to use a black background, part of the results may be invisible
;;    (because the foreground color may be black, too).  There are at least
;;    two solutions for this:
;;
;;    2a. Turn off colored faces, by setting `cscope-use-face' to `nil',
;;        e.g.:
;;
;;            (setq cscope-use-face nil)
;;
;;    2b. Explicitly set colors for the faces used by cscope.  The faces
;;        are:
;;
;;            cscope-file-face
;;            cscope-function-face
;;            cscope-line-number-face
;;            cscope-line-face
;;            cscope-mouse-face
;;
;;        The face most likely to cause problems (e.g., black-on-black
;;        color) is `cscope-line-face'.
;;
;; 3. The support for cscope databases different from that specified by
;;    `cscope-database-file' is quirky.  If the file does not exist, it
;;    will not be auto-created (unlike files names by
;;    `cscope-database-file').  You can manually force the file to be
;;    created by using touch(1) to create a zero-length file; the
;;    database will be created the next time a search is done.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easymenu)


(defgroup cscope nil
  "Cscope interface for (X)Emacs.
Using cscope, you can easily search for where symbols are used and defined.
It is designed to answer questions like:

        Where is this variable used?
        What is the value of this preprocessor symbol?
        Where is this function in the source files?
        What functions call this function?
        What functions are called by this function?
        Where does the message \"out of space\" come from?
        Where is this source file in the directory structure?
        What files include this header file?
"
  :prefix "cscope-"
  :group 'tools)


(defcustom cscope-do-not-update-database nil
  "*If non-nil, never check and/or update the cscope database when searching.
Beware of setting this to non-nil, as this will disable automatic database
creation, updating, and maintenance."
  :type 'boolean
  :group 'cscope)


(defcustom cscope-database-regexps nil
  "*List to force directory-to-cscope-database mappings.
This is a list of `(REGEXP DBLIST [ DBLIST ... ])', where:

REGEXP is a regular expression matched against the current buffer's
current directory.  The current buffer is typically some source file,
and you're probably searching for some symbol in or related to this
file.  Basically, this regexp is used to relate the current directory
to a cscope database.  You need to start REGEXP with \"^\" if you want
to match from the beginning of the current directory.

DBLIST is a list that contains one or more of:

    ( DBDIR )
    ( DBDIR ( OPTIONS ) )
    ( t )
    t

Here, DBDIR is a directory (or a file) that contains a cscope database.
If DBDIR is a directory, then it is expected that the cscope database,
if present, has the filename given by the variable,
`cscope-database-file'; if DBDIR is a file, then DBDIR is the path name
to a cscope database file (which does not have to be the same as that
given by `cscope-database-file').  If only DBDIR is specified, then that
cscope database will be searched without any additional cscope
command-line options.  If OPTIONS is given, then OPTIONS is a list of
strings, where each string is a separate cscope command-line option.

In the case of \"( t )\", this specifies that the search is to use the
normal hierarchical database search.  This option is used to
explicitly search using the hierarchical database search either before
or after other cscope database directories.

If \"t\" is specified (not inside a list), this tells the searching
mechanism to stop searching if a match has been found (at the point
where \"t\" is encountered).  This is useful for those projects that
consist of many subprojects.  You can specify the most-used
subprojects first, followed by a \"t\", and then followed by a master
cscope database directory that covers all subprojects.  This will
cause the most-used subprojects to be searched first (hopefully
quickly), and the search will then stop if a match was found.  If not,
the search will continue using the master cscope database directory.

Here, `cscope-database-regexps' is generally not used, as the normal
hierarchical database search is sufficient for placing and/or locating
the cscope databases.  However, there may be cases where it makes
sense to place the cscope databases away from where the source files
are kept; in this case, this variable is used to determine the
mapping.

This module searches for the cscope databases by first using this
variable; if a database location cannot be found using this variable,
then the current directory is searched, then the parent, then the
parent's parent, until a cscope database directory is found, or the
root directory is reached.  If the root directory is reached, the
current directory will be used.

A cscope database directory is one in which EITHER a cscope database
file (e.g., \"cscope.out\") OR a cscope file list (e.g.,
\"cscope.files\") exists.  If only \"cscope.files\" exists, the
corresponding \"cscope.out\" will be automatically created by cscope
when a search is done.  By default, the cscope database file is called
\"cscope.out\", but this can be changed (on a global basis) via the
variable, `cscope-database-file'.  There is limited support for cscope
databases that are named differently than that given by
`cscope-database-file', using the variable, `cscope-database-regexps'.

Here is an example of `cscope-database-regexps':

        (setq cscope-database-regexps
              '(
                ( \"^/users/jdoe/sources/proj1\"
                  ( t )
                  ( \"/users/jdoe/sources/proj2\")
                  ( \"/users/jdoe/sources/proj3/mycscope.out\")
                  ( \"/users/jdoe/sources/proj4\")
                  t
                  ( \"/some/master/directory\" (\"-d\" \"-I/usr/local/include\") )
                  )
                ( \"^/users/jdoe/sources/gnome/\"
                  ( \"/master/gnome/database\" (\"-d\") )
                  )
                ))

If the current buffer's directory matches the regexp,
\"^/users/jdoe/sources/proj1\", then the following search will be
done:

    1. First, the normal hierarchical database search will be used to
       locate a cscope database.

    2. Next, searches will be done using the cscope database
       directories, \"/users/jdoe/sources/proj2\",
       \"/users/jdoe/sources/proj3/mycscope.out\", and
       \"/users/jdoe/sources/proj4\".  Note that, instead of the file,
       \"cscope.out\", the file, \"mycscope.out\", will be used in the
       directory \"/users/jdoe/sources/proj3\".

    3. If a match was found, searching will stop.

    4. If a match was not found, searching will be done using
       \"/some/master/directory\", and the command-line options \"-d\"
       and \"-I/usr/local/include\" will be passed to cscope.

If the current buffer's directory matches the regexp,
\"^/users/jdoe/sources/gnome\", then the following search will be
done:

    The search will be done only using the directory,
    \"/master/gnome/database\".  The \"-d\" option will be passed to
    cscope.

If the current buffer's directory does not match any of the above
regexps, then only the normal hierarchical database search will be
done.

"
  :type '(repeat (list :format "%v"
		       (choice :value ""
			       (regexp :tag "Buffer regexp")
			       string)
		       (choice :value ""
			       (directory :tag "Cscope database directory")
			       string)
		       (string :value ""
			       :tag "Optional cscope command-line arguments")
		       ))
  :group 'cscope)
(defcustom cscope-name-line-width -30
  "*The width of the combined \"function name:line number\" field in the
cscope results buffer.  If negative, the field is left-justified."
  :type 'integer
  :group 'cscope)


(defcustom cscope-truncate-lines truncate-lines
  "*The value of `truncate-lines' to use in cscope buffers.
This variable exists because it can be easier to read cscope buffers
with truncated lines, while other buffers do not have truncated lines."
  :type 'boolean
  :group 'cscope)


(defcustom cscope-display-times t
  "*If non-nil, display how long each search took.
The elasped times are in seconds.  Floating-point support is required
for this to work."
  :type 'boolean
  :group 'cscope)


(defcustom cscope-program "cscope"
  "*The pathname of the cscope executable to use."
  :type 'string
  :group 'cscope)


(defcustom cscope-index-file "cscope.files"
  "*The name of the cscope file list file."
  :type 'string
  :group 'cscope)


(defcustom cscope-database-file "cscope.out"
  "*The name of the cscope database file."
  :type 'string
  :group 'cscope)


(defcustom cscope-edit-single-match t
  "*If non-nil and only one match is output, edit the matched location."
  :type 'boolean
  :group 'cscope)


(defcustom cscope-display-cscope-buffer t
  "*If non-nil automatically display the *cscope* buffer after each search."
  :type 'boolean
  :group 'cscope)


(defcustom cscope-stop-at-first-match-dir nil
  "*If non-nil, stop searching through multiple databases if a match is found.
This option is useful only if multiple cscope database directories are being
used.  When multiple databases are searched, setting this variable to non-nil
will cause searches to stop when a search outputs anything; no databases after
this one will be searched."
  :type 'boolean
  :group 'cscope)


(defcustom cscope-use-relative-paths t
  "*If non-nil, use relative paths when creating the list of files to index.
The path is relative to the directory in which the cscope database
will be created.  If nil, absolute paths will be used.  Absolute paths
are good if you plan on moving the database to some other directory
(if you do so, you'll probably also have to modify
\`cscope-database-regexps\').  Absolute paths  may also be good if you
share the database file with other users (you\'ll probably want to
specify some automounted network path for this)."
  :type 'boolean
  :group 'cscope)


(defcustom cscope-index-recursively t
  "*If non-nil, index files in the current directory and all subdirectories.
If nil, only files in the current directory are indexed.  This
variable is only used when creating the list of files to index, or
when creating the list of files and the corresponding cscope database."
  :type 'boolean
  :group 'cscope)


(defcustom cscope-no-mouse-prompts nil
  "*If non-nil, use the symbol under the cursor instead of prompting.
Do not prompt for a value, except for when seaching for a egrep pattern
or a file."
  :type 'boolean
  :group 'cscope)


(defcustom cscope-suppress-empty-matches t
  "*If non-nil, delete empty matches.")


(defcustom cscope-indexing-script "cscope-indexer"
  "*The shell script used to create cscope indices."
  :type 'string
  :group 'cscope)


(defcustom cscope-symbol-chars "A-Za-z0-9_"
  "*A string containing legal characters in a symbol.
The current syntax table should really be used for this."
  :type 'string
  :group 'cscope)


(defcustom cscope-filename-chars "-.,/A-Za-z0-9_~!@#$%&+=\\\\"
  "*A string containing legal characters in a symbol.
The current syntax table should really be used for this."
  :type 'string
  :group 'cscope)


(defcustom cscope-allow-arrow-overlays t
  "*If non-nil, use an arrow overlay to show target lines.
Arrow overlays are only used when the following functions are used:

    cscope-show-entry-other-window
    cscope-show-next-entry-other-window
    cscope-show-prev-entry-other-window

The arrow overlay is removed when other cscope functions are used.
Note that the arrow overlay is not an actual part of the text, and can
be removed by quitting the cscope buffer."
  :type 'boolean
  :group 'cscope)


(defcustom cscope-overlay-arrow-string "=>"
  "*The overlay string to use when displaying arrow overlays."
  :type 'string
  :group 'cscope)


(defvar cscope-minor-mode-hooks nil
  "List of hooks to call when entering cscope-minor-mode.")


(defconst cscope-separator-line
  "-------------------------------------------------------------------------------\n"
  "Line of text to use as a visual separator.
Must end with a newline.")


;;;;
;;;; Faces for fontification
;;;;

(defcustom cscope-use-face t
