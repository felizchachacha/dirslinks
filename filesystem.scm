#!/usr/bin/scheme --script
;;; http://www.madore.org/~david/linux/gcfs/filesystem.scm
;;; Filesystem demonstration code, written in ANSI Scheme.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.


;;; This code implements a simple filesystem (files have no contents)
;;; in Scheme.  This is to demonstrate what the desired semantics for
;;; the Linux Garbage-Collected Filesystem are.  The user-serviceable
;;; entry points are at the very end.


;;; Terminology: a ``node'' is an object of the filesystem
;;; (essentially, a Unix inode); there are two kinds of nodes, files
;;; and directories.  A directory is a list of entries, each having a
;;; ``name'' (non empty character string not containing the directory
;;; separator character) and an associated node.  A ``path'' is a non
;;; empty character string, which is spliced into names following the
;;; directory separator character.


;;; For more information, see
;;;   http://www.eleves.ens.fr:8080/home/madore/linux/gcfs/
;;; This implements the ``stateless'' semantics.


;;; --- Path name manipulation functions ---

;; The directory separator character.  We use the Unix slash.
(define path-dirsep-char #\/)

;; The names of the ``current directory'' and ``parent directory''
;; entries in any directory node.
(define path-curdir-name ".")
(define path-pardir-name "..")

;; Return the first component of path, i.e. everything up to the first
;; non directory separator character that follows a directory
;; separator character.  (For example, the first component of
;; "foo/bar/baz" is "foo/".)
(define (path-first-component path)
  (define (scan n) ; Search for directory separator character.
    (define (subscan n) ; Search for non directory separator character.
      (if (>= n (string-length path))
	  path ; Path is a component by itself.
	  (if (eqv? (string-ref path n) path-dirsep-char)
	      (subscan (+ n 1))
	      (substring path 0 n))))
    (if (>= n (string-length path))
	path ; Path contains no directory separator character
	(if (eqv? (string-ref path n) path-dirsep-char)
	    (subscan n)
	    (scan (+ n 1)))))
  (scan 0))

;; Return whether path starts with a directory separator character
;; (this is used to determine whether it is an absolute path).
(define (path-has-starting-dirsep path)
  (and (> (string-length path) 0)
       (eqv? (string-ref path 0) path-dirsep-char)))

;; Return path with the initial directory separator characters
;; removed.
(define (path-remove-starting-dirsep path)
  (define (scan n)
    (if (>= n (string-length path))
	"" ; Path contains only directory separator characters
	(if (eqv? (string-ref path n) path-dirsep-char)
	    (scan (+ n 1))
	    (substring path n (string-length path)))))
  (scan 0))

;; Return whether path (actually always used with a path component)
;; ends in a directory separator character (this is used to determine
;; wether the name must refer to a directory, to set the must-dir
;; flag).
(define (path-has-trailing-dirsep path)
  (and (> (string-length path) 0)
       (eqv? (string-ref path (- (string-length path) 1))
	     path-dirsep-char)))

;; Return path with the trailing directory separator characters
;; removed.
(define (path-remove-trailing-dirsep path)
  (define (scan n)
    (if (< n 0)
	"" ; Path contains only directory separator characters
	(if (eqv? (string-ref path n) path-dirsep-char)
	    (scan (- n 1))
	    (substring path 0 (+ n 1)))))
  (scan (- (string-length path) 1)))


;;; --- Mutex stubs ---

;;; ANSI Scheme has no parallelism, and hence no mutexes.  If your
;;; particular variant of Scheme has, replace these stub functions by
;;; the appropriate ones.

;; Return a new, unlocked, mutex object.
(define (mutex-make-new)
  #t)

;; Lock mutex.
(define (mutex-lock! mutex)
  #t)

;; Unlock mutex.
(define (mutex-unlock! mutex)
  #t)


;;; --- Low-level node manipulation ---

;;; A node is represented by a pair.  Its car is a tag, and its cdr
;;; contains the information contained by the node.  In the case of a
;;; directory, the cdr is another pair, whose car is the directory
;;; mutex (which must be held to read or modify the node's data) and
;;; whose cdr is an association list of (name . node) pairs.

;;; These low-level functions should abstract away the details of the
;;; node's representation.  (One thing is used, however, for
;;; simplicity's sake: no valid node should ever have value #f.  This
;;; is so we can return #f in some failure cases.)

;; The tags for a directory and a file node
(define node-directory-tag 'dir)
(define node-file-tag 'file)

;; Return a new (completely empty) directory node.
(define (node-make-new-directory)
  `(,node-directory-tag ,(mutex-make-new)))

;; Returne a new file node.  We do not have any file content.
(define (node-make-new-file)
  `(,node-file-tag))

;; Test whether a node is a directory.
(define (node-directory? node)
  (eqv? (car node) node-directory-tag))


;;; --- Error results ---

;;; An error result is returned as a pair whose car is the error tag
;;; (below) and whose cdr is the error code (a symbol, as in Unix:
;;; ENOENT, ENOTDIR, EISDIR, and so on).  A generic non error result
;;; is returned as a pair whose car is the noerror tag (below).

;; The tag for an error
(define error-tag 'error)

;; Return an error object with given error code.
(define (error-make code)
  `(,error-tag . ,code))

;; Test whether something (a node, non-error or error) is in fact an
;; error.
(define (error? thing)
  (eqv? (car thing) error-tag))

;; Return the error string for an error code.
(define (error-string error)
  (case (cdr error)
    ((ENOTDIR) "Not a directory")
    ((ENOENT) "No such file or directory")
    ((EISDIR) "Is a directory")
    ((EEXIST) "File exists")
    ((EINVAL) "Invalid argument")
    (else "Error")))

;; Print the error string for an error code.
(define (error-print str error)
  (display str)
  (display ": ")
  (display (error-string error))
  (newline))

;; The tag for a non error
(define noerror-tag 'ok)

;; Return a non error object.
(define (noerror-make)
  `(,noerror-tag))


;;; --- Low-level directory manipulation ---

;; Return a directory node's mutex object.
(define (directory-mutex directory-node)
  (cadr directory-node))

;; Lock a directory node's mutex.
(define (directory-mutex-lock! directory-node)
  (mutex-lock! (directory-mutex directory-node)))

;; Unlock a directory node's mutex.
(define (directory-mutex-unlock! directory-node)
  (mutex-unlock! (directory-mutex directory-node)))

;; Return the association table of directory entries in a directory
;; node (this table must be the real one, not a copy: it might be
;; altered, and this should effectively alter the node's contents).
(define (directory-entries-table directory-node)
  (cddr directory-node))

;; Replace the association table of directory entries in a directory
;; node.  In practice, this is use for adding a new entry (at the
;; start) or removing the first entry.
(define (directory-replace-entries-table! directory-node table)
  (set-cdr! (cdr directory-node) table))


;;; --- High-level directory manipulation ---

;;; Warning: directory-node mutex must be held to use these functions.

;; Lookup name in the directory node and return the associated node,
;; or #f if there is no such entry.
(define (directory-lookup directory-node name)
  (let ((entry (assoc name (directory-entries-table directory-node))))
    (and entry (cdr entry))))

;; Replace a directory entry: name should now be bound to node: add a
;; new entry if none was already existent, otherwise replace the
;; existing entry.  Replacement is atomic since the mutex is being
;; held all the time.
(define (directory-replace! directory-node name node)
  (let* ((table (directory-entries-table directory-node))
	 (entry (assoc name table)))
    (if entry
	(set-cdr! entry node)
	(directory-replace-entries-table! directory-node
					  `((,name . ,node) . ,table)))))

;; Remove directory entry with given name in the directory node.
;; Return #t if it was successfully removed, #f if there was no entry
;; of that name.
(define (directory-remove! directory-node name)
  (let ((table (directory-entries-table directory-node)))
    (define (remove-loop prev t)
      (if (null? t)
	  #f
	  (if (equal? (caar t) name)
	      (begin
		(if prev
		    (set-cdr! prev (cdr t))
		    (directory-replace-entries-table! directory-node
						      (cdr t)))
		#t)
	      (remove-loop t (cdr t)))))
    (remove-loop #f table)))


;;; --- Main operations, abstract form ---

;;; If must-dir is true, the manipulated node *must* be a directory.
;;; More precisely, in the concrete forms, this corresponds to the
;;; case when the name had a trailing slash.

;; The "open" operation: essentially the same as directory-lookup
;; above, but hold the directory mutex.  Return ENOENT if the name
;; does not exist in the directory.
(define (abstract-open directory-node name must-dir)
  (directory-mutex-lock! directory-node)
  (let ((node (directory-lookup directory-node name)))
    (directory-mutex-unlock! directory-node)
    (if node
	(if (and must-dir (not (node-directory? node)))
	    (error-make 'ENOTDIR) ; open("foo/") where foo is not a directory
	    node)
	(error-make 'ENOENT))))

;; The "creat" operation: create (and return) a file node with given
;; name in the given directory node.
(define (abstract-creat directory-node name must-dir)
  (directory-mutex-lock! directory-node)
  (let ((node (directory-lookup directory-node name)))
    (if node
	(begin
	  (directory-mutex-unlock! directory-node)
	  (if (node-directory? node)
	      (error-make 'EISDIR) ; creat("foo") where foo is a directory
	      (if must-dir
		  (error-make 'ENOTDIR) ; creat("foo/") where foo is not a dir
		  node)))
	(if must-dir
	    (begin
	      (directory-mutex-unlock! directory-node)
	      (error-make 'ENOENT)) ; creat("foo/") where foo doesn't exist
	    (let ((new-node (node-make-new-file)))
	      (directory-replace! directory-node name new-node)
	      (directory-mutex-unlock! directory-node)
	      new-node)))))

;; The "mkdir" operation: create a new directory node with given name
;; in the given directory node.
(define (abstract-mkdir directory-node name must-dir)
  (directory-mutex-lock! directory-node)
  (let ((node (directory-lookup directory-node name)))
    (if node
	(begin
	  (directory-mutex-unlock! directory-node)
	  (if (and must-dir (not (node-directory? node)))
	      (error-make 'ENOTDIR) ; mkdir("foo/") where foo is not a dir
	      (error-make 'EEXIST))) ; mkdir("foo") where foo exists
	(let ((new-node (node-make-new-directory)))
	  (directory-replace! new-node path-curdir-name new-node)
	  (directory-replace! new-node path-pardir-name directory-node)
	  (directory-replace! directory-node name new-node)
	  (directory-mutex-unlock! directory-node)
	  (noerror-make)))))

;; The "unlink" operation: remove a directory entry with given name
;; from a given directory node.
(define (abstract-unlink directory-node name must-dir)
  (if (or (equal? name path-curdir-name)
	  (equal? name path-pardir-name))
      (error-make 'EINVAL) ; Cannot remove "." or "..".
      (begin
	(directory-mutex-lock! directory-node)
	(let ((node (directory-lookup directory-node name)))
	  (if node
	      (if (and must-dir (not (node-directory? node)))
		  (begin
		    (directory-mutex-unlock! directory-node)
		    (error-make 'ENOTDIR)) ; unlink("foo/") with foo not a dir
		  (begin
		    (directory-remove! directory-node name)
		    (directory-mutex-unlock! directory-node)
		    (noerror-make)))
	      (begin
		(directory-mutex-unlock! directory-node)
		(error-make 'ENOENT)))))))

;; The "rmdir" operation: function just like 'unlink' except that what
;; is removed must be a directory (nothing says it must be empty,
;; though).
(define (abstract-rmdir directory-node name must-dir)
  (if (or (equal? name path-curdir-name)
	  (equal? name path-pardir-name))
      (error-make 'EINVAL) ; Cannot remove "." or "..".
      (begin
	(directory-mutex-lock! directory-node)
	(let ((node (directory-lookup directory-node name)))
	  (if node
	      (if (not (node-directory? node))
		  (begin
		    (directory-mutex-unlock! directory-node)
		    (error-make 'ENOTDIR)) ; rmdir("foo") with foo not a dir
		  (begin
		    (directory-remove! directory-node name)
		    (directory-mutex-unlock! directory-node)
		    (noerror-make)))
	      (begin
		(directory-mutex-unlock! directory-node)
		(error-make 'ENOENT)))))))

;; The "link" operation: add a new directory entry, with given target
;; name, in the given target directory node, that points to the given
;; source node in the given source directory (the latter is opened as
;; per "open").
(define (abstract-link source-directory-node source-name source-must-dir
		       target-directory-node target-name target-must-dir)
  (let* ((source-node (begin
			(directory-mutex-lock! source-directory-node)
			(directory-lookup source-directory-node
					  source-name)))
	 (target-node (begin
			(directory-mutex-unlock! source-directory-node)
			(directory-mutex-lock! target-directory-node)
			(directory-lookup target-directory-node
					  target-name))))
    (cond
     ((not source-node)
      (directory-mutex-unlock! target-directory-node)
      (error-make 'ENOENT))
     ((and source-must-dir (not (node-directory? source-node)))
      (directory-mutex-unlock! target-directory-node)
      (error-make 'ENOTDIR)) ; link("foo/","bar") with foo not dir
     ((and target-must-dir target-node (not (node-directory? node)))
      (directory-mutex-unlock! target-directory-node)
      (error-make 'ENOTDIR)) ; link("foo","bar/") with bar exist but not dir
     (target-node
      (directory-mutex-unlock! target-directory-node)
      (error-make 'EEXIST)) ; link("foo","bar") with bar exist
     ((and target-must-dir (not (node-directory? source-node)))
      (directory-mutex-unlock! target-directory-node)
      (error-make 'ENOENT)) ; link("foo","bar/") with foo not dir
     (else
      (directory-replace! target-directory-node target-name source-node)
      (directory-mutex-unlock! target-directory-node)
      (noerror-make)))))

;; The "rename" operation: ``move'' the node called source-name in
;; source-directory-node and call it target-name in
;; target-directory-node; if it is a directory, also make its ".."
;; point to target-directory-node.
(define (abstract-rename source-directory-node source-name source-must-dir
			 target-directory-node target-name target-must-dir)
  (if (or (equal? source-name path-curdir-name)
	  (equal? source-name path-pardir-name)
	  (equal? target-name path-curdir-name)
	  (equal? source-name path-pardir-name))
      (error-make 'EINVAL) ; Cannot move or replace "." or "..".
      (let* ((source-node (begin
			    (directory-mutex-lock! source-directory-node)
			    (directory-lookup source-directory-node
					      source-name)))
	     (target-node (begin
			    (directory-mutex-unlock! source-directory-node)
			    (directory-mutex-lock! target-directory-node)
			    (directory-lookup target-directory-node
					      target-name))))
	(cond
	 ((not source-node)
	  (directory-mutex-unlock! target-directory-node)
	  (error-make 'ENOENT))
	 ((and source-must-dir (not (node-directory? source-node)))
	  (directory-mutex-unlock! target-directory-node)
	  (error-make 'ENOTDIR))
	 ((and target-must-dir target-node (not (node-directory? target-node)))
	  (directory-mutex-unlock! target-directory-node)
	  (error-make 'ENOTDIR))
	 ((and target-must-dir (not (node-directory? source-node)))
	  (directory-mutex-unlock! source-directory-node)
	  (error-make 'ENOENT))
	 (else
	  (directory-replace! target-directory-node target-name
			      source-node)
	  (if (node-directory? source-node)
	      (directory-replace! source-node path-pardir-name
				  target-directory-node))
	  (directory-mutex-unlock! target-directory-node)
	  (directory-mutex-lock! source-directory-node)
	  (if (not (eq? source-node target-node))
	      (directory-remove! source-directory-node source-name))
					; Ignore failure
	  (directory-mutex-unlock! source-directory-node)
	  (noerror-make))))))


;;; --- Path lookup ---

;;; The global variable *root-node* should be the root node of the
;;; filesystem.

;; Perform path lookup on path starting from base-node (or *root-node*
;; if the path is absolute).  On the last step (i.e. on the last path
;; component), call continuation with three values: the directory so
;; far, the name of the last component minus trailing directory
;; separator characters, and a boolean (must-dir) that indicates
;; whether there were trailing directory separator characters.  This
;; function is used in continuation passing style.
(define (path-lookup base-node path continuation)
  (define (loop node path)
    (let* ((cname (path-first-component path))
	   (name (path-remove-trailing-dirsep cname))
	   (must-dir (path-has-trailing-dirsep cname)))
      (if (= (string-length cname)
	     (string-length path))
	  (continuation node name must-dir) ; Last component
	  (let ((next-node (abstract-open node name #t)))
	    (if (error? next-node)
		next-node ; ENOENT or ENOTDIR
		(loop next-node (substring
				 path
				 (string-length cname) ; Remove component
				 (string-length path))))))))
  (let* ((do-root (path-has-starting-dirsep path))
	 (init-node (if do-root *root-node* base-node))
	 (real-path (path-remove-starting-dirsep path)))
    (if (= (string-length real-path) 0) ; Path has only dir. sep. chars
	(if do-root
	    (loop init-node path-curdir-name) ; Disgusting hack
	    (error-make 'ENOENT)) ; Empty path
	(loop init-node real-path))))


;;; --- Miscellaneous ---

;; Make a new *root-node* (i.e. filesystem).
(define (mkfs)
  (let ((new-root (node-make-new-directory)))
    (directory-replace! new-root path-curdir-name new-root)
    (directory-replace! new-root path-pardir-name new-root)
    (set! *root-node* new-root)))

;; The *root-node*
(define *root-node* #t)
(mkfs)


;;; --- Concrete form of the main operations ---

(define (open base-node path)
  (path-lookup base-node path abstract-open))

(define (creat base-node path)
  (path-lookup base-node path abstract-creat))

(define (mkdir base-node path)
  (path-lookup base-node path abstract-mkdir))

(define (unlink base-node path)
  (path-lookup base-node path abstract-unlink))

(define (rmdir base-node path)
  (path-lookup base-node path abstract-rmdir))

;; Beautiful Continuation Passing Style
(define (link base-node source-path target-path)
  (path-lookup
   base-node source-path
   (lambda (source-directory-node source-name source-must-dir)
     (path-lookup
      base-node target-path
      (lambda (target-directory-node target-name target-must-dir)
	(abstract-link source-directory-node source-name source-must-dir
		       target-directory-node target-name target-must-dir))))))

;; Ditto
(define (rename base-node source-path target-path)
  (path-lookup
   base-node source-path
   (lambda (source-directory-node source-name source-must-dir)
     (path-lookup
      base-node target-path
      (lambda (target-directory-node target-name target-must-dir)
	(abstract-rename source-directory-node source-name source-must-dir
			 target-directory-node target-name target-must-dir))))))


;;; --- User operations ---

(define (make-shell)
  (let ((*cwd* *root-node*))
    (lambda (command)
      (case command
	; Dispatch command
       ((chdir cd)
	(lambda (path)
	  (let ((new-cwd (open *cwd* path)))
	    (if (error? new-cwd)
		(begin
		  (error-print path new-cwd)
		  #f)
		(if (not (node-directory? new-cwd))
		    (begin
		      (error-print path (error-make 'ENOTDIR))
		      #f)
		    (begin
		      (set! *cwd* new-cwd)
		      #t))))))
       ((creat touch)
	(lambda paths
	  (let loop ((rem-paths paths)
		     (value #t))
	    (if (null? rem-paths)
		value
		(let ((result (creat *cwd* (car rem-paths))))
		  (if (error? result)
		      (begin
			(error-print (car rem-paths) result)
			(loop (cdr rem-paths) #f))
		      (loop (cdr rem-paths) value)))))))
       ((mkdir)
	(lambda paths
	  (let loop ((rem-paths paths)
		     (value #t))
	    (if (null? rem-paths)
		value
		(let ((result (mkdir *cwd* (car rem-paths))))
		  (if (error? result)
		      (begin
			(error-print (car rem-paths) result)
			(loop (cdr rem-paths) #f))
		      (loop (cdr rem-paths) value)))))))
       ((unlink rm)
	(lambda paths
	  (let loop ((rem-paths paths)
		     (value #t))
	    (if (null? rem-paths)
		value
		(let ((result (unlink *cwd* (car rem-paths))))
		  (if (error? result)
		      (begin
			(error-print (car rem-paths) result)
			(loop (cdr rem-paths) #f))
		      (loop (cdr rem-paths) value)))))))
       ((rmdir)
	(lambda paths
	  (let loop ((rem-paths paths)
		     (value #t))
	    (if (null? rem-paths)
		value
		(let ((result (rmdir *cwd* (car rem-paths))))
		  (if (error? result)
		      (begin
			(error-print (car rem-paths) result)
			(loop (cdr rem-paths) #f))
		      (loop (cdr rem-paths) value)))))))
       ((link ln)
	(lambda (old-path new-path)
	  (let ((result (link *cwd* old-path new-path)))
	    (if (error? result)
		(begin
		  (error-print "link" result)
		  #f)
		#t))))
       ((rename move mv)
	(lambda (old-path new-path)
	  (let ((result (rename *cwd* old-path new-path)))
	    (if (error? result)
		(begin
		  (error-print "rename" result)
		  #f)
		#t))))
       ((ls)
	(lambda paths
	  (let loop ((rem-paths paths)
		     (value #t))
	    (if (null? rem-paths)
		value
		(let ((node (open *cwd* (car rem-paths))))
		  (if (error? node)
		      (begin
			(error-print (car rem-paths) result)
			(loop (cdr rem-paths) #f))
		      (begin
			(if (node-directory? node)
			    (begin
			      (display "Listing of directory ")
			      (display (car rem-paths))
			      (display ":") (newline)
			      (map (lambda (pair)
				     (display (car pair))
				     (newline))
				   (directory-entries-table node))
			      (display "---") (newline))
			    (begin
			      (display "File: ")
			      (display (car rem-paths))
			      (newline)))
			(loop (cdr rem-paths) value))))))))
       (else
	(display "Command not understood")
	#f)))))

;;; Sample session:
;;;   guile> (define shell (make-shell))
;;;   guile> ((shell 'mkdir) "/tmp/")
;;;   #t
;;;   guile> ((shell 'touch) "/tmp/file1")
;;;   #t
;;;   guile> ((shell 'touch) "/tmp/file2")
;;;   #t
;;;   guile> ((shell 'mkdir) "/tmp/dir1")
;;;   #t
;;;   guile> ((shell 'mkdir) "/tmp/dir2")
;;;   #t
;;;   guile> ((shell 'touch) "/tmp/dir1/file")
;;;   #t
;;;   guile> ((shell 'ls) "/tmp/")
;;;   Listing of directory /tmp/:
;;;   dir2
;;;   dir1
;;;   file2
;;;   file1
;;;   ..
;;;   .
;;;   ---
;;;   #t
;;;   guile> ((shell 'ls) "/tmp/dir1")
;;;   Listing of directory /tmp/dir1:
;;;   file
;;;   ..
;;;   .
;;;   ---
;;;   #t
;;;   guile> ((shell 'ln) "/tmp/dir1/" "/tmp/dir1-link")
;;;   #t
;;;   guile> ((shell 'ls) "/tmp/dir1-link/")
;;;   Listing of directory /tmp/dir1-link/:
;;;   file
;;;   ..
;;;   .
;;;   ---
;;;   #t
;;;   guile> ((shell 'ln) "/tmp/dir2" "/tmp/dir2-link/")
;;;   #t
;;;   guile> ((shell 'touch) "/tmp/dir2/another_file")
;;;   #t
;;;   guile> ((shell 'ls) "/tmp/dir2-link")
;;;   Listing of directory /tmp/dir2-link:
;;;   another_file
;;;   ..
;;;   .
;;;   ---
;;;   #t
;;;   guile> ((shell 'cd) "/tmp/dir1")
;;;   #t
;;;   guile> ((shell 'ln) "." "loop")   
;;;   #t
;;;   guile> ((shell 'cd) "loop")
;;;   #t
;;;   guile> ((shell 'cd) "loop")
;;;   #t
;;;   guile> ((shell 'ls) ".")
;;;   Listing of directory .:
;;;   loop
;;;   file
;;;   ..
;;;   .
;;;   ---
;;;   #t
;;;   guile> ((shell 'cd) "..")
;;;   #t
;;;   guile> ((shell 'ln) "dir2/" "/tmp/another_link/")
;;;   #t
;;;   guile> ((shell 'cd) "/tmp/another_link/")
;;;   #t
;;;   guile> ((shell 'ls) ".")
;;;   Listing of directory .:
;;;   another_file
;;;   ..
;;;   .
;;;   ---
;;;   #t
;;;   guile> ((shell 'cd) "..")
;;;   #t
;;;   guile> ((shell 'ls) ".")
;;;   Listing of directory .:
;;;   another_link
;;;   dir2-link
;;;   dir1-link
;;;   dir2
;;;   dir1
;;;   file2
;;;   file1
;;;   ..
;;;   .
;;;   ---
;;;   #t
;;;   guile> ((shell 'cd) "..")
;;;   #t
;;;   guile> ((shell 'ls) ".")
;;;   Listing of directory .:
;;;   tmp
;;;   ..
;;;   .
;;;   ---
;;;   #
;;;   guile> ((shell 'ln) "/tmp/another_link" "root_link")
;;;   #t
;;;   guile> ((shell 'ls) "root_link")
;;;   Listing of directory root_link:
;;;   another_file
;;;   ..
;;;   .
;;;   ---
;;;   #t
;;;   guile> ((shell 'ls) "root_link/..")
;;;   Listing of directory root_link/..:
;;;   another_link
;;;   dir2-link
;;;   dir1-link
;;;   dir2
;;;   dir1
;;;   file2
;;;   file1
;;;   ..
;;;   .
;;;   ---
;;;   #t
;;;   guile> ((shell 'rmdir) "/tmp/another_link")
;;;   #t
;;;   guile> ((shell 'rm) "/tmp/file2")
;;;   #t
;;;   guile> ((shell 'mv) "/tmp/dir2-link" "/root_link")
;;;   #t
;;;   guile> ((shell 'ls) "root_link/..")
;;;   Listing of directory root_link/..:
;;;   root_link
;;;   tmp
;;;   ..
;;;   .
;;;   ---
;;;   #t
