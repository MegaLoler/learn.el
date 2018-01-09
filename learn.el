;; okay, so this is my learning code
;; there will be three modes:
;;   1. flash -- regular flash cards, just a cue for an answer
;;   2. query -- given cue, type in the answer, and itll diff it
;;   3. multi -- multiple choice, choose the answer from a set
;;
;; it operates on a data set
;; the data set is a table, each entry has fields
;; each entry is a learning item with different info associated
;; so given the learning data set, and a mode, and the fields to work with, (which field(s) to cue you on, and which you should recall)
;; and then a filter function to filter out which rows to drill you on
;; and then a sort function, to sort them how you like
;;
;;
;; so uh... in summary:
;;   three prompt functions: flash, query, multi
;;   learning table, loadable from buffers or files, & some formats
;;   custom sort functions
;;   custom filter functions
;;   integration modes...... to be expanded upon.....
;;
;; TODO:
;;   clean up duplicate codes for buffers vs files with Macros
;;   make default buffer selection the current buffer
;;   custom "correct answer" check functions?
;;   possibly more format support than just .tsv
;;   wrapper drill function which handles memory integration (spacing based on corrects and incorrects)
;;   !filter and sort function shorthands
;;   implicitly number all entries in case theres no explit numbering
;;   explicitly stringify values before `message` (in case of nums)
;;   !make multiple choice questions clickable (AND NUMBERABLE)
;;   emacs defcustom usage
;;   maybe even port to CL who knows :shrug:
;;   skip/suspend/mark hotkey
;;   !better lisp input?? with default support??
;;   prompt-func should be a defaultable/againable value
;;   remove correct answer from scrambled answers so theres no dup

(defvar *audio-player* "mplayer")
(defvar *audio-directory* (expand-file-name "~/audio"))
(global-set-key (kbd "C-c C-r") 'play-audio-again)

(defun make-audio-file-path (filename)
  (concat *audio-directory* "/" filename))

(defun is-audio-file (filename)
  (file-exists-p (make-audio-file-path filename)))

(defun play-audio-file (filename)
  (setq *last-audio-filename* filename)
  (make-process :name "*audio*"
		:command (list *audio-player*
			       (make-audio-file-path filename))))

(defun play-audio-again ()
  (interactive)
  (play-audio-file *last-audio-filename*))

(defun play-any-audio (ls)
  "If any string in ls is an audio file, play it."
  (dolist (e ls)
    (if (is-audio-file e)
	(play-audio-file e))))

(defun buffer-whole-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun read-file (filename)
  "Return a string from a file given its filename."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun read-file-first-line (filename)
  "Return the first line in a file."
  (car (split-string (read-file filename) "\n")))

(defun buffer-first-line (buffer)
  "Return the first line in a buffer."
  (car (split-string (buffer-whole-string buffer) "\n")))

(defun read-hash-header (filename)
  "If file starts with # then return that first line."
  (let ((first-line (read-file-first-line filename)))
    (if (string-prefix-p "#" first-line)
	(substring first-line 1))))

(defun read-hash-header-from-buffer (buffer)
  "If buffer starts with # then return that first line."
  (let ((first-line (buffer-first-line buffer)))
    (if (string-prefix-p "#" first-line)
	(substring first-line 1))))

(defun parse-tsv (string &optional fields)
  "Convert tab-separated-values into a list of property lists."
  (mapcar (lambda (line) (parse-tsv-line line fields))
	  (split-string string "\n" t)))

(defun string-is-num (str)
  (string-match "\\`[0-9]*[1-9][0-9]*\\'" str))

(defun convert-num (obj)
  "Convert stringified numbers into numbers."
  (if (and (stringp obj) (string-is-num obj))
      (string-to-number obj)
    obj))

(defun parse-tsv-line (line &optional fields)
  "Convert a tab-separated-values line into a property list"
  (let* ((vals (mapcar #'convert-num (split-string line "\t")))
	 (names (or fields (number-sequence 1 (length vals)))))
    (name-props vals names)))

(defun name-props (vals names)
  "Create a property list from a list of values and property names."
  (if names
      (append (list (car names) (car vals))
	      (name-props (cdr vals) (cdr names)))
    nil))

(defun parse-buffer (parse-func &optional buffer)
  "Parse a buffer as a table using a given parse function."
  (funcall parse-func (buffer-whole-string (or buffer
					       (current-buffer)))))

(defun parse-file (filename parse-func)
  "Parse a file as a table given the filename using a given parse function."
  (funcall parse-func (read-file filename)))

;; it'd be nicer if this only prepended : if it wasnt already there
(defun make-keyword (string)
  (intern (concat ":" string)))

(defun read-fields (&optional string delimiter prompt)
  (mapcar #'make-keyword
	  (split-string (or string
			    (read-string (or prompt "Fields: ")))
			(or delimiter " ") t)))

(defun load-tsv-file (&optional filename)
  "Interactively load a learning table from a .tsv file."
  (let* ((filename (or filename
		       (read-file-name "Learning .tsv file: ")))
	 (header (read-hash-header filename))
	 (fields (if header
		     (read-fields header "\t")
		   (read-fields)))
	 (table (parse-file filename
			    (lambda (string)
			      (parse-tsv string fields)))))
    (if header (cdr table) table)))

(defun load-learning-file (&optional filename)
  "Interactively load a file to learn from and automatically detect file type by file extension."
  (let* ((filename (or filename
		       (read-file-name "Learn from file: ")))
	 (file-ext (downcase (or (file-name-extension filename)
				 ""))))
    (cond ((string= file-ext "tsv") (load-tsv-file filename))
	  (t (message "Unknown file type!")))))

(defun load-tsv-buffer (&optional buffer)
  "Interactively load a learning table from a .tsv buffer."
  (let* ((buffer (get-buffer
		  (or buffer
		      (read-buffer "Learning .tsv buffer: "))))
	 (header (read-hash-header-from-buffer buffer))
	 (fields (if header
		     (read-fields header "\t")
		   (read-fields)))
	 (table (parse-buffer (lambda (string)
				(parse-tsv string fields))
			      buffer)))
    (if header (cdr table) table)))

(defun load-learning-buffer (&optional buffer)
  "Interactively load a buffer to learn from and automatically detect buffer type by file extension."
  (let* ((buffer (get-buffer
		  (or buffer
		      (read-buffer "Learn from buffer: "))))
	 (file-ext (downcase (or (file-name-extension
				  (buffer-name buffer))
				 ""))))
    (cond ((string= file-ext "tsv") (load-tsv-buffer buffer))
	  (t (message "Unknown buffer type!")))))

;; learning item = ([cue items list] [recall items list])
(defun drill (items prompt-func)
  "Prompt you consecutively with each learning item in a list using a given prompt function. Also return list of wrongly answered items."
  (remove-if
   (lambda (item)
     (funcall prompt-func (car item) (cdr item) items))
   items))

(defun get-properties (plist props)
  "Collect certain properties from a plist."
  (mapcar (lambda (prop) (getf plist prop)) props))

(defun make-learning-item-from-row (row cue-fields recall-fields)
  "Take a property list and create a learning item from it."
  (cons (get-properties row cue-fields)
	(get-properties row recall-fields)))

(defun make-learning-items (rows cue-fields recall-fields)
  "Make a list of learning items from a list of rows."
  (mapcar (lambda (row)
	    (make-learning-item-from-row row cue-fields
					 recall-fields))
	  rows))

;; i dont like how this function is written, rewrite it better!!!!
(defun read-mode (&optional prompt default)
  "Select a drill mode prompt function."
  (let* ((default (or default "flash"))
	 (prompt (or prompt (format "Mode (flash/query/multi) (Default: %s): " default)))
	 (answer (downcase (read-string prompt nil nil default))))
    (cond ((string= answer "flash") #'prompt-flash)
	  ((string= answer "query") #'prompt-query)
	  ((string= answer "multi") #'prompt-multi)
	  (t (progn
	       (read-string "Invalid entry!")
	       (read-mode prompt default))))))

(defun prompt-flash (cue-items recall-items learning-items)
  (play-any-audio cue-items)
  (read-string (format "%s" cue-items))
  (play-any-audio recall-items)
  (y-or-n-p (format "%s Correct? " recall-items)))

(defun correct? (source-answer input-answer)
  "Decide whether or not the user gave the right answer."
					;(string= (downcase source-answer) (downcase input-answer)))
  (string-match-p (regexp-quote input-answer) source-answer))

(defun prompt-query (cue-items recall-items learning-items)
  (play-any-audio cue-items)
  (let* ((answer (read-string (format "%s? Answer: " cue-items)))
	 (corrects (mapcar (lambda (item)
			     (cons item
				   (if (correct? item answer)
				       'correct
				     'incorrect)))
			   recall-items)))
    (play-any-audio recall-items)
    (read-string (format "%s" corrects))
    (member 'correct (mapcar #'cdr corrects))))

(defun remove-nth-element (nth list)
  (cond
   ((equal nil list) list)
   ((zerop nth) (cdr list))
   (t (cons (car list)
            (remove-nth-element (- nth 1)
                                (cdr list))))))

(defun random-selection (ls count)
  "Randomly select count unique items from ls."
  (if (and ls (> count 0))
      (let ((i (random (length ls))))
	(cons (nth i ls)
	      (random-selection (remove-nth-element i ls)
				(1- count))))))

(defun scramble (ls)
  "Rearrange ls randomly."
  (random-selection ls (length ls)))

;; this is ugly and messy, rewrite it!!
(defun prompt-multi (cue-items recall-items learning-items
			       &optional choice-count)
  (play-any-audio cue-items)
  (let* ((choice-count 4)
	 (choices (scramble
		   (append
		    (random-selection
		     (mapcar #'cadr learning-items)
		     (1- choice-count))
		    (list (car recall-items)))))
	 (answer (read-options choices
			       (format "%s? Answer" cue-items)
			       (car choices)
			       t t))
	 (correct (equalp recall-items answer)))
    (play-any-audio recall-items)
    (read-string (format "%s"
			 (mapcar (lambda (item)
				   (cons item
					 (if correct
					     'correct
					   'incorrect)))
				 recall-items)))
    correct))

(defun get-props (plist)
  (if plist (cons (car plist) (get-props (cddr plist)))))

(defun keyword-name (keyword)
  (substring (symbol-name keyword) 1))

(defun get-table-fields (table)
  (mapcar #'keyword-name (get-props (car table))))

(defun read-cue-recall-fields (table prompt-name &optional default)
  (let ((fields (get-table-fields table)))
    (mapcar #'make-keyword
	    (read-options fields prompt-name
			  (mapcar #'keyword-name default)))))

;; make the input here multi-value support
;; basically, if u enter a list instead, accept all the vals
(defun read-options (fields prompt-name &optional
			    default quote numbers)
  (let* ((default (or (if (and default (listp default)
			       (= (length default) 1))
			  (car default)
			default)
		      (car fields)))
	 (prompt (format "%s %s (Default: %s): "
			 prompt-name
			 (if quote
			     (mapcar (lambda (field)
				       (format "\"%s\"" field))
				     fields)
			   fields)
			 default))
	 (answer (read-string prompt nil nil default)))
    (if (member answer fields) (list answer)
      (progn
	(read-string "Invalid entry!")
	(read-options fields prompt-name)))))

(defun read-func (prompt)
  (read-string prompt)
  (let ((exp (read)))
    (if (functionp exp) exp
      `(lambda (x) ,exp))))

(defun learn-from-table (table &optional cue-fields recall-fields
			       filter-predicate sort-function)
  "Interactively initiate a learning session from a learning table."
  (let ((prompt-func (read-mode))
	(cue-fields
	 (or cue-fields
	     (read-cue-recall-fields table "Cue"
				     *last-cue-fields*)))
	(recall-fields
	 (or recall-fields
	     (read-cue-recall-fields table "Recall"
				     *last-recall-fields*)))
	(filter-predicate
	 (or filter-predicate
	     (read-func "Prepare to input filter function! (\"t\" for unfiltered)")))
	(sort-function
	 (or sort-function
	     (read-func "Prepare to input sort function! (\"identity\" for unchanged, \"scramble\" for random)"))))
    (setq *last-table* table)
    (setq *last-cue-fields* cue-fields)
    (setq *last-recall-fields* recall-fields)
    (setq *last-filter-predicate* filter-predicate)
    (setq *last-sort-function* sort-function)
    (setq *missed*
	  (drill (make-learning-items
		  (funcall sort-function
			   (remove-if-not filter-predicate table))
		  cue-fields recall-fields)
		 prompt-func)))
  (message "Session complete!"))

(defun review-missed ()
  "Drill the items you missed last session."
  (interactive)
  (setq *missed* (drill *missed* (read-mode)))
  (message "Review complete!"))

(defun learn-again (&optional table cue-fields recall-fields
			      filter-predicate sort-function)
  (interactive)
  "Repeat the last learning session with optional modifications."
  (learn-from-table (or table *last-table*)
		    (or cue-fields *last-cue-fields*)
		    (or recall-fields *last-recall-fields*)
		    (or filter-predicate *last-filter-predicate*)
		    (or sort-function *last-sort-function*)))

(defun learn-from-buffer (&rest rest)
  "Interactively initiate a learning session from a buffer."
  (interactive)
  (apply #'learn-from-table
	 (append (list (load-learning-buffer))
		 rest)))

(defun learn-from-file (&rest rest)
  "Interactively initiate a learning session from a file."
  (interactive)
  (apply #'learn-from-table
	 (append (list (load-learning-file))
		 rest)))
