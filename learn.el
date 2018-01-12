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
;;   hide mp3 file name when playing one
;;   undo ?!
;;   stats ? ? tables and stuff
;;   org mode integration?
;;   emacs mode????
;;   learn presets
;;   html formatting/unformatting
;;   dedicated buffer?????? instead of everything happen in mini
;;   and!! auto magical romaji input allowed???
;;   show /your/ answer in the results as well
;;   make previous audio STOP before starting new audio
;;   correct/incorrect OVERRIDING
;;   saving sets of learning items!!!
;;       either by filter, or like.. save ur missed ones
;;   make the scramble function not stack overflow
;;   input system with builtin default answer handling
;;   learning preset system
;;   log all study sessions and times

(require 'cl)

(setq *audio-player* "wavy")
(setq *audio-player* "mplayer")
(setq *audio-directory* (expand-file-name "~/audio"))
(global-set-key (kbd "C-c C-r") 'play-audio-again)
(global-set-key (kbd "C-c C-l") 'learn-from-buffer)
(global-set-key (kbd "C-c r") 'review-missed)
(global-set-key (kbd "C-c a") 'learn-again)
(global-set-key (kbd "C-c C-s") 'scramble-table)
(global-set-key (kbd "C-c C-c") 'slice-table-chunk)

;; i dont know what im doing
(setq *last-mode* nil)
(setq *last-table* nil)
(setq *last-cue-fields* nil)
(setq *last-recall-fields* nil)
(setq *last-filter-predicate* nil)
(setq *last-sort-function* nil)

;; stuff to convert hiragana to romaji
;; this is a mess
(setq hiragana-romaji '(
			   きゃ kya
			   きゅ kyu
			   きょ kyo
			   しゃ sha
			   しゅ shu
			   しょ sho
			   ちゃ cha
			   ちゅ chu
			   ちょ cho
			   ひゃ hya
			   ひゅ hyu
			   ひょ hyo
			   りゃ rya
			   りゅ ryu
			   りょ ryo
			   ぎゃ gya
			   ぎゅ gyu
			   ぎょ gyo
			   じゃ ja
			   じゅ ju
			   じょ jo
			   ぢゃ ja
			   ぢゅ ju
			   ぢょ jo
			   びゃ bya
			   びゅ byu
			   びょ byo
			   ぴゃ pya
			   ぴゅ pyu
			   ぴょ pyo
			   "あ" "a"
			   "い" "i"
			   "う" "u"
			   "え" "e"
			   "お" "o"
			   "か" "ka"
			   き ki
			   く ku
			   け ke
			   こ ko
			   さ sa
			   し shi
			   す su
			   せ se
			   そ so
			   た ta
			   ち chi
			   つ tsu
			   て te
			   と to
			   な na
			   に ni
			   ぬ nu
			   ね ne
			   の no
			   ま ma
			   み mi
			   む mu
			   め me
			   も mo
			   は wa
			   ha wa
			   ひ hi
			   ふ fu
			   へ e
			   he e
			   ほ ho
			   や ya
			   ゆ yu
			   よ yo
			   ら ra
			   り ri
			   る ru
			   れ re
			   ろ ro
			   わ wa
			   を o
			   wo o
			   ん n
			   が ga
			   ぎ gi
			   ぐ gu
			   げ ge
			   ご go
			   ざ za
			   じ ji
			   ず zu
			   ぜ ze
			   ぞ zo
			   だ da
			   ぢ ji
			   づ dzu
			   で de
			   ど do
			   ば ba
			   び bi
			   ぶ bu
			   べ be
			   ぼ bo
			   ぱ pa
			   ぴ pi
			   ぷ pu
			   ぺ pe
			   ぽ po
			   ;。 "."
			   ;！ "!"
			   ;？ "?"
			   。 ""
			   ！ ""
			   ？ ""
			   、 ""
			   っ "'"))

(defun strip-html (string)
  (replace-regexp-in-string "<[^>]*>" "" string))

(defun transcode (string conversion-scheme)
  "Convert sequences in string according to a plist."
  (dolist (key (get-props conversion-scheme))
    (let ((source (format "%s" key))
	  (destin (format "%s" (getf conversion-scheme key))))
      (setq string
	    (replace-regexp-in-string source destin string))))
  string)

(defun romanify (string)
  (transcode (strip-html string) hiragana-romaji))

(defun romanify-no-spaces (string)
  (transcode string (append hiragana-romaji (list " " ""))))

(defun romanify-no-spaces-or-html (string)
  (transcode (strip-html string)
	     (append hiragana-romaji (list " " ""))))

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

(defun read-buffer-or-current (prompt)
  (let ((buffer (read-buffer prompt)))
    (if (string= buffer "")
	(current-buffer)
      buffer)))

(defun load-learning-buffer (&optional buffer)
  "Interactively load a buffer to learn from and automatically detect buffer type by file extension."
  (let* ((buffer (get-buffer
		  (or buffer
		      (read-buffer-or-current "Learn from buffer: "))))
	 (file-ext (downcase (or (file-name-extension
				  (buffer-name buffer))
				 ""))))
    (cond ((string= file-ext "tsv") (load-tsv-buffer buffer))
	  (t (message "Unknown buffer type!")))))

;; learning item = ([cue items list] [recall items list])
(defun drill (items prompt-func)
  "Prompt you consecutively with each learning item in a list using a given prompt function. Also return list of wrongly answered items."
  (let ((n 0))
    (remove-if
     (lambda (item)
       (setq n (1+ n))
       (funcall prompt-func (car item) (cdr item) items
		n (length items)))
     items)))

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

(defun format-cue-items-for-display (cue-items)
  "Make cue items look presentable!"
  (mapcar (lambda (item)
	    (if (is-audio-file item)
		"[audio]"
	      (strip-html item)))
	  cue-items))

(defun make-progress-string (n len)
  (if n
      (if len
	  (format "(%s/%s) " n len)
	(format "(%s) " n))
    ""))

(defun prompt-flash (cue-items recall-items learning-items
			       &optional n len)
  (play-any-audio cue-items)
  (read-string (format "%s%s"
		       (make-progress-string n len)
		       (format-cue-items-for-display cue-items)))
  (play-any-audio recall-items)
  (y-or-n-p (format "%s Correct? " recall-items)))

;; i think there should be selectable versions of this test
(defun correct? (source-answer input-answer)
  "Decide whether or not the user gave the right answer."
  (and (string-match-p (regexp-quote (replace-regexp-in-string
				      " " ""
				      (romanify input-answer)))
		       (replace-regexp-in-string
			" " ""
			(romanify source-answer)))
       (> (length input-answer) 0)))

(defun prompt-query (cue-items recall-items learning-items
			       &optional n len)
  (play-any-audio cue-items)
  (let* ((answer (read-string (format "\n%s%s? Answer: "
				      (make-progress-string n len)
				      (format-cue-items-for-display cue-items))))
	 (good nil)
	 (corrects
	  (mapcar
	   (lambda (item)
	     (format "\nAnswer: %s\nYou:    %s\n%s\n"
		     (strip-html item) answer 
		     (if (correct? item answer)
			 (progn
			   (setq good t)
			   "CORRECT!")
		       "INCORRECT!")))
	   recall-items)))
    (play-any-audio recall-items)
    (read-string (format "%s" corrects))
    good))

;;(defun remove-nth-element (nth list)
;;  (cond
;;   ((equal nil list) list)
;;   ((zerop nth) (cdr list))
;;   (t (cons (car list)
;;            (remove-nth-element (- nth 1)
;;                                (cdr list))))))

;; iterative version
(defun remove-nth-element (nth list)
  (let ((result '()))
    (dotimes (i (length list))
      (if (not (= i nth))
	  (push (nth i list) result)))
    result))

;;(defun random-selection (ls count)
;;  "Randomly select count unique items from ls."
;;  (if (and ls (> count 0))
;;      (let ((i (random (length ls))))
;;	(cons (nth i ls)
;;	      (random-selection (remove-nth-element i ls)
;;				(1- count))))))

;; iterative version
(defun random-selection (ls count)
  "Randomly select count unique items from ls."
  (let ((source ls)
	(result '()))
    (dotimes (i count)
      (let ((j (random (length source))))
	(push (nth j source) result)
	(setq source (remove-nth-element j source))))
    result))

(defun scramble (ls)
  "Rearrange ls randomly."
  (random-selection ls (length ls)))

;; this is ugly and messy, rewrite it!!
(defun prompt-multi (cue-items recall-items learning-items
			       &optional n len choice-count)
  (play-any-audio cue-items)
  (let* ((choice-count 4)
	 (choices (scramble
		   (append
		    (random-selection
		     (mapcar #'cadr learning-items)
		     (1- choice-count))
		    (list (car recall-items)))))
	 (answer (read-options choices
			       (format "%s%s? Answer"
				       (make-progress-string n len)
				       (format-cue-items-for-display cue-items))
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

(defun get-vals (plist)
  (if plist (cons (cadr plist) (get-vals (cddr plist)))))

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

(defun all-elems-in-list (elems ls)
  "Whether all items in elems is a member of ls."
  (let ((result t))
    (dolist (el elems)
      (if (not (member el ls))
	  (setq result nil)))
    result))

(defun read-options (fields prompt-name &optional
			    default quote numbers)
  "Read in a comma separated list of options from a given list."
  (let* ((default (or (if (and default (listp default))
			  (if (= (length default) 1)
			      (car default)
			    (mapconcat #'identity default ","))
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
	 (answer (split-string
		  (read-string prompt nil nil default)
		  ",")))
    (if (all-elems-in-list answer fields) answer
      (progn
	(read-string "Invalid entry!")
	(read-options fields prompt-name default quote numbers)))))

(defun read-func (prompt)
  (read-string prompt)
  (let ((exp (read)))
    (if (functionp exp) exp
      `(lambda (x) ,exp))))

(defun learn-from-table-direct (table &optional mode cue-fields recall-fields)
  "Same as learn-from-table but skip filtering and sorting"
  (learn-from-table table mode cue-fields recall-fields
		    (lambda (x) t) #'identity))

(defun get-mode-name (func-name)
  (cond ((string= func-name "prompt-flash") "flash")
	((string= func-name "prompt-query") "query")
	((string= func-name "prompt-multi") "multi")))

(defun learn-from-table (table &optional mode cue-fields recall-fields
			       filter-predicate sort-function)
  "Interactively initiate a learning session from a learning table."
  (let ((prompt-func (or mode (read-mode nil *last-mode*)))
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
    (setq *last-mode* (get-mode-name (format "%s" prompt-func)))
    (setq *last-table* table)
    (setq *last-cue-fields* cue-fields)
    (setq *last-recall-fields* recall-fields)
    (setq *last-filter-predicate* filter-predicate)
    (setq *last-sort-function* sort-function)
    (setq *start-time* (float-time))
    (setq *missed*
	  (drill (make-learning-items
		  (funcall sort-function
			   (remove-if-not filter-predicate table))
		  cue-fields recall-fields)
		 prompt-func)))
  (message (format "Session complete! (Time: %s seconds)"
		   (- (float-time) *start-time*))))

(defun review-missed ()
  "Drill the items you missed last session."
  (interactive)
  (setq *missed* (drill *missed* (read-mode nil *last-mode*)))
  (message "Review complete!"))

(defun learn-again (&optional table mode cue-fields recall-fields
			      filter-predicate sort-function)
  (interactive)
  "Repeat the last learning session with optional modifications."
  (learn-from-table (or mode *last-mode*)
		    (or table *last-table*)
		    (or cue-fields *last-cue-fields*)
		    (or recall-fields *last-recall-fields*)
		    (or filter-predicate *last-filter-predicate*)
		    (or sort-function *last-sort-function*)))

(defun learn-from-buffer (&rest rest)
  "Interactively initiate a learning session from a buffer."
  (interactive)
  (apply #'learn-from-table-direct
	 (append (list (load-learning-buffer))
		 rest)))

(defun learn-from-file (&rest rest)
  "Interactively initiate a learning session from a file."
  (interactive)
  (apply #'learn-from-table
	 (append (list (load-learning-file))
		 rest)))

(defun make-tsv-row (row)
  (mapconcat (lambda (item) (format "%s" item))
	     row "\t"))

(defun make-tsv (table)
  "Given a learning table, return a .tsv formatted string."
  (concat "#" (make-tsv-row (get-table-fields table)) "\n"
	  (mapconcat (lambda (row)
		       (make-tsv-row (get-vals row)))
		     table "\n")))

;; later make it output same table format as input
;; once more than just .tsv is supported that is
(defun modify-table (transform-func)
  "Perform some transformation function on a table in a buffer."
  (setf (buffer-string)
	(make-tsv (funcall
		   transform-func
		   (load-learning-buffer (current-buffer))))))

;; fix this so its not hardcoded for romanify
(defun modify-column (transform-func field)
  "Transform a specific column in a table in a buffer."
  (modify-table (lambda (table)
		  (let ((result '()))
		    (dolist (row table)
		      (setq
		       result
		       (append result
			       (list
				(plist-put row
					   field
					   (funcall
					    #'romanify
					;transform-func
					    (plist-get row field)))))))
		    result))))

(defun romanify-column ()
  "Convert a hiragana column into romaji."
  (interactive)
  (modify-column #'romanify
		 (car (read-cue-recall-fields
		       (load-learning-buffer (current-buffer))
		       "Romanify which column?"))))

(defun scramble-table ()
  "Scramble the table in the current buffer."
  (interactive)
  (modify-table #'scramble))

(defun slice-table (&optional first last)
  "Filter out a specified range of the table in the current buffer."
  (interactive)
  (modify-table (lambda (table)
		  (subseq
		   table
		   (1- (or first
			   (read-number "First row: " 1)))
		   (or last
		       (read-number "Last row: "
				    (length table)))))))

(defun slice-table-chunk ()
  "Slice the nth m-sized chunk of the table."
  (interactive)
  (let* ((chunk-size (read-number "Chunk size: " 10))
	 (n (1- (read-number "Chunk index (1+): " 1)))
	 (start-index (* chunk-size n))
	 (last (+ start-index chunk-size)))
    (slice-table (1+ start-index) last)))

(defun filter-table-field-matches ()
  "Filter rows whose given field matches a given regular expression."
  (interactive)
  (modify-table (lambda (table)
		  (let ((field
			 (make-keyword
			  (car(read-options
			       (get-table-fields table)
			       "Match which field? "))))
			(regex (read-regexp "Regexp match: ")))
		    (remove-if-not
		     (lambda (row)
		       (string-match-p regex
				       (format
					"%s" (getf row field))))
		     table)))))
