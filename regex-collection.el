;;; regex-collection.el --- A collection of useful regular expressions and helper functions

;; Filename: regex-collection.el
;; Description: A collection of useful regular expressions and helper functions
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2016, Joe Bloggs, all rites reversed.
;; Created: 2016-06-10 01:26:09
;; Version: 0.1
;; Last-Updated: Sun Jun 12 14:58:14 2016
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/regexp-collection
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires: ((ample-regexps "0.1") (dash "2.5.0"))
;;
;; Features that might be required by this library:
;;
;; ample-regexps dash 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;;
;; A collection of useful regular expressions and helper functions
;; 
;;;;;;;;

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;

;;
;; All of the above can be customized by:
;;      M-x customize-group RET regex-collection RET
;;

;;; Installation:
;;
;; Put regex-collection.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'regex-collection)

;;; NOTE: have a look at these tools: http://machinelearning.inginf.units.it/data-and-tools
;; Web application for learning regexps from examples: http://regex.inginf.units.it./

;;; History:

;;; Require
(require 'ample-regexps)
(require 'dash)

;;; Code:



;;;###autoload
;; simple-call-tree-info: CHECK  :passed:
(defun regex-collection-list-to-regex (lst strs &optional seps rxstrs rxseps)
  "Convert a list of numbers and corresponding strings to a regular expression.
LST should be a list of positive integers and cons cells of pairs of integers, 
and STRS should be either a list of strings, or a list of such lists.
A number, n, in position pos of LST will be exchanged for either the nth element 
of STRS (if STRS is a list of strings) or the nth element of the pos'th list
in STRS (if STRS is a list of lists). A regular expression will then be returned
which matches the sequence of strings produced. Any cons cells in LST will be exchanged
for a regexp which matches all strings corresponding to indices between the pair of 
numbers in the cons cell.
If RXSTRS is non-nil then the elements of STRS will be treated as regexps otherwise
they will be treated as strings and `regexp-quote' will be applied to them.
Similarly if RXSEPS is non-nil then the elements of SEPS will be treated as regexps."
  (setq strs (if (stringp (car strs))
		 (make-list (length lst) strs)
	       strs)
	seps (if (stringp seps)
		 (make-list (1- (length lst)) seps)
	       seps))
  (apply 'concat
	 (cl-loop for pos from 0 to (1- (length lst))
		  for item = (nth pos lst)
		  if (integerp item)
		  collect (if rxstrs (nth item (nth pos strs))
			    (regexp-quote (nth item (nth pos strs))))
		  else if (consp item)
		  collect (if rxstrs
			      (concat "\\(?:"
				      (mapconcat
				       'identity
				       (subseq (nth pos strs) (car item) (1+ (cdr item)))
				       "\\|")
				      "\\)")
			    (regexp-opt (subseq (nth pos strs) (car item) (1+ (cdr item)))))
		  else if (stringp item)
		  collect (if rxstrs item
			    (regexp-quote item))
		  if (and seps (< pos (1- (length lst))))
		  collect (if rxseps (nth pos seps)
			    (regexp-quote (nth pos seps))))))

(expectations
  (desc "list to regex")
  (expect "foo-1-3-[2-5]"
    (regex-collection-list-to-regex
     '("foo" 1 3 (2 . 5))
     (cl-loop for i from 0 upto 9 collect (number-to-string i))
     "-" nil nil))
  (expect "foo-1-3-\\(?:2\\|3\\|4\\|5\\)"
    (regex-collection-list-to-regex
     '("foo" 1 3 (2 . 5))
     (cl-loop for i from 0 upto 9 collect (number-to-string i))
     "-" t nil))
  (expect "foo\\s-1\\s-3\\s-[2-5]"
    (regex-collection-list-to-regex
     '("foo" 1 3 (2 . 5))
     (cl-loop for i from 0 upto 9 collect (number-to-string i))
     "\\s-" nil t))
  (expect "foo\\s-1\\s-3\\s-\\(?:2\\|3\\|4\\|5\\)"
    (regex-collection-list-to-regex
     '("foo" 1 3 (2 . 5))
     (cl-loop for i from 0 upto 9 collect (number-to-string i))
     "\\s-" t t)))

;;;###autoload
;; simple-call-tree-info: CHECK  :passed:
(defun regex-collection-lex-range (min max maxdigit &optional pad)
  "Return lists representing all sequences between MIN and MAX in lexicographic (dictionary) order.
MIN and MAX are lists of numbers representing sequences that can be ordered, e.g. decimal 
numbers (each item in the list represents a digit), words (each item represents a letter), 
dates (first item represents a day, second item represents a month, third item represents 
a year), etc. In each position 0 represents the lowest digit/letter/day/etc. 
The MAXDIGIT argument can be either a single positive integer representing the highest 
digit/letter/day/etc. or a list of such integers - one for each position in the MAX list 
 (so that different positions can have different numbers of digits, e.g. dates).
MIN cannot be longer than MAX; if shorter then resulting lists will be left-padded with PAD
 (note: PAD should not be a number or a list).
The return value is a list of lists, each representing a set of sequences between MIN and MAX.
The elements of the returned lists are either single numbers (representing digits/letters/days/etc.)
or cons cells representing a range of digits/letter/days/etc."
  (let* ((minlen (length min))
	 (maxlen (length max))
	 accum initdgts initlen)
    ;; make sure we have a separate list of digits for each position
    (if (integerp maxdigit)
	(setq maxdigit (make-list maxlen maxdigit)))
    ;; define some helper functions
    (cl-flet* ((numorzero (x) (cond ((numberp x) x)
				    ((consp x) (car x))
				    (t 0)))
	       (lastelem (lst) (car (last lst)))
	       (item (a b) (cond ((< a b) (cons a b))
				 ((= a b) a)
				 (t nil)))
	       ;; newparts returns new parts to be prepended at POS'th position
	       (newparts (pos initlen)
			 (let* ((dmax (nth pos maxdigit))
				(pmin (nth pos min))
				(pmin1 (1+ (numorzero pmin)))
				(pmax (nth pos max))
				(pmax1 (1- (numorzero pmax))))
			   (list pmin
				 (if (> pos initlen) (item pmin1 dmax))
				 (if (> pos initlen) (item 0 dmax) (item pmin1 pmax1))
				 (if (> pos initlen) (item 0 pmax1))
				 pmax)))
	       ;; prepend2parts prepends N to all lists in PARTS
	       (prepend2parts (n &rest parts) 
			      (-flatten-n 1
					  (cl-loop for part in parts
						   collect (cl-loop for lst in part
								    if lst collect (cons n lst))))))
      ;; check that MIN is lower than MAX
      (if (or (> minlen maxlen)
	      (and (= minlen maxlen)
		   (cl-loop for pos from 0 to (1- maxlen)
			    if (< (numorzero (nth pos max))
				  (numorzero (nth pos min)))
			    return t
			    else if (> (numorzero (nth pos max))
				       (numorzero (nth pos min)))
			    return nil)))
	  (error "MIN is >= than MAX"))
      ;; add initial left-padding to min if necessary
      (if (< minlen maxlen)
	  (setq min (append (make-list (- maxlen minlen) pad) min)))
      ;; extract initial digits that are the same for both MIN and MAX
      (setq initdgts (cl-loop for pos from 0 upto maxlen
			      for minval = (nth pos min)
			      while (equal minval (nth pos max))
			      collect minval)
	    initlen (length initdgts))
      ;; initial value of accumulator corresponding to final digit
      (let ((lastmin (lastelem min))
	    (lastmax (lastelem max))
	    (lastdgt (lastelem maxdigit)))
	(setq accum (list (list (list (item (numorzero lastmin) lastdgt)))
			  (list nil)
			  (list (if (> (- maxlen initlen) 1) (list (item 0 lastdgt))))
			  (list nil)
			  (list (if (> (- maxlen initlen) 1) (list (item 0 (numorzero lastmax))))))))
      ;; loop backward over digit positions, prepending appropriate digits as we go
      (cl-loop for pos downfrom (- maxlen 2) to initlen
	       for (a1 b1 c1 d1 e1) = accum  
	       for (a2 b2 c2 d2 e2) = (newparts pos initlen)
	       do (setq accum (list (prepend2parts a2 a1 b1)
				    (if (and (> pos initlen)
					     (not (= (numorzero a2) (numorzero b2))))
					(prepend2parts b2 c1))
				    (if (or (> pos initlen)
					    (> (numorzero e2) (1+ (numorzero a2))))
					(prepend2parts c2 c1))
				    (if (and (> pos initlen)
					     (not (= (numorzero d2) (numorzero e2))))
					(prepend2parts d2 c1))
				    (prepend2parts e2 d1 e1)))))
    ;; prepend initial identical digits    
    (if initdgts (mapcar (lambda (lst) (append initdgts lst))
			 (remove nil (-flatten-n 1 accum)))
      (remove nil (-flatten-n 1 accum)))))

;;;###autoload
;; simple-call-tree-info: CHECK  :passed:
(defun regex-collection-colex-range (min max maxdigit &optional pad)
  "Return lists representing all sequences between MIN and MAX in colexicographic order.
If MIN is shorter than MAX and then PAD will be used to right-pad the MIN list.
See `regex-collection-lex-range' for details on the MIN, MAX, and MAXDIGIT arguments.
This function calls `regex-collection-lex-range' after reversing the MIN and MAX lists, 
and then reverses each resulting list (handling padded values appropriately)."
  (let ((maxlen (length max))
	(minlen (length min)))
    ;; add initial right-padding to min if necessary
    (if (> maxlen minlen)
	(setq min (append min (make-list (- maxlen minlen) pad)))))
  ;; get lexicographic range on reversed lists, and return reverse of results
  (mapcar 'reverse (regex-collection-lex-range
		    (reverse min) (reverse max)
		    (if (listp maxdigit) (reverse maxdigit) maxdigit))))

(expectations
  (desc "lex/colex range")
  ;; lex tests
  (cl-loop for args in
	   '(((error) . ('(2) '(1)))
	     ((error) . ('(2) '(1) 2))
	     ((error) . ('(1 1) '(2) 2))
	     ('(((1 . 2))) . ('(1) '(2) 2))
	     ('((nil (1 . 2)) (1 (0 . 2)) (2 (0 . 2)))
	      . ('(1) '(2 2) 2))
	     ('(("foo" (1 . 2)) (1 (0 . 2)) (2 (0 . 2)))
	      . ('(1) '(2 2) 2 "foo")))
	   do (eval `(expect ,(car args)
		       (regex-collection-lex-range ,@(cdr args)))))
  ;; colex tests
  (cl-loop for args in
  	   '(((error) . ((2) (1)))
  	     ((error) . ((2) (1) 2))
  	     ((error) . ((1 1) (2) 2))
  	     ((error) . ((1 1) (2 nil) 2))
  	     ('(((1 . 2))) . ('(1) '(2) 2))
  	     ('(((1 . 2) nil) ((0 . 2) 1) ((0 . 2) 2))
  	      . ('(1) '(2 2) 2))
  	     ('(((1 . 2) "foo") ((0 . 2) 1) ((0 . 2) 2))
  	      . ('(1) '(2 2) 2 "foo")))
  	   do (eval `(expect ,(car args)
  		       (regex-collection-colex-range ,@(cdr args))))))

;; simple-call-tree-info: DONE  :passed:
(defsubst regex-collection-integerp (n)
  "Return non-nil if N is a string containing an integer."
  (and (stringp n)
       (string-equal (replace-regexp-in-string "^\\(0+\\)[1-9]" "" n nil nil 1)
		     (number-to-string (string-to-number n)))))

;; simple-call-tree-info: DONE
(defsubst regex-collection-charp (x)
  "Return non-nil if X is a string containing a single non-numeric char."
  (and (stringp x)
       (eq (length x) 1)
       (not (regex-collection-integerp x))))

;; simple-call-tree-info: CHECK
(defcustom regex-collection-word-lists
  '((month "Month"
	   (("jan" "january")
	    ("feb" "february")
	    ("mar" "march")
	    ("apr" "april")
	    ("may")
	    ("jun" "june")
	    ("jul" "july")
	    ("aug" "august")
	    ("sep" "sept" "september")
	    ("oct" "october")
	    ("nov" "november")
	    ("dec" "december")))
    (weekday "Weekday"
	     (("mon" "monday")
	      ("tue" "tues" "tuesday")
	      ("wed" "weds" "wednesday")
	      ("thu" "thurs" "thursday")
	      ("fri" "friday")
	      ("sat" "saturday")
	      ("sun" "sunday"))))
  "Lists of words."
  :group 'regex-collection
  :type '(repeat (list (symbol :tag "Symbol")
		       (string :tag "Description")
		       (repeat (repeat (string :tag "Word"))))))

;; simple-call-tree-info: CHECK
(defun regex-collection-word-index (symb str &optional invp)
  "Return integer index of STR for word list corresponding to SYMB.
SYMB should be one of the symbols in `regex-collection-word-lists'
If INVP is non-nil then STR is assumed to contain an integer,
and the corresponding list of words will be returned which can be fed into `regexp-opt'.
If STR does not match any of the words then nil is returned."
  (let ((case-fold-search t)
	(wordlists (cl-third (assoc symb regex-collection-word-lists))))
    (if (stringp str)
	(cl-loop for i from 0 upto 11
		 if (string-match (regexp-opt (nth i wordlists)) str)
		 return i)
      (if (integerp str) (nth str wordlists)))))

;; simple-call-tree-info: DONE  :passed:
(defsubst regex-collection-length (str)
  "Return length of string STR as a string."
  (number-to-string (length str)))

;; simple-call-tree-info: DONE  :passed:
(defsubst regex-collection-consp (c &optional pred)
  "Return non-nil if C is a cons cell that is not a nil terminated list.
If PRED (a function) is provided then also check that both the car and cdr satisfy it,
i.e. that (funcall PRED (car C)) and (funcall PRED (cdr C)) both return non-nil."
  (and (listp c) (not (listp (cdr c)))
       (or (not pred)
	   (and (funcall pred (car c))
		(funcall pred (cdr c))))))

(expectations
  (desc "simple inline functions")
  (expect (true) (regex-collection-integerp "214"))
  (expect nil (regex-collection-integerp "foo"))
  (expect "3" (regex-collection-length "214"))
  (expect (true) (regex-collection-consp '(1 . 2)))
  (expect (true) (regex-collection-consp '(1 . 2) 'numberp))
  (expect (true) (regex-collection-consp '("1" . "2") 'stringp))
  (expect nil (regex-collection-consp '(1 2)))
  (expect 0 (regex-collection-word-index 'month "jan"))
  (expect 1 (regex-collection-word-index 'month "February"))
  (expect 11 (regex-collection-word-index 'month "Dec"))
  (expect 0 (regex-collection-word-index 'weekday "Monday"))
  (expect 1 (regex-collection-word-index 'weekday "tues"))
  (expect 5 (regex-collection-word-index 'weekday "saturday")))

;; simple-call-tree-info: CHECK  :passed:
(defun regex-collection-fieldrx (field)
  "Return regexp matching values implied by FIELD arg.
This is used by `regex-collection-split-into-fields'.
FIELD can be either:
 - a list of strings
 - a number
 - a string containing a number
 - a regular expression
 - a cons cell containing a pair of numbers
 - a cons cell containing a pair of numbers as strings
 - a cons cell containing a pair of chars as strings
 - a symbol corresponding to a member of `regex-collection-word-lists'"
  (cl-flet ((errorf (f) (error "Invalid field argument: %s" f))) ;check if arg is cons cell but not a list
    (cond
     ((integerp field) ;; FIELD arg is single integer
      (let ((num (number-to-string field)))
	(concat "[0-9]\\{1"
		(if (> (length num) 1) (concat "," (regex-collection-length num))) "\\}")))
     ;; FIELD refers to a member of `regex-collection-word-lists'
     ((and field (symbolp field))
      (let ((lst (cl-third (assoc field regex-collection-word-lists))))
	(regexp-opt (-flatten lst))))
     ;; FIELD arg is single string containing an integer
     ((and (stringp field) (string-match "[0-9]+" field))
      (concat "[0-9]\\{" (regex-collection-length field) "\\}"))
     ;; if FIELD is any other string its assumed to contain a regexp,
     ;; and is wrapped in a shy group before returning
     ((stringp field) (concat "\\(?:" field "\\)"))
     ;; FIELD is '(N . M) where N & M are integers
     ((regex-collection-consp field 'integerp)
      (if (> (car field) (cdr field)) (errorf field))
      (let ((carlen (regex-collection-length (number-to-string (car field))))
	    (cdrlen (regex-collection-length (number-to-string (cdr field)))))
	(concat "[0-9]\\{" carlen (unless (string-equal carlen cdrlen) (concat "," cdrlen)) "\\}")))
     ;; FIELD is '(N . M) where N & M are strings (assumed to contain integers)
     ((regex-collection-consp field 'regex-collection-integerp)
      (if (> (string-to-number (car field)) (string-to-number (cdr field))) (errorf field))
      (let ((carlen (regex-collection-length (car field)))
	    (cdrlen (regex-collection-length (cdr field))))
	(concat "[0-9]\\{" carlen (unless (string-equal carlen cdrlen) (concat "," cdrlen)) "\\}")))
     ;; FIELD is '(N . M) where N & M are strings of length one (assumed to contain letters)
     ((regex-collection-consp field 'regex-collection-charp)
      (if (> (string-to-char (car field)) (string-to-char (cdr field)))
	  (errorf field)
	(if (string-equal (car field) (cdr field))
	    (car field)
	  (concat "[" (car field) "-" (cdr field) "]\\{1\\}"))))
     ;; FIELD is a list of strings
     ((and (listp field)
	   (-all-p 'stringp field))
      (regexp-opt field))
     ;; no other possibilities are allowed
     (t (errorf field)))))

(expectations
  (desc "fieldrx")
  (cl-loop for args in
	   '(("[0-9]\\{1,2\\}" . '(1 . 10)) 
	     ("[0-9]\\{2\\}" . '("01" . "10")) 
	     ("[0-9]\\{1,2\\}" . 32)
	     ("[0-9]\\{2\\}" . "02")
	     ("\\(?:foo\\|baa\\)"  . "foo\\|baa") 
	     ("\\(?:\\(foo\\|baa\\)\\)"  . "\\(foo\\|baa\\)") 
	     ("[a-z]\\{1\\}" . '("a" . "z")) 
	     ("a" . '("a" . "a"))				   
	     ("\\(?:choo\\|foo\\|man\\)" . '("foo" "man" "choo"))
	     ("\\(?:a\\(?:pr\\(?:il\\)?\\|ug\\(?:ust\\)?\\)\\|dec\\(?:ember\\)?\\|feb\\(?:ruary\\)?\\|j\\(?:an\\(?:uary\\)?\\|u\\(?:ly\\|ne\\|[ln]\\)\\)\\|ma\\(?:rch\\|[ry]\\)\\|nov\\(?:ember\\)?\\|oct\\(?:ober\\)?\\|sep\\(?:t\\(?:ember\\)?\\)?\\)"
	      . 'month))
	   do (eval `(expect ,(car args)
		       (regex-collection-fieldrx ,(cdr args))))))

;; simple-call-tree-info: DONE
(defun regex-collection-split-into-fields (str &optional seps fieldvals noerror)
  "Helper function to split STR into list of separate fields.

SEPS can be a regexp matching the separators between fields, or a list of such
regexps (one for each consecutive separator). FIELDVALS is either a list of field 
values, or a single field value for all fields. See `regex-collection-fieldrx' 
for more information about the FIELDVALS argument.
Only one of SEPS and FIELDVALS is necessary, but if both are supplied then FIELDVALS will 
be used to extract the values and the implied separators will be checked against SEPS. 
If both SEPS and FIELDVALS are missing then the separators will be taken to be contiguous 
sequences of non-word chars, and the text between these sequences will be taken as the fields.
If there are missing fields, i.e. values of FIELDVALS or SEPS that don't match STR, 
then an error will be thrown unless NOERROR is non-nil in which case a nil value will 
be returned for that field.

The function returns a list whose first element is the list of fields, and whose
second element is the list of separators."
  (let (str2 fieldrx (seppos 0) seps2)
    (cond ((and (null fieldvals) (stringp seps))
	   (setq str2 (split-string str seps)
		 seps2 (make-list (1- (length str2)) seps)))
	  ((and (null fieldvals) seps (listp seps))
	   (string-match (car seps) str 0)
	   (setq str2
		 (append
		  (list (substring str 0 (match-beginning 0)))
		  (cl-loop for sep in (cdr seps)
			   for seppos = (or (match-end 0) seppos)
			   for nextpos = (string-match sep str seppos)
			   if nextpos collect (substring str seppos nextpos)
			   else if noerror collect nil
			   else do (error "No match for separator %S" sep))
		  (list (substring str (match-end 0))))
		 seps2 seps))
	  ;; if no separators or fieldvals are supplied we have to infer them..
	  ((and (null fieldvals) (null seps))
	   (while (string-match "\\W+" str seppos)
	     (setq seps2 (append seps2 (list (match-string 0 str)))
		   str2 (append str2 (list (substring str seppos (match-beginning 0))))
		   seppos (match-end 0)))
	   (setq str2 (append str2 (list (substring str seppos)))))
	  ;; if we have separate FIELDVALS for each position
	  ((and (listp fieldvals) (listp (cdr fieldvals)))
	   ;; loop over each field, collecting the separators and fields into SEPS and STR2 respectively
	   (cl-loop for i from 0 upto (1- (length fieldvals))
		    for fieldval = (nth i fieldvals)
		    for fieldrx = (regex-collection-fieldrx fieldval)
		    if (string-match fieldrx str seppos)
		    do (if (> seppos 0)
			   (let ((sep2 (substring str seppos (match-beginning 0)))
				 (sep (if (stringp seps) seps (nth (1- i) seps))))
			     (if seps	;if we already have seps check they match
				 (unless (string-match-p sep sep2)
				   (error "No match for separator %S: %S" i sep))
			       (setq seps2 (append seps2 (list sep2))))))
		    (setq str2 (append str2 (list (match-string 0 str)))
			  seppos (match-end 0))
		    else if noerror do (setq str2 (append str2 (list nil))
					     seps2 (append seps2 (list nil)))
		    else do (error "No match for field %S: %S" (1+ i) fieldval))
	   (if seps
	       (setq seps2 (if (stringp seps)
			       (make-list (1- (length str2)) seps)
			     seps))))
	  ;; otherwise use the same FIELDVALS for all positions
	  (fieldvals
	   (setq fieldrx (regex-collection-fieldrx fieldvals))
	   (let ((i 0))
	     (while (string-match fieldrx str seppos)
	       (if (> seppos 0)
		   (let ((sep2 (substring str seppos (match-beginning 0)))
			 (sep (if (stringp seps) seps (nth (1- i) seps))))
		     (if seps  ;if we already have seps check they match
			 (unless (string-match-p sep sep2)
			   (error "No match for separator %S: %S" i sep))
		       (setq seps2 (append seps2 (list sep2))))))
	       (setq str2 (append str2 (list (match-string 0 str)))
		     seppos (match-end 0)
		     i (1+ i))))
	   (if seps
	       (setq seps2 (if (stringp seps)
			       (make-list (1- (length str2)) seps)
			     seps))))
	  (t (error "No value for seps or fieldvals")))
    (list str2 seps2)))

(expectations
  (desc "split-into-fields")
  (cl-loop for args in
	   '(
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" "/" nil))
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" "/" '(1 . 10000)))
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" nil '(1 . 10000)))
	     ('(("01" "02" "2010") ("/" "/")) . ("1/01/02/2010" nil '("01" . "10000")))
	     ('(("1" "01" "02" "2010") ("/" "/" "/")) . ("1/01/02/2010" "/" nil))
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" nil 10000))
	     ('(("01" "02" "20" "10") ("/" "/" "")) . ("01/02/2010" nil "10"))
	     ('(("201") nil) . ("01/02/2010" nil "100"))
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" "/" '("10" "10" "1000"))) 
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" nil nil))
	     ('(("01" "March" "2010") ("/" "/")) . ("01/March/2010" nil '("10" month "1000")))
	     )
	   do (eval `(expect ,(car args)
		       (regex-collection-split-into-fields
			,@(cdr args))))))

;; TODO: what about different orderings apart from lex and colex, e.g. american style dates: 01/31/2010 = Jan 31 2010
;; (could have an order arg - a list of numbers indicating order of fields, then rearrange the fields and get lex range)
;; simple-call-tree-info: TODO
(defun regex-collection-range-regex (start end &optional fieldvals seps rxseps colex)
  "Return a regexp matching all strings between START and END strings, in lexicographic order.
The START and END strings are treated as sequences of fields and separators, e.g: \"01/01/2000\".
By default any non-word chars will be treated as separators, and the fields can be numbers whose 
min values are in START and whose max values are in END, or elements of one of the lists 
in `regex-collection-word-lists'.
Alternatively you can specify the allowable field values and separators with the FIELDVALS
and SEPS args, see `regex-collection-split-into-fields' for more details.
If RXSEPS is non-nil then SEPS will be treated as regular expressions matching the separators, 
otherwise they will be matched literally.
If COLEX is non-nil then colexicographic order will be used instead of lexicographic order.

Examples: 
  (regex-collection-range-regex \"192.168.1.1\" \"192.168.2.255\") = matches a range of IP addresses
  (regex-collection-range-regex \"01/01/2000\" \"31/12/2015\" nil nil nil t) = matches a range of dates
  (regex-collection-range-regex \"Sat 1st Jan 2000\" \"Thurs 31rd Dec 2015\" \"\\(?:\\|rd\\|st\\|nd\\)\\W+\" nil nil t) 
  = matches a range of dates"
  ;; check FIELDVALS
  (cl-flet ((checkf (f) (or (integerp f)
			    (regex-collection-integerp f)
			    (regex-collection-consp f 'integerp)
			    (regex-collection-consp f 'regex-collection-integerp)
			    (regex-collection-consp f 'regex-collection-charp)
			    (and f (symbolp f) (assoc f regex-collection-word-lists))
			    (and (listp f) (-all-p 'stringp f)))))
    (unless (or (checkf fieldvals)
		(and (listp fieldvals)
		     (listp (cdr fieldvals))
		     (-all-p (lambda (f) (checkf f)) fieldvals)))
      (error "Invalid field value: %s" fieldvals)))
  ;; fix SEPS
  (unless rxseps (if (stringp seps)
		     (regexp-opt (list seps))
		   (mapcar (lambda (x) (regexp-opt (list x))) seps)))
  ;; split START and END into their separate parts
  (let (start2 end2 fieldmax fieldmaxs start3 end3)
    (setq start2 (if (consp start)
		     (if fieldvals
			 (let* ((idxs (mapcar '1- (cdr start)))
				(lst (regex-collection-split-into-fields
				      (car start) seps (-select-by-indices idxs fieldvals) t))
				(n 0))
			   (cl-loop for i from 0 upto (1- (length fieldvals))
				    for pos = (position i idxs)
				    collect (and pos (nth pos (car lst))) into fields
				    collect (and pos (nth pos (cl-second lst))) into separators
				    finally return (list fields separators)))
		       (regex-collection-split-into-fields (car start) seps))
		   (regex-collection-split-into-fields start seps fieldvals t))
	  end2 (if (consp end)
		   (if fieldvals
		       (let* ((idxs (mapcar '1- (cdr end)))
			      (lst (regex-collection-split-into-fields
				    (car end) seps (-select-by-indices idxs fieldvals) t))
			      (n 0))
			 (cl-loop for i from 0 upto (1- (length fieldvals))
				  for pos = (position i idxs)
				  collect (and pos (nth pos (car lst))) into fields
				  collect (and pos (nth pos (cl-second lst))) into separators
				  finally return (list fields separators)))
		     (regex-collection-split-into-fields (car end) seps))
		 (regex-collection-split-into-fields end seps fieldvals t)))
    ;; first elements of start2 & end2 now contain the list of fields, second elements contain the separators
    ;; check that they have the same number of fields
    (unless (= (length (car start2)) (length (car end2)))
      (error "Different number of fields in START and END args: %S %S" start end))
    ;; remove initial and final empty strings if SEPS was used to find fields
    (if seps
	(progn
	  (if (string-equal (car (last (car start2))) "")
	      (setf (car start2) (-butlast (car start2))))
	  (if (string-equal (car (last (car end2))) "")
	      (setf (car end2) (-butlast (car end2))))
	  (if (string-equal (car (car start2)) "")
	      (setf (car start2) (cdr (car start2))))
	  (if (string-equal (car (car end2)) "")
	      (setf (car end2) (cdr (car end2))))))
    (setq start3 (make-list (length (car start2)) 0)
	  end3 (make-list (length (car end2)) 0))
    ;; calculate maximum value for each field, and change values of start and end lists to integers
    (cl-flet* ((errorf (f) (error "Invalid field argument: %s" f))
	       (errorm (min max) (error "Invalid min and max values: %s, %s" min max))
	       (setidxs (min max i) (setf (nth i start3) min (nth i end3) max)) ;set indices for obtaining range
	       ;; check if min & max are members of a list in `regex-collection-word-lists'
	       (getsymb (min max)
			(and (stringp min)
			     (stringp max)
			     (save-match-data 
			       (cl-loop for (symb desc lst) in regex-collection-word-lists
					for regex = (regexp-opt (-flatten lst))
					if (and (posix-string-match regex min)
						(= (match-beginning 0) 0)
						(= (match-end 0) (length min))
						(posix-string-match regex max)
						(= (match-beginning 0) 0)
						(= (match-end 0) (length max)))
					return symb))))
	       (getval (i min max vals)	;get value corresponding to index i
		       (cond ((and vals (listp vals) (listp (cdr vals))) ;string in list
			      (nth i vals))
			     ;; vals can be a symbol referring to an element of `regex-collection-word-lists'
			     ((and vals (symbolp vals) (assoc vals regex-collection-word-lists))
			      (nth i (cl-third (assoc vals regex-collection-word-lists))))
			     ;; integers (vals can be an integer, string containing an integer, 
			     ;;           or a cons cell of those things)
			     ((regex-collection-consp
			       (cons min max) 'regex-collection-integerp)
			      (let ((minval2 (string-to-number min))
				    (maxval2 (string-to-number max)))
				(cond ((or (integerp vals)
					   (regex-collection-integerp vals))
				       (number-to-string i))
				      ((regex-collection-consp vals 'integerp)
				       (number-to-string (+ i (car vals))))
				      ((regex-collection-consp vals 'regex-collection-integerp)
				       (number-to-string (+ i (string-to-number (car vals)))))
				      ((null vals) (number-to-string (+ i minval2)))
				      (t (errorm min max)))))
			     ;; chars
			     ((regex-collection-consp
			       (cons min max) 'regex-collection-charp)
			      ;; vals must be a cons cell, since other case (vals is a list)
			      ;; has already been dealth with
			      (unless (regex-collection-consp vals 'regex-collection-charp)
				(errorm min max))
			      (char-to-string (+ (string-to-char min) i)))
			     ;; nil values
			     ((and (regex-collection-integerp minval) (null maxval))
			      (number-to-string (+ i (if vals 0 (string-to-number minval)))))
			     ((null minval)
			      (number-to-string i))
			     ;; check if min and max are members of one of the lists in `regex-collection-word-lists'
			     ;; (this might not always get the right word list)
			     ((getsymb min max)
			      (let ((symb (getsymb min max)))
				(regex-collection-word-index symb i t)))
			     ;; anything else
			     (t (errorm min max)))))
      (setq fieldmaxs ;for each field get the number of different possible values for that field
	    (cl-loop for i from 0 upto (1- (length (car start2)))
		     for fieldval = (if (and fieldvals (listp fieldvals) (listp (cdr fieldvals)))
					(nth i fieldvals) fieldvals)
		     for minval = (nth i (car start2))
		     for maxval = (nth i (car end2))
		     if fieldval collect
		     ;; if we have a fieldval for this field then use that to find the number of possible values
		     (setq fieldmax
			   (cond ((integerp fieldval) fieldval)
				 ((regex-collection-integerp fieldval)
				  (string-to-number fieldval))
				 ((regex-collection-consp fieldval 'integerp)
				  (if (> (car fieldval) (cdr fieldval)) (errorf fieldval))
				  (- (cdr fieldval) (car fieldval)))
				 ((regex-collection-consp fieldval 'regex-collection-integerp)
				  (if (> (string-to-number (car fieldval))
					 (string-to-number (cdr fieldval)))
				      (errorf fieldval))
				  (- (string-to-number (cdr fieldval))
				     (string-to-number (car fieldval))))
				 ((regex-collection-consp fieldval 'regex-collection-charp)
				  (- (string-to-char (cdr fieldval))
				     (string-to-char (car fieldval))))
				 ((and (listp fieldval) (listp (cdr fieldval)))
				  (1- (length fieldval)))
				 ((and fieldval (symbolp fieldval))
				  (1- (length (cl-third (assoc fieldval regex-collection-word-lists)))))
				 (t (errorf fieldval))))
		     else collect
		     ;; otherwise (no fieldval), use the values from the start and end lists to
		     ;; calculate the number of possible values
		     (setq fieldmax 
			   (cond ((regex-collection-consp
				   (cons minval maxval) 'regex-collection-integerp)
				  (- (string-to-number maxval)
				     (string-to-number minval)))
				 ((and (null minval)
				       (regex-collection-integerp maxval))
				  (string-to-number maxval))
				 ((getsymb minval maxval)
				  (let ((symb (getsymb minval maxval)))
				    (setq fieldval symb)
				    (1- (length
					 (cl-third (assoc symb regex-collection-word-lists))))))
				 (t (errorm minval maxval))))
		     end
		     ;; then calculate new start and end values (for START3 & END3 lists)
		     do (cond (	;; minval and maxval are from a word list supplied as arg
			       (and fieldval (listp fieldval) (listp (cdr fieldval)))
			       (setidxs (cl-position minval fieldval :test 'equal)
					(cl-position maxval fieldval :test 'equal) i))
			      ;; minval and maxval are members of a saved word list
			      ((and fieldval (symbolp fieldval))
			       (setidxs (regex-collection-word-index fieldval minval)
					(regex-collection-word-index fieldval maxval) i))
			      ;; minval & maxval are integers
			      ((regex-collection-consp (cons minval maxval)
						       'regex-collection-integerp)
			       (let ((minval2 (string-to-number minval))
				     (maxval2 (string-to-number maxval)))
				 (if (> minval2 maxval2) (errorm minval maxval))
				 (cond ((null fieldval)
					(setidxs 0 fieldmax i))
				       ((integerp fieldval)
					(if (or (> maxval2 fieldmax)
						(< minval2 0))
					    (errorm minval maxval))
					(setidxs minval2 maxval2 i))
				       ((regex-collection-consp fieldval)
					;; since minval and maxval are numbers we know that elements of fieldval
					;; must be numbers or strings containing numbers
					(let ((fmin (if (stringp (car fieldval))
							(string-to-number (car fieldval))
						      (car fieldval)))
					      (fmax (if (stringp (cdr fieldval))
							(string-to-number (cdr fieldval))
						      (cdr fieldval))))
					  (if (or (< minval2 fmin)
						  (> maxval2 fmax))
					      (errorm minval maxval))
					  (setidxs (- minval2 fmin) (- maxval2 fmin) i)))
				       (t (errorm minval maxval)))))
			      ;; minval & maxval are chars as strings
			      ((regex-collection-consp (cons minval maxval)
						       'regex-collection-charp)
			       ;; if minval and maxval are strings containing chars then fieldval must be a cons cell
			       ;; of strings containing chars (other cases already accounted for)
			       (unless (regex-collection-consp fieldval 'regex-collection-charp)
				 (errorm minval maxval))
			       (let ((minval2 (string-to-char minval))
				     (maxval2 (string-to-char maxval))
				     (fmin (string-to-char (car fieldval)))
				     (fmax (string-to-char (cdr fieldval))))
				 (if (or (> minval2 maxval2)
					 (< minval2 fmin)
					 (> maxval2 fmax))
				     (errorm minval maxval))
				 (setidxs (- minval2 fmin) (- maxval2 fmax) i)))
			      ;; one of minval or maxval is nil
			      ((and (or (null minval)
					(regex-collection-integerp minval))
				    (or (null maxval)
					(regex-collection-integerp maxval)))
			       (setidxs (and minval (string-to-number minval))
					(and maxval (string-to-number maxval)) i))
			      ;; minval or maxval are members of an unspecified saved wordlist 
			      ((getsymb min max)
			       (let ((symb (getsymb min max)))
				 (setidxs (regex-collection-word-index symb minval)
					  (regex-collection-word-index symb maxval) i)))
			      (t (errorm minval maxval)))))
      ;; get lists in range and convert back into regexp 
      (substring
       (cl-loop for lst in (if colex (regex-collection-colex-range start3 end3 fieldmaxs)
			     (regex-collection-lex-range start3 end3 fieldmaxs))
		;; create separate regexp for each list, and join them together
		concat (cl-loop for i from 0 upto (1- (length (car start2)))
				;; get string matching index
				concat (let ((fieldval (and fieldvals
							    (if (and (listp fieldvals)
								     (listp (cdr fieldvals)))
								(nth i fieldvals)
							      fieldvals)))
					     (minval (nth i (car start2)))
					     (maxval (nth i (car end2)))
					     (idx (nth i lst)))
					 (cond ((integerp idx) ;idx can be a single index
						;; TODO: 
						(let ((val (getval idx minval maxval fieldval)))
						  (if (listp val) (regexp-opt val) val)))
					       ;; or a cons cell indicating a range of indices
					       ((regex-collection-consp idx 'integerp)
						(regexp-opt
						 (-flatten
						  (cl-loop for j from (car idx) upto (cdr idx)
							   collect (getval j minval maxval fieldval)))))
					       ;; or nil
					       ((null idx) nil)))
				;; add separator 
				concat (if (nth i (cl-second start2))
					   (concat "\\(?:"
						   (if rxseps (nth i (cl-second start2))
						     (regexp-opt (list (nth i (cl-second start2))
								       (nth i (cl-second end2)))))
						   "\\)")))
		concat "\\|") 0 -2))))

(expectations
  (desc "range-regex")
  (cl-loop for args in
	   '(('("0.0.0.0" "1.2.3.4" "9.9.9.9") . ("0.0.0.0" "255.255.255.255"))
	     ('("0" "512" "1023") . ('("0" 4) "1023" '(9 9 9 9)))
	     ('("July 23rd" "July 24th" "October 25th") . ("July 23rd" "October 25th" nil '(" " "rd\\|th\\|st\\|nd") t))
	     ('("23rd July 1990" "24th July 1991" "31st August 1999" "25th October 2000") .
	      ("23rd July 1990" "25th October 2000" '((1 . 31) ("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") (1990 . 2000)) '("rd \\|th \\|st \\|nd " " ") t t))
	     ('("23rd July 1990" "24th July 1991" "31st August 1999" "25th October 2000") .
	      ("23rd July 1990" "25th October 2000" '((1 . 31) month (1990 . 2000)) '("rd \\|th \\|st \\|nd " " ") t t))
	     ('("Monday 23rd July 1990" "Tues 24th July 1991" "Weds 31st August 1999" "Friday 25th October 2000") .
	      ("Monday 23rd July 1990" "Friday 25th October 2000" '(weekday (1 . 31) month (1990 . 2000)) '(" " "rd \\|th \\|st \\|nd " " ") t t)))
	   do (eval `(expect (no-error)
		       (cl-loop for str in ,(car args)
				unless (string-match (regex-collection-range-regex ,@(cdr args)) str)
				do (error "No match for %s" str)))))
  (cl-loop for args in
  	   '(('("0.0.0.0" "1.2.3.4" "9.9.9.a") . ("0.0.0.0" "9.9.9.9" '(9 9 9 9)))
  	     ('("0" "512" "1023") . ('("10" 3 4) "1023" '(9 9 9 9))))
  	   do (eval `(expect (error)
  		       (cl-loop for str in ,(car args)
  				unless (string-match (regex-collection-range-regex ,@(cdr args)) str)
  				do (error "No match for %s" str))))))


;; Following functions are for optimizing regexps

;; - recurse into each subexpression
;; --- find matching bracket
;; --- call recursively on contents of subexpression
;; --- optimize results, using only top-level \\|'s

;; simple-call-tree-info: DONE
(defun regex-collection-reverse (str)
  "Reverse string STR."
  (concat (reverse (string-to-list str))))

;; simple-call-tree-info: CHECK  
(cl-defun regex-collection-subexp (rx &optional lastp pos opener)
  "Return a list containing the first subexpression in regexp RX after POS.
If LASTP is non nil return the last subexpression of RX ending before POS.
If there are no subexpressions in RX then return nil.
If OPENER is non-nil then it should be a regexp matching the opening parenthesis
of the subexpression to extract, otherwise any subexpression will be extracted.

First element of returned list is the opening parenthesis and any associated
question mark and number. Second element is the contents of the subexpression.
Third element is the closing parenthesis. Fourth element is the position in RX
at which the subexpression starts. Fifth element is the position in RX at which 
the subexpression ends. Sixth element is the number of opening parentheses within
of the subexpression, i.e. the depth of the deepest subexpression.

If LASTP is nil and RX contains a half open subexpression, the third element of 
the returned list will be nil and the fourth element will be the length of RX. 
If LASTP is non-nil and RX contains a half closed subexpression, the first 
element of the returned list will be nil, and the fifth element will be the
position after the last closing parenthesis.
If all other cases return nil."
  (let* ((rx (if lastp (regex-collection-reverse rx) rx))
	 (pos (if lastp (if pos (if (= pos 0) (length rx) (- pos)) 0)
		(or pos 0)))
	 (openp (string-match (if lastp ")\\\\" (or opener "\\\\(\\(?:\\?\\(?:[0-9]*\\):\\)?"))
			      rx pos))
	 (openparen (if openp (match-string 0 rx)))
	 (openpos (if openp (match-beginning 0)))
	 (parenrx (if lastp "\\(?::[0-9]*\\?\\)?(\\\\\\|)\\\\" "\\\\(\\|\\\\)"))
	 (parenstr1 (if lastp ")\\" "\\("))
	 (parenstr2 (if lastp "(\\" "\\)"))
	 (depth 1) (maxdepth 0) (type t) paren (notstart t))
    (when openp
      (setq pos openpos maxdepth 1)
      (while (and (> depth 0)
		  (setq pos (string-match parenrx rx (+ pos (if paren (length paren) 2)))
			paren (if pos (match-string 0 rx))
			str (if pos (substring paren -2))
			type (if pos (cond ((string-equal str parenstr1) 1)
					   ((string-equal str parenstr2) -1)
					   (t nil))))
		  ;; stop if we've reached user specified opener when searching backwards 
		  (setq notstart (not (and opener
					   lastp
					   (string-match opener (regex-collection-reverse paren))))))
	(setq depth (+ depth (or type 0))
	      maxdepth (+ maxdepth (max 0 type))))
      (if (not notstart)
	  ;; if we found user specified opener searching backwards, then search forwards to get rest of subexp
	  (regex-collection-subexp (regex-collection-reverse rx)
				   nil (- (length rx) pos (length paren)) opener)
	(list (if lastp (and paren (regex-collection-reverse paren)) openparen)
	      (funcall (if lastp 'regex-collection-reverse 'identity)
		       (substring rx (+ openpos (length openparen))
				  (if type (match-beginning 0) (length rx))))
	      (if (or lastp type) "\\)")
	      (if lastp (if type (- (length rx) (match-end 0)) 0) openpos)
	      (if lastp (- (length rx) openpos) (if type (match-end 0) (length rx)))
	      maxdepth)))))

(expectations
  (desc "subexp")
  (expect nil (regex-collection-subexp "zzzzz"))
  (expect nil (regex-collection-subexp "zzzzz" t))
  (expect '("\\(" "bbb" "\\)" 3 10 1) (regex-collection-subexp "aaa\\(bbb\\)ccc"))
  (expect '("\\(?:" "bbb" "\\)" 3 12 1) (regex-collection-subexp "aaa\\(?:bbb\\)ccc"))
  (expect '("\\(?3:" "bbb" "\\)" 3 13 1) (regex-collection-subexp "aaa\\(?3:bbb\\)ccc"))  
  (expect '("\\(" "bbb" "\\)" 3 10 1) (regex-collection-subexp "aaa\\(bbb\\)ccc\\(ddd\\)eee"))
  (expect '("\\(" "ddd" "\\)" 13 20 1) (regex-collection-subexp "aaa\\(bbb\\)ccc\\(ddd\\)eee" nil 10))
  (expect '("\\(" "bbb\\(ccc\\)ddd" "\\)" 3 20 2) (regex-collection-subexp "aaa\\(bbb\\(ccc\\)ddd\\)eee"))
  (expect '("\\(" "bbb" nil 3 8 1) (regex-collection-subexp "aaa\\(bbb"))
  (expect '(nil "" "\\)" 0 2 1) (regex-collection-subexp "\\)+?" t))
  (expect '("\\(?:" "bbb" nil 3 10 1) (regex-collection-subexp "aaa\\(?:bbb"))
  (expect '("\\(?2:" "bbb" nil 3 11 1) (regex-collection-subexp "aaa\\(?2:bbb"))  
  (expect '("\\(" "bbb\\(ccc" nil 3 13 2) (regex-collection-subexp "aaa\\(bbb\\(ccc"))
  (expect '("\\(" "bbb" "\\)" 3 10 1) (regex-collection-subexp "aaa\\(bbb\\)ccc" t))
  (expect '("\\(?:" "bbb" "\\)" 3 12 1) (regex-collection-subexp "aaa\\(?:bbb\\)ccc" t))
  (expect '("\\(?1:" "bbb" "\\)" 3 13 1) (regex-collection-subexp "aaa\\(?1:bbb\\)ccc" t))
  (expect '("\\(" "ddd" "\\)" 13 20 1) (regex-collection-subexp "aaa\\(bbb\\)ccc\\(ddd\\)eee" t))
  (expect '("\\(" "bbb" "\\)" 3 10 1)
    (regex-collection-subexp "aaa\\(bbb\\)ccc\\(ddd\\)eee" t 10))
  (expect '("\\(" "bbb\\(ccc\\)ddd" "\\)" 3 20 2)
    (regex-collection-subexp "aaa\\(bbb\\(ccc\\)ddd\\)eee" t))
  (expect '(nil "aaa\\)bbb" "\\)" 0 10 2) (regex-collection-subexp "aaa\\)bbb\\)ccc" t))
  (expect '("\\(?:" "eee" "\\)" 6 15 1)
    (regex-collection-subexp "aaa\\(b\\(?:eee\\)bb\\)ccc" nil 0 "\\\\(\\?:"))
  (expect '("\\(?:" "eee" "\\)" 6 15 1)
    (regex-collection-subexp "aaa\\(b\\(?:eee\\)bb\\)ccc" t 20 "\\\\(\\?:"))
  (expect '("\\(?3:" "eee" "\\)" 6 16 1)
    (regex-collection-subexp "aaa\\(b\\(?3:eee\\)bb\\)ccc" nil 0 "\\\\(\\?3:"))
  (expect '("\\(?3:" "eee" "\\)" 6 16 1)
    (regex-collection-subexp "aaa\\(b\\(?3:eee\\)bb\\)ccc" t 20 "\\\\(\\?3:"))
  (expect '("\\(?3:" "eee" "\\)" 6 16 1)
    (regex-collection-subexp "aaa\\(b\\(?3:eee\\)bb\\)ccc" nil 0 "\\\\(\\?[0-9]:"))
  (expect '("\\(?3:" "eee" "\\)" 6 16 1)
    (regex-collection-subexp "aaa\\(b\\(?3:eee\\)bb\\)ccc" t 20 "\\\\(\\?[0-9]:"))
  (expect '("\\(?3:" "eeebbccc" nil 6 19 1)
    (regex-collection-subexp "aaa\\(b\\(?3:eeebbccc" nil 0 "\\\\(\\?[0-9]:"))
  (expect '(nil "aaa\\(beee\\)bb" "\\)" 0 15 2)
    (regex-collection-subexp "aaa\\(beee\\)bb\\)ccc" t 18 "\\\\(\\?[0-9]:")))

;; simple-call-tree-info: CHECK
(defsubst regex-collection-chargroup (chars)
  "Return a regexp char group matching all string chars in list CHARS.
CHARS should be a list of chars as strings."
  (regexp-opt-charset (mapcar 'string-to-char chars)))

(defconst regex-collection-chargroup-regex
  "\\[.\\([^]]\\|\\[:[a-z]+:\\]\\)*\\]"
  "Regular expression to match a char group within another regular expression.")

;; simple-call-tree-info: DONE
(defsubst regex-collection-chargroup-p (grp)
  "Return non-nil if GRP contains a chargroup regexp."
  (string-match (concat "^" regex-collection-chargroup-regex "$") grp))

;; simple-call-tree-info: DONE
(defsubst regex-collection-complemented-p (grp)
  "Return t if GRP is a complemented chargroup (e.g. \"[^a-z]\"), and nil if uncomplemented.
If GRP doesn't contain a chargroup regexp, throw an error."
  (assert (regex-collection-chargroup-p grp))
  (string-match "^\\[^" grp))

;; simple-call-tree-info: CHECK
(defun regex-collection-extract-chars (grp)
  "Return list of chars and char classes matched by regexp chargroup or single char GRP.
If GRP is a complemented chargroup (e.g. \"[^a-z]\"), then return
list of chars and char classes not matched by GRP.
Result will be a list whose first element is nil if the chargroup is
negated (i.e. starts with \"^\") and t otherwise, whose second element
is the list of chars and whose third element is the list of char classes,
e.g: (regex-collection-extract-chars \"[a-d[:space:][:cntrl:]]\")
     returns '(nil (97 98 99 100) (\"[:space:]\" \"[:cntrl:]\"))
whereas (regex-collection-extract-chars \"[^a-d[:space:][:cntrl:]]\")
        returns '(t (97 98 99 100) (\"[:space:]\" \"[:cntrl:]\"))"
  (if (or (regex-collection-charp grp)
	  (regex-collection-integerp grp))
      (list t (list (string-to-char grp)) nil)
    (assert (regex-collection-chargroup-p grp))
    (let* ( ;; indicate whether chargroup is complemented or not
	   (compp (string-match "^\\[^" grp))
	   ;; remove surrounding brackets and initial ^ if present
	   (grp1 (replace-regexp-in-string
		  "^^" "" (replace-regexp-in-string
			   "\\]$" "" (replace-regexp-in-string "^\\[" "" grp))))
	   (len (length grp1))
	   (pos -5) charclasses charranges chars)
      ;; extract any char classes
      (while (setq pos (string-match "\\[:[a-z]+:\\]" grp1 (min (+ pos 5) len)))
	(push (match-string 0 grp1) charclasses))
      (cl-remove-duplicates charclasses :test 'equal) ;remove duplicate char classes
      (setq grp1 (replace-regexp-in-string "\\[:[a-z]+:\\]" "" grp1))
      ;; extract ranges
      (setq pos -3)
      (while (setq pos (string-match ".-." grp1 (+ pos 3)))
	(setq charranges (cons (match-string 0 grp1) charranges)))
      (setq grp1 (replace-regexp-in-string ".-." "" grp1))
      ;; now extract all single chars and ranges of chars into a single list
      (setq chars
	    (cl-remove-duplicates
	     (-flatten
	      (append (cl-loop for i being the elements of grp1
			       collect i)
		      (cl-loop for chrng in charranges
			       for start = (aref chrng 0)
			       for end = (aref chrng 2)
			       if (< end start) do (error "Invalid char range %s" chrng)
			       else collect (cl-loop for i from start upto end collect i))))))
      ;; remove any chars which are matched by one of the char classes
      (cl-loop for charclass in charclasses
	       do (setq chars
			(cl-remove-if
			 (lambda (c) (string-match
				      (concat "[" charclass "]") (char-to-string c)))
			 chars)))
      ;; return chars and charclasses
      (list (not compp) chars charclasses))))

;; simple-call-tree-info: CHECK
(defun regex-collection-merge-chargroups (grp1 grp2)
  "Merge regexp chargroup GRP1 with regexp chargroup GRP2, and return resulting regexp.
GRP1 and/or GRP2 may also be single chars.
For example (regex-collection-merge-chargroups \"[a-f,;]\" \"[b-h,@]\") will return \"[a-h,;@]\".
If one of the chargroups is complemented, e.g. \"[^a-z]\", and the other isn't then both will be
returned unchanged in a list with the uncomplemented chargroup first, and the complemented one second."
  (let ((single1 (or (regex-collection-charp grp1)
		     (regex-collection-integerp grp1)))
	(single2 (or (regex-collection-charp grp2)
		     (regex-collection-integerp grp2))))
    ;; check args 
    (assert (and (or (regex-collection-chargroup-p grp1) single1)
		 (or (regex-collection-chargroup-p grp2) single2)))
    ;; check if one group is complemented and the other isn't
    (if (or (and (not single1) (regex-collection-complemented-p grp1)
		 (or single2 (not (regex-collection-complemented-p grp2))))
	    (and (not single2) (regex-collection-complemented-p grp2)
		 (or single1 (not (regex-collection-complemented-p grp1)))))
	(concat "\\(?:" grp2 "\\|" grp1 "\\)")
      ;; otherwise both groups are complemented/uncomplemented
      (let* ((pair1 (regex-collection-extract-chars grp1))
	     (pair2 (regex-collection-extract-chars grp2))
	     (all (concat (substring (regexp-opt-charset (append (cl-second pair1) (cl-second pair2))) 1 -1)
			  (mapconcat 'identity (cl-remove-duplicates
						(append (cl-third pair1) (cl-third pair2)) :test 'equal)
				     ""))))
	(if (and (not single1)
		 (regex-collection-complemented-p grp1))
	    ;; both groups are complemented
	    (concat "[^" all "]")
	  ;; both groups are uncomplemented
	  (concat "[" all "]"))))))

;; simple-call-tree-info: TODO
(expectations
  (desc "chargroup")
  (expect "[abc]" (regex-collection-chargroup '("b" "a" "c")))
  (expect "[ab]" (regex-collection-chargroup '("b" "a")))
  (expect "[abcg]" (regex-collection-chargroup '("g" "b" "a" "c")))
  (expect "[abcghi]" (regex-collection-chargroup '("g" "b" "a" "c" "h" "i")))
  (expect "[abcgh]" (regex-collection-chargroup '("g" "b" "a" "c" "h")))
  (expect "[!\"$%&()*abcgh£^]"
    (regex-collection-chargroup '("g" "b" "a" "c" "h" "!" "\"" "£" "$" "%" "^" "&" "*" "(" ")")))
  (expect '(t (?a) nil) (regex-collection-extract-chars "a"))
  (expect '(t (?a) nil) (regex-collection-extract-chars "[a]"))
  (expect '(t (?a ?b ?c) nil) (regex-collection-extract-chars "[abc]"))
  (expect '(t nil ("[:alpha:]")) (regex-collection-extract-chars "[abc[:alpha:]]"))
  (expect '(t (?, ?: ?') ("[:alpha:]")) (regex-collection-extract-chars "[abc[:alpha:],:']"))
  (expect '(t (?, ?: ?') ("[:alpha:]")) (regex-collection-extract-chars "[abc[:alpha:],:']"))
  (expect '(nil (?, ?: ?') ("[:alpha:]")) (regex-collection-extract-chars "[^abc[:alpha:],:']"))
  (expect '(t (?, ?: ?') ("[:xdigit:]" "[:alpha:]"))
    (regex-collection-extract-chars "[abc[:alpha:],:'[:xdigit:]]"))
  (expect '(nil (?, ?: ?') ("[:xdigit:]" "[:alpha:]"))
    (regex-collection-extract-chars "[^abc[:alpha:],:'[:xdigit:]]"))
  (expect '(t (?, ?: ?' ?a ?b ?c ?d ?e ?f) ("[:space:]"))
    (regex-collection-extract-chars "[a-f,:'[:space:]]"))
  (expect '(t (?, ?: ?' ?a ?b ?c ?d ?e ?f) ("[:space:]"))
    (regex-collection-extract-chars "[,:'a-f[:space:]]"))
  (expect '(t (?, ?: ?' ?a ?b ?c ?d ?e ?f) ("[:space:]"))
    (regex-collection-extract-chars "[,:'[:space:]a-f]"))
  (expect "[0-9a-z]" (regex-collection-merge-chargroups "[a-z]" "[0-9]"))
  (expect "[a-m]" (regex-collection-merge-chargroups "[a-f]" "[g-m]"))
  (expect "[a-m]" (regex-collection-merge-chargroups "[g-m]" "[a-f]"))
  (expect "[^a-m]" (regex-collection-merge-chargroups "[^a-f]" "[^g-m]"))
  (expect "\\(?:[g-m]\\|[^a-f]\\)" (regex-collection-merge-chargroups "[^a-f]" "[g-m]"))
  (expect "\\(?:[a-f]\\|[^g-m]\\)" (regex-collection-merge-chargroups "[^g-m]" "[a-f]"))
  (expect "[a-m[:space:]]"
    (regex-collection-merge-chargroups "[a-f[:space:]]" "[g-m[:space:]]"))
  (expect "[^a-m[:space:]]"
    (regex-collection-merge-chargroups "[^a-f[:space:]]" "[^g-m[:space:]]"))
  (expect "[^a-m[:punct:][:space:]]"
    (regex-collection-merge-chargroups "[^a-f[:space:]]" "[^g-m[:space:][:punct:]]")))

;; simple-call-tree-info: CHECK
(defun regex-collection-parts (regex)
  "Return list of parts of REGEX separated by \\|."
  (let ((oldpos 0) newpos thispart parts)
    ;; traverse `regex' looking for \\(, \\) or \\|, while keeping track of the subexpression depth
    (cl-loop while (setq newpos (string-match "\\\\(\\|\\\\)\\|\\\\|" regex oldpos))
	     for break = (substring regex newpos (+ newpos 2))
	     for type = (cond ((string-equal break "\\(") 1)
			      ((string-equal break "\\)") -1)
			      ((string-equal break "\\|") 0)
			      (t nil))
	     do (cl-case type
		  ;; if at beginning of a subexpression, pass over it
		  (1 (let* ((subx (regex-collection-subexp regex nil newpos)))
		       (setq thispart (concat thispart (substring regex oldpos (cl-fifth subx)))
			     oldpos (cl-fifth subx))))
		  ;; if at the end of a subexpression, it's an error
		  (-1 (error "Unmatched \\)"))
		  ;; if at end of a toplevel alternative, i.e. \\|, add it to `parts'
		  (0 (setq parts (append parts
					 (list (concat thispart (substring regex oldpos newpos))))
			   thispart "" ;reset this part
			   oldpos (+ newpos 2)))
		  (t (error "Invalid type: %s" type))))
    (append parts (list (concat thispart (substring regex oldpos))))))

;; TODO
(expectations
  (desc "parts")
  (expect '("aa" "bb" "cc") (regex-collection-parts "aa\\|bb\\|cc"))
  (expect '("aa" "b\\(foo\\|bar\\)b" "cc")
    (regex-collection-parts "aa\\|b\\(foo\\|bar\\)b\\|cc")))

;; simple-call-tree-info: CHECK
(defun regex-collection-prefix (regexs)
  "Get longest usable common prefix of regexps in REGEXS."
  (if (<= (length regexs) 1)
      (car regexs)
    (let* ((prefix (try-completion "" regexs)) (pos 0) subx (stop nil))
      (while (not (or (string-equal prefix "") stop))
	;; check if the prefix ends just before a postfix operator
	(if (cl-loop for regex in regexs
		     if (string-match "^\\(?:[*+?]\\|\\\\{\\)"
				      (substring regex (length prefix)))
		     return t)
	    (progn
	      ;; it could have been the 2nd char of a 2-char postfix operator
	      ;; in which case remove the 1st char from the end of prefix
	      ;; (could have either no slash or four slashes (matching a double slash)
	      ;; before the postfix operator)
	      (if (string-match "\\(?:\\\\\\\\\\|[^\\]\\)\\(?:[+*?]\\)$" prefix)
		  (setq prefix (substring prefix 0 (match-beginning 0))))
	      ;; now remove the thing that the postfix operator is operating on
	      (cond ((string-match "\\\\)$" prefix (- (min 2 (length prefix)))) ;operator is preceeded by subexpression
		     ;; get last subexpression of prefix
		     (setq subx (regex-collection-subexp prefix t))
		     ;; error if subexpression is incomplete, or doesn't end at the end of prefix
		     (unless (and (cl-first subx) (cl-third subx) (= (cl-fifth subx) (length prefix)))
		       (error "Invalid regexp: %S" regex))
		     ;; remove the subexpression from prefix
		     (setq prefix (substring prefix 0 (cl-fourth subx))))
		    ((string-match "\\]$" prefix -1) ;operator is preceeded by char alternative/class
		     ;; remove it
		     (setq prefix (substring
				   prefix 0 (string-match (concat regex-collection-chargroup-regex "$") prefix))))
		    ;; operator is preceeded by backslashed char/number
		    ((string-match "\\\\.$" prefix)
		     (setq prefix (substring prefix 0 (- (min 2 (length prefix))))))
		    ;; operator is preceeded by syntax code, char category, or \\_<, or \\_>
		    ((string-match "\\\\\\(_[<>]\\|[cCsS].\\)$" prefix)
		     (setq prefix (substring prefix 0 (- (min 3 (length prefix))))))
		    ;; in all other cases just remove the previous char
		    (t (setq prefix (substring prefix 0 -1)))))
	  ;; otherwise check that we didn't break up some other construct
	  (cond ((string-match "[^\\]\\\\$" prefix) ;backslash at the end
		 (setq prefix (substring prefix 0 -1)))
		;; unfinished syntax code / character category
		((string-match "[^\\]\\\\[sScC_]$" prefix)
		 (setq prefix (substring prefix 0 (- (min 2 (length prefix))))))
		;; unfinished enumeration operator
		((setq pos (string-match "\\\\{[^}]*$" prefix))
		 (setq prefix (substring prefix 0 pos)))
		;; unfinished char alternative/class
		((setq pos (string-match
			    (concat (substring regex-collection-chargroup-regex 0 -2) "$")
			    prefix))
		 (setq prefix (substring prefix 0 pos)))
		;; unfinished subexpression
		;; (skip over complete subexpressions, and then match first opening parenthesis)
		((progn (while (cl-third (setq subx (regex-collection-subexp prefix nil pos)))
			  (setq pos (cl-fifth subx)))
			(setq pos (cl-fourth subx)))
		 (setq prefix (substring prefix 0 pos) pos 0))
		(t (setq stop t)))))
      ;; return result
      prefix)))

(expectations
  (desc "prefix")
  (expect "a" (regex-collection-prefix '("ab" "ac" "ad")))
  (expect "" (regex-collection-prefix '("a?b" "ac" "ad")))
  (expect "" (regex-collection-prefix '("a+b" "ac" "ad")))
  (expect "" (regex-collection-prefix '("a*b" "ac" "ad")))
  (expect "" (regex-collection-prefix '("a??b" "ac" "ad")))
  (expect "" (regex-collection-prefix '("a+?b" "ac" "ad")))
  (expect "" (regex-collection-prefix '("a*?b" "ac" "ad")))
  (expect "a" (regex-collection-prefix '("a\\\\*?b" "a\\\\c" "a\\\\d")))
  (expect "a" (regex-collection-prefix '("a\\(b\\)?b" "a\\(b\\)c" "a\\(b\\)d")))
  (expect "a" (regex-collection-prefix '("a[c]?b" "a[c]c" "a[c]d")))
  (expect "a" (regex-collection-prefix '("a\\3?b" "a\\3c" "a\\3d")))
  (expect "a" (regex-collection-prefix '("a\\w?b" "a\\wc" "a\\wd")))
  (expect "a" (regex-collection-prefix '("a\\cg?b" "a\\cgc" "a\\cgd")))
  (expect "a" (regex-collection-prefix '("a\\Cg?b" "a\\Cgc" "a\\Cgd")))
  (expect "a" (regex-collection-prefix '("a\\s-?b" "a\\s-c" "a\\s-d")))
  (expect "a" (regex-collection-prefix '("a\\S-?b" "a\\S-c" "a\\S-d")))
  (expect "a" (regex-collection-prefix '("a\\_>?b" "a\\_>c" "a\\_>d")))
  (expect "a" (regex-collection-prefix '("a\\_<?b" "a\\_<c" "a\\_<d")))  
  (expect "a" (regex-collection-prefix '("ab?b" "abc" "abd")))
  (expect "" (regex-collection-prefix '("a\\{2\\}b" "ac" "ad")))
  (expect "" (regex-collection-prefix '("a\\{2,3\\}b" "ac" "ad")))
  (expect "a" (regex-collection-prefix '("a\\.b" "a\\*c" "a\\+d")))
  (expect "a" (regex-collection-prefix '("a\\(bb\\)b" "a\\(bc\\)c" "a\\(bd\\)d")))
  (expect "a" (regex-collection-prefix '("a\\_>b" "a\\_<c" "a\\_>d")))
  (expect "a" (regex-collection-prefix '("a\\cgb" "a\\ccc" "a\\ccd")))
  (expect "a" (regex-collection-prefix '("a\\Cgb" "a\\Ccc" "a\\Ccd")))
  (expect "a" (regex-collection-prefix '("a\\s-b" "a\\swc" "a\\s-d")))
  (expect "a" (regex-collection-prefix '("a\\S-b" "a\\Swc" "a\\S-d")))
  (expect "" (regex-collection-prefix '("a\\{1,2\\}b" "a\\{1,3\\}c" "a\\{1,4\\}d")))
  (expect "a" (regex-collection-prefix '("a[cd]b" "a[ce]c" "a[cf]d")))
  (expect "a"
    (regex-collection-prefix '("a\\(ffa" "a\\(ffb" "a\\(ffc")))
  (expect "a\\(f\\)z"
    (regex-collection-prefix '("a\\(f\\)z\\(fa" "a\\(f\\)z\\(fb" "a\\(f\\)z\\(fc")))
  (expect "a"
    (regex-collection-prefix '("a\\(fff\\{1,2\\}gg\\)b" "a\\(fff\\{1,3\\}gg\\)b" "a\\(fff\\{1,4\\}gg\\)b")))
  (expect "a"
    (regex-collection-prefix '("a\\(ff\\(f[cd]\\)gg\\)b" "a\\(ff\\(f[ce]\\)gg\\)b" "a\\(ff\\(f[cf]\\)gg\\)b")))
  (expect "a"
    (regex-collection-prefix '("a\\(ff\\(fc?d\\)gg\\)b" "a\\(ff\\(fce\\)gg\\)b" "a\\(ff\\(fcf\\)gg\\)b")))
  (expect "a"
    (regex-collection-prefix '("a\\(ff\\(f[c?d]\\)gg\\)b" "a\\(ff\\(f[ce]\\)gg\\)b" "a\\(ff\\(f[cf]\\)gg\\)b")))
  (expect ""
    (regex-collection-prefix '("\\(?:[0-9][0-9]\\|[0-5]\\)" "\\(?:[0-9]\\|5[0-5]\\)"))))

;; simple-call-tree-info: CHECK
(defun regex-collection-suffix (regexs)
  "Get longest usable common suffix of regexps in REGEXS."
  (if (<= (length regexs) 1)
      (car regexs)
    (let ((suffix
	   (regex-collection-reverse
	    (try-completion
	     "" (mapcar (lambda (s) (regex-collection-reverse s)) regexs))))
	  (stop nil) pos subx)
      (while (not (or (string-equal suffix "") stop))
	;; check that the suffix doesn't break some construct
	(cond ( ;; complete postfix operator
	       (string-match "^\\(?:\\*[?]?\\|\\+[?]?\\|\\?[?]?\\|\\\\{[^}]*\\\\}\\)" suffix)
	       (setq suffix (substring suffix (length (match-string 0 suffix)))))
	      ;; unfinished enumeration postfix operator
	      ((string-match "^[^{]*\\\\}" suffix)
	       (setq suffix (substring suffix (length (match-string 0 suffix)))))
	      ;; unfinished char alternative/class
	      ((string-match "^[^[]*\\]" suffix)
	       (setq suffix (substring suffix (length (match-string 0 suffix)))))
	      ;; unfinished backslashed item / reference
	      ((and (cl-loop for regex in regexs ;check all regexs for trailing backslash
			     if (string-match "[^\\][\\]$"
					      (substring regex
							 (- (min (length regex)
								 ;; remember to count [^\\]
								 (+ 2 (length suffix))))
							 (- (length suffix))))
			     return t)
		    (string-match "^\\(?:[cCsS].\\|_[<>]\\|.\\)" suffix))
	       (setq suffix (substring suffix (match-end 0))))
	      ;; unfinished syntax code / character category
	      ((cl-loop for regex in regexs ;check all regexs for trailing \\c/C/s/S/_
			if (string-match "[^\\][\\][cCsS_]$"
					 (substring regex
						    (- (min (length regex)
							    ;; remember to count [^\\]
							    (+ 3 (length suffix))))
						    (- (length suffix))))
			return t)
	       (setq suffix (substring suffix 1)))
	      ;; unfinished subexpression
	      ;; (skip over complete subexpressions, and then match first closing parenthesis)
	      ((progn (setq pos (length suffix))
		      (while (cl-first (setq subx (regex-collection-subexp suffix t pos)))
			(setq pos (cl-fourth subx)))
		      (setq pos (cl-fifth subx)))
	       (setq suffix (substring suffix pos)))
	      ;; if there are no more problems, stop processing
	      (t (setq stop t))))
      ;; return result
      suffix)))

(expectations
  (desc "suffix")
  (expect "a" (regex-collection-suffix '("ba" "ca" "da")))
  (expect "a" (regex-collection-suffix '("b?a" "c?a" "d?a")))
  (expect "a" (regex-collection-suffix '("b+a" "c+a" "d+a")))
  (expect "a" (regex-collection-suffix '("b*a" "c*a" "d*a")))  
  (expect "a" (regex-collection-suffix '("b??a" "c??a" "d??a")))
  (expect "a" (regex-collection-suffix '("b+?a" "c+?a" "d+?a")))
  (expect "a" (regex-collection-suffix '("b*?a" "c*?a" "d*?a")))  
  (expect "a" (regex-collection-suffix '("b\\{2\\}a" "c\\{2\\}a" "d\\{2\\}a")))
  (expect "a" (regex-collection-suffix '("b\\{2,3\\}a" "c\\{2,3\\}a" "d\\{2,3\\}a")))
  (expect "a" (regex-collection-suffix '("b\\{1,4\\}a" "c\\{2,4\\}a" "d\\{3,4\\}a")))  
  (expect "a" (regex-collection-suffix '("b[a-z]a" "c[b-z]a" "d[c-z]a")))
  (expect "a" (regex-collection-suffix '("b\\s-a" "ca-a" "d-a")))
  (expect "a" (regex-collection-suffix '("b\\S-a" "ca-a" "d-a")))
  (expect "a" (regex-collection-suffix '("b\\cca" "cca" "dca")))
  (expect "a" (regex-collection-suffix '("b\\Cca" "cca" "dca")))
  (expect "a" (regex-collection-suffix '("b\\_>a" "c>a" "d>a")))
  (expect "a" (regex-collection-suffix '("b\\ba" "cba" "dba")))
  (expect "a" (regex-collection-suffix '("b\\(fff\\)a" "cb\\(bff\\)a" "db\\(bff\\)a")))
  (expect "aa\\(b\\)c"
    (regex-collection-suffix '("b\\(fff\\)aa\\(b\\)c" "cb\\(bff\\)aa\\(b\\)c" "db\\(bff\\)aa\\(b\\)c")))
  (expect "\\(ffa" (regex-collection-suffix '("b\\(ffa" "cb\\(ffa" "dbk\\(ffa")))
  (expect "ffa" (regex-collection-suffix '("b\\(ffa" "cb\\(ffa" "dbk\\(affa")))
  (expect "ffa" (regex-collection-suffix '("b\\)ffa" "cb\\)ffa" "db\\)ffa")))
  (expect ""
    (regex-collection-suffix '("1\\(?:[0-9]\\|[0-9][0-9]\\)" "2\\(?:[0-4][0-9]\\|5[0-5]\\|[0-9]\\)"))))

;; simple-call-tree-info: CHECK
(defun regex-collection-act-on-subexps (rx func &optional keepparens)
  "Replace the contents of each parenthesised subexpression in RX with result of applying FUNC on it.
FUNC must be a function which takes a single string argument and returns a string.
If the subexpression is a shy group, and the text returned by FUNC contains no \\|'s then the parentheses
will be removed unless KEEPPARENS is non-nil."
  (let ((pos 0) subx)
    (concat (cl-loop while (setq subx (regex-collection-subexp rx nil pos))
		     for newpart = (funcall func (cl-second subx))
		     for keepp = (or keepparens
				     (not (string-match "\\?:" (cl-first subx)))
				     (string-match "\\\\|" newpart))
		     concat (concat (substring rx pos (cl-fourth subx))
				    (if keepp (cl-first subx))
				    newpart
				    (if keepp (cl-third subx)))
		     do (setq pos (cl-fifth subx)))
	    (substring rx pos))))

(expectations
  (desc "act on subexps")
  (expect "foo" (regex-collection-act-on-subexps "foo" 'identity))
  (expect "a.*\\([0-9]+\\);?" (regex-collection-act-on-subexps "a.*\\([0-9]+\\);?" 'identity))
  (expect "a.*[0-9]+;?" (regex-collection-act-on-subexps "a.*\\(?:[0-9]+\\);?" 'identity))
  (expect "a.*\\(?:[0-9]+\\);?"
    (regex-collection-act-on-subexps "a.*\\(?:[0-9]+\\);?" 'identity t))
  (expect "a.*\\(?2:[0-9]+\\);?"
    (regex-collection-act-on-subexps "a.*\\(?2:[0-9]+\\);?" 'identity t))
  (expect "a\\(FOO\\)zCHOO"
    (regex-collection-act-on-subexps "a\\(foo\\)z\\(?:choo\\)" 'upcase))
  (expect "a\\(\\)z\\(\\)"
    (regex-collection-act-on-subexps "a\\(foo\\)z\\(choo\\)" (lambda (x) ""))))

;; simple-call-tree-info: CHECK
(defun regex-collection-lex-sort (strs)
  "Sort strings in STRS lexicographically (i.e. by initial chars).
Also remove any duplicate strings."
  (cl-remove-duplicates (sort strs 'string-lessp) :test 'equal))

;; simple-call-tree-info: CHECK
(defun regex-collection-colex-sort (strs)
  "Sort strings in STRS colexicographically (i.e. by final chars).
Also remove any duplicate strings."
  (cl-remove-duplicates
   (mapcar 'regex-collection-reverse
	   (sort (mapcar 'regex-collection-reverse strs) 'string-lessp))
   :test 'equal))

(expectations
  (desc "sort")
  (expect '("a" "b" "c") (regex-collection-lex-sort '("b" "a" "c")))
  (expect '("a" "b" "c") (regex-collection-lex-sort '("b" "a" "c")))
  (expect '("" "aaz" "aba") (regex-collection-lex-sort '("aaz" "aba" "")))
  (expect '("" "aba" "aaz") (regex-collection-colex-sort '("aaz" "aba" "")))
  (expect '("") (regex-collection-colex-sort '(""))))

;; simple-call-tree-info: CHECK
(defun regex-collection-fix-subseq (regexs &optional suffixp)
  "Find first contiguous subsequence of REGEXS with non-empty prefix.
If SUFFIXP is non-nil then find first contiguous subsequence with non-empty suffix.
Return a list containing the start and end indices of the subsequence and the prefix/suffix, 
or nil if none found."
  (let ((i 0)
	(func (if suffixp 'regex-collection-suffix
		'regex-collection-prefix))
	fix1)
    (while (and (<= (+ i 2) (length regexs))
		(setq fix1 (funcall func (cl-subseq regexs i (+ i 2))))
		(= (length fix1) 0))
      (setq i (1+ i)))
    (when (<= (+ i 2) (length regexs))
      (let ((j (+ i 3))
	    (fix2 fix1))
	(while (and (<= j (length regexs))
		    (setq fix1 (funcall func (cl-subseq regexs i j)))
		    (> (length fix1) 0))
	  (setq j (1+ j) fix2 fix1))
	(list i (1- j) fix2)))))

(expectations
  (desc "prefixed subseq")
  (expect '(1 5 "b") (regex-collection-fix-subseq '("aa" "ba" "bb" "bzzzzza" "bzzzzzc" "c")))
  (expect '(0 2 "a") (regex-collection-fix-subseq '("aa" "ba" "bb" "bzzzzza" "bzzzzzc" "c") t))
  (expect '(0 2 "a") (regex-collection-fix-subseq '("aa" "aba" "bb" "bzzzzza" "b" "c")))
  (expect '(3 5 "bzzzzz") (regex-collection-fix-subseq '("aa" "ba" "cbb" "bzzzzza" "bzzzzzc" "c")))
  (expect '(4 6 "c") (regex-collection-fix-subseq '("aa" "bz" "cbb" "bzzzzza" "bzzzzzc" "c") t)))

;; simple-call-tree-info: CHECK
(defun regex-collection-opt-group (regexs &optional suffixp)
  "Return a regexp to match anything matched by regexps in list REGEXS.
It is assumed that REGEXS is sorted lexicographically (to make it easier to find
a common prefix), unless SUFFIXP is non-nil in which case it's assumed REGEXS
is sorted colexicographically (to make it easier to find a common suffix)."
  ;; This code is based on `regexp-opt-group'
  ;; `regexs' must be sorted beforehand (this prevents sorting the same list twice)
  (let* ((sortfn (if suffixp 'regex-collection-colex-sort
		   'regex-collection-lex-sort))
	 (othersortfn (if suffixp 'regex-collection-lex-sort
			'regex-collection-colex-sort))
	 chargrps rest)
    (cond
     ;; If there are no regexs, just return the empty string.
     ((= (length regexs) 0) "")
     ;; If there is only one regexp, just return it.
     ((= (length regexs) 1) (car regexs))
     ;; If there is an empty string, remove it and recurse on the rest.
     ((= (length (car regexs)) 0)
      (let* ((rx (regex-collection-opt-group (cdr regexs))))
	(concat (if (regex-collection-chargroup-p rx)
		    rx
		  (concat "\\(?:" rx "\\)")) "?")))
     ;; Otherwise...
     (t	;; first merge chargroups and single chars into a single chargroup
      (dolist (rx regexs)
	(if (or (= (length rx) 1) (regex-collection-chargroup-p rx))
	    (push rx chargrps)
	  (push rx rest)))
      (if chargrps
	  (setq regexs
		(funcall sortfn
			 (append rest (and chargrps
					   (list (-reduce 'regex-collection-merge-chargroups chargrps)))))))
      ;; function for extracting prefix/suffix and creating regexp
      (cl-flet ((process (rxs sfxp) ;suffixp indicates whether we are extracting a prefixes or suffixes
			 (let* ((res (regex-collection-fix-subseq rxs sfxp))
				(start (cl-first res))
				(end (cl-second res))
				(fix (cl-third res))
				(len (length fix))
				fixes parts)
			   ;; if no (pre/suf)fix can be extracted return nil
			   (unless (= len 0)
			     ;; otherwise find consecutive group of rxs with common (pre/suf)fix
			     ;; and create single regexp from them
			     (setq fixes (regex-collection-opt-group
					  (mapcar (lambda (s) (if sfxp (substring s 0 (- len))
								(substring s len)))
						  (cl-subseq rxs start end))
					  sfxp)
				   ;; get parts of new regexp so we can put the prefixes/suffixes
				   ;; back if this turns out to be shorter
				   parts (regex-collection-parts fixes))
			     (concat
			      (if (> start 0) ;if there are rxs before those in `fixes'
				  ;; create regexp by sorting and extracting (suf/pre)fix from other end
				  ;; of each regex, since there is no common (pre/suf)fix at this end
				  (concat (regex-collection-opt-group
					   (funcall othersortfn (cl-subseq rxs 0 start))
					   (not sfxp))
					  "\\|"))
			      ;; for rxs with common (pre/suf)fix..
			      (if (< (+ len 6) (* len (length parts)))
				  ;; use extracted (pre/suf)fix only if result is shorter
				  (concat (unless sfxp fix)
					  (if (> (length parts) 1) (concat "\\(?:" fixes "\\)") fixes)
					  (if sfxp fix))
				;; otherwise put the (pre/suf)fix back again
				(mapconcat (if sfxp (lambda (s) (concat s fix))
					     (lambda (s) (concat fix s)))
					   parts "\\|"))
			      ;; create regex for last group (no need to change end from which
			      ;; (pre/suf)fix is extracted, or sort order)
			      (if (< end (length rxs))
				  (concat "\\|" (regex-collection-opt-group
						 (cl-subseq rxs end) sfxp))))))))
	(or (process regexs suffixp) ;; try extracting a common (pre/suf)fix
	    (process (funcall othersortfn regexs) (not suffixp)) ;; or a common (suf/pre)fix
	    ;; otherwise just join the regexs together
	    (mapconcat 'identity regexs "\\|")))))))

(expectations
  (desc "best-split")
  (expect "0000\\(?:01\\|12\\|23\\)"
    (regex-collection-opt-group '("000001" "000012" "000023")))
  (expect "[0-5]5\\|[0-9][0-4]?"
    (regex-collection-opt-group
     (sort '("[0-9]0" "[0-9]1" "[0-9]2" "[0-9]3" "[0-9]4" "[0-5]5" "[0-9]") 'string-lessp)))
  (expect "0000[012]\\|0001[12]\\|0011[12]\\|0111[12]"
    (regex-collection-opt-group '("00000" "00001" "00002" "00011" "00012" "00111" "00112" "01111" "01112")))
  (expect "[0-9a-z]" (regex-collection-opt-group '("[a-z]" "[0-9]")))
  (expect "1\\(?:[0-9]\\|[0-9][0-9]\\)\\|2\\(?:[0-4][0-9]\\|5[0-5]\\|[0-9]\\)"
    (regex-collection-opt-group '("1\\(?:[0-9]\\|[0-9][0-9]\\)" "2\\(?:[0-4][0-9]\\|5[0-5]\\|[0-9]\\)")))
  (expect "5[0-5]\\|[012]?[0-9]" (regex-collection-opt-group '("0[0-9]" "1[0-9]" "5[0-5]" "2[0-9]" "[0-9]"))))

;; simple-call-tree-info: CHECK
(defun regex-collection-remove-redundant (regex &optional nonshy)
  "Remove unnecessary shy group parentheses from REGEX.
If NONSHY is non-nil then also remove unnecessary non-shy group parentheses."
  (let ((pos 0) subexp parts middle keep)
    (while (setq subexp
		 (regex-collection-subexp regex nil pos (unless nonshy "\\\\(\\?:")))
      (setq parts (regex-collection-parts (cl-second subexp))
	    keep (> (length parts) 1)
	    middle (concat (if keep (cl-first subexp))
			   (mapconcat (lambda (p) (regex-collection-remove-redundant p nonshy))
				      parts "\\|")
			   (if keep (cl-third subexp)))
	    regex (concat (substring regex 0 (fourth subexp))
			  middle
			  (substring regex (fifth subexp)))
	    pos (+ (fourth subexp) (length middle))))
    regex))

(expectations
  (desc "remove redundant parentheses")
  (expect "zzz" (regex-collection-remove-redundant "zzz"))
  (expect "" (regex-collection-remove-redundant ""))
  (expect "aaabbbfo\\(pp\\)ozzz"
    (regex-collection-remove-redundant "aaabbb\\(?:fo\\(pp\\)o\\)zzz"))
  (expect "aa\\(?:a\\|b\\)bbfo\\(pp\\)ozzz"
    (regex-collection-remove-redundant "aa\\(?:a\\|b\\)bb\\(?:fo\\(pp\\)o\\)zzz"))
  (expect "aa\\(?:a\\|b\\)bbfoppozzz"
    (regex-collection-remove-redundant "aa\\(?:a\\|b\\)bb\\(?:fo\\(pp\\)o\\)zzz" t))
  (expect "aa\\(?:a\\|b\\)bb\\(?:fo\\|\\(pp\\)o\\)zzz"
    (regex-collection-remove-redundant "aa\\(?:a\\|b\\)bb\\(?:fo\\|\\(pp\\)o\\)zzz"))
  (expect "aa\\(?:a\\|b\\)bb\\(?:fo\\|ppo\\)zzz"
    (regex-collection-remove-redundant "aa\\(?:a\\|b\\)bb\\(?:fo\\|\\(?:pp\\)o\\)zzz"))
  (expect "aa\\(?:a\\|b\\)bb\\(?:fo\\|ppo\\)zzz"
    (regex-collection-remove-redundant "aa\\(?:a\\|b\\)bb\\(?:fo\\|\\(pp\\)o\\)zzz" t))
  (expect "aaabbbfo\\(pp\\)ozzz"
    (regex-collection-remove-redundant "aa\\(?:ab\\)bb\\(?:fo\\(pp\\)o\\)zzz"))
  (expect "aaabbbfoppozzz"
    (regex-collection-remove-redundant "aa\\(?:ab\\)bb\\(?:fo\\(pp\\)o\\)zzz" t))
  (expect "bbfo\\(pp\\|mm\\)ozzz"
    (regex-collection-remove-redundant "bb\\(?:fo\\(pp\\|mm\\)o\\)zzz")))

;; simple-call-tree-info: CHECK
(defun regex-collection-optimize (regex)
  "Return optimised version of REGEX."
  (let ((regex2 (regex-collection-remove-redundant regex))) ;; remove redundant shy group parentheses
    ;; split regex into separate parts and recursively
    ;; optimize subexpressions within each part
    (if (string-match "\\\\|" regex2)
	;; extract common prefixes & suffixes from separate parts
	(regex-collection-opt-group
	 (regex-collection-lex-sort
	  (cl-loop for part in (regex-collection-parts regex2)
		   ;; optimize subexpressions recursively
		   collect (regex-collection-act-on-subexps part 'regex-collection-optimize))))
      regex2)))

;; simple-call-tree-info: DONE  
(defalias 'regex-collection-optimise
  'regex-collection-optimize)

(expectations
  (desc "regex-optimise")
  (expect "abc" (regex-collection-optimize "abc"))
  (expect "ab[cde]" (regex-collection-optimize "abc\\|abe\\|abd"))
  (expect "ab[cde]oo" (regex-collection-optimize "abcoo\\|abeoo\\|abdoo"))
  (expect "xxxab[cde]oozz" (regex-collection-optimize "xxx\\(?:abcoo\\|abeoo\\|abdoo\\)zz"))
  (expect "xxx\\(ab[cde]oo\\)zz" (regex-collection-optimize "xxx\\(abcoo\\|abeoo\\|abdoo\\)zz"))
  (expect "aa\\|bb\\|cc" (regex-collection-optimize "aa\\|bb\\|cc"))
  (expect "[abc]" (regex-collection-optimize "a\\|b\\|c"))
  (expect "xxxab[cde]oozz[123]yu" (regex-collection-optimize "xxx\\(?:abcoo\\|abeoo\\|abdoo\\)zz\\(?:1\\|2\\|3\\)yu"))
  (expect "xxxab[cde]oozz\\([123]\\)yu"
    (regex-collection-optimize "xxx\\(?:abcoo\\|abeoo\\|abdoo\\)zz\\(1\\|2\\|3\\)yu"))
  (expect "xxx\\(ab[cde]oo\\)zz[123]yu"
    (regex-collection-optimize "xxx\\(abcoo\\|abeoo\\|abdoo\\)zz\\(?:1\\|2\\|3\\)yu"))
  (expect "[abc]" (regex-collection-optimize "\\(?:a\\|b\\|c\\)"))
  (expect "\\(?:aa\\|bb\\|cc\\)zz\\{1,5\\}"
    (regex-collection-optimize "aazz\\{1,5\\}\\|bbzz\\{1,5\\}\\|cczz\\{1,5\\}"))
  (expect "aa\\{1,5\\}\\|bb\\{1,5\\}\\|cc\\{1,5\\}"
    (regex-collection-optimize "aa\\{1,5\\}\\|bb\\{1,5\\}\\|cc\\{1,5\\}"))
  (expect "aa?\\|bb?\\|cc?"
    (regex-collection-optimize "aa?\\|bb?\\|cc?"))
  (expect "aa??\\|bb??\\|cc??"
    (regex-collection-optimize "aa??\\|bb??\\|cc??"))
  (expect "aa*\\|bb*\\|cc*"
    (regex-collection-optimize "aa*\\|bb*\\|cc*"))
  (expect "aa*?\\|bb*?\\|cc*?"
    (regex-collection-optimize "aa*?\\|bb*?\\|cc*?"))
  (expect "aa+\\|bb+\\|cc+"
    (regex-collection-optimize "aa+\\|bb+\\|cc+"))
  (expect "aa+?\\|bb+?\\|cc+?"
    (regex-collection-optimize "aa+?\\|bb+?\\|cc+?"))
  (expect "\\(?1:aa\\)+?\\|\\(?2:bb\\)+?\\|\\(?3:cc\\)+?"
    (regex-collection-optimize "\\(?1:aa\\)+?\\|\\(?2:bb\\)+?\\|\\(?3:cc\\)+?"))
  (expect "aazz\\{1,5\\}\\|bbzz\\{2,5\\}\\|cczz\\{3,5\\}"
    (regex-collection-optimize "aazz\\{1,5\\}\\|bbzz\\{2,5\\}\\|cczz\\{3,5\\}"))
  (expect "5[0-5]\\|[0-4]?[0-9]"
    (regex-collection-optimize "0[0-9]\\|1[0-9]\\|2[0-9]\\|3[0-9]\\|4[0-9]\\|5[0-5]\\|[0-9]")))

;; simple-call-tree-info: TODO
(defun regex-collection-invalidp (regex &optional throwerror)
  "Check if REGEX is an invalid regular expression.
The `re-search-forward' function is used to test REGEX.
If REGEX is valid then nil is returned, otherwise return the text of
the error message that would normally be displayed (without throwing
an error). If THROWERROR is non-nil, then re-throw the error that would
normally be thrown by `re-search-forward'."
  (condition-case err
      (with-temp-buffer (re-search-forward regex nil t)
			nil)
    (error (format "%s" (cdr err)))))

;; TODO: regexps for extracting json fields, html parts, etc. country names, wikipedia tables
;;       Also following regexp functions are unfinished
;;       Get some ideas from here: http://www.regexmagic.com/patterns.html

;; regexps related to the internet
(define-arx internet-rx
  '((dgt (regex "[0-9]"))
    (hexdgt (regexp "[:xdigit:]"))
    (octet (repeat 2 hexdgt))
    (mac (seq (repeat 5 (seq octet ":")) octet))
    (ipv4digit (regex-collection-range-regex "0.0.0.0" "255.255.255.255" '(255 255 255 255)))
    (systemport (regex-collection-range-regex '("0" 4) "1023" '(9 9 9 9)))
    (registeredport (regex-collection-range-regex "1024" "49151"))
    (dynamicport (regex-collection-range-regex "49151" "65535"))
    (portnum (regexp ":[0-9]\\{1,5\\}\\|/[0-9]+"))
    (ipv4 (seq (repeat 3 (seq ipv4digit "\\.")) ipv4digit (? ":" portnum)))
    (ipv6 (regexp "(([0-9a-fA-F]{0,4}:){2,7}(:|[0-9a-fA-F]{1,4})(:[0-9]{1,5}|/[0-9]+)?)"))
    (uuid (regexp "([A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[34][A-Fa-f0-9]{3}-[89ab][A-Fa-f0-9]{3}-[A-Fa-f0-9]{12})"))
    (url (regexp "((http|https|ftp)://[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}(/\S*)?)"))
    (ftp (regexp "(ftp://[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}(/\S*)?)"))
    (http (regexp "(http://[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}(/\S*)?)"))
    (https (regexp "(https://[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}(/\S*)?)"))))

(define-arx datetime-rx
  '((date (regexp "([0-9]{2}/[0-9]{2}/(19|20)[0-9]{2})"))
    (datetime (regexp "([0-9]{2}/[0-9]{2}/(19|20)[0-9]{2} [012][0-9]:[012345][0-9])"))
    (timehms (regexp "([012][0-9]:[012345][0-9]:[012345][0-9])"))
    (timehm (regexp "([012][0-9]:[012345][0-9])"))
    (hour (regexp "([012]?[0-9])"))
    (minute (regexp "([012345]?[0-9])"))
    (daypart (regexp "([Mm]orning|[Aa]fternoon|[Ee]vening|[Nn]ight)"))
    (weekday (regexp "([Mm]onday|MONDAY|[Tt]uesday|TUESDAY|[Ww]ednesday|WEDNESDAY|[Tt]hursday|THURSDAY|[Ff]riday|FRIDAY)"))
    (month (regexp "([Jj]anuary)|JANUARY|[Ff]ebruary|FEBRUARY|[Mm]arch|MARCH|[Aa]pril|APRIL|[Mm]ay|MAY|[Jj]une|JUNE|[Jj]uly|JULY|[Aa]ugust|AUGUST|[Ss]eptember|SEPTEMBER|[Oo]ctober|OCTOBER|[Nn]ovember|NOVEMBER)"))
    (year (regexp "(19[0-9]{2}|20[0-9]{2})"))))

(define-arx address-rx
  '((email (regexp "(\w[[:alnum:]._-]*\w@\w[[:alnum:].-]*\w\.\w{2,3})"))
    (ukpostcode (regexp "([a-zA-Z]{1,2}[0-9][0-9A-Za-z]{0,1} ?[0-9]?[A-Za-z]{2})"))
    (uszipcode (regexp ""))
    (ukphone (regexp "(\s*\(?0[0-9]{3,5}\)?\s*[0-9]{3,4}\s*[0-9]{3,4}\s*)"))
    (intlphone (regexp "((\+[1-9][0-9]*(\([0-9]*\)|-[0-9]*-))?[0]?[1-9][0-9\ -]*)"))))

(address-rx ukpostcode)

;; TODO
;; <(extract-text)>

(provide 'regex-collection)

;; (org-readme-sync)
;; (magit-push)

;;; regex-collection.el ends here
