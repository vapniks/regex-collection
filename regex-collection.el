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
and strs should be either a list of strings, or a list of such lists.
A number, n, in position pos of LST will be exchanged for either the nth element 
of DIGITS (if DIGITS is a list of strings) or the nth element of the pos'th list
in DIGITS (if DIGITS is a list of lists). A regular expression will then be returned
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
  (desc "lex range")
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
If MIN is shorter than MAX and if PAD is non-nil then its value will be used to right-pad
the MIN list.
See `regex-collection-lex-range' for details on the MIN, MAX, and MAXDIGIT arguments.
This function calls `regex-collection-lex-range' after reversing the MIN and MAX lists, 
and then reverses each resulting list (handling padded values appropriately)."
  (let ((maxlen (length max))
	(minlen (length min)))
    ;; add initial right-padding to min if necessary
    (if (> maxlen minlen)
	(setq min (append min (make-list (- maxlen minlen) pad)))))
  ;; do lexicographic sort on reversed lists, and return reverse of results
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
       (equal (replace-regexp-in-string "^\\(0+\\)[1-9]" "" n nil nil 1)
	      (number-to-string (string-to-number n)))))

;; simple-call-tree-info: TODO
(defun regex-collection-month (str &optional invp)
  "Return integer representing month in STR.
If INVP is non-nil then STR is assumed to contain an integer,
and a list of alternative names corresponding month will be returned
which can then be fed into `regexp-opt'.
If STR is an string not containing a month name return nil."
  (let ((case-fold-search t)
	(monthstrs '(("jan" "january")
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
		     ("dec" "december"))))
    (if (stringp str)
	(cl-loop for i from 0 upto 11
		 if (string-match (regexp-opt (nth i monthstrs)) str)
		 return i)
      (nth str monthstrs))))

;; simple-call-tree-info: TODO
(defun regex-collection-weekday (str &optional invp)
  "Return integer representing weekday in STR.
If INVP is non-nil then STR is assumed to contain an integer,
and a list of names for the corresponding weekday will be returned
which can then be fed into `regexp-opt'.
If STR is an string not containing a weekday return nil."
  (let ((case-fold-search t)
	(weekstrs '(("mon" "monday")
		    ("tue" "tues" "tuesday")
		    ("wed" "weds" "wednesday")
		    ("thu" "thurs" "thursday")
		    ("fri" "friday")
		    ("sat" "saturday")
		    ("sun" "sunday"))))
    (if (stringp str)
	(cl-loop for i from 0 upto 6
		 if (string-match (regexp-opt (nth i weekstrs)) str)
		 return i)
      (nth str weekstrs))))

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
  (expect nil (regex-collection-consp '(1 2))))

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
 - a cons cell containing a pair of chars as strings"
  (cl-flet ((errorf (f) (error "Invalid field argument: %s" f))) ;check if arg is cons cell but not a list
    (cond
     ((integerp field)			;FIELD arg is single integer
      (let ((num (number-to-string field)))
	(concat "[0-9]\\{1"
		(if (> (length num) 1) (concat "," (regex-collection-length num))) "\\}")))
     ((and (stringp field) (string-match "[0-9]+" field)) ;FIELD arg is single string containing an integer
      (concat "[0-9]\\{" (regex-collection-length field) "\\}"))
     ;; if FIELD is any other string its assumed to contain a regexp, and is wrapped in a shy group before returning
     ((stringp field) (concat "\\(?:" field "\\)"))
     ;; FIELD is '(N . M) where N & M are integers
     ((regex-collection-consp field 'integerp)
      (if (> (car field) (cdr field)) (errorf field))
      (let ((carlen (regex-collection-length (number-to-string (car field))))
	    (cdrlen (regex-collection-length (number-to-string (cdr field)))))
	(concat "[0-9]\\{" carlen (unless (equal carlen cdrlen) (concat "," cdrlen)) "\\}")))
     ;; FIELD is '(N . M) where N & M are strings (assumed to contain integers)
     ((regex-collection-consp field 'regex-collection-integerp)
      (if (> (string-to-number (car field)) (string-to-number (cdr field))) (errorf field))
      (let ((carlen (regex-collection-length (car field)))
	    (cdrlen (regex-collection-length (cdr field))))
	(concat "[0-9]\\{" carlen (unless (equal carlen cdrlen) (concat "," cdrlen)) "\\}")))
     ;; FIELD is '(N . M) where N & M are strings of length one (assumed to contain letters)
     ((and (regex-collection-consp
	    field (lambda (x) (and (stringp x)
				   (equal (length x) 1)
				   (not (regex-collection-integerp x))))))
      (if (> (string-to-char (car field)) (string-to-char (cdr field)))
	  (errorf field)
	(if (equal (car field) (cdr field))
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
	   '(("[0-9]\\{1,2\\}" . '(1 . 10)) ;DONE
	     ("[0-9]\\{2\\}" . '("01" . "10")) ;DONE
	     ("[0-9]\\{1,2\\}" . 32)	;DONE
	     ("[0-9]\\{2\\}" . "02")	;DONE
	     ("\\(?:foo\\|baa\\)"  . "foo\\|baa") ;DONE
	     ("\\(?:\\(foo\\|baa\\)\\)"  . "\\(foo\\|baa\\)") ;DONE
	     ("[a-z]\\{1\\}" . '("a" . "z")) ;DONE
	     ("a" . '("a" . "a"))				   ;DONE
	     ("\\(?:choo\\|foo\\|man\\)" . '("foo" "man" "choo"))) ;DONE
	   do (eval `(expect ,(car args)
		       (regex-collection-fieldrx ,(cdr args))))))

;; simple-call-tree-info: DONE
(defun regex-collection-split-into-fields (str &optional seps fieldvals)
  "Helper function to split STR into list of separate fields.

SEPS can be a regexp matching the separators between fields, or a list of such
regexps (one for each consecutive separator). FIELDVALS is either a list of field 
values, or a single field value for all fields. See `regex-collection-fieldrx' 
for more information about the FIELDVALS argument.
Only one of SEPS and FIELDVALS needs be specified, if both are specified then SEPS
gets priority. If both SEPS and FIELDVALS are missing then the separators will be 
taken to be contiguous sequences of non-word chars, and the text between these 
sequences will be taken as the fields.

The function returns a list whose first element is the list of fields, and whose
second element is the list of separators."
  (let (str2 fieldrx (seppos 0))
    (cond ((stringp seps)
	   (setq str2 (split-string str seps)
		 seps (make-list (1- (length str2)) seps)))
	  ((and seps (listp seps))
	   (string-match (car seps) str 0)
	   (setq str2
		 (append
		  (list (substring str 0 (match-beginning 0)))
		  (cl-loop for sep in (cdr seps)
			   collect (substring
				    str (match-end 0) (string-match sep str (match-end 0))))
		  (list (substring str (match-end 0))))))
	  ;; if no separators or fieldvals are supplied we have to infer them..
	  ((and (null seps) (null fieldvals))
	   (while (string-match "\\W+" str seppos)
	     (setq seps (append seps (list (match-string 0 str)))
		   str2 (append str2 (list (substring str seppos (match-beginning 0))))
		   seppos (match-end 0)))
	   (setq str2 (append str2 (list (substring str seppos (length str))))))
	  ;; if we have separate FIELDVALS for each position
	  ((and (null seps) (listp fieldvals) (listp (cdr fieldvals)))
	   ;; loop over each field, collecting the separators and fields into SEPS and STR2 respectively
	   (cl-loop for i from 0 upto (1- (length fieldvals))
		    for fieldval = (nth i fieldvals)
		    for fieldrx = (regex-collection-fieldrx fieldval)
		    if (string-match fieldrx str seppos)
		    do (if (> seppos 0)
			   (setq seps
				 (append seps (list (substring str seppos (match-beginning 0))))))
		    (setq str2 (append str2 (list (match-string 0 str)))
			  seppos (match-end 0))
		    else do (error "No match for field %S: %S" (1+ i) fieldval)))
	  ;; otherwise use the same FIELDVALS for all positions
	  ((null seps)
	   (setq fieldrx (regex-collection-fieldrx fieldvals))
	   (while (string-match fieldrx str seppos)
	     (if (> seppos 0)
		 (setq seps (append seps (list (substring str seppos (match-beginning 0))))))
	     (setq str2 (append str2 (list (match-string 0 str)))
		   seppos (match-end 0)))))
    (list str2 seps)))

(expectations
  (desc "split-into-fields")
  (cl-loop for args in
	   '(('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" "/" nil))
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" "/" '(1 . 10000)))
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" nil '(1 . 10000)))
	     ('(("01" "02" "2010") ("/" "/")) . ("1/01/02/2010" nil '("01" . "10000")))
	     ('(("1" "01" "02" "2010") ("/" "/" "/")) . ("1/01/02/2010" "/" nil))
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" nil 10000))
	     ('(("01" "02" "20" "10") ("/" "/" "")) . ("01/02/2010" nil "10"))
	     ('(("201") nil) . ("01/02/2010" nil "100"))
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" "/" "100")) ; SEPS takes priority over FIELDVALS
	     ('(("01" "02" "2010") ("/" "/")) . ("01/02/2010" nil nil))
	     )
	   do (eval `(expect ,(car args)
		       (regex-collection-split-into-fields
			,@(cdr args))))))

;; simple-call-tree-info: CHECK
(defun regex-collection-range-regex (start end &optional fieldvals seps rxseps colex)
  "Return a regexp matching all strings between START and END strings, in lexicographic order.
The START and END strings are treated as sequences of fields and separators, e.g: \"01/01/2000\".
By default any non-word chars will be treated as separators, and the fields can be weekdays, 
months, or numbers whose min values are in START and whose max values are in END.
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
  ;; any elements of FIELDVALS eq to a symbol represents a repeat of the previous non-symbol value
  (if (and (listp fieldvals) (listp (cdr fieldvals)))
      (dotimes (i (length fieldvals))
	(if (symbolp (nth i fieldvals))
	    (setf (nth i fieldvals) (nth (1- i) fieldvals)))))
  ;; check FIELDVALS (loop is only run if it is a list or cons cell)
  (cl-loop for x in fieldvals
	   if (not (or (integerp x) (stringp x)
		       (and (listp x) (or (-all-p 'stringp x)
					  (-all-p 'numberp x)))))
	   do (error "Invalid field value: %s" fieldvals))
  ;; fix SEPS
  (unless rxseps (if (stringp seps)
		     (regexp-opt (list seps))
		   (mapcar (lambda (x) (regexp-opt (list x))) seps)))
  ;; split START and END into their separate parts
  (let (start2 end2 fieldmax fieldmaxs start3 end3)
    (setq start2 (regex-collection-split-into-fields start seps fieldvals)
	  end2 (regex-collection-split-into-fields end seps fieldvals)
	  start3 (make-list (length (car start2)) 0)
	  end3 (make-list (length (car end2)) 0))
    ;; first elements of start2 & end2 now contain the list of fields, second elements contain the separators
    ;; check that they have the same number of fields
    (unless (= (length (car start2)) (length (car end2)))
      (error "Different number of fields in START and END args: %S %S" start end))
    ;; calculate maximum value for each field, and change values of start and end lists to integers
    (cl-flet ((errorf (f) (error "Invalid field argument: %s" f))
	      (errorm (min max) (error "Invalid min and max values: %s, %s" min max))
	      (setidxs (min max i) (setf (nth i start3) min (nth i end3) max)) ;set indices for obtaining range
	      (getval (i min max vals)	;get value corresponding to index i
		      (cond ((and vals (listp vals) (listp (cdr vals)))
			     (nth i vals))
			    ((regex-collection-consp
			      (cons min max) 'regex-collection-integerp)
			     (if vals (number-to-string i)
			       (number-to-string (+ (string-to-number min) i))))
			    ((regex-collection-consp
			      (cons min max) 'regex-collection-month)
			     (regex-collection-month i t))
			    ((regex-collection-consp
			      (cons min max) 'regex-collection-weekday)
			     (regex-collection-weekday i t))
			    (t (errorm min max)))))
      (setq fieldmaxs
	    (cl-loop for i from 0 upto (1- (length (car start2)))
		     for fieldval = (if (and (listp fieldvals) (listp (cdr fieldvals)))
					(nth i fieldvals) fieldvals)
		     for minval = (nth i (car start2))
		     for maxval = (nth i (car end2))
		     if fieldval collect
		     (setq fieldmax
			   (cond ((integerp fieldval) fieldval) ; get the max value from the fieldval
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
				 ((regex-collection-consp
				   fieldval (lambda (x)
					      (and (stringp x)
						   (equal (length x) 1)
						   (not (regex-collection-integerp x)))))
				  (- (string-to-char (cdr fieldval))
				     (string-to-char (car fieldval))))
				 ((and (listp fieldval) (listp (cdr fieldval)))
				  (1- (length fieldval)))
				 (t (errorf fieldval))))
		     else collect
		     (setq fieldmax
			   (cond ((regex-collection-consp
				   (cons minval maxval) 'regex-collection-integerp)
				  (- (string-to-number maxval)
				     (string-to-number minval)))
				 ((regex-collection-consp
				   (cons minval maxval) 'regex-collection-month)
				  11)
				 ((regex-collection-consp
				   (cons minval maxval) 'regex-collection-weekday)
				  6)
				 (t (errorm minval maxval))))
		     end
		     ;; then calculate new START and END lists
		     do (cond ((and fieldval (listp fieldval) (listp (cdr fieldval)))
			       (setidxs (cl-position minval fieldval :test 'equal)
					(cl-position maxval fieldval :test 'equal) i))
			      ((regex-collection-consp
				(cons minval maxval) 'regex-collection-integerp)
			       (if fieldval
				   (let ((minval2 (string-to-number minval))
					 (maxval2 (string-to-number maxval)))
				     (if (or (> minval2 maxval2)
					     (> maxval2 fieldmax)
					     (< minval2 0))
					 (errorm minval maxval)
				       (setidxs minval2 maxval2 i)))
				 (setidxs 0 fieldmax i)))
			      ((regex-collection-consp
				(cons minval maxval) 'regex-collection-month)
			       (setidxs (regex-collection-month minval)
					(regex-collection-month maxval) i))
			      ((regex-collection-consp
				(cons minval maxval) 'regex-collection-weekday)
			       (setidxs (regex-collection-weekday minval)
					(regex-collection-weekday maxval) i))
			      (t (errorm minval maxval)))))
      ;; get lists in range and convert back into regexp 
      (substring
       (cl-loop for lst in (if colex (regex-collection-colex-range start3 end3 fieldmaxs)
			     (regex-collection-lex-range start3 end3 fieldmaxs))
		;; create separate regexp for each list, and join them together
		concat (cl-loop for i from 0 upto (1- (length (car start2)))
				;; get string matching index
				concat (let ((fieldval (if (and (listp fieldvals) (listp (cdr fieldvals)))
							   (nth i fieldvals) fieldvals))
					     (minval (nth i (car start2)))
					     (maxval (nth i (car end2)))
					     (idx (nth i lst)))
					 (if (integerp idx) ;idx can be a single index
					     (let ((val (getval idx minval maxval fieldval)))
					       (if (stringp val) val (regexp-opt val)))
					   ;; otherwise idx is a cons cell indicating a range of indices
					   (regexp-opt
					    (-flatten
					     (cl-loop for j from (car idx) upto (cdr idx)
						      collect (getval j minval maxval fieldval))))))
				;; add separator 
				concat (if (nth i (second start2))
					   (concat "\\(?:"
						   (if rxseps (nth i (second start2))
						     (regexp-opt (list (nth i (second start2))
								       (nth i (second end2)))))
						   "\\)")))
		concat "\\|") 0 -2))))

;; TODO: regexps for extracting json fields, html parts, etc. country names, wikipedia tables
;;       Also following regexp functions are unfinished
;;       Get some ideas from here: http://www.regexmagic.com/patterns.html
(define-arx ip-rx
  '((anydigit (regex "[0-9]"))
    (range (:func (lambda (_form min max &optional alldigits)
		    (let ((maxdgts (mapcar 'string-to-number
					   (split-string (number-to-string max) "" t)))
			  (mindgts (mapcar 'string-to-number
					   (split-string (number-to-string min) "" t))))
		      
		      (setq mindgts (append (make-list (- (length maxdgts)
							  (length mindgts)) 0) mindgts))
		      (cl-loop for i from 0 to (1- (length maxdgts))
			       for mind = (nth i mindgts)
			       for maxd = (nth i maxdgts)
			       collect (number-to-string mind)
			       collect (concat "[" (cl-loop for j from (1+ mind) to (1- maxd)
							    concat (number-to-string j))
					       "]")
			       collect (number-to-string maxd)
			       )
		      ))))
    (hexdigit (regexp "[:xdigit:]"))
    (octet (repeat 2 hexdigit))
    (mac (seq (repeat 5 (seq octet ":")) octet))
    (ipv4digit (regexp "25[0-5]\\|2[0-4][0-9]\\|1?[0-9]{1,2}"))
    (systemport (regexp "[0-9]\\{1,3\\}\\|10\\(?:[01][0-9]\\|2[01234]\\)"))
    (registeredport (regexp ""))
    (dynamicport (regexp ""))
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



(provide 'regex-collection)

;; (org-readme-sync)
;; (magit-push)

;;; regex-collection.el ends here
