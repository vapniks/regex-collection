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
(defun regex-collection-lex-range (min max maxdigit &optional pad)
  "Return lists representing all sequences between MIN and MAX in lexicographic order.
MIN and MAX are lists of numbers representing sequences that can be ordered, e.g. decimal 
numbers (each item in the list represents a digit), words (each item represents a letter), 
dates (first item represents a day, second item represents a month, third item represents 
a year), etc. In each position 0 represents the lowest digit/letter/day/etc. The MAXDIGIT
argument can be either a single positive integer representing the highest digit/letter/day/etc.
or a list of such integers - one for each position in the MAX list (so that different positions
can have different numbers of digits, e.g. dates).
MIN cannot be longer than MAX; if shorter then resulting lists will be left-padded with PAD
 (note: PAD should not be a number or a list).
The return value is a list of lists, each representing a set of sequences between MIN and MAX.
The elements of the returned lists are either single numbers (representing digits/letters/days/etc.)
or cons cells representing a range of digits/letter/days/etc."
  (let* ((minlen (length min))
	 (maxlen (length max))
	 accum)
    ;; make sure we have a separate list of digits for each position
    (if (numberp maxdigit)
	(setq maxdigit (make-list maxlen maxdigit)))
    ;; define some helper functions
    (cl-flet* ((lastelem (lst) (car (last lst)))
	       (item (a b) (cond ((< a b) (cons a b))
				 ((= a b) a)
				 (t nil)))
	       ;; newparts returns new parts to be prepended at POS'th position
	       (newparts (pos) (let* ((dmax (nth pos maxdigit))
				      (pmin (nth pos min))
				      (pmin1 (if (numberp pmin) (1+ pmin) 1))
				      (pmax (nth pos max))
				      (pmax1 (if (numberp pmax) (1- pmax) (1- dmax))))
				 (list pmin
				       (if (> pos 0) (item pmin1 dmax))
				       (if (> pos 0) (item 0 dmax) (item pmin1 pmax1))
				       (if (> pos 0) (item 0 pmax1))
				       pmax)))
	       ;; prepend2parts prepends N to all lists in PARTS
	       (prepend2parts (n &rest parts) 
			      (-flatten-n 1
					  (cl-loop for part in parts
						   collect (cl-loop for lst in part
								    if lst collect (cons n lst))))))
      ;; remove leading 0's from MAX
      (while (not (and (numberp (car max)) (> (car max) 0)))
	(setq max (cdr max)
	      min (cdr min)
	      maxdigit (cdr maxdigit)))
      ;; check that MIN is lower than MAX
      (if (or (> minlen maxlen)
	      (and (= minlen maxlen)
		   (cl-loop for pos from 0 to (1- maxlen)
			    if (< (nth pos max) (nth pos min))
			    return nil)))
	  (error "MIN is larger than MAX"))
      ;; add initial left-padding to min if necessary
      (if (< minlen maxlen)
	  (setq min (append (make-list (- maxlen minlen) pad) min)))
      ;; initial value of accumulator corresponding to final digit
      (let ((lastmin (lastelem min))
	    (lastmax (lastelem max))
	    (lastdgt (lastelem maxdigit)))
	(setq accum (list (list (list (item lastmin lastdgt)))
			  (list nil)
			  (list (list (item 0 lastdgt)))
			  (list nil)
			  (list (list (item 0 lastmax))))))
      ;; loop backward over digit positions, prepending appropriate digits as we go
      (cl-loop for pos downfrom (- maxlen 2) to 0
	       for (a1 b1 c1 d1 e1) = accum  
	       for (a2 b2 c2 d2 e2) = (newparts pos)
	       do (setq accum (list (prepend2parts a2 a1 b1)
				    (if (> pos 0) (prepend2parts b2 c1))
				    (prepend2parts c2 c1)
				    (if (> pos 0) (prepend2parts d2 c1))
				    (prepend2parts e2 d1 e1)))))
    (-flatten-n 1 accum)))

;;;###autoload
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
		  if (numberp item)
		  collect (if rxstrs (nth item (nth pos strs))
			    (regexp-quote (nth item (nth pos strs))))
		  else if (consp item)
		  collect (if rxstrs (apply 'concat (subseq (nth pos strs) (car item) (cdr item)))
			    (regexp-opt (subseq (nth pos strs) (car item) (cdr item))))
		  else if (stringp item)
		  collect (if rxstrs item
			    (regexp-quote item))
		  if (and seps (< pos (1- (length lst))))
		  collect (if rxseps (nth pos seps)
			    (regexp-quote (nth pos seps))))))


;; TODO: regexps for extracting json fields, html parts, etc.
;;       Also following regexp functions are unfinished

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
