(require 'thingatpt)

;; Automatically fix indentation when the user presses space or the enter key.
;; There are a few general cases across all programming languages that govern
;; how we handle fixing the line:
;;
;;  * Inside a comment: If we're typing inside a comment we want to
;;    automatically put any text that overflows into another comment. If the
;;    delimeter used is for single lines, than that means putting another
;;    delimeter on the next line. Otherwise we want to intelligently determine
;;    indentation.
;;
;;  * Inside a string: If we're typing in some type of string literal, we want
;;    to continue the string on another line in however way the language allows
;;    us to continue strings.
;;
;;  * Overlow in other types of code: We want to intelligently determine what
;;    we put on the next line. Usually we do just the word that overflows, but
;;    the user should be able to specify extra words that would continue on the
;;    next line, such as logical or mathematical operators.
;;

(defgroup wrap nil
  "Wrap text group."
  :prefix "wrap-"
  :group 'tools)

(defvar wrap-line-width 80
  "Length of a line to wrap at. Technically specifies a column number to wrap
at, but that distinction is irrelevent until I can ensure this works with
proportional fonts")

(defun find-column-at (pos)
  (save-excursion
	(goto-char pos)
	(current-column)))

(defun find-delimeter (delim)
  (save-excursion
	(beginning-of-line)
	(search-forward delim (line-end-position) t)))
  
(defun find-comment-spacing (pos delim-len)
  "Finds the spacing between the comment delimeter and the text in the comment.
We want to ignore all characters except for alphabetical characters and the
first number that appears on the line. This will properly indent comments that
contain bullet points or number lists."
  (save-excursion
	(goto-char pos)
	(forward-char delim-len)
;;	(message "'%c'" (following-char))
	(let ((i 0))
	  (while (progn
			   (forward-char 1)
;;			   (message "'%c'" (following-char))
			   (setq i (1+ i))
			   (= (following-char) ? )))
;;	  (message "'%c'" (char-after i))
	  (1+ i))))

(defun wrap-string (start-delim line-end line-continue end-delim)
  "Wrap a string that overflows the line. Assumes that the point is located at
the start of the string.
Wraps by ensuring that line-end can fit to the end of the current line, creating
a new line, adding line-continue the wrapped text and then end.
If a line with continue already exists the text is added to that, unless there
are characters that appear after the end of the string, such as a comma."
  (let* ((string-start (point))
		 (string-end (find-delimeter end-delim)))
	(if string-end
		(progn
		  ;; The string has been terminated on this line so we must preserve the
		  ;; content that appears after it.
		  ;; REMEMBER: Only fix overflow if the string-end appears 
		  )
	  (progn
		;; The string has not been terminated so we must create a new line
		;; and continue the string.
		(move-to-column (- 80 (length line-end)))
		(let ((wrap-beg (bounds-of-thing-at-point 'word)))
		  (when wrap-beg
			(goto-char (- (car wrap-beg) 1))))
		(let ((wrap (buffer-substring-no-properties
					 (point) (line-end-position))))
		  (delete-region (point) (line-end-position))
		  ;; Unlike with comments, we do not care about strings that appear on
		  ;; the next line. We should keep each string seperate to avoid
		  ;; confusing distinct literals.
		  ;; Additionally, we let the major mode decide indentation and undo
		  ;; if text becomes badness 10000.
		   )))))
			
								  

;; TODO: Constantly check to ensure we have enough room. If we don't, default to
;; something that is good enough, such as not wrapping.
(defun wrap-comment-on-line ()
  "Wrap the comment on the current line.
Single line comments are wrapped by repeating whatever characters precede the
first alphabetic character on the current line on the next and then adding the
wrapped word. Adjustments are made based on if a comment appears in the next
line.

If the word that we want to wrap is the only word on that line, we do not wrap."
  (let ((end-of-line (find-column-at (line-end-position))))
	(message "Line end pos: %d" end-of-line)
	(if (> end-of-line 79)
		(progn
		  ;; This line exceeds the limit
		  (let ((comment-pos (find-delimeter "//")))
			(when comment-pos
			  ;; There is a comment on this line and we need to wrap it.
			  ;; Find how much spacing there was between the comment delimeter
			  ;; and the first word on the line and find the content that needs
			  ;; to be wrapped.
			  (let* ((comment-indent (find-comment-spacing
									  comment-pos (length "//")))
					 (delimeter-indent
					  (- (find-column-at comment-pos)
						 (find-column-at (line-beginning-position))
						 2)))
				(move-to-column 80)
				(let ((wrap-beg (bounds-of-thing-at-point 'word)))
				  (when wrap-beg
					(goto-char (- (car wrap-beg) 1))))
				;; Move backwards at most one character to capture any spaces
;;				(if ( (following-char) ? )
;;					(backward-char))
				(let ((wrap (buffer-substring-no-properties
							 (point) (line-end-position))))
				  (delete-region (point) (line-end-position))
				  ;; If there's already a comment delimiter on the next line we
				  ;; want to insert the wrapped text into it. Otherwise we want
				  ;; to create our own line.
				  (beginning-of-line)
				  ;; (message "Line %d"
				  ;; (1+ (count-lines 1 (point))))
				  (forward-line)
				  (let ((next-comment-pos (search-forward "//"
														  (line-end-position)
														  t)))
					(if next-comment-pos
						(progn
						  ;; The line has a comment. Insert the text
						  (let ((next-line-indent (find-comment-spacing
												   next-comment-pos
												   (length "//"))))
							(what-line)
							(forward-char next-line-indent)
							(insert-char ?  (- comment-indent next-line-indent))
							(insert wrap))
						  ;; Wrap the next line of the comment.
						  (save-excursion
							(beginning-of-line)
							(wrap-comment-on-line)))
					  (progn
						;; We need to put the comment on a new line.
						(when (= (point) (point-max))
						  (newline))
						(insert-char ?  delimeter-indent)
						(insert "//")
						(insert-char ?   comment-indent)
						(message wrap)
						(insert wrap)
						(save-excursion
						  (newline)))))))))))))


(defun wrap-comment-at-point ()
  "Wrap comments at the point"
  (interactive)
  ;;	(beginning-of-line)
  ;;   (insert " ")
  (if (= (point) (line-end-position))
	  (progn
		(wrap-comment-on-line)
		(insert " "))
	(progn
	  (insert " ")
	  (save-excursion
		(wrap-comment-on-line)))))

;;;###autoload
(define-minor-mode wrap-comments-mode
  "Toggle wrap comments mode.

Automatically wrap comments in source code that exceed the maximum column width."
  :init-value nil
  :lighter " wrap comments"
  :keymap '(([? ] . wrap-comment-at-point)))

(provide wrap-comments-mode)
