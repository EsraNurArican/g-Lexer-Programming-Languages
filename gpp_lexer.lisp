;Determine whether given character alphabetical or not
(defun isLetter (letter )
	(let ( (alphabet (list )))
		(setq alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t  #\u  #\v  #\w  #\x #\y #\z) )
		(if (not (equal(position letter alphabet )nil ))
			t
			nil
		)		
	)
)	

;Determine whether given letter is digit or not
(defun isDigit (letter )
	(let ( (digit (list )))
		(setq digit '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 ) )
		(if (not (equal(position letter digit )nil ))
			t
			nil
		)		
	)
)

;function that makes lexical analysis
(defun lexer(text size)
	;create file object to format t results to parsed_lisp.txt
	(with-open-file (stream "parsed_lisp.txt" 
					:direction :output
                    :if-exists :append
                    :if-does-not-exist :create)

	
	(setq i 0)
	(setq j 0)
	(setq flag nil)
	(setq oc_cc_control 1)
	(loop while (< i size) do 
		
		(setq a_char (char text i))	; Each character of string input

		(cond((or (isLetter a_char) (isDigit a_char))
			(setq temp (string ""))
			(setf j i)

			(loop while (or (isDigit a_char) (isLetter a_char)) do 
				(setq temp (concatenate 'string temp (string a_char)))
				(setq j (+ j 1))
				(setq i (+ i 1))
				(setq a_char (char text j))
			)
			(cond
				;keyword control statements
				((string-equal temp "true")   (progn(format stream "KW_TRUE ~%") (format t "KW_TRUE ~%")) (setq flag t))
			 	((string-equal temp "false")  (progn(format stream "KW_FALSE ~%") (format t "KW_FALSE ~%")) (setq flag t))
			 	((string-equal temp "and")    (progn(format stream "KW_AND ~%") (format t "KW_AND ~%")) (setq flag t))
			 	((string-equal temp "or")     (progn(format stream "KW_OR ~%") (format t "KW_OR ~%")) (setq flag t))
			 	((string-equal temp "not")    (progn(format stream "KW_NOT ~%") (format t "KW_NOT ~%")) (setq flag t))
			 	((string-equal temp "equal")  (progn(format stream "KW_EQUAL ~%") (format t "KW_EQUAL ~%")) (setq flag t))
			 	((string-equal temp "less")   (progn(format stream "KW_LESS ~%") (format t "KW_LESS ~%")) (setq flag t))
			 	((string-equal temp "nil")    (progn(format stream "KW_NIL ~%") (format t "KW_NIL ~%")) (setq flag t))
			 	((string-equal temp "list")   (progn(format stream "KW_LIST ~%") (format t "KW_LIST ~%")) (setq flag t))
			 	((string-equal temp "append") (progn(format stream "KW_APPEND ~%") (format t "KW_APPEND ~%")) (setq flag t))
			 	((string-equal temp "concat") (progn(format stream "KW_CONCAT ~%") (format t "KW_CONCAT ~%")) (setq flag t))
			 	((string-equal temp "set")    (progn(format stream "KW_SET ~%") (format t "KW_SET ~%")) (setq flag t))
			 	((string-equal temp "deffun") (progn(format stream "KW_DEFFUN ~%") (format t "KW_DEFFUN ~%")) (setq flag t))
			 	((string-equal temp "for")    (progn(format stream "KW_FOR ~%") (format t "KW_FOR ~%")) (setq flag t))
			 	((string-equal temp "if")     (progn(format stream "KW_IF ~%") (format t "KW_IF ~%")) (setq flag t))
			 	((string-equal temp "laod")   (progn(format stream "KW_LOAD ~%") (format t "KW_LOAD ~%")) (setq flag t))
			 	((string-equal temp "disp")   (progn(format stream "KW_DISP ~%") (format t "KW_DISP ~%")) (setq flag t))
			 	((string-equal temp "exit")   (exit)(setq flag nil))
			 	
			 	;controlling the value which should not start with zero value
			 	((eq (char temp 0) #\0)
					(if (and ( > (length temp) 1)(isDigit (char temp 1)))
						(progn
							(format stream "SYNTAX ERROR ~%")
							(format t "SYNTAX ERROR ~%")
							(return-from lexer nil)
						)
					)
				)
			 	((isDigit (char temp 0))(progn(format stream "VALUE ~%") (format t "VALUE ~%"))(setq flag t)) 
			 	((isLetter (char temp 0)) (progn(format stream "IDENTIFIER ~%") (format t "IDENTIFIER ~%"))(setq flag t)) 

			)
		))			 

		(cond
			((equal a_char #\tab) (setq flag t))
			((equal a_char #\+) (progn(format stream "OP_PLUS ~%") (format t "OP_PLUS ~%"))(setq flag t))
			((equal a_char #\-) (progn(format stream "OP_MINUS ~%")(format t "OP_MINUS ~%"))(setq flag t))
			((equal a_char #\/) (progn(format stream "OP_DIV ~%")(format t "OP_DIV ~%"))(setq flag t))
			((equal a_char #\*)
				(if(equal (char text (+ i 1)) #\*)
					(progn
						(format stream "OP_DBLMULT ~%")
						(format t "OP_DBLMULT ~%")
						(setq flag t)
						(setq i (+ i 1))
					)
					(progn
						(setq flag t)
						(format stream "OP_MULT ~%")
						(format t "OP_MULT ~%")
					)
				)
			)
			((equal a_char #\() (progn(format stream "OP_OP ~%") (format t "OP_OP ~%"))(setq flag t))	
			((equal a_char #\)) (progn(format stream "OP_CP ~%")(format t"OP_CP ~%")) (setq flag t))
			((equal a_char #\")
				(if(equal oc_cc_control 1)
					(progn
						(format stream "OP_OC ~%")
						(format t "OP_OC ~%")
						(setq flag t)
					 	(setf oc_cc_control 0)
					)
					(progn
						(format stream "OP_CC ~%")
						(format t "OP_CC ~%")
						(setq flag t)
					  	(setf oc_cc_control 1)
					)
				)
			)	
		)
		(if(equal a_char #\;)
			(if(equal (char text (+ i 1)) #\;)
				(progn
					(format stream "COMMENT ~%")
					(format t "COMMENT ~%")
					(setq flag t)
					(return-from lexer flag)
				)
		))
		(setf i (+ i 1))
	) 	
	(return-from lexer flag)

	)
)

	
;Reads file
(defun readFile(filename)

	(let ((in (open filename :if-does-not-exist nil)))
	 	(when in
	    	(loop for line = (read-line in nil)
	       		while line do
					
					(setq input (coerce line 'string))
					(setq size (length input)); text length.
					(setq test (lexer input size))
					(if (eq nil test)
						(progn
							(format stream "Syntax ERROR ~%")
							(return-from readFile)
						)
					)
	    	)
	  	)
	  	(close in)
	 )
)

;function to start interpreter
(defun gppinterpreter() 

	;(with-open-file (str "parsed_lisp.txt") :direction :output )

	(setq arguments *args*)
	(if(> (length arguments) 0 )

		(progn ;input from command line arguments
			(setq filename (first arguments));get the filename from command-line arguments
			(readFile filename))

		(progn ;input from terminal
			(loop
			
				(setf st (read-line))
				(setq size (length st))
				(setq test (lexer st size))
				(if(equal test nil)
					(progn
						(return-from gppinterpreter)
					)
				)
			)
		)
		
	)

)

;calling interpreter function
(gppinterpreter)