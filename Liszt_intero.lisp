;verica che una frase rispetta il pattern 
(defun match (pattern frase)
  (cond ((null pattern) (null frase))
        ((= (car pattern) '*) (wildcard pattern frase))
        ((= (car pattern) (car frase)) (match (cdr patter) (cdr lista))) ; chiamata ricorsiva
        (T NIL)
  )
)

(defparameter *bindings* nil)

;assegna alle variabili i binding
(defun wildcard (pattern frase)
  (cond
    
    ;es match:
    ;pattern -> * x come * y
    ;frase -> come va
    ;x = null
    ((match (cddr pattern) frase) ;saltiamo la variabile 
         (progn (setf *bindings* (bind (cadr pattern) NIL *bindings*)) t)    
    ) 
    
    ;se la frase è nulla (pattern non può esserlo per match()) allora non va bene (non ho niente da assengare alla variabile)
    ((null frase) NIL)

    ;es match:
    ;pattern -> * x come * y
    ;frase -> ciao come va
    ;x = ciao
    ((match pattern (cdr frase)) 
         (progn (setf *bindings* (bind (cadr pattern) (car frase) *bindings*)) t)    
    )

    ;altrimenti restituisco false
    (T NIL)
  )
)

(defun bind(tagLista valore bindings)
  (cond ((null bindings) (list (cond ((= value nil) (list tagLista))
                                     (T (list tagLista valore)))))
        (T (setq ((key (caar bindings))
                  (valoriRestanti (cadr bindings))
                  (new (swap valore))))
           (cond ((= tagLista key) (cons (cons key (cons new valoriRestanti)) (cdr bindings)))
                 (T (cons (car bindings) (bind tagLista new (cdr bindings))))))))

(defun lookup (key lista)
        (cond   ((null lista) nil)
                ((eq (caar lista) key) (car lista))
                (T (lookup key (cdr lista)))
        )
)


(defun lookup (key lista)
	(cond  	((null lista) nil)
		((eq (caar lista) key) (car lista))
		(T (lookup key (cdr lista)))		
	)
) 

(defparameter *viewpoint* '((I you) (you I) (me you) (am are) (was were) (my your)))

(defun swap (value)
	(setq a (lookup value *viewpoint*))
	(cond 	((null a) value)
		(T (cadr a))
	)
)

(defun subs (list)
	(cond 	((null list) nil)
		(T	(setq a (lookup (car list) *bindings*))
			(cond 	((null a) (cons (car list) (subs (cdr list))))
				(T (append (cdr a) (subs (cdr list))))
			)
		)					
	)
)

;regole generali

(defparameter *rules*
      '(((* x ehi * y) (hello. how can I help ?))

	((* x i want * y) (what would it mean if you got y ?) (why do you want y ?))
	((* x i wish * y) (why would it be better if y ?))
	((* x i hate * y) (what makes you hate y ?))
	((* x if * y)
		(do you really think it is likely that y)
		(what do you think about y))
	
	((* x no * y) (why not?))
	((* x i was * y) (why do you say x you were y ?))
	((* x i feel * y) (do you often feel y ?))
	((* x i felt * y) (what other feelings do you have?))
	((* x) (you say x ?) (tell me more.))
       )
)

;random element

(defun random-elt (list)
	(nth (random (length list)) list)
)

(defparameter  *saluti* '("arrivederci" "ciao" "a presto" "buona giornata"))

;eliza main def

(defun eliza()
	(loop
		(princ "> ")
		(let* ((line (read-line)) (input (read-from-string (concatenate 'string "(" line ")"))))

                (dolist (s *saluti*)
                        (when (string= (format nil "~(~a~)" line) s) (exit))
                )

                (setq *bindings* nil)
                (format t "~{~(~a ~)~}~%"
                        (dolist (r *rules*)
                                (when (match (car r) input)
                                        (return
                                                (subs (random-elt (cdr r))))
					)
				)
			)
		)
	)
)

(eliza)
