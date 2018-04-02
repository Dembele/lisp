;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;№10. Определите функцию, осуществляющую удаление указанного количества по-	;
;;;следних элементов исходного списка.						;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN DELETE-END (LIST)
  (COND ((NULL (CDR LIST)) NIL) (T (CONS (CAR LIST) (DELETE-END (CDR LIST))))))
  
(DEFUN DELETE-END (LIST)
  (COND ((NULL (CDR LIST)) NIL) (T (CONS (CAR LIST) (DELETE-END (CDR LIST))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Тест-кейсы:					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(delete-last-n '(1 2 3 4 5) 0)	; (1 2 3 4 5)
(delete-last-n '(1 2 3 4 5) 1)	; (1 2 3 4)
(delete-last-n '(1 2 3 4 5) 2)	; (1 2 3)
(delete-last-n '(1 2 3 4 5) 3)	; (1 2)
(delete-last-n '(1 2 3 4 5) 4)	; (1)
(delete-last-n '(1 2 3 4 5) 5)	; NIL
(delete-last-n '(1 2 3 4 5) 6)	; NIL
(delete-last-n '(1 2 3 4 5) -1)	; (1 2 3 4 5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;№11. Определите функцию, осуществляющую разделение исходного списка на два	;
;;;подсписка. В первый из них должно попасть указанное количество элементов	;
;;;с начала списка, во второй — оставшиеся элементы.				;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN SEPARATE-AFTER-N (INPUT-LIST N)
  (IF INPUT-LIST
      (IF (ZEROP N)
          (CONS NIL (CONS INPUT-LIST NIL))
          ((LAMBDA (ELEM RESULT) (CONS (CONS ELEM (CAR RESULT)) (CDR RESULT)))
           (CAR INPUT-LIST) (SEPARATE-AFTER-N (CDR INPUT-LIST) (1- N))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Тест-кейсы:					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(separate-after-n '(1 2 3 4 5) 0)	; (NIL (1 2 3 4 5))
(separate-after-n '(1 2 3 4 5) 1)	; ((1) (2 3 4 5))
(separate-after-n '(1 2 3 4 5) 2)	; ((1 2) (3 4 5))
(separate-after-n '(1 2 3 4 5) 3)	; ((1 2 3) (4 5))
(separate-after-n '(1 2 3 4 5) 4)	; ((1 2 3 4) (5))
(separate-after-n '(1 2 3 4 5) 5)	; ((1 2 3 4 5))
(separate-after-n '(1 2 3 4 5) 6)	; ((1 2 3 4 5))
(separate-after-n '(1 2 3 4 5) -1)	; ((1 2 3 4 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;№14. Определите функцию, осуществляющую перестановку двух элементов списка	;
;;;с заданными номерами.							;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN GET-RANGE-START-END (INPUT-LIST ST END)
  (COND ((ZEROP END) NIL)
        ((AND (= ST 1) (> END 0))
         (CONS (CAR INPUT-LIST)
               (GET-RANGE-START-END (CDR INPUT-LIST) 1 (1- END))))
        (T (GET-RANGE-START-END (CDR INPUT-LIST) (1- ST) END))))
 
(DEFUN SWAP (INPUT-LIST N1 N2)
  (COND ((= N1 N2) INPUT-LIST) ((> N1 N2) (SWAP INPUT-LIST N2 N1))
        (T
         (APPEND (GET-RANGE-START-END INPUT-LIST 1 (- N1 1))
                 (LIST
                  (CAR
                   (GET-RANGE-START-END INPUT-LIST N2
                    (- (LENGTH INPUT-LIST) N2 -1))))
                 (CDR (GET-RANGE-START-END INPUT-LIST N1 (- N2 N1)))
                 (LIST (CAR (GET-RANGE-START-END INPUT-LIST N1 (- N2 N1))))
                 (CDR
                  (GET-RANGE-START-END INPUT-LIST N2
                   (- (LENGTH INPUT-LIST) N2 -1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Тест-кейсы:					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(swap `(1 2 3 4 5) 2 4)		; (1 4 3 2 5)
(swap `(1 2 3 4 5) 1 2)		; (2 1 3 4 5)
(swap `(1 2 3 4 5) 3 1)		; (3 2 1 4 5)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;№18. Определите предикат, проверяющий, является ли аргумент одноуровневым	;
;;;списком.									;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN FLATLISTP (INPUT-LIST)
  (COND ((NULL (CAR INPUT-LIST)) T) ((NOT (ATOM (CAR INPUT-LIST))) NIL)
        (T (FLATLISTP (CDR INPUT-LIST)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Тест-кейсы:					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(flatlistp '(1 2 3 4))			; T
(flatlistp '((1 2) 3 4))		; NIL
(flatlistp '((1) 2 3 4))		; NIL
(flatlistp `(1 2 (3) 4))		; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;№22. Определите функцию, которая обращает список (а b с) и разбивает его на	;
;;;уровни (((с) b) а).								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN NEST-AND-REVERSE (INPUT-LIST)
  (COND ((NULL INPUT-LIST) NIL)
        (T
         ((LAMBDA (ELEM RESULT)
            (COND ((NULL RESULT) (LIST ELEM))
                  (T (CONS (CONS (CAR RESULT) (CDR RESULT)) (LIST ELEM)))))
          (CAR INPUT-LIST) (NEST-AND-REVERSE (CDR INPUT-LIST))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Тест-кейсы:					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nest-and-reverse `())		; NIL
(nest-and-reverse `(1))		; (1)
(nest-and-reverse `(1 2))	; ((2) 1)
(nest-and-reverse `(1 2 3 4))	; ((((4) 3) 2) 1) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;№26. Определите функцию, разбивающую список (a b с d...) на пары ((а b) (с	;
;;;d)...).									;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN PAIR (INPUT-LIST)
  (COND ((NULL INPUT-LIST) NIL)
        ((NULL (CDR INPUT-LIST)) (LIST (CAR INPUT-LIST)))
        (T
         (CONS (LIST (CAR INPUT-LIST) (CADR INPUT-LIST))
               (PAIR (CDDR INPUT-LIST))))))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Тест-кейсы:					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pair `(1 2 3 4))	;((1 2) (3 4))
(pair `(1 2 3))		;((1 2) 3)
(pair `(1 2 3 4 5))	;((1 2) (3 4) 5)
(pair `())		;NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;№27. Определите функцию, которая, чередуя элементы списков (a b...) и 	;
;;;(1 2...), образует новый список (a 1 b 2 ...).				;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN MERGE-ELEMENTS (INPUT-LIST1 INPUT-LIST2)
  (COND ((AND (NULL INPUT-LIST1) (NULL INPUT-LIST2)) NIL)
        (T
         (COND
          ((NULL INPUT-LIST1)
           (CONS (CAR INPUT-LIST2) (MERGE-ELEMENTS NIL (CDR INPUT-LIST2))))
          ((NULL INPUT-LIST2)
           (CONS (CAR INPUT-LIST1) (MERGE-ELEMENTS (CDR INPUT-LIST1) NIL)))
          (T
           (CONS (CAR INPUT-LIST1)
                 (CONS (CAR INPUT-LIST2)
                       (MERGE-ELEMENTS (CDR INPUT-LIST1)
                                       (CDR INPUT-LIST2)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Тест-кейсы:					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(merge-elements `(1 2 3) `(a b c))		;(1 A 2 B 3 C)
(merge-elements `(1 2 3 4 5 6) `(a b c))	;(1 A 2 B 3 C 4 5 6)
(merge-elements `(1 2 3) `(a b c d e f))	;(1 A 2 B 3 C D E F)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;№29. Определите функцию, вычисляющую глубину списка (самой глубокой ветви)	;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN DEPTH (LIST)
  (COND ((ATOM LIST) 0) ((NULL LIST) 0)
        (T (MAX (+ 1 (DEPTH (CAR LIST))) (DEPTH (CDR LIST))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Тест-кейсы:					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEPTH `(3
			(2
				(1 NIL NIL)
			NIL)
				(5
					(6 NIL NIL) ; this one (4)
				(6 NIL
					(7 NIL NIL))))) ; or this one (4)
					
(DEPTH `(1)) ; 1
(DEPTH `()) ; 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;№46. Предположим, что отец и мать некоторого лица, хранятся как значения со-	;
;;;ответствующих свойств у символа, обозначающего это лицо. Напишите функ-	;
;;;цию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей, и пре-	;
;;;дикат (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 — сест-	;
;;;ры или братья, родные или с одним общим родителем.				;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN GET-PARENTS (PERSON)
  (LIST (LIST `(MOTHER) (GET PERSON `MOTHER))
        (LIST `(FATHER) (GET PERSON `FATHER))))

(DEFUN SET-PARENTS (&KEY PERSON MOTHER FATHER)
  (SETF (GET PERSON `MOTHER) MOTHER)
  (SETF (GET PERSON `FATHER) FATHER)
  (GET-PARENTS PERSON))

(DEFUN ARE-SIBLINGS (PERSON1 PERSON2)
  (COND ((EQUAL (CDAR (GET-PARENTS PERSON1)) (CDAR (GET-PARENTS PERSON2))) T)
        ((EQUAL (CDADR (GET-PARENTS PERSON1)) (CDADR (GET-PARENTS PERSON2))) T)
        (T NIL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Тест-кейсы:					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-parents :person 'Peter :mother 'Alice :father 'Dread)
(set-parents :person 'Dennis :mother 'Pall :father 'Jane)
(set-parents :person 'Colin :mother 'Merry :father 'Dread)
(set-parents :person 'Genry :mother 'Alice :father 'Dread)
(set-parents :person 'Susie :mother 'Alice :father 'Mat)

(are-siblings `peter `genry) ;both mother and father match    :		T
(are-siblings `peter `susie) ;only mother		      :		T
(are-siblings `genry `colin) ;only father		      :		T
(are-siblings `Dennis `genry);no match			      :		NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;№47. Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства	;
;;;символа.									;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN REMOVE-ALL-PROPERTIES (SYMBOL)
  (COND ((NULL (SYMBOL-PLIST SYMBOL)) T)
        (T (REMPROP SYMBOL (CAR (SYMBOL-PLIST SYMBOL)))
         (REMOVE-ALL-PROPERTIES SYMBOL))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				Тест-кейсы:					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (get 'annie 'age) 43)
(setf (get 'annie 'job) 'accountant)
(setf (get 'annie 'sex) 'female)
(setf (get 'annie 'children) 3)

(symbol-plist 'annie)					;(CHILDREN 3 SEX FEMALE JOB ACCOUNTANT AGE 43)
(remove-all-properties 'annie)				;T (all properties removed)
(symbol-plist 'annie)					;NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
