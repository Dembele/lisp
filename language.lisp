;Новая структура parser с слотами token output operator
(defstruct parser tokens output operators)

;забирает один символ из парсера инфиксной записи
(defun consume-token (parser)
  (pop (parser-tokens parser)))

;определяет список операторов и их важность
(defvar operator-precedence
  '((nil . 0)
    (+ . 1) (- . 1)
    (* . 2) (/ . 2)
))

;определяет важность операторов
(defun precedence (operator)
  (cdr (assoc operator operator-precedence)))

 ;определяет является ли символ оператором
(defun operatorp (symbol)
  (member symbol operator-precedence :key #'car))

 ;приводит инфиксную запись в перфиксную посредством редуцирования выражения
 ;(inf 2 + 1 - 3) -> ((+ 2 1) -> ((- (+ 2 1) 3))
(defun call-operator (operator parser)
  (let* ((y (pop (parser-output parser)))
         (x (pop (parser-output parser)))
         (nils (count nil (list x y))))
    (if (> nils 0)
        (error "Operator ~s expected 2 arguments, but got ~s"
               operator (- 2 nils))
        (push (list operator x y) (parser-output parser)))))

;вспомогательная функция которая вызывает осточные операторы после первого редуцирования
(defun call-remaining-operators (parser)
  (loop for operator = (pop (parser-operators parser))
     while operator do (call-operator operator parser)
     finally (return (first (parser-output parser)))))

;разбирает выражение на нумералы и операторы
(defun push-operator (operator parser)
  (loop while (<= (precedence operator)
                  (precedence (first (parser-operators parser))))
     do (call-operator (pop (parser-operators parser)) parser))
  (push operator (parser-operators parser)))

 ;основная функция-парсер. Она и возращает лисповое выражение из токенов.
(defun run-parser (parser)
  (loop while (parser-tokens parser)
     for token = (consume-token parser)
     if (operatorp token) do
       (push-operator token parser)
     else do
       (push token (parser-output parser))
     finally (return (call-remaining-operators parser))))

;окружение для вызова парсера.
(defmacro inf (&rest tokens)
  (run-parser (make-parser :tokens tokens)))


;tests
(inf 2 + 1 - 3)
(inf 2 * 2 / 4)