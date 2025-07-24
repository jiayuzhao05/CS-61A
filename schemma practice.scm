;函数式编程语言，属于 Lisp 语言家族
;极简语法：所有代码都用括号包裹，表达式统一为前缀记法 (操作数 参数1 参数2 ...)
;一等函数：函数可以作为参数、返回值、变量
;广泛使用递归
;动态类型：变量类型在运行时确定


; Let example

(define (star n m) 
  (let ((a (/ (* 360 m) n)))
    (define (side k) 
      (if (< k n) (begin (fd 100) (rt a) (side (+ k 1))))) 
    (side 0)))

; List demos

(define s (cons 1 (cons 2 nil)))
(cons 3 s)
(cons 4 (cons 3 s))
(cons (cons 4 (cons 3 nil)) s)
(car (car (cons (cons 4 (cons 3 nil)) s)))
(cons s (cons s nil))

(list? s)
(list? nil)
(list? 4)
(null? nil)
(null? s)

(list 1 2)
(list 1 2 3 4)
(cdr (list 1 2 3 4))
(cons 0 (cdr (list 1 2 3 4)))

(list s)
(list 3 s)
(cons 3 s)
(list s s)
(cons s s)

;;; Return a list of two lists; the first n elements of s and the rest
;;; scm> (split (list 3 4 5 6 7 8) 3)
;;; ((3 4 5) (6 7 8))
(define (split s n)
  ; The first n elements of s
  (define (prefix s n)
    (if (zero? n) nil (cons (car s) (prefix (cdr s) (- n 1)))))
  ; The elements after the first n
  (define (suffix s n)
    (if (zero? n) s (suffix (cdr s) (- n 1))))
  (list (prefix s n) (suffix s n)))


(define (split s n)
  (if (= n 0) 
      (list nil s) 
      (let ((split-rest (split (cdr s) (- n 1)))) 
	(cons (cons (car s) (car split-rest)) 
	      (cdr split-rest)))))

; Quotation demos

'(1 2 3)
(quote (1 2 3))
'(1 (2 3) 4)
(car (cdr (car (cdr '(1 (2 3) 4)))))
(car (cdr (car (cdr '(a (b c) d)))))
'(+ 1 2)
(car (quote (+ 1 2)))
(car '(+ 1 2))
(cons '+ (list 1 2))


; (fd 100)：向前移动 100 单位
; (rt 90)：右转 90 度
; (fd 40)：向前移动 40 单位
; (bk 100)：向后移动 100 单位
; (lt 90)：左转 90 度

#|
(define (line) (fd 50))  定义了一个名为 line 的函数，每次调用会向前画 50 单位的线
(line)   调用 line，画一条线。
(define (twice fn) (fn) (fn))  定义了 twice，它接受一个函数 fn，并调用两次
|#


#|
(sier 5 200)   调用 sier 函数,但报错 unknown identifier: sier，说明当前环境没有定义 sier
(load 'ex.scm)   加载外部 Scheme 文件 ex.scm，文件中定义了 sier 及其它辅助函数
(define (repeat k fn) (fn) (if (> k 1) (repeat (- k 1) fn)))   重复调用 fn 共 k 次
(define (tri fn) (repeat 3 (lambda () (fn) (lt 120))))   画三次 fn 并每次左转 120 度，形成三角形
(define (sier d k) ...)   递归画 Sierpinski 三角形
|#


(define (sum-while initial-x condition add-to-total update-x)
  (begin
    (define (f x total)
      (if condition
          (f update-x (+ total add-to-total))
          total))
    (f initial-x 0)))

#|
sum-while 是一个高阶函数，参数分别是：
initial-x：初始值
condition：循环条件（表达式）
add-to-total：每次要加到总和的表达式
update-x：每次如何更新 x 的表达式

|#

(sum-while 1
  '(< (* x x x) 50)
  'x
  '(+ x 1))

#|
1：初始 x
(< (* x x x) 50)：条件，x 的立方小于 50
x：每次加到总和的就是当前 x
(+ x 1)：每次 x 加 1
|#


#| Q1
(define (curry-cook formals body)
  (if (null? (cdr formals))
      (list 'lambda (list (car formals)) body)
      (list 'lambda (list (car formals))
            (curry-cook (cdr formals) body))))



#| Q2
(define (curry-consume curry args)
  (if (null? args)
      curry
      (curry-consume (curry (car args)) (cdr args))))

#| Q3
(define (switch-to-cond switch-expr)
  (cons 'cond
        (map
         (lambda (option)
           (cons (list 'equal? (cadr switch-expr) (car option))
                 (cdr option)))
         (car (cddr switch-expr)))))


(define (switch-to-cond switch-expr)
  (cons 'cond
        (map
         (lambda (option)
           (cons (list 'equal? (cadr switch-expr) (car option))
                 (cdr option)))
         (car (cddr switch-expr)))))


#| Problem 8: make-child-frame
(define (make-child-frame parent formals vals)
  (if (not (= (length formals) (length vals)))
      (error "SchemeError: Number of formals and values do not match"))
  (let ((child (make-frame parent)))
    (define (bind-params f v)
      (if (null? f)
          'done
          (begin
            (frame-define! child (car f) (car v))
            (bind-params (cdr f) (cdr v)))))
    (bind-params formals vals)
    child))


#| Problem 9: LambdaProcedure in scheme-apply
(define (scheme-apply procedure args env)
  (cond
    ((lambda-procedure? procedure)
     (let ((new-frame (make-child-frame (lambda-procedure-env procedure)
                                        (lambda-procedure-formals procedure)
                                        args)))
       (eval-all (lambda-procedure-body procedure) new-frame)))
    ;; ... 其他分支 ...
  ))


#| Problem 10: define
(define (do-define-form exprs env)
  (let ((target (car exprs)))
    (if (pair? target)
        ;; 语法糖 (define (f x) body ...)
        (let ((func-name (car target))
              (formals (cdr target))
              (body (cdr exprs)))
          (env-define! env func-name
            (make-lambda-procedure formals body env))
          func-name)
        ;; 普通 define
        (let ((value (eval (cadr exprs) env)))
          (env-define! env target value)
          target))))

  
#| Problem 11: mu
(define (do-mu-form exprs env)
  (let ((formals (car exprs))
        (body (cdr exprs)))
    (make-mu-procedure formals body)))

(define (scheme-apply procedure args env)
  (cond
    ((mu-procedure? procedure)
     (let ((new-frame (make-child-frame env
                                        (mu-procedure-formals procedure)
                                        args)))
       (eval-all (mu-procedure-body procedure) new-frame)))
    ;; ... 其他分支 ...
  ))


#| Problem 12: and/or
(define (do-and-form exprs env)
  (if (null? exprs)
      #t
      (let ((result (eval (car exprs) env)))
        (if (not result)
            #f
            (do-and-form (cdr exprs) env)))))

(define (do-or-form exprs env)
  (if (null? exprs)
      #f
      (let ((result (eval (car exprs) env)))
        (if result
            result
            (do-or-form (cdr exprs) env)))))


#| ((a b) c d (e))
(define with-list
  (list
    (list 'a 'b) 'c 'd (list 'e)
  )
)
; (draw with-list)  ; Uncomment this line to draw with-list


#| (first c d (e))
(define with-cons
      (cons
          first (cons 'c (cons 'd (cons (cons 'e nil) nil)))
      )
  )
  ; (draw with-cons)  ; Uncomment this line to draw with-cons



;;; Return a list of pairs containing the elements of s.
    ;;;
    ;;; scm> (pair-up '(3 4 5 6 7 8))
    ;;; ((3 4) (5 6) (7 8))
    ;;; scm> (pair-up '(3 4 5 6 7 8 9))
    ;;; ((3 4) (5 6) (7 8 9))
    (define (pair-up s)
        (if (<= (length s) 3)
            (list s)
            (cons (list (car s) (car (cdr s))) (pair-up (cdr (cdr s))))
        ))

    (expect (pair-up '(3 4 5 6 7 8)) ((3 4) (5 6) (7 8)) )
    (expect (pair-up '(3 4 5 6 7 8 9)) ((3 4) (5 6) (7 8 9)) )