#lang racket

(define descriptions '((1 "what type of movies do you like?")
                       (2 "so u like horror movies?")
                       (3 "want me to search for some horror movies for you?")
                       (4 "want me to search for some non horror movies for you?")))

(define decisiontable `((1 ((horror) 2) ((scary) 2)
                        
                        (2 ((yes) 3) ((yea) 3) ((yeah) 3) ((correct) 3) ((no) 1) ((not correct) 1)
                      
                        (3 ((yes) horror) ((yea) horror) ((yeah) horror) ((no) 4)
            
                        (4 ((yes) non-horror) ((no) quit)))))))


(define (get-location id)
  (printf "~a\n" (car (assq-ref descriptions id))))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (lookup id tokens) 
  (let* ((record (assv-ref decisiontable id))        
         (keylist (get-keywords id))         
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
       (if index 
      (cadr (list-ref record index))
      #f)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))

(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       (* (/ (length set) (length x)) (length set))))
   keylist))
 
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))  


(define (search initial-id)

    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              
              ((eq? #f response)
               (format #t "Sorry! I didn't understand that... \n")
               (loop id #f))))))

(search 1)
