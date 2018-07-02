#lang racket

(define descriptions '((1 "what type of movies do you like?")
                       (2 "so u like horror movies?")
                       (3 "want me to search for some horror movies for you?")
                       (4 "want me to search for some non horror movies for you?")))

(define decisiontable `((1 ((horror) 2) ((scary) 2)
                        
                        (2 ((yes) 3) ((yea) 3) ((yeah) 3) ((correct) 3) ((no) 1) ((not correct) 1)
                      
                        (3 ((yes) horror) ((yea) horror) ((yeah) horror) ((no) 4)
            
                        (4 ((yes) non-horror) ((no) quit)))))))
                 
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
