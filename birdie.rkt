#lang racket

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
