#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

(define descriptions '((1 "\n
                           Hello! I am Birdie.
                           I can search and recommend millions of movies within seconds... \n
Type in a movie genre:")

                      
                       (2 "So you like horror, am I correct?")
                       (22 "So you like comedy, am I correct?")
                       (222 "So you like romantic, am I correct?")
                       (2222 "So you like animated, am I correct?")
                       (22222 "So you like sci-fi, am I correct?")
                                            
                       (3 "Want me to search for some horror movies?")
                       (33 "Want me to search for some comedy movies?")
                       (333 "Want me to search for some romantic movies?")
                       (3333 "Want me to search for some animated movies?")
                       (33333 "Want me to search for some sci-fi movies?")
                       
                       (4 "Well then... Want me to fly elsewhere and suggest some non-horror movies for you then instead?")
                       (44 "Well then... Want me to fly elsewhere and suggest some non-comedy movies for you then instead?")
                       (444 "Well then... Want me to fly elsewhere and suggest some non-romantic movies for you then instead?")
                       (4444 "Well then... Want me to fly elsewhere and suggest some non-animated movies for you then instead?")
                       (44444 "Well then... Want me to fly elsewhere and suggest some non-sci-fi movies for you then instead?")))

(define faq '(((faq) faq)))
(define quit '(((quit) quit) ((exit) quit) ((end) quit)))

(define commands `(,@faq ,@quit))

(define decisiontable `((1 ((horror) 2) ((scary) 2) ((comedy) 22) ((funny) 22) ((romantic) 222) ((love) 222) ((animated) 2222) ((disney) 2222) ((sci-fi) 22222) ((scifi) 22222) ,@commands)
                        
                        (2 ((yes) 3) ((yea) 3) ((yeah) 3) ((correct) 3) ((no) 1) ((not correct) 1) ,@commands)
                        (22 ((yes) 33) ((yea) 33) ((yeah) 333) ((correct) 33) ((no) 1) ((not correct) 1) ,@commands)
                        (222 ((yes) 333) ((yea) 333) ((yeah) 333) ((correct) 333) ((no) 1) ((not correct) 1) ,@commands)
                        (2222 ((yes) 3333) ((yea) 3333) ((yeah) 3333) ((correct) 3333) ((no) 1) ((not correct) 1) ,@commands)
                        (22222 ((yes) 33333) ((yea) 33333) ((yeah) 33333) ((correct) 33333) ((no) 1) ((not correct) 1) ,@commands)
                        
                        (3 ((yes) horror) ((yea) horror) ((yeah) horror) ((no) 4) ,@commands)
                        (33 ((yes) comedy) ((yea) comedy) ((yeah) comedy) ((no) 44) ,@commands)
                        (333 ((yes) romantic) ((yea) romantic) ((yeah) romantic) ((no) 444),@commands)
                        (3333 ((yes) animated) ((yea) animated) ((yeah) animated) ((no) 4444) ,@commands)
                        (33333 ((yes) sci-fi) ((yea) sci-fi) ((yeah) sci-fi) ((no) 44444) ,@commands)
                                                
                        (4 ((yes) non-horror) ((no) quit) ,@commands)
                        (44 ((yes) non-comedy) ((no) quit) ,@commands)
                        (444 ((yes) non-romantic) ((no) quit) ,@commands)
                        (4444 ((yes) non-animated) ((no) quit) ,@commands)
                        (44444 ((yes) non-sci-fi) ((no) quit) ,@commands)))                    
        
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

(define (display-faq)
  (printf "
          Frequently Asked Questions \n
          What is Birdie?
          I am a highly powerful search engine capable of finding almost any movie released after 1950. \n
          How does Birdie work?
          Type in any movie genre into the search box and I will do the rest for you. \n
          How many movies is Birdie able to recommend?
          I can search for over 200 million movies from over 15 different genres. \n
          Are you case-sensitive?
          yes! lowercase only please! \n"))

(define (search initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        (get-location id)
        (printf "> "))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              
              ((eq? #f response)
               (format #t "Sorry! I didn't understand that... \n")
               (loop id #f))

              ((eq? response 'horror)              
               (format #t "Flying away to search for horror movies... \n")
               (exit))
              ((eq? response 'non-horror)              
               (format #t "Flying away to search for non-horror movies... \n")
               (exit))

              ((eq? response 'comedy)              
               (format #t "Flying away to search for comedy movies... \n")
               (exit))
              ((eq? response 'non-comedy)              
               (format #t "Flying away to search for noncomedy movies... \n")
               (exit))

              ((eq? response 'romantic)              
               (format #t "Flying away to search for romantic movies... \n")
               (exit))
              ((eq? response 'non-romantic)              
               (format #t "Flying away to search for non-romantic movies... \n")
               (exit))

              ((eq? response 'animated)              
               (format #t "Flying away to search for animated movies... \n")
               (exit))
              ((eq? response 'non-animated)              
               (format #t "Flying away to search for non-animated movies... \n")
               (exit))

              ((eq? response 'sci-fi)              
               (format #t "Flying away to search for sci-fi movies... \n")
               (exit))
              ((eq? response 'non-sci-fi)              
               (format #t "Flying away to search for non-sci-fi movies... \n")
               (exit))
              
              ((eq? response 'faq)
                (display-faq)
                (loop id #f))

              ((eq? response 'quit)
               (format #t "Thanks for using Birdie! \n")
               (exit)))))))
(search 1)