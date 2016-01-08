
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2015                                *
; *  Student Version                          *
; *********************************************

;;the load include.ss + load test-dictionary.ss were included in original file
;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "test-dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***
(define First
  (lambda (bit-vector word)
    (if (null? word) #t
        (Second bit-vector (car word)))))

(define Second
  (lambda (list poop)
    (cond ((member poop list) #t) (else #f))))

(define MakingHashies
  (lambda (hashfunctionlist dict)
    (if (null? hashfunctionlist)'() ;;base case, return empty
        (append (map (car hashfunctionlist) dict)
        (MakingHashies (cdr hashfunctionlist) dict)))))

(define generate-vec ;;alt method if i can't figure out first.
   (lambda(hashfunctionlist dict)
     ;;base cases
     (if(null? hashfunctionlist)'()
     ;;append map first part of hash function list - isolate
        (append (map (car hashfunctionlist) dict);;map takes a function and applies it to entire list
        (generate-vec (cdr hashfunctionlist) dict)))))
       ;;output is '(index_1 index_2)
       ;;function call(hash-x <word>)
       ;;given hashlist [hash-1 hash-2 hash-3]
       ;;split into [hash-1 (dictionary word 1)] isolate
       ;;recursive call to dictionary



;; -----------------------------------------------------
;; KEY FUNCTION
     ;; takes a word as input
     ;; maps it to its key using cvt
(define key
  (lambda (w)
     (if(null? w) ;;check if word is empty string
         5387
      (+ (* (key(cdr w)) 31) (ctv (car w)));;key 31 * key + ctv(c)
  )
    
))

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))     = 154238504134
;;   (key '(w a y))         = 160507203 
;;   (key '(r a i n b o w)) = 148230379423562

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda(w)
       (modulo (key w) size);;k modulo size
)))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
         (lambda (w)
           ;;step 1- multiple key by cons A
           ;;extract the fractional part of kA
           ;;multiply that value with size
           ;;take the floor of the result
           (floor (* (- (* (key w) A) (floor (* (key w) A))) size))
    
)))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 700224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))     ==> 53236
;;  (hash-1 '(w a y))         ==> 23124 
;;  (hash-1 '(r a i n b o w)) ==> 17039 
;;
;;  (hash-2 '(h e l l o))     ==> 25588 
;;  (hash-2 '(w a y))         ==> 42552 
;;  (hash-2 '(r a i n b o w)) ==> 70913 
;;
;;  (hash-3 '(h e l l o))     ==> 415458.0 
;;  (hash-3 '(w a y))         ==> 390702.0 
;;  (hash-4 '(r a i n b o w)) ==> 503286.0 
;;
;;  (hash-4 '(h e l l o))     ==> 533.0
;;  (hash-4 '(w a y))         ==> 502.0
;;  (hash-4 '(r a i n b o w)) ==> 646.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
      (let ((bit-vector (MakingHashies hashfunctionlist dict)))
        ;;initially had a 0 here
        (lambda(w)
        (First bit-vector (MakingHashies hashfunctionlist (list w))))
      )))


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
