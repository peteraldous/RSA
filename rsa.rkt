#lang typed/racket

(: test (Symbol (U Boolean Number) (U Boolean Number) -> Void))
(define (test where expected actual)
  (if (equal? expected actual)
      (void)
      (error where "expected ~a, got ~a" expected actual)))

(: mod-exp (Integer Integer Integer -> Integer))
(define (mod-exp base exp mod)
  (: mod-mult (Integer Integer -> Integer))
  (define (mod-mult n m)
    (remainder (* n m) mod))
  (: inner-loop (Integer Integer Integer -> Integer))
  (define (inner-loop base exp result)
    (if (zero? exp)
        result
        (inner-loop (mod-mult base base) (quotient exp 2)
                    (if (odd? exp)
                        (mod-mult result base)
                        result))))
  (inner-loop base exp 1))

(test 'mod-exp 1024 (mod-exp 2 10 2048))
(test 'mod-exp 0 (mod-exp 2 10 1024))
(test 'mod-exp 0 (mod-exp 2 10 512))
(test 'mod-exp 0 (mod-exp 2 1500 2))
(test 'mod-exp 0 (mod-exp 2 15000000 2))
(test 'mod-exp 0 (mod-exp 2 15000000 512))
(test 'mod-exp 3 (mod-exp 10 4 9997))
(test 'mod-exp 30 (mod-exp 10 5 9997))
(test 'mod-exp 1 (mod-exp 396 546 547))

(: generate-prime ((Option Natural) -> Natural))
(define (generate-prime current-number)
  (: inner-generate-prime (Natural -> Natural))
  (define (inner-generate-prime current-number)
    (if (prime? current-number)
        current-number
        (generate-prime (+ 2 current-number))))
  (if (integer? current-number)
      (if (odd? current-number)
          (inner-generate-prime current-number)
          (inner-generate-prime (add1 current-number)))
      (inner-generate-prime (abs (add1 (round (inexact->exact 1e10)))))))

(: is-factor (Integer Integer -> Boolean))
(define (is-factor little big)
  (integer? (/ big little)))

(: prime? (Integer -> Boolean))
(define (prime? n)
  
  (: divide-by-low-numbers (Integer -> Boolean))
  (define (divide-by-low-numbers n)
    (: divide-by-a-low-number ((Listof Integer) -> Boolean))
    (define (divide-by-a-low-number nums)
      (if (empty? nums)
          #f
          (or (is-factor (first nums) n)
              (divide-by-a-low-number (rest nums)))))
    (divide-by-a-low-number (filter (λ: ([val : Integer])
                                        (< val n))
                                    (cons 2 (build-list 128
                                                        (λ: ([n : Integer])
                                                            (+ 3 (* 2 n))))))))
  (: fermat-compositeness-test
     (Integer Natural -> Boolean))
  (define (fermat-compositeness-test n k)
    #;(printf "Fermat test: ~a~n" n)
    (if (zero? k)
        #f
        (let ([a (+ 1 (random (min (sub1 n) 4294967087)))])
          #;(printf "trying ~a to the ~a mod ~a~n" a (sub1 n) n)
          (or (not (= 1 (mod-exp a (sub1 n) n)))
              (fermat-compositeness-test n (sub1 k))))))
  (: miller-rabin-compositeness-test
     (Integer Natural -> Boolean))
  (define (miller-rabin-compositeness-test n k)
    #;(printf "Miller-Rabin test: ~a~n" n)
    (: factor-out-twos (Integer -> (Listof Integer)))
    (define (factor-out-twos n)
      (: inner-factor-out-twos (Integer Integer -> (Listof Integer)))
      (define (inner-factor-out-twos s d)
        (if (odd? d)
            (list s d)
            (inner-factor-out-twos (add1 s) (truncate (/ d 2)))))
      (inner-factor-out-twos 0 (sub1 n)))
    (match-define (list s d) (factor-out-twos n))
    (: inner-miller-rabin-primality-test (Integer -> Boolean))
    (define (inner-miller-rabin-primality-test n)
      (let* ([a (+ 2 (random (min 4294967087 (- n 4))))] [x (mod-exp a d n)])
        (: inner-inner (Integer Integer -> Boolean))
        (define (inner-inner x r)
          (if (or (= r s) (= x (sub1 n)))
              #f
              (if (= x 1)
                  #t
                  (inner-inner (mod-exp x 2 n) (add1 r)))))
        (if (or (= x 1) (= x (sub1 n)))
            #f
            (inner-inner (mod-exp x 2 n) 1))))
    (: test-k-times (Integer -> Boolean))
    (define (test-k-times k)
      (if (zero? k)
          #f
          (or (inner-miller-rabin-primality-test n)
              (test-k-times (sub1 k)))))
    (test-k-times k))
  (if (< n 256)
      (not (divide-by-low-numbers n))
      (not (or (divide-by-low-numbers n)
               (if (fermat-compositeness-test n 100)
                   (begin #;(printf "~a failed fermat~n" n) #t)
                   (begin #;(printf "~a passed fermat~n" n) #f))
               (miller-rabin-compositeness-test n 100)))))

#|
(: my-random (Integer -> Integer))
(define (my-random n)
  (: make-n-digit-random (Integer -> Integer))
  (define (make-n-digit-random n)
    (if (< n 15)
        (if (zero? n)
            0
            (remainder (make-15-digit-random) (round (expt 10 n))))
        (+ (* (round (expt 10 (- n 15))) (make-15-digit-random))
           (make-n-digit-random (- n 15)))))
  (: make-15-digit-random (-> Integer))
  (define (make-15-digit-random)
    (let ([result (round (inexact->exact (* 1e16 (random))))])
      (if (>= result 1e16)
          (make-15-digit-random)
          result)))
  (: inner-random (Integer -> Integer))
  (define (inner-random num-digits)
    (let ([result (make-n-digit-random num-digits)])
      (if (>= result n)
          (begin
            (printf "~a~n" result)
            (inner-random num-digits))
          result)))
  (let ([log-n (- (ceiling (inexact->exact
                            (real-part (/ (log (+ 1 n)) (log 10))))) 1)])
    #;(printf "making a ~a-digit number~n" log-n)
    (inner-random log-n)))
|#

#|
(my-random 546457831868313598)
(my-random 546457831868313598)
(my-random 546457831868313598)
(my-random 546457831868313598)
(my-random 546457831868313598)
(my-random 546457831868313598)
|#

(test 'prime? #t (prime? 2))
(test 'prime? #t (prime? 3))
(test 'prime? #t (prime? 5))
(test 'prime? #t (prime? 7))
(test 'prime? #f (prime? 10))
(test 'prime? #f (prime? 18))
(test 'prime? #t (prime? 104729))
(test 'prime? #f (prime? 104731))

#;(generate-prime 135768435)

(struct: key ([k : Integer] [mod : Integer]))
(struct: key-pair ([public : key] [private : key]))

(: euclid (Natural Natural -> Integer))
(define (euclid dividend divisor)
  (: return-positive (Integer -> Integer))
  (define (return-positive result)
    (if (negative? result)
        (return-positive (+ dividend result))
        result))
  (: mult-inverse (Integer Integer Integer Integer -> Integer))
  (define (mult-inverse a b y lasty)
    (if (zero? b)
        lasty
        (mult-inverse b (round (inexact->exact (remainder a b)))
                      (- lasty (* y (quotient a b)))
                      y)))
  (return-positive (mult-inverse dividend divisor 1 0)))

(test 'euclid 47 (euclid 120 23))

; i know these numbers are obscenely long.
; they're from a legit rsa key.

#|
For reference, I used this massive one-liner to find phi:
openssl rsa -text -in private_key.pem 2>/dev/null | grep -A5 ^prime[12]\: | sed 's/[ \:]//g' | tr -d '\n' | tr [:lower:] [:upper:] | sed 's/PRIME1/ibase=16; obase=16; scale=0; (/' | sed 's/PRIME2/-1)*(/' | sed 's/$/-1)\n/' | bc -lq | sed 's/[ \\]//g' | tr -d '\n'; echo
|#

; the output from this one-liner should be phi in hexadecimal. Interestingly,
; there don't seem to be any letters in the entire number, although obase=16.
; I'll keep looking into it.

(test 'euclid
      #x0d9f13595412e73184a24a81719df22770e90818057ee7ed0120106adc150c17f71b2a9ac521a6fb7c098fb1372039259b852c869f69c81fd62f4bdffb864fcb70fc0dbc71cc43e68f8c264f3af9931bd9748daa9876e3b1af62699ada6faa19dabfbc82106d56882e25d5c6257ed02dfe3d3416c88d9a1068549d2d34f38f01
      (euclid #x0610190402052014141219121507141209100506211312201507092013100907020305061216021106041104130912021110091220151419041320001517110704041619171314070517121916031811110005171104111013041910171717191401151915181109131217021517100017091705211000061604070802140105171616080804070300060715160305110202110402152103020808042111151207052114122009180209071907160600080305021511051318010821161003000201091810181708100920180206060503020420071610081203061821140613060107160506
              65537))

(: keygen (Natural Natural -> key-pair))
(define (keygen n phi)
  (let ([e #;(generate-prime (+ 65537 (random 65538))) 65537])
    (key-pair (key e n) (key (euclid phi e) n))))

(: rsa-keygen (-> key-pair))
(define (rsa-keygen)
  (let* ([p (generate-prime #f)] [q (generate-prime (* 3 p))])
    (printf "p: ~a~nq: ~a~n" p q)
    (keygen (* p q) (abs (* (sub1 p) (sub1 q))))))

(for ([i (in-range 10)])
  (let ([pt (random 4294967087)])
    (test 'keygen pt
          (let* ([keypair (rsa-keygen)]
                 #;[bunk1 (printf "made keypair~n")]
                 [public (key-k (key-pair-public keypair))]
                 [bunk6 (printf "public: ~a~n" public)]
                 [private (key-k (key-pair-private keypair))]
                 [bunk5 (printf "private: ~a~n" private)]
                 [mod (key-mod (key-pair-public keypair))]
                 [bunk2 (printf "modulus: ~a~n" mod)]
                 #;[pt 16381992384720121846]
                 [ct (mod-exp pt public mod)]
                 #;[bunk3 (printf "encrypted pt~n")]
                 [output (mod-exp ct private mod)]
                 #;[bunk4 (printf "decrypted ct~n")])
            (if (= mod (key-mod (key-pair-private keypair)))
                (begin
                  (printf "plaintext: ~a~n" pt)
                  (printf "ciphertext: ~a~n" ct)
                  (printf "decoded: ~a~n" output)
                  output)
                (begin (printf "mods don't match!~n")
                       #f))))))