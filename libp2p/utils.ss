;;; -*- Gerbil -*-
;;; libp2p utils

(import :std/misc/list)

(export #t)

(def (protocol-or-list? obj)
  (or (string? obj)
      (and (list? obj)
           (andmap string? obj))))

;; Splits a list into chunks of n items
(def (chunk lst n)
  (match lst
    ((? (cut length=n? <> n))
     [(slice lst 0 n)])
    ((? (cut length<n? <> n))
     (error "Chunking error: Not Divisible!"))
    (else (cons (slice lst 0 n) (chunk (slice lst n) n)))))
