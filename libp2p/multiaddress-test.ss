(import "multiaddress"
        :std/test
        :std/misc/alist
        :std/iter)

(export #t)

(def (test-setup!)
  (displayln "setting up..."))

(def (test-cleanup!)
  (displayln "cleaning up..."))

(def happy-1 "ip/192.168.1.1/tcp/8888")
(def happy-2 "ip/192.168.1.1/tcp/8888")
(def happy-3 "ip/192.255.1.1/tcp/4000/")
(def invalid-1 "ip/192.168.1.1/tcp/8888/foo")

(def multiaddress-test
  (test-suite "test multiaddress"
    (def ma1 #f)
    (def ma2 #f)
    (def ma3 #f)

    (test-case "test invalid multiaddress"
      ;; Not sure if this is the best of way handling this.
      (check-exception (DefaultMultiAddr invalid-1) (lambda (ex) (equal?  (error-message ex) "Chunking error: Not Divisible!"))))

    (test-case "test happy path parsing"
      (set! ma1 (DefaultMultiAddr happy-1))
      (set! ma2 (DefaultMultiAddr happy-2))
      (set! ma3 (DefaultMultiAddr happy-3)))

    (test-case "test protocols"
      (check-equal? {ma1.protocols} [["ip" "192.168.1.1"]
                                     ["tcp" "8888"]]))

    (test-case "test bytes"
      (check-equal? (u8vector? {ma1.bytes}) #t))

    (test-case "test string"
      (check-equal? (string? {ma1.string}) #t))

    (test-case "test equality"
      (check-equal? {ma1.equal? ma2} #t)
      (check-equal? {ma1.string} {ma2.string})
      (check-equal? {ma1.bytes} {ma2.bytes}))

    (test-case "test value-for-protocol"
      (check-equal? {ma1.value-for-protocol "tcp"} "8888")
      (check-equal? {ma1.value-for-protocol "ip"} "192.168.1.1"))

    ;; (test-case "test encapsulate"
    ;;   {ma1.encapsulate! ma3}
    ;;   (check-equal? {ma1.protocols}  [["ip" "192.255.1.1"]
    ;;                                   ["tcp" "4000"]]))

    ;; (test-case "test decapsulate"
    ;;   {ma1.decapsulate! (DefaultMultiAddr "tcp/4000")}
    ;;   (check-equal? {ma1.protocols}  [["ip" "192.255.1.1"]]))
    ))
