(import :std/io
        :std/format
        :std/logger
        :std/misc/ports
        :std/sugar)

(export #t)

(deflogger thread-logger)
(current-logger-options 3)

(def protocol-id "/multistream/1.0.0")

;; A function which adds multistream-select to a particular connection
(def (upgrade-stream! (stream :- Socket))
  ;; Send over the multistream select protocol
  (send-multistream-select stream)
  ;; Read the multistream protocol
  (spawn read-multistream-select stream))

(def (send-multistream-select (stream :- StreamSocket))
  (stream.send (string->bytes protocol-id)))

(def (read-multistream-select (stream :- StreamSocket))
  (def output (make-u8vector 1000))
  (while #t
    (stream.recv output)
    (errorf "received: ~a" (utf8->string output))
    (equal? (utf8->string output) protocol-id)))
