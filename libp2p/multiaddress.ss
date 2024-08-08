;;; -*- Gerbil -*-
;;; multiaddress implementation

(import :std/io/interface
        :std/sugar
        :std/generic
        :std/misc/list
        :std/format
        :std/iter
        :std/lazy
        "utils")

(export #t)

;; A macro to define a predicate against the protocols in a multiaddress
(defrule (def-multipattern name . protocols)
  (def (name ma)
    (andmap (lambda (protocol) (get-protocol-value ma protocol)) 'protocols)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Multi Address Type
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct multi-address (protocols str bytes)
  constructor: init!)

(defmethod {init! multi-address}
  (lambda (self input-str)
    (set! self.str (lazy input-str))
    (set! self.bytes (lazy (string->bytes input-str)))
    (let* ((protocol-list (chain (string-split input-str #\/)
                            (filter (lambda (e) (not (string-empty? e))) <>))))
      (set! self.protocols protocol-list))))

(defgeneric get-protocol-value
  (lambda (ma protocol) #f))

(defmethod (get-protocol-value (ma :string) (protocol :string))
  (get-protocol-value (multi-address ma) protocol))

(defmethod (get-protocol-value (ma :vector) (protocol :string))
  (get-protocol-value (multi-address (utf8->string ma)) protocol))

(defmethod (get-protocol-value (ma multi-address) (protocol :string))
  (pget protocol ma.protocols))

(def (get-protocol-values ma protocols)
  (map (lambda (protocol) (get-protocol-value ma protocol)) protocols))

(def-multipattern tcp-ip? "ip4" "tcp")
(def-multipattern p2p? "p2p")

(def (get-tcp-addr ma)
  (with ([ip port] (get-protocol-values ma ["ip4" "tcp"]))
    (format "~a:~a" ip port)))
