;;; -*- Gerbil -*-
;;; multiaddress implementation

(import :std/io/interface
        :std/sugar
        :std/misc/alist
        :std/misc/list
        :std/format
        :std/iter
        "utils")

(export #t)

;; A macro to define a predicate against the protocols in a multiaddress
(defrule (def-multipattern name . protocols)
  (def (name ma)
    (andmap (lambda (protocol) (get-protocol-value ma protocol)) 'protocols)))

(defstruct multi-address (protocols str)
  constructor: init!)

(defmethod {init! multi-address}
  (lambda (self input-str)
    (set! (@ self str) input-str)
    (let* ((protocol-map (chain (string-split input-str #\/)
                           (filter (lambda (e) (not (string-empty? e))) <>)
                           plist->hash-table)))
      (set! (@ self protocols) protocol-map))))

(def (get-protocol-value ma protocol)
  (match ma
    ((? string?) (get-protocol-value (multi-address ma) protocol))
    (_ (hash-get (@ ma protocols) protocol))))

(def (get-protocol-values ma protocols)
  (map (lambda (protocol) (get-protocol-value ma protocol)) protocols))

(def (get-protocols ma)
  (hash-keys (@ ma protocols)))

(def-multipattern tcp-ip? "ip4" "tcp")
(def-multipattern p2p? "p2p")

(def (get-tcp-addr ma)
  (with ([ip port] (get-protocol-values ma ["ip4" "tcp"]))
    (format "~a:~a" ip port)))
