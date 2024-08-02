;;; -*- Gerbil -*-
;;; libp2p host implementation

(import :std/io
        :std/misc/timeout
        :std/format
        :std/sugar
        :std/crypto
        :std/text/base58
        :std/logger
        "utils"
        "multiaddress")

;; TODO This should derive the peer-id from the key-pair
(def (make-peer-id key)
  (base58-encode key))

(defstruct peer-info (peer-id multi-address public-key))

(defstruct p2p-host (id key multi-address listener peer-store handlers connections)
  constructor: init!)

(defmethod {init! p2p-host}
  (lambda (self (key (keygen/ed25519)) (multi-addr "ip4/127.0.0.1/tcp/4444"))
    (set! (@ self multi-address) (multi-address multi-addr))
    (set! (@ self peer-store) (make-hash-table))
    (set! (@ self handlers) (make-hash-table))
    (set! (@ self connections) (make-hash-table))
    ;; TODO Make this a proper implementation
    (set! (@ self id) (make-peer-id key))))

(def (tcp-listener address (proc 123))
  (let* ((addr (get-tcp-addr address))
         (sock (tcp-listen addr)))
    (while #t
      (try
       (let (cli (ServerSocket-accept sock))
         (when cli
           (println  (format "~a" cli))
           (println (format "Accepted connection from ~a" (StreamSocket-peer-address cli)))
           ;;(spawn proc cli)
           ))
       (catch (e)
         (errorf "Error accepting connection: ~a" e))))))

(defmethod {listen! p2p-host}
  (lambda (self)
    (def addr (@ self multi-address))
    (match addr
      ((? tcp-ip?) (set! (@ self listener)
                     (spawn tcp-listener addr))))))

(defmethod {set-protocol-handler! p2p-host}
  (lambda (self protocol handler)
    (hash-put! (@ self handlers) protocol handler)))

(defmethod {connect! p2p-host}
  (lambda (self addr-info)
    (match addr-info
      ((? string?) {self.connect! (multi-address addr-info)})
      ((? tcp-ip?) (with ([ip port] (get-protocol-values addr-info ["ip4" "tcp"]))
                     (let* ((address (format "~a:~a" ip port))
                            (socket (tcp-connect address)))
                       (hash-put! (@ self connections) (@ addr-info str) socket)
                       (StreamSocket-send socket (string->bytes "Hello!"))
                       socket)))
      ((? p2p?) (error "TODO: Handle peerID connection")))))

(def a (p2p-host))
(def b (p2p-host "ip4/127.0.0.1/tcp/8888"))

;;{a.listen!}
;;{b.connect! "ip4/127.0.0.1/tcp/4444"}


;;{a.connect! "ip4/127.0.0.1/tcp/5000"}
;;(def socket (hash-get (@ a connections) "ip4/127.0.0.1/tcp/5000"))
;;{a.set-protocol-handler! "ipfs/id/1.0.0" (lambda () (println "Identified!"))}
