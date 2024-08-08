;;; -*-
;;; libp2p host implementation

(import :std/io
        :std/misc/timeout
        :std/format
        :std/generic
        :std/sugar
        :std/crypto
        :std/text/base58
        :std/text/utf8
        :std/logger
        "utils"
        "multiaddress"
        (prefix-in "protocols/multistream-select/lib" mux#))

(start-logger! (current-output-port))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Peer Info
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defstruct peer-info (peer-id multi-address public-key))
;; TODO This should derive the peer-id from the key-pair
(def (make-peer-id key)
  (base58-encode key))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Host Interface
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; TODO I should hide [id, key, multi-address] inside of the peer store.
(defstruct p2p-host (id key multi-address listener peer-store handlers connections)
  constructor: init!)

(defmethod {init! p2p-host}
  (lambda (self (key (keygen/ed25519)) (multi-addr "ip4/127.0.0.1/tcp/4444"))
    (set! self.multi-address (multi-address multi-addr))
    (set! self.peer-store (make-hash-table))
    (set! self.handlers (make-hash-table))
    (set! self.connections (make-hash-table))
    ;; TODO Make this a proper implementation
    (set! self.id (make-peer-id key))))

;; TODO maybe make this dispatch to a generic?
(defmethod {listen! p2p-host}
  (lambda (self)
    (def addr self.multi-address)
    (match addr
      ((? tcp-ip?) (set! self.listener
                     (spawn tcp-listener addr self.handlers))))))

(defmethod {set-protocol-handler! p2p-host}
  (lambda (self protocol handler)
    (hash-put! self.handlers protocol handler)))

(defgeneric connect
  (lambda args #f))

(defmethod {connect! p2p-host}
  (lambda (self addr-info)
    (match addr-info
      ((? string?) {self.connect! (multi-address addr-info)})
      ((? tcp-ip?) (with ([ip port] (get-protocol-values addr-info ["ip4" "tcp"]))
                     (let* ((address (format "~a:~a" ip port))
                            (socket (tcp-connect address)))
                       ;;(hash-put! self.connections (@ addr-info str) socket)
                       ;; Once we have a connection with a peer, we will start upgrading the connection
                       ;; First we will add a multiplexer, for now we are using multistream-select
                       (printf "Connected to node: ~a\n" address)
                       (mux#upgrade-stream! socket))))
      ((? p2p?) (errorf "TODO: Handle peerID connection")))))

(def (tcp-listener address handlers)
  (let* ((addr (get-tcp-addr address)))
    (printf "Listening on: ~a\n" addr)
    (using (sock (tcp-listen addr) :- ServerSocket)
      (while #t
        (try
         (using (cli (sock.accept) :- StreamSocket)
           (when cli
             (printf "Accepted connection from ~a\n" (cli.peer-address))
             (spawn dumb-reader cli)
             ;;(mux#upgrade-stream! sock)
             ))
         (catch (e)
           (errorf "Error accepting connection: ~a" e)))))))

(def (dumb-reader (socket :- StreamSocket))
  (def output (make-u8vector 1000))
  (when #t
    (let ((bytes-read (socket.recv output)))
      (when (> 0 bytes-read)
        (printf "Got message: ~a" (utf8->string output 0 bytes-read))
        (socket.send (string->utf8 "hello"))))))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Playground for testing
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(def a-multi-address "ip4/127.0.0.1/tcp/4444")
(def a-ip-address (get-tcp-addr a-multi-address))

(def b-multi-address "ip4/127.0.0.1/tcp/8888")
(def b-ip-address (get-tcp-addr b-multi-address))

(def node-a (p2p-host (keygen/ed25519) a-multi-address))
(def node-b (p2p-host (keygen/ed25519) b-multi-address))

{node-a.listen!}
{node-b.connect! "ip4/127.0.0.1/tcp/4444"}
