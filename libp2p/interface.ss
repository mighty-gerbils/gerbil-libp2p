;;; -*- Gerbil -*-
;;; libp2p host interface

(import :std/io/interface
        :std/misc/timeout
        "utils"
        "multiaddress")

(export #t)

(defstruct PeerID (id))
(defstruct PeerStore ())

(interface (Stream Closer)
  (protocol) => :string
  (reader)   => Reader
  (writer)   => Writer

  ;; abruptly close/abort the stream
  (reset!)   => :void

  ;; input timeout
  (set-input-timeout! (timeo :~ (maybe timeout?)))
  => :void
  ;; output timeout
  (set-output-timeout! (timeo :~ (maybe timeout? )))
  => :void)


;;; Host is an object participating in a p2p network, which
;;; implements protocols or provides services. It handles
;;; requests like a Server, and issues requests like a Client.
;;; It is called Host because it is both Server and Client (and Peer
;;; may be confusing).
(interface (Host Closer)
  (id) => PeerID

  (peer-store) => PeerStore

  ;; => [MultiAddr]
  (addrs) => :list

  (connect! (addrs :~ multi-address?) => :void)

  (open-stream (p : PeerID)
               (protos :~ protocol-or-list?))
  => Stream

  (set-protocol-handler! (proto-or-list :~ protocol-or-list?)
                         (handler : :procedure))
  => :void
  )
