;;; -*- Gerbil -*-
;;; libp2p host interface

(import :std/io/interface
        :std/misc/timeout)
(export #t)

(defstruct PeerID ())
(defstruct MultiAddr ())
(defstruct AddrInfo ())

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

(interface (Host Closer)
  (ID) => PeerID

  ;; => [MultiAddr]
  (addrs) => :list

  (connect! (addrs : AddrInfo) => :void)

  (open-stream (p : PeerID)
               (protos :~ protocol-or-list?))
  => Stream

  ;; handler: lambda (Stream)
  (set-protocol-handler! (proto-or-list :~ protocol-or-list? )
                         (handler : :procedure))
  => :void)

(defrule (protocol-or-list? obj)
  (or (string? obj)
      (and (list? obj)
           (andmap string? obj))))
