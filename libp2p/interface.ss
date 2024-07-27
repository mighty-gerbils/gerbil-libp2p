;;; -*- Gerbil -*-
;;; libp2p host interface

(import :std/io/interface)
(export #t)

(defstruct PeerID ())
(defstruct MultiAddr ())
(defstruct AddrInfo ())

(interface (Stream Closer)
  (protocol) => :string
  (reader)   => Reader
  (writer)   => Writer
  (reset!)   => :void)

(interface (Host Closer)
  (ID) => PeerID

  ;; => [MultiAddr]
  (addrs) => :list

  (connect! (addrs : AddrInfo))

  (open-stream (p : PeerID) (protos : :list)) => Stream

  ;; handler: lambda (Stream)
  (set-protocol-handler! (proto : :string) (handler : :procedure)))
