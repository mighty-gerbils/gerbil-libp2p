#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script
        :std/misc/process)

(invoke "gxtags" ["."])

(defbuild-script
  '("libp2p/interface"))
