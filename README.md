# gerbil-libp2p

This packages provides a native Gerbil implementation of the
[libp2p](https://github.com/libp2p/specs/) networking stack.

## Overview

The implementation will loosely follow the
[go-libp2p](https://github.com/libp2p/go-libp2p) architecture, as far
as the host interface is concerned.

Specifically, the main user interface is the `Host`, and the I/O interface is the `Stream` -- see [libp2p/interface.ss](libp2p/interface.ss).


## Development Roadmap

This is just beginning; these are our plans:

### MVP

This is the very basic functionality of libp2p; we just want to
implement enough of the host to talk to a go-libp2p program
implementing a basic echo protocol, both as a client and as a server.

That means:
- implement enough of the Host to connect to a peer, open a stream as a client, and handle a stream as server.
- protocol requirements:
  - multistream select for handshake/mux negotiation
  - tls handshake (or noise, eventually both)
  - yamux
  - identify

### Advanced Host APIs

- peerstore
- network
- resource manager
- event bus

### Advanced Functionality

As we build towards production, we are going to need some more components:
- autonat client for nat detection
- relay/v2 client, dcutr, and dht client for hole punching
- dht client for bootstrap/routing
- pubsub/gossipsub

### Public Node Functionality

- autonat server
- relay/v2 server
- dht server


### QUIC

This is a very big endeavor in itself, so it might take a while; volunteers welcome.

## Copyright and License

© 2024 The Mighty Gerbils Contributors;
distributed with the Gerbil License, dual Apache-2.0 and LGPL v2.1.
