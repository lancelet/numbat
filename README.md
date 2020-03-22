# Network Numbat

![Haskell CI](https://github.com/lancelet/numbat/workflows/Haskell%20CI/badge.svg)

> Learning how the Network of Tubes works

This project is not intended to implement any "real-world" functionality.
Instead, it's my personal learning about the network stack.

## Implemented Features

- [ ] Encoding / decoding TCP header
- [ ] TCP state machine

## Sources of Documentation

- [RFC 793][rfc-793]: The main TCP RFC.
- [TCP at Wikipedia][tcp-wikipedia]: Extra summary of background information.
- [`rust_tun`][rust-tun]: A Rust binding for TUN which works on MacOS.
- [`mac-utun`][mac-utun]: Another Rust TUN binding for MacOS.

[rfc-793]: https://tools.ietf.org/html/rfc793
[tcp-wikipedia]: https://en.wikipedia.org/wiki/Transmission_Control_Protocol
[rust-tun]: https://github.com/meh/rust-tun
[mac-utun]: https://crates.io/crates/mac_utun