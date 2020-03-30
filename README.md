# Network Numbat

![Haskell CI](https://github.com/lancelet/numbat/workflows/Haskell%20CI/badge.svg)

> Learning how the Network of Tubes works

This project is not intended to implement any "real-world" functionality.
Instead, it's my personal learning about the network stack.

## Implemented Features

- [ ] Encoding / decoding TCP header
- [ ] Encoding / decoding IP segments
- [ ] TCP state machine

## Sources of Documentation

- [RFC 793][rfc-793]: The main TCP RFC.
- [TCP at Wikipedia][tcp-wikipedia]: Extra summary of background information.
- [`tcpdump` Cheatsheet][tcpdump-cheatsheet]: Useful summary of `tcpdump`
  options.
- [`rust_tun`][rust-tun]: A Rust binding for TUN which works on MacOS.
- [`mac-utun`][mac-utun]: Another Rust TUN binding for MacOS.
- [WWDC Presentation on DriverKit][wwdc-driverkit]: DriverKit allows user-space
  macOS drivers. Maybe even a TAP (layer 2 virtualisation) implementation.
- [WWDC Presentation on Network Extensions][wwdc-network-extensions]: I think 
  this is more background on DriverKit, etc.
- [NetworkingDriverKit][networkingdriverkit]: A sub-component of DriverKit,
  for Ethernet drivers.

[rfc-793]: https://tools.ietf.org/html/rfc793
[tcp-wikipedia]: https://en.wikipedia.org/wiki/Transmission_Control_Protocol
[rust-tun]: https://github.com/meh/rust-tun
[mac-utun]: https://crates.io/crates/mac_utun
[tcpdump-cheatsheet]: https://www.andreafortuna.org/2018/07/18/tcpdump-a-simple-cheatsheet/
[wwdc-driverkit]: https://developer.apple.com/videos/play/wwdc2019/702/
[wwdc-network-extensions]: https://developer.apple.com/videos/play/wwdc2019/714/
[networkingdriverkit]: https://developer.apple.com/documentation/networkingdriverkit
