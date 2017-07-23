#![cfg_attr(not(feature = "std"), no_std)]
#![allow(unused_mut)]

#[macro_use]
extern crate log;
#[cfg(feature = "std")]
extern crate env_logger;
#[cfg(feature = "std")]
extern crate getopts;
extern crate smoltcp;

#[cfg(feature = "std")]
mod utils;

use smoltcp::Error;
use smoltcp::phy::Loopback;
#[cfg(feature = "std")]
use smoltcp::phy::Tracer;
use smoltcp::wire::{EthernetAddress, IpAddress};
#[cfg(feature = "std")]
use smoltcp::wire::EthernetFrame;
use smoltcp::iface::{ArpCache, SliceArpCache, EthernetInterface};
use smoltcp::socket::{AsSocket, SocketSet};
use smoltcp::socket::{TcpSocket, TcpSocketBuffer};

#[cfg(not(feature = "std"))]
mod mock {
    use core::cell::Cell;

    #[derive(Debug)]
    pub struct Clock(Cell<u64>);

    impl Clock {
        pub fn new() -> Clock {
            Clock(Cell::new(0))
        }

        pub fn advance(&mut self, millis: u64) {
            self.0.set(self.0.get() + millis)
        }

        pub fn elapsed(&self) -> u64 {
            self.0.get()
        }
    }
}

#[cfg(feature = "std")]
mod mock {
    use std::sync::Arc;
    use std::sync::atomic::{Ordering, AtomicUsize};

    // should be AtomicU64 but that's unstable
    #[derive(Debug, Clone)]
    pub struct Clock(Arc<AtomicUsize>);

    impl Clock {
        pub fn new() -> Clock {
            Clock(Arc::new(AtomicUsize::new(0)))
        }

        pub fn advance(&self, millis: u64) {
            self.0.fetch_add(millis as usize, Ordering::SeqCst);
        }

        pub fn elapsed(&self) -> u64 {
            self.0.load(Ordering::SeqCst) as u64
        }
    }
}

fn main() {
    let clock = mock::Clock::new();

    #[cfg(feature = "std")]
    {
        let clock = clock.clone();
        utils::setup_logging_with_clock(move || clock.elapsed());
    }

    let mut device = Loopback::new();
    #[cfg(feature = "std")]
    let mut device = Tracer::<_, EthernetFrame<&'static [u8]>>::new(device, utils::trace_writer);

    let mut arp_cache_entries: [_; 8] = Default::default();
    let mut arp_cache = SliceArpCache::new(&mut arp_cache_entries[..]);

    let     hardware_addr  = EthernetAddress::default();
    let mut protocol_addrs = [IpAddress::v4(127, 0, 0, 1)];
    let mut iface = EthernetInterface::new(
        &mut device, &mut arp_cache as &mut ArpCache,
        hardware_addr, &mut protocol_addrs[..]);

    let server_socket = {
        // It is not strictly necessary to use a `static mut` and unsafe code here, but
        // on embedded systems that smoltcp targets it is far better to allocate the data
        // statically to verify that it fits into RAM rather than get undefined behavior
        // when stack overflows.
        static mut TCP_SERVER_RX_DATA: [u8; 1024] = [0; 1024];
        static mut TCP_SERVER_TX_DATA: [u8; 1024] = [0; 1024];
        let tcp_rx_buffer = TcpSocketBuffer::new(unsafe { &mut TCP_SERVER_RX_DATA[..] });
        let tcp_tx_buffer = TcpSocketBuffer::new(unsafe { &mut TCP_SERVER_TX_DATA[..] });
        TcpSocket::new(tcp_rx_buffer, tcp_tx_buffer)
    };

    let client_socket = {
        static mut TCP_CLIENT_RX_DATA: [u8; 1024] = [0; 1024];
        static mut TCP_CLIENT_TX_DATA: [u8; 1024] = [0; 1024];
        let tcp_rx_buffer = TcpSocketBuffer::new(unsafe { &mut TCP_CLIENT_RX_DATA[..] });
        let tcp_tx_buffer = TcpSocketBuffer::new(unsafe { &mut TCP_CLIENT_TX_DATA[..] });
        TcpSocket::new(tcp_rx_buffer, tcp_tx_buffer)
    };

    let mut socket_set_entries: [_; 2] = Default::default();
    let mut socket_set = SocketSet::new(&mut socket_set_entries[..]);
    let server_handle = socket_set.add(server_socket);
    let client_handle = socket_set.add(client_socket);

    let mut did_listen  = false;
    let mut did_connect = false;
    let mut done = false;
    while !done && clock.elapsed() < 10_000 {
        {
            let socket: &mut TcpSocket = socket_set.get_mut(server_handle).as_socket();
            if !socket.is_active() && !socket.is_listening() {
                if !did_listen {
                    socket.listen(1234).unwrap();
                    did_listen = true;
                }
            }

            if socket.can_recv() {
                debug!("got {:?}", socket.recv(32).unwrap());
                socket.close();
                done = true;
            }
        }

        {
            let socket: &mut TcpSocket = socket_set.get_mut(client_handle).as_socket();
            if !socket.is_open() {
                if !did_connect {
                    socket.connect((IpAddress::v4(127, 0, 0, 1), 1234),
                                   (IpAddress::v4(127, 0, 0, 1), 65000)).unwrap();
                    did_connect = true;
                }
            }

            if socket.can_send() {
                socket.send_slice(b"0123456789abcdef").unwrap();
                socket.close();
            }
        }

        match iface.poll(&mut socket_set, clock.elapsed()) {
            Ok(()) | Err(Error::Exhausted) => (),
            Err(e) => debug!("poll error: {}", e)
        }

        clock.advance(1);
    }

    if !done {
        error!("this is taking too long, bailing out");
    }
}
