use core::fmt;
use std::sync::{Arc};
use parking_lot::{RwLock};

use managed::ManagedSlice;

use crate::socket::{AnySocket, Socket};

use super::socket_meta::Meta;

/// Opaque struct with space for storing one socket.
///
/// This is public so you can use it to allocate space for storing
/// sockets when creating an Interface.
/// Think of padding, reference:
/// https://www.reddit.com/r/rust/comments/bijoco/rust_array_of_mutexes/
#[derive(Debug, Default)]
pub struct SocketStorage<'a> {
    inner: Option<Arc<RwLock<Option<Item<'a>>>>>,
}

impl<'a> SocketStorage<'a> {
    pub const EMPTY: Self = Self { inner: None };
}

/// An item of a socket set.
#[derive(Debug)]
pub(crate) struct Item<'a> {
    pub(crate) meta: Meta,
    pub(crate) socket: Socket<'a>,
}

/// A handle, identifying a socket in an Interface.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub struct SocketHandle(usize);

impl fmt::Display for SocketHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

/// An extensible set of sockets.
///
/// The lifetime `'a` is used when storing a `Socket<'a>`.
#[derive(Debug)]
pub struct SocketSet<'a> {
    sockets: ManagedSlice<'a, SocketStorage<'a>>,
}

impl<'a> SocketSet<'a> {
    /// Create a socket set using the provided storage.
    pub fn new<SocketsT>(sockets: SocketsT) -> SocketSet<'a>
        where
            SocketsT: Into<ManagedSlice<'a, SocketStorage<'a>>>,
    {
        let sockets = sockets.into();
        SocketSet { sockets }
    }

    /// Add a socket to the set, and return its handle.
    ///
    /// # Panics
    /// This function panics if the storage is fixed-size (not a `Vec`) and is full.
    pub fn add<T: AnySocket<'a>>(&mut self, socket: T) -> SocketHandle {
        fn put<'a>(index: usize, slot: &mut SocketStorage<'a>, socket: Socket<'a>) -> SocketHandle {
            net_trace!("[{}]: adding", index);
            let handle = SocketHandle(index);
            let mut meta = Meta::default();
            meta.handle = handle;
            *slot = SocketStorage {
                inner: Some(Arc::new(RwLock::new(Some(Item { meta, socket })))),
            };
            handle
        }

        let socket = socket.upcast();

        for (index, slot) in self.sockets.iter_mut().enumerate() {
            if slot.inner.is_none() {
                return put(index, slot, socket);
            }
        }

        match self.sockets {
            ManagedSlice::Borrowed(_) => panic!("adding a socket to a full SocketSet"),
            #[cfg(any(feature = "std", feature = "alloc"))]
            ManagedSlice::Owned(ref mut sockets) => {
                sockets.push(SocketStorage { inner: None });
                let index = sockets.len() - 1;
                put(index, &mut sockets[index], socket)
            }
        }
    }

    /// Get a socket from the set by its handle, as mutable.
    ///
    /// # Panics
    /// This function may panic if the handle does not belong to this socket set
    /// or the socket has the wrong type.
    pub fn with<T: AnySocket<'a>, F, R>(&self, handle: SocketHandle, map_fn: F) -> R
        where F: FnOnce(&T) -> R
    {
        let guard = self.sockets[handle.0].inner.as_ref()
            .expect("handle does not refer to a valid socket");

        let maybe_item = guard.read();
        let item = maybe_item.as_ref()
            .expect("handle does not refer to a valid socket");

        let socket = T::downcast(&item.socket)
            .expect("inner handle refers to a socket of a wrong type");
        map_fn(socket)
    }

    /// Get a mutable socket from the set by its handle, as mutable.
    ///
    /// # Panics
    /// This function may panic if the handle does not belong to this socket set
    /// or the socket has the wrong type.
    pub fn with_mut<T: AnySocket<'a>, F, R>(&self, handle: SocketHandle, map_fn: F) -> R
        where F: FnOnce(&mut T) -> R
    {
        let guard = self.sockets[handle.0].inner.as_ref()
            .expect("handle does not refer to a valid socket");

        let mut maybe_item = guard.write();
        let item = maybe_item.as_mut()
            .expect("handle does not refer to a valid socket");

        let mut socket = T::downcast_mut(&mut item.socket)
            .expect("inner handle refers to a socket of a wrong type");
        map_fn(&mut socket)
    }

    /// Remove a socket from the set, without changing its state.
    ///
    /// # Panics
    /// This function may panic if the handle does not belong to this socket set.
    pub fn remove(&mut self, handle: SocketHandle) -> Socket<'a> {
        net_trace!("[{}]: removing", handle.0);
        let guard = self.sockets[handle.0].inner.take()
            .expect("handle does not refer to a valid socket");

        let item = guard.write().take()
            .expect("inner handle does not refer to a valid socket");

        item.socket
    }

    /// Iterate every socket in this set.
    pub(crate) fn items(&self) -> impl Iterator<Item = Arc<RwLock<Option<Item<'a>>>>> + '_ {
        self.sockets.iter().filter_map(|x| x.inner.clone())
    }
}
