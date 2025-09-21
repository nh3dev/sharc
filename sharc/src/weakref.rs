use std::sync::{Arc, RwLock, RwLockReadGuard};

pub struct WeakRef<T: ?Sized> {
	drop_flag: Arc<RwLock<bool>>,
	data:      *const T,
}

pub struct WeakRefGuard<'a, T: ?Sized> {
	lock: RwLockReadGuard<'a, bool>,
	data: &'a T,
}

impl<T: ?Sized> WeakRef<T> {
	pub fn new(data: &T) -> Self {
		Self { 
			drop_flag: Arc::new(RwLock::new(false)),
			data:      std::ptr::from_ref(data),
		}
	}

	pub fn as_ref(&self) -> Option<WeakRefGuard<'_, T>> {
		let Ok(lock) = self.drop_flag.read() else { return None; };
		if *lock { return None; }

		Some(WeakRefGuard { lock, data:  unsafe { &*self.data } })
	}

	pub fn drop(&self) {
		let Ok(mut lock) = self.drop_flag.write() else { return; };
		*lock = true;
	}
}

impl<T: ?Sized> Clone for WeakRef<T> {
	fn clone(&self) -> Self {
		Self { drop_flag: self.drop_flag.clone(), data: self.data }
	}
}

impl<T: ?Sized> std::ops::Deref for WeakRefGuard<'_, T> {
	type Target = T;
	fn deref(&self) -> &Self::Target { self.data }
}
