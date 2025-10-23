use std::sync::atomic::{AtomicBool, Ordering};
use std::mem::ManuallyDrop;

#[derive(Debug)]
pub struct MaybeDrop<T> {
	val:  ManuallyDrop<T>,
	drop: AtomicBool,
}

impl<T> MaybeDrop<T> {
	#[inline]
	pub fn new(val: T) -> Self {
		Self { val: ManuallyDrop::new(val), drop: AtomicBool::new(true) }
	}
	
	#[inline]
	pub fn into_inner(mut self) -> T {
		let t = unsafe { ManuallyDrop::take(&mut self.val) };
		std::mem::forget(self);
		t
	}

	#[inline]
	pub fn set_drop(&self, drop: bool) {
		self.drop.store(drop, Ordering::Relaxed);
	}

	#[inline]
	pub fn drop(self) -> Self {
		self.set_drop(true); self
	}
}

impl<T> Drop for MaybeDrop<T> {
	#[inline]
	fn drop(&mut self) {
		if self.drop.load(Ordering::Relaxed) {
			unsafe { ManuallyDrop::drop(&mut self.val) };
		}
	}
}

impl<T> std::ops::Deref for MaybeDrop<T> {
	type Target = T;
	#[inline]
	fn deref(&self) -> &Self::Target { &self.val }
}

impl<T> std::ops::DerefMut for MaybeDrop<T> {
	#[inline]
	fn deref_mut(&mut self) -> &mut Self::Target { &mut self.val }
}

impl<T: Clone> Clone for MaybeDrop<T> {
	fn clone(&self) -> Self {
		Self { 
			val: ManuallyDrop::new((*self.val).clone()), 
			drop: AtomicBool::new(self.drop.load(Ordering::Relaxed)) 
		}
	}
}
