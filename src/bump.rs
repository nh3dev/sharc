use std::fmt::{self, Debug, Display};
use std::ops::Deref;

thread_local! {
	pub static THREAD_BUMP: bump::Bump = bump::Bump::new();
}



#[repr(transparent)]
pub struct Box<T: ?Sized + 'static>(bump::Box<'static, T>);

impl<T: 'static> Box<T> {
	pub fn new(value: T) -> Self {
		Self(THREAD_BUMP.with(|bump| bump.alloc(value).into_static_unsafe()))
	}

	pub fn empty_slice() -> Box<[T]> {
		Box(bump::Box::empty_slice())
	}

	pub fn from_iter(iter: impl ExactSizeIterator<Item = T>) -> Box<[T]> {
		Box(THREAD_BUMP.with(|bump| bump.alloc_from_iter(iter).into_static_unsafe()))
	}

	pub fn into_inner(self) -> T {
		self.0.into_inner()
	}
}

impl<T: 'static> From<Vec<T>> for Box<[T]> {
	fn from(vec: Vec<T>) -> Self {
		Self(THREAD_BUMP.with(|bump| bump.alloc_from_vec(vec).into_static_unsafe()))
	}
}

impl<T: ?Sized + 'static> From<bump::Box<'static, T>> for Box<T> {
	fn from(bump_box: bump::Box<'static, T>) -> Self {
		Self(bump_box)
	}
}

impl<T: ?Sized + 'static> std::ops::Deref for Box<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target { self.0.deref() }
}

impl<T: ?Sized + 'static> std::ops::DerefMut for Box<T> {
	fn deref_mut(&mut self) -> &mut Self::Target { self.0.deref_mut() }
}

impl<T: ?Sized + Debug> Debug for Box<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Debug::fmt(&self.0, f)
	}
}

impl<T: ?Sized + Display> Display for Box<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(&self.0, f)
	}
}

impl<T: ?Sized + 'static + Clone> Clone for Box<T> {
	fn clone(&self) -> Self {
		Self(THREAD_BUMP.with(|bump| bump.alloc(self.0.deref().clone())).into_static_unsafe())
	}
}

impl<T: Clone> Clone for Box<[T]> {
	fn clone(&self) -> Self {
		Self(THREAD_BUMP.with(|bump| bump.alloc_slice_clone(&self.0.deref())).into_static_unsafe())
	}
}

impl<T: 'static> IntoIterator for Box<[T]> {
	type Item = T;
	type IntoIter = bump::BoxIter<'static, T>;

	fn into_iter(self) -> Self::IntoIter {
		self.0.into_iter()
	}
}



#[repr(transparent)]
#[derive(Clone)]
pub struct Rc<T: ?Sized + 'static>(bump::Rc<'static, T>);

impl<T: 'static> Rc<T> {
	pub fn new(value: T) -> Self {
		Self(THREAD_BUMP.with(|bump| bump.alloc_rc(value).into_static_unsafe()))
	}
}

impl<T: 'static> From<Vec<T>> for Rc<[T]> {
	fn from(vec: Vec<T>) -> Self {
		Self(THREAD_BUMP.with(|bump| bump.alloc_from_vec_rc(vec).into_static_unsafe()))
	}
}


impl<T: 'static> std::ops::Deref for Rc<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target { self.0.deref() }
}

impl<T: Debug> Debug for Rc<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Debug::fmt(&self.0, f)
	}
}

impl<T: Display> Display for Rc<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Display::fmt(&self.0, f)
	}
}
