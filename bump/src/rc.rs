use std::cell::Cell;
use std::fmt;

pub(crate) struct RcInner<T: ?Sized> {
	count: Cell<usize>,
	data:  T,
}

impl<T> RcInner<T> {
	#[inline]
	fn new(data: T) -> Self {
		Self { count: Cell::new(0), data }
	}
}

pub struct Rc<'bump, T: ?Sized>(&'bump RcInner<T>);

impl crate::Bump {
	#[inline]
	pub fn alloc_rc<'bump, T>(&self, val: T) -> Rc<'bump, T> {
		let data = self.alloc_size::<RcInner<T>>(std::mem::size_of::<RcInner<T>>()) as *mut RcInner<T>;
		unsafe { std::ptr::write(data, RcInner::new(val)); }
		Rc(unsafe { &*data })
	}

	#[inline]
	fn alloc_slice_raw_rc<'bump, T>(&self, val: &[T]) -> Rc<'bump, [T]> {
		let size = std::mem::size_of_val(val) + std::mem::size_of::<RcInner<()>>();
		let data = self.alloc_size::<RcInner<T>>(size) as *mut RcInner<T>;

		unsafe { 
			(*data).count.set(0);
			std::ptr::copy_nonoverlapping(val.as_ptr(), &raw mut (*data).data, size); 
			Rc(&*std::ptr::from_raw_parts::<RcInner<[T]>>(data, val.len()))
		}
	}

	#[inline]
	pub fn alloc_from_vec_rc<'bump, T>(&self, mut val: Vec<T>) -> Rc<'bump, [T]> {
		let data = self.alloc_slice_raw_rc(&val);

		let ptr = val.as_mut_ptr();
		let cap = val.capacity();
		std::mem::forget(val);

		let layout = std::alloc::Layout::array::<T>(cap).unwrap();
		unsafe {
			std::alloc::dealloc(ptr as *mut u8, layout);
		}

		data
	} 

	#[inline]
	pub fn alloc_slice_rc<'bump, T: Copy>(&self, val: &[T]) -> Rc<'bump, [T]> {
		self.alloc_slice_raw_rc(val)
	}

	#[inline]
	pub fn alloc_slice_clone_rc<'bump, T: Clone>(&self, val: &[T]) -> Rc<'bump, [T]> {
		let size = std::mem::size_of_val(val) + std::mem::size_of::<RcInner<()>>();
		let data = self.alloc_size::<RcInner<T>>(size) as *mut RcInner<T>;

		unsafe { 
			(*data).count.set(0);
			val.iter().enumerate().for_each(|(i, e)| std::ptr::write((&raw mut (*data).data).add(i), e.clone()));
			Rc(&*std::ptr::from_raw_parts::<RcInner<[T]>>(data, val.len()))
		}
	}

	pub fn alloc_from_iter_rc<'bump, T>(&self, iter: impl ExactSizeIterator<Item = T>) -> Rc<'bump, [T]> {
		let len = iter.len();
		let data = self.alloc_size::<RcInner<T>>(len * std::mem::size_of::<T>() 
			+ std::mem::size_of::<RcInner<()>>()) as *mut RcInner<T>;

		unsafe {
			(*data).count.set(0);
			iter.enumerate().for_each(|(i, val)| std::ptr::write((&raw mut (*data).data).add(i), val));
			Rc(&*std::ptr::from_raw_parts::<RcInner<[T]>>(data, len))
		}
	}
}

impl<T: ?Sized> Rc<'_, T> {
	#[inline]
	pub fn into_static_unsafe(self) -> Rc<'static, T> {
		unsafe { std::mem::transmute(self) }
	}
}

impl<T: ?Sized> Drop for Rc<'_, T> {
	#[inline]
	fn drop(&mut self) {
		match self.0.count.get() {
			0 => unsafe { std::ptr::drop_in_place(&self.0.data as *const T as *mut T) },
			_ => { self.0.count.update(|c| c - 1); },
		}
	}
}

impl<T: ?Sized> Clone for Rc<'_, T> {
	#[inline]
	fn clone(&self) -> Self {
		self.0.count.update(|c| c + 1);
		Rc(self.0)
	}
}

impl<T: ?Sized> std::ops::Deref for Rc<'_, T> {
	type Target = T;
	#[inline]
	fn deref(&self) -> &Self::Target { &self.0.data }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for Rc<'_, T> {
	#[inline]
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Debug::fmt(&**self, f)
	}
}

impl<T: ?Sized + fmt::Display> fmt::Display for Rc<'_, T> {
	#[inline]
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Display::fmt(&**self, f)
	}
}

impl<T: ?Sized> fmt::Pointer for Rc<'_, T> {
	#[inline]
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Pointer::fmt(&(&self.0.data as *const T), f)
	}
}
