use std::fmt;
use std::mem::ManuallyDrop;

impl crate::Bump {
	#[inline]
	pub fn alloc_box<'bump, T>(&self, val: T) -> Box<'bump, T> {
		let data = self.alloc_size::<T>(std::mem::size_of::<T>()) as *mut T;

		unsafe { std::ptr::write(data, val); }

		Box(unsafe { &mut *data })
	}

	#[inline]
	fn alloc_slice_raw_box<'bump, T>(&self, val: &[T]) -> Box<'bump, [T]> {
		if val.is_empty() { return Box::empty_slice() }
		let data = self.alloc_size::<T>(std::mem::size_of_val(val)) as *mut T;

		unsafe {
			std::ptr::copy_nonoverlapping(val.as_ptr(), data, val.len()); 
			Box(std::slice::from_raw_parts_mut(data, val.len()))
		}
	}

	#[inline]
	pub fn alloc_from_vec_box<'bump, T>(&self, mut val: Vec<T>) -> Box<'bump, [T]> {
		if val.is_empty() { return Box::empty_slice() }
		let data = self.alloc_slice_raw_box(&val);

		let layout = std::alloc::Layout::array::<T>(val.capacity()).unwrap();
		unsafe {
			std::alloc::dealloc(val.as_mut_ptr() as *mut u8, layout);
		}

		std::mem::forget(val);

		data
	}

	pub fn alloc_array_box<'bump, const N: usize, T>(&self, val: [T; N]) -> Box<'bump, [T; N]> {
		let data = self.alloc_size::<T>(std::mem::size_of_val(&val)) as *mut T;

		unsafe {
			val.into_iter().enumerate().for_each(|(i, e)| std::ptr::write(data.add(i), e));
			Box(std::slice::from_raw_parts_mut(data, N).try_into().unwrap())
		}
	}

	pub fn alloc_sized_slice_box<'bump, const N: usize, T>(&self, val: [T; N]) -> Box<'bump, [T]> {
		let data = self.alloc_size::<T>(std::mem::size_of_val(&val)) as *mut T;

		unsafe {
			val.into_iter().enumerate().for_each(|(i, e)| std::ptr::write(data.add(i), e));
			Box(std::slice::from_raw_parts_mut(data, N))
		}
	}

	#[inline]
	pub fn alloc_slice_box<'bump, T: Copy>(&self, val: &[T]) -> Box<'bump, [T]> {
		self.alloc_slice_raw_box(val)
	}

	#[inline]
	pub fn alloc_slice_clone_box<'bump, T: Clone>(&self, val: &[T]) -> Box<'bump, [T]> {
		let data = self.alloc_size::<T>(std::mem::size_of_val(val)) as *mut T;

		unsafe {
			val.iter().enumerate().for_each(|(i, e)| std::ptr::write(data.add(i), e.clone()));
			Box(std::slice::from_raw_parts_mut(data, val.len()))
		}
	}

	pub fn alloc_from_iter_box<'bump, T>(&self, iter: impl ExactSizeIterator<Item = T>) -> Box<'bump, [T]> {
		let len = iter.len();
		let data = self.alloc_size::<T>(len * std::mem::size_of::<T>()) as *mut T;

		unsafe {
			iter.enumerate().for_each(|(i, val)| std::ptr::write(data.add(i), val));
			Box(std::slice::from_raw_parts_mut(data, len))
		}
	}
}


#[repr(transparent)]
pub struct Box<'bump, T: ?Sized>(pub(crate) &'bump mut T);

impl<T: ?Sized> Box<'_, T> {
	// this is uhhhh unsafe buuut only if the allocator gets dropped before the box
	// which shouldnt happen in the usual case
	#[inline]
	pub fn into_static_unsafe(self) -> Box<'static, T> {
		unsafe { std::mem::transmute(self) }
	}

	#[inline]
	pub fn as_static_ref_unsafe(&self) -> &'static T {
		unsafe { std::mem::transmute(&*self.0) }
	}
	
	#[inline]
	pub fn into_raw(self) -> *mut T {
		let ptr = self.0 as *mut T;
		std::mem::forget(self);
		ptr
	}
}

impl<T> Box<'_, T> {
	#[inline]
	pub fn into_inner(self) -> T {
		unsafe { std::ptr::read(self.into_raw()) }
	}

	#[inline(never)]
	#[cold]
	pub fn empty_slice<'a>() -> Box<'a, [T]> {
		Box(&mut [])
	}
}

pub struct BoxIter<'a, T> {
	index: usize,
	data:  ManuallyDrop<Box<'a, [T]>>,
}

impl<T> Iterator for BoxIter<'_, T> {
	type Item = T;

	#[inline]
	fn next(&mut self) -> Option<Self::Item> {
		if self.index >= self.data.len() { return None; }

		let item = unsafe {
			std::ptr::read(&raw const self.data[self.index])
		};

		self.index += 1;
		Some(item)
	}

	#[inline]
	fn size_hint(&self) -> (usize, Option<usize>) {
		let len = self.data.len() - self.index;
		(len, Some(len))
	}
}

impl<T> ExactSizeIterator for BoxIter<'_, T> {
	#[inline]
	fn len(&self) -> usize {
		self.data.len() - self.index
	}
}

impl<T> Drop for BoxIter<'_, T> {
	#[inline]
	fn drop(&mut self) {
		for _ in self {}
	}
}

impl<'a, T> IntoIterator for Box<'a, [T]> {
	type Item = T;
	type IntoIter = BoxIter<'a, T>;

	#[inline]
	fn into_iter(self) -> Self::IntoIter { BoxIter { index: 0, data: ManuallyDrop::new(self) } }
}

impl<T: ?Sized> Drop for Box<'_, T> {
	#[inline]
	fn drop(&mut self) {
		unsafe { std::ptr::drop_in_place(self.0) };
	}
}

impl<T: ?Sized> std::ops::Deref for Box<'_, T> {
	type Target = T;
	#[inline]
	fn deref(&self) -> &Self::Target { self.0 }
}

impl<T: ?Sized> std::ops::DerefMut for Box<'_, T> {
	#[inline]
	fn deref_mut(&mut self) -> &mut Self::Target { self.0 }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for Box<'_, T> {
	#[inline]
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Debug::fmt(&**self, f)
	}
}

impl<T: ?Sized + fmt::Display> fmt::Display for Box<'_, T> {
	#[inline]
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Display::fmt(&**self, f)
	}
}

impl<T: ?Sized> fmt::Pointer for Box<'_, T> {
	#[inline]
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Pointer::fmt(&(self.0 as *const T), f)
	}
}

impl<T: ?Sized> AsRef<T> for Box<'_, T> {
	#[inline]
	fn as_ref(&self) -> &T { self }
}

impl<T: ?Sized> AsMut<T> for Box<'_, T> {
	#[inline]
	fn as_mut(&mut self) -> &mut T { self }
}

impl<T: ?Sized> std::borrow::Borrow<T> for Box<'_, T> {
	#[inline]
	fn borrow(&self) -> &T { self }
}

impl<T: PartialEq + ?Sized> PartialEq for Box<'_, T> {
	#[inline]
	fn eq(&self, other: &Self) -> bool { **self == **other }
}

impl<T: Eq + ?Sized> Eq for Box<'_, T> {}

impl<T: PartialOrd + ?Sized> PartialOrd for Box<'_, T> {
	#[inline]
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> { (**self).partial_cmp(&**other) }
}

impl<T: Ord + ?Sized> Ord for Box<'_, T> {
	#[inline]
	fn cmp(&self, other: &Self) -> std::cmp::Ordering { (**self).cmp(&**other) }
}

impl<T: std::hash::Hash + ?Sized> std::hash::Hash for Box<'_, T> {
	#[inline]
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) { (**self).hash(state) }
}
