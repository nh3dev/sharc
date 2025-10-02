use std::mem::MaybeUninit;

/// By default Drop will call drop on all elements in the Vec.
pub struct Vec<'b, T> {
	len:   usize,
	slice: &'b mut [MaybeUninit<T>],
}

impl crate::Bump {
	pub fn alloc_vec<'b, T>(&self, len: usize) -> Vec<'b, T> {
		Vec { len: 0, slice: self.alloc_array_dyn_mut(|| MaybeUninit::uninit(), len) }
	}
}

impl<'b, T> Vec<'b, T> {
	pub fn as_slice(&self) -> &[T] {
		unsafe { std::slice::from_raw_parts(self.slice.as_ptr() as *const T, self.len) }
	}

	pub fn as_slice_mut(&mut self) -> &mut [T] {
		unsafe { std::slice::from_raw_parts_mut(self.slice.as_mut_ptr() as *mut T, self.len) }
	}

	pub fn as_raw_slice(&self) -> &[MaybeUninit<T>] {
		self.slice
	}

	pub fn as_raw_slice_mut(&mut self) -> &mut [MaybeUninit<T>] {
		self.slice
	}

	/// Consumes the Vec and returns ref slice. This leaks all elems in the vec.
	pub fn into_slice(self) -> &'b [T] {
		let slice = unsafe { std::slice::from_raw_parts(self.slice.as_ptr() as *const T, self.len) };
		std::mem::forget(self);
		slice
	}

	#[must_use]
	pub fn push(&mut self, elem: T) -> Option<()> {
		(self.len < self.slice.len()).then(|| {
			unsafe { *self.slice.get_unchecked_mut(self.len) = MaybeUninit::new(elem); }
			self.len += 1;
		})
	}

	pub unsafe fn push_unchecked(&mut self, elem: T) {
		unsafe { *self.slice.get_unchecked_mut(self.len) = MaybeUninit::new(elem); }
		self.len += 1;
	}

	pub fn pop(&mut self) -> Option<T> {
		(self.len > 0).then(|| {
			self.len -= 1;
			unsafe { std::ptr::read(self.slice.get_unchecked(self.len).as_ptr()) }
		})
	}

	pub fn get(&self, index: usize) -> Option<&T> {
		(index < self.len).then(|| unsafe { &*self.slice.get_unchecked(index).as_ptr() })
	}

	pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
		(index < self.len).then(|| unsafe { &mut *self.slice.get_unchecked_mut(index).as_mut_ptr() })
	}

	pub fn last(&self) -> Option<&T> {
		(self.len > 0).then(|| unsafe { &*self.slice.get_unchecked(self.len - 1).as_ptr() })
	}

	pub fn last_mut(&mut self) -> Option<&mut T> {
		(self.len > 0).then(|| unsafe { &mut *self.slice.get_unchecked_mut(self.len - 1).as_mut_ptr() })
	}

	pub fn iter(&self) -> std::slice::Iter<'_, T> {
		self.as_slice().iter()
	}

	pub const fn len(&self) -> usize { self.len }
	pub const fn is_empty(&self) -> bool { self.len == 0 }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Vec<'_, T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_list().entries(self.iter()).finish()
	}
}

impl<T> Drop for Vec<'_, T> {
	fn drop(&mut self) {
		self.as_slice_mut().iter_mut().for_each(
			|elem| unsafe { std::ptr::drop_in_place(elem) });
	}
}

impl<T: PartialEq> PartialEq for Vec<'_, T> {
	fn eq(&self, other: &Self) -> bool {
		self.as_slice() == other.as_slice()
	}
}

impl<T: Eq> Eq for Vec<'_, T> {}

impl<T> std::ops::Index<usize> for Vec<'_, T> {
	type Output = T;

	fn index(&self, index: usize) -> &Self::Output {
		&self.as_slice()[index]
	}
}

impl<T> std::ops::IndexMut<usize> for Vec<'_, T> {
	fn index_mut(&mut self, index: usize) -> &mut Self::Output {
		&mut self.as_slice_mut()[index]
	}
}

impl<T> Extend<T> for Vec<'_, T> {
	/// # Panics
	/// Panics if the Vec overflows. iter_len > vec_capacity - vec_len
	///
	fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
		iter.into_iter().for_each(|e| self.push(e).expect("Vec full"));
	}
}

// should this even be here?
pub trait CollectWith<T>: IntoIterator<Item = T> + Sized {
	fn collect_with<E: Extend<Self::Item>>(self, mut acc: E) -> E {
		acc.extend(self); acc
	}
}

impl<T, I: Iterator<Item = T>> CollectWith<T> for I {}
