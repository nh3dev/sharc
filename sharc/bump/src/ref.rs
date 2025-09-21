impl crate::Bump {
	#[inline]
	pub fn alloc<'bump, T>(&self, val: T) -> &'bump T {
		let data = self.alloc_size::<T>(std::mem::size_of::<T>()) as *mut T;

		unsafe { 
			std::ptr::write(data, val); 
			&*data
		}
	}

	#[inline]
	fn alloc_slice_raw<'bump, T>(&self, val: &[T]) -> &'bump [T] {
		if val.is_empty() { return &[][..] }
		let data = self.alloc_size::<T>(std::mem::size_of_val(val)) as *mut T;

		unsafe {
			std::ptr::copy_nonoverlapping(val.as_ptr(), data, val.len()); 
			std::slice::from_raw_parts(data, val.len())
		}
	}

	#[inline]
	pub fn alloc_from_vec<'bump, T>(&self, mut val: Vec<T>) -> &'bump [T] {
		if val.is_empty() { return &[][..] }
		let data = self.alloc_slice_raw(&val);

		let layout = std::alloc::Layout::array::<T>(val.capacity()).unwrap();
		unsafe {
			std::alloc::dealloc(val.as_mut_ptr() as *mut u8, layout);
		}

		std::mem::forget(val);

		data
	}

	pub fn alloc_array<'bump, const N: usize, T>(&self, val: [T; N]) -> &'bump [T; N] {
		let data = self.alloc_size::<T>(std::mem::size_of_val(&val)) as *mut T;

		unsafe {
			val.into_iter().enumerate().for_each(|(i, e)| std::ptr::write(data.add(i), e));
			std::slice::from_raw_parts(data, N).try_into().unwrap()
		}
	}

	pub fn alloc_str<'bump>(&self, val: &str) -> &'bump str {
		let data = self.alloc_size::<u8>(val.len()) as *mut u8;

		unsafe {
			std::ptr::copy_nonoverlapping(val.as_ptr(), data, val.len()); 
			std::str::from_raw_parts(data, val.len())
		}
	}

	pub fn alloc_sized_slice<'bump, const N: usize, T>(&self, val: [T; N]) -> &'bump [T] {
		let data = self.alloc_size::<T>(std::mem::size_of_val(&val)) as *mut T;

		unsafe {
			val.into_iter().enumerate().for_each(|(i, e)| std::ptr::write(data.add(i), e));
			std::slice::from_raw_parts(data, N)
		}
	}

	#[inline]
	pub fn alloc_slice<'bump, T: Copy>(&self, val: &[T]) -> &'bump [T] {
		self.alloc_slice_raw(val)
	}

	#[inline]
	pub fn alloc_slice_clone<'bump, T: Clone>(&self, val: &[T]) -> &'bump [T] {
		let data = self.alloc_size::<T>(std::mem::size_of_val(val)) as *mut T;

		unsafe {
			val.iter().enumerate().for_each(|(i, e)| std::ptr::write(data.add(i), e.clone()));
			std::slice::from_raw_parts(data, val.len())
		}
	}

	pub fn alloc_from_iter<'bump, T>(&self, iter: impl ExactSizeIterator<Item = T>) -> &'bump [T] {
		let len = iter.len();
		let data = self.alloc_size::<T>(len * std::mem::size_of::<T>()) as *mut T;

		unsafe {
			iter.enumerate().for_each(|(i, val)| std::ptr::write(data.add(i), val));
			std::slice::from_raw_parts(data, len)
		}
	}

	pub fn try_alloc_from_iter<'bump, T, E>(&self, iter: impl ExactSizeIterator<Item = Result<T, E>>) -> Result<&'bump [T], E> {
		let len = iter.len();
		let data = self.alloc_size::<T>(len * std::mem::size_of::<T>()) as *mut T;

		unsafe {
			iter.enumerate().try_for_each(|(i, val)| { std::ptr::write(data.add(i), val?); Ok(()) })?;
			Ok(std::slice::from_raw_parts(data, len))
		}
	}
}

impl crate::Bump {
	#[inline]
	pub fn alloc_mut<'bump, T>(&self, val: T) -> &'bump mut T {
		let data = self.alloc_size::<T>(std::mem::size_of::<T>()) as *mut T;

		unsafe { 
			std::ptr::write(data, val); 
			&mut *data
		}
	}

	#[inline]
	fn alloc_slice_raw_mut<'bump, T>(&self, val: &[T]) -> &'bump mut [T] {
		if val.is_empty() { return &mut [][..] }
		let data = self.alloc_size::<T>(std::mem::size_of_val(val)) as *mut T;

		unsafe {
			std::ptr::copy_nonoverlapping(val.as_ptr(), data, val.len()); 
			std::slice::from_raw_parts_mut(data, val.len())
		}
	}

	#[inline]
	pub fn alloc_from_vec_mut<'bump, T>(&self, mut val: Vec<T>) -> &'bump mut [T] {
		if val.is_empty() { return &mut [][..] }
		let data = self.alloc_slice_raw_mut(&val);

		let layout = std::alloc::Layout::array::<T>(val.capacity()).unwrap();
		unsafe {
			std::alloc::dealloc(val.as_mut_ptr() as *mut u8, layout);
		}

		std::mem::forget(val);

		data
	}

	pub fn alloc_array_mut<'bump, const N: usize, T>(&self, val: [T; N]) -> &'bump mut [T; N] {
		let data = self.alloc_size::<T>(std::mem::size_of_val(&val)) as *mut T;

		unsafe {
			val.into_iter().enumerate().for_each(|(i, e)| std::ptr::write(data.add(i), e));
			std::slice::from_raw_parts_mut(data, N).try_into().unwrap()
		}
	}

	pub fn alloc_str_mut<'bump>(&self, val: &str) -> &'bump mut str {
		let data = self.alloc_size::<u8>(val.len()) as *mut u8;

		unsafe {
			std::ptr::copy_nonoverlapping(val.as_ptr(), data, val.len()); 
			std::str::from_utf8_unchecked_mut(std::slice::from_raw_parts_mut(data, val.len()))
		}
	}

	pub fn alloc_sized_slice_mut<'bump, const N: usize, T>(&self, val: [T; N]) -> &'bump mut [T] {
		let data = self.alloc_size::<T>(std::mem::size_of_val(&val)) as *mut T;

		unsafe {
			val.into_iter().enumerate().for_each(|(i, e)| std::ptr::write(data.add(i), e));
			std::slice::from_raw_parts_mut(data, N)
		}
	}

	#[inline]
	pub fn alloc_slice_mut<'bump, T: Copy>(&self, val: &[T]) -> &'bump mut [T] {
		self.alloc_slice_raw_mut(val)
	}

	#[inline]
	pub fn alloc_slice_clone_mut<'bump, T: Clone>(&self, val: &[T]) -> &'bump mut [T] {
		let data = self.alloc_size::<T>(std::mem::size_of_val(val)) as *mut T;

		unsafe {
			val.iter().enumerate().for_each(|(i, e)| std::ptr::write(data.add(i), e.clone()));
			std::slice::from_raw_parts_mut(data, val.len())
		}
	}

	pub fn alloc_from_iter_mut<'bump, T>(&self, iter: impl ExactSizeIterator<Item = T>) -> &'bump mut [T] {
		let len = iter.len();
		let data = self.alloc_size::<T>(len * std::mem::size_of::<T>()) as *mut T;

		unsafe {
			iter.enumerate().for_each(|(i, val)| std::ptr::write(data.add(i), val));
			std::slice::from_raw_parts_mut(data, len)
		}
	}

	pub fn try_alloc_from_iter_mut<'bump, T, E>(&self, iter: impl ExactSizeIterator<Item = Result<T, E>>) -> Result<&'bump mut [T], E> {
		let len = iter.len();
		let data = self.alloc_size::<T>(len * std::mem::size_of::<T>()) as *mut T;

		unsafe {
			iter.enumerate().try_for_each(|(i, val)| { std::ptr::write(data.add(i), val?); Ok(()) })?;
			Ok(std::slice::from_raw_parts_mut(data, len))
		}
	}
}
