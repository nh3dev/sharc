#![feature(ptr_metadata)]
#![feature(cell_update)]
#![feature(str_from_raw_parts)]

use std::cell::Cell;
use std::ptr::NonNull;

mod boxed;
mod rc;
mod r#ref;

pub use boxed::{Box, BoxIter};
pub use rc::Rc;

unsafe extern "C" {
	fn mmap(addr: *mut u8, length: usize, prot: i32, flags: i32, fd: i32, offset: usize) -> *mut u8;
	fn munmap(addr: *mut u8, length: usize) -> i32;
	fn mremap(addr: *mut u8, old_size: usize, new_size: usize, flags: i32) -> *mut u8;
}

pub struct Bump {
	chunk:     Cell<Option<NonNull<Chunk>>>,
	page_size: Cell<usize>,
}

struct Chunk {
	prev:  Option<NonNull<Chunk>>,
	index: usize,
	data:  [u8],
}

impl Chunk {
	fn header_size() -> usize {
		std::mem::size_of::<Option<NonNull<Chunk>>>() + std::mem::size_of::<usize>()
	}

	fn new(page_size: usize, alloc_pages: usize) -> NonNull<Chunk> {
		let size = page_size * alloc_pages;

		let ptr = unsafe { mmap(std::ptr::null_mut(), size, 0b11, 0x22, -1, 0) };

		if ptr.is_null() {
			panic!("Failed to allocate chunk of size {size}");
		}

		// metadata stores self.data length
		let ptr: *mut Chunk = std::ptr::from_raw_parts_mut(ptr, size - Self::header_size());

		unsafe {
			(*ptr).prev  = None;
			(*ptr).index = 0;

			NonNull::new_unchecked(ptr)
		}
	}

	fn get_free_bytes(&self) -> usize {
		std::ptr::metadata(self) - self.index
	}

	fn size(&self) -> usize {
		self.data.len() + Self::header_size()
	}
}

impl Drop for Chunk {
	fn drop(&mut self) {
		unsafe {
			munmap(self as *mut Self as *mut u8, self.size());
		}
	}
}

impl Default for Bump {
	fn default() -> Self { Self::new() }
}

impl Bump {
	const DEFAULT_PAGE_SIZE: usize = 4096;

	#[inline]
	pub const fn new() -> Self {
		Bump {
			chunk:     Cell::new(None),
			page_size: Cell::new(Self::DEFAULT_PAGE_SIZE),
		}
	}

	#[inline]
	pub fn with_page_size(self, page_size: usize) -> Self {
		self.set_page_size(page_size); self
	}

	#[inline]
	pub fn set_page_size(&self, page_size: usize) {
		self.page_size.set(page_size);
	}

	#[inline]
	pub fn get_page_size(&self) -> usize {
		self.page_size.get()
	}

	fn try_extend_chunk(&self, new_size: usize) -> Option<()> {
		let chunk = self.chunk.get()?;

		let new_ptr = unsafe { mremap(chunk.as_ptr() as *mut u8, chunk.as_ref().size(), new_size, 0) };
		if new_ptr.is_null() { return None; }

		let new_ptr = unsafe {
			NonNull::new_unchecked(std::ptr::from_raw_parts_mut(new_ptr, new_size - Chunk::header_size()))
		};

		self.chunk.set(Some(new_ptr));

		Some(())
	}

	#[inline(never)]
	#[cold]
	fn make_new_chunk(&self, size: usize) {
		let mut new_chunk = Chunk::new(self.page_size.get(), size);

		unsafe {
			new_chunk.as_mut().prev  = self.chunk.get();
			new_chunk.as_mut().index = 0;
		}

		self.chunk.set(Some(new_chunk));
	}

	pub fn alloc_size<T>(&self, size: usize) -> *mut [T] {
		if self.chunk.get().is_none() {
			self.make_new_chunk(size);
		}

		// blah blah blah retag fuck you rust and miri and borrowing and aliasing rules
		// TODO: if someone has an easy fix then pls do it, if not i an throwing rust
		// out the fucking window because this is absolute and utter bullshit
		let chunk = unsafe { self.chunk.get().unwrap().as_mut() };

		if size > chunk.get_free_bytes() { 
			let page_size = self.page_size.get();
			let new_size = 2_usize.pow((size / page_size + 1) as u32);

			if self.try_extend_chunk(new_size).is_none() { 
				self.make_new_chunk(new_size);
			}
		}

		let offset = std::mem::align_of::<T>() - 
			(&raw const chunk.data[chunk.index]).addr() % std::mem::align_of::<T>();

		let ptr = &raw mut chunk.data[chunk.index + offset..=chunk.index + offset + size];
		chunk.index += size + offset;
		ptr as *mut [T]
	}

	fn drop_chunks(&self) {
		if self.chunk.get().is_none() { return; }

		let mut chunk = self.chunk.get().unwrap();
		
		loop {
			let prev = unsafe { chunk.as_ref().prev };

			unsafe { std::ptr::drop_in_place(chunk.as_ptr()); }

			match prev {
				None => break,
				Some(prev_chunk) => chunk = prev_chunk,
			}
		}
	}

	pub fn clear(&self) {
		self.drop_chunks();
		self.chunk.set(None);
	}
}

impl Drop for Bump {
	fn drop(&mut self) {
		self.drop_chunks();
	}
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_bump_alloc() {
		let bump = Bump::new();
		let val = bump.alloc(42);
		let _val2 = bump.alloc(43);
		assert_eq!(*val, 42);
	}

	#[test]
	fn test_bump_alloc_slice() {
		let bump = Bump::new();
		let slice = bump.alloc_slice_copy(&[1, 2, 3]);
		assert_eq!(&*slice, &[1, 2, 3]);
	}

	#[test]
	fn test_bump_alloc_raw() {
		let bump = Bump::new();
		let raw_ptr = bump.alloc_raw(42);
		assert_eq!(unsafe { *raw_ptr }, 42);
	}

	#[test]
	fn test_bump_alloc_big() {
		let bump = Bump::new();
		let boxed_slice = bump.alloc_slice_copy(&[0u8; 8192]);
		assert_eq!(boxed_slice.len(), 8192);
		let _boxed_slice = bump.alloc_slice_copy(&[0u8; 8192]);

	}
}
