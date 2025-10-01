#[cfg(unix)]
pub use unix::*;
#[cfg(unix)]
mod unix {
	const PROT_READ: i32 = 0x1;
	const PROT_WRITE: i32 = 0x2;
	const MAP_PRIVATE: i32 = 0x02;
	const MAP_ANONYMOUS: i32 = 0x20;

	unsafe extern "C-unwind" {
		fn mmap(addr: *mut u8, length: usize, prot: i32, flags: i32, fd: i32, offset: usize) -> *mut u8;
		fn munmap(addr: *mut u8, length: usize) -> i32;
		fn mremap(addr: *mut u8, old_size: usize, new_size: usize, flags: i32) -> *mut u8;
	}

	pub unsafe fn map(size: usize) -> *mut u8 {
		unsafe { mmap(std::ptr::null_mut(), size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0) }
	}

	pub unsafe fn unmap(addr: *mut u8, size: usize) {
		unsafe { munmap(addr, size); }
	}

	pub unsafe fn remap(addr: *mut u8, old_size: usize, new_size: usize) -> *mut u8 {
		unsafe { mremap(addr, old_size, new_size, 0) }
	}
}

#[cfg(windows)]
pub use windows::*;
#[cfg(windows)]
mod windows {
	const MEM_COMMIT: u32 = 0x1000;
	const MEM_RESERVE: u32 = 0x2000;
	const MEM_RELEASE: u32 = 0x8000;
	const PAGE_READWRITE: u32 = 0x04;

	unsafe extern "system" {
		fn VirtualAlloc(lpaddress: *mut u8, dwsize: usize, flallocationtype: u32, flprotect: u32) -> *mut u8;
		fn VirtualFree(lpaddress: *mut u8, dwsize: usize, dwfreeType: u32) -> i32;
	}

	pub unsafe fn map(size: usize) -> *mut u8 {
		unsafe { VirtualAlloc(std::ptr::null_mut(), size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE) }
	}

	pub unsafe fn unmap(addr: *mut u8, size: usize) {
		unsafe { VirtualFree(addr, 0, MEM_RELEASE); }
	}

	pub unsafe fn remap(addr: *mut u8, old_size: usize, new_size: usize) -> *mut u8 {
		unsafe { 
			let new_addr = map(new_size);
			if !new_addr.is_null() {
				std::ptr::copy_nonoverlapping(addr, new_addr, old_size.min(new_size));
				unmap(addr, old_size);
			}
			new_addr
		}
	}
}

#[cfg(target_family = "wasm")]
pub use wasm::*;
#[cfg(target_family = "wasm")]
mod wasm {
	pub unsafe fn map(size: usize) -> *mut u8 {
		let layout = std::alloc::Layout::from_size_align(size, 8).unwrap();
		unsafe { std::alloc::alloc(layout) }
	}

	pub unsafe fn unmap(addr: *mut u8, size: usize) {
		let layout = std::alloc::Layout::from_size_align(size, 8).unwrap();
		unsafe { std::alloc::dealloc(addr, layout); }
	}

	pub unsafe fn remap(addr: *mut u8, old_size: usize, new_size: usize) -> *mut u8 {
		let new_layout = std::alloc::Layout::from_size_align(new_size, 8).unwrap();
		unsafe { std::alloc::realloc(addr, new_layout, new_size) }
	}
}
