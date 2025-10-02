#[cfg(unix)]
pub use unix::*;
#[cfg(unix)]
mod unix {
	use std::ffi::c_void;
	use std::sync::LazyLock;

	#[repr(transparent)]
	#[derive(Clone, Copy)]
	struct DlHandle(*mut u8);

	unsafe impl Send for DlHandle {}
	unsafe impl Sync for DlHandle {}

	unsafe extern "C-unwind" {
		fn dlopen(path: *const u8, flags: i32) -> DlHandle;
		fn dlsym(handle: DlHandle, symbol: *const u8) -> *const c_void;
	}

	static CURRENT_PROCESS: LazyLock<DlHandle> = LazyLock::new(|| 
		unsafe { dlopen(std::ptr::null(), 0x0) });

	pub unsafe fn ldsym(symbol: &str) -> *const c_void {
		unsafe { dlsym(*CURRENT_PROCESS, symbol.as_bytes().as_ptr()) }
	}
}

#[cfg(not(unix))]
pub unsafe fn ldsym(symbol: &str) -> *const c_void {
	compile_warn::compile_warn!("miri extern functions are not supported on this platform");
	unimplemented!("miri extern functions are not supported on this platform")
}
