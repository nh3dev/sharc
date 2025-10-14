#[cfg(unix)]
pub use unix::*;
#[cfg(unix)]
mod unix {
	use std::ffi::c_void;
	use std::sync::LazyLock;

	#[repr(transparent)]
	#[derive(Clone, Copy)]
	struct DlHandle(*mut c_void);

	unsafe impl Send for DlHandle {}
	unsafe impl Sync for DlHandle {}

	unsafe extern "C-unwind" {
		fn dlopen(path: *const u8, flags: i32) -> DlHandle;
		fn dlsym(handle: DlHandle, symbol: *const u8) -> *const c_void;
		fn dlerror() -> *const u8;
	}

	static CURRENT_PROCESS: LazyLock<DlHandle> = LazyLock::new(|| unsafe { 
		let handle = dlopen(std::ptr::null(), 0x1);
		if handle.0.is_null() {
			panic!("{}", std::ffi::CStr::from_ptr(dlerror() as *const i8).to_string_lossy());
		}
		handle
	});

	pub unsafe fn ldsym(symbol: &str) -> *const c_void {
		unsafe { dlsym(*CURRENT_PROCESS, symbol.as_bytes().as_ptr()) }
	}
}

#[cfg(not(unix))]
pub unsafe fn ldsym(symbol: &str) -> *const std::ffi::c_void {
	compile_warn::compile_warn!("miri extern functions are not supported on this platform");
	unimplemented!("miri extern functions are not supported on this platform")
}
