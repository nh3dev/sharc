pub type Box<T> = bump::Box<'static, T>;
pub type Rc<T>  = bump::Rc<'static, T>;

thread_local! {
	pub static THREAD_BUMP: bump::Bump = bump::Bump::new();
}
