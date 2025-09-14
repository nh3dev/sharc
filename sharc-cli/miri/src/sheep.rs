pub enum Sheep<T: Clone> {
	Owned(T),
	Ptr(*mut T),
}

impl<T: Clone> Sheep<T> {
	pub fn as_mut(&mut self) -> &mut T {
		match self {
			Sheep::Owned(v) => v,
			Sheep::Ptr(v)   => unsafe { &mut **v },
		}
	}

	pub fn as_ref(&self) -> &T {
		match self {
			Sheep::Owned(v) => v,
			Sheep::Ptr(v)   => unsafe { &**v },
		}
	}

	pub fn into_owned(self) -> T {
		match self {
			Sheep::Owned(v) => v,
			Sheep::Ptr(v)   => unsafe { (*v).clone() },
		}
	}
}

impl<T: Clone> std::ops::Deref for Sheep<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target { self.as_ref() }
}

impl<T: Clone> std::ops::DerefMut for Sheep<T> {
	fn deref_mut(&mut self) -> &mut Self::Target { self.as_mut() }
}

impl<T: Clone> From<T> for Sheep<T> {
	fn from(v: T) -> Self { Sheep::Owned(v) }
}

impl<T: Clone + std::fmt::Debug> std::fmt::Debug for Sheep<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("Sheep").field(self.as_ref()).finish()
	}
}
