use std::collections::HashMap;
use std::sync::{LazyLock, RwLock};

use crate::report::ReportKind;

pub static CACHE: LazyLock<Cache> = LazyLock::new(Cache::new);
pub struct Cache(RwLock<HashMap<&'static str, &'static str>>); // TODO: make hash faster if slow
impl Cache {
	fn new() -> Self {
		Self(RwLock::new(HashMap::new()))
	}

	pub fn get(&self, filename: &'static str) -> &'static str {
		if let Some(contents) = self.0.read().unwrap().get(&filename) {
			return contents;
		}

		let contents = std::fs::read_to_string(filename).unwrap_or_else(|e| {
			print!("{}",
				ReportKind::IOError
					.title(format!("Failed to read file '{filename}'"))
					.footer(e));
			std::process::exit(1);
		});

		let contents = Box::leak(contents.into_boxed_str());

		self.0.write().unwrap().insert(filename, contents);
		contents
	}
}
