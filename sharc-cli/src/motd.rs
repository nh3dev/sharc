fn rand() -> u32 {
	let mut x = std::time::SystemTime::now()
		.duration_since(std::time::SystemTime::UNIX_EPOCH)
		.unwrap().as_nanos() as u32;
	x ^= x << 13;
	x ^= x >> 7;
	x ^= x << 17;
	x
}

pub fn get_progress_msg() -> &'static str {
	const MSGS: &[&str] = &[ 
		"Sharding...",
		"Collecting garbage...",
	];

	MSGS[rand() as usize % MSGS.len()]
}

pub fn get_fail_msg() -> &'static str {
	const MSGS: &[&str] = &[ 
		"THIS INCIDENT WILL BE REPORTED",
		"9 + 10 = 21",
		"next time you're getting rm -rf'd",
		"developer we have a bug",
	];

	MSGS[rand() as usize % MSGS.len()]
}

// TODO: maybe success msg?
