fn main() {
	std::process::Command::new("git").args(["rev-parse", "HEAD"])
		.output()
		.map(|output| match output.status.success() {
			true => {
				let git_rev = String::from_utf8_lossy(&output.stdout);
				println!("cargo:rustc-env=GIT_REV={}", git_rev.trim());
			},
			false => {
				println!("cargo:warning=Failed to get git revision");
			},
		})
		.unwrap_or_else(|e| 
			println!("cargo:warning=Failed to execute git command: {}", e));
}
