fn main() {
	std::process::Command::new("git")
		.args(["log", "-1", "--format=%H", "."])
		.output()
		.map(|output| match output.status.success() {
			true => println!("cargo:rustc-env=GITREV={}", String::from_utf8_lossy(&output.stdout)),
			_    => println!("cargo:warning=Failed to get git revision"),
		})
		.unwrap_or_else(|e| println!("cargo:warning=Failed to execute git command: {}", e));
}
