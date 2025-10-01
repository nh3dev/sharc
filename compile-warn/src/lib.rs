#![feature(proc_macro_diagnostic)]

use proc_macro::TokenStream;

#[proc_macro]
pub fn compile_warn(stream: TokenStream) -> TokenStream {
	proc_macro::Diagnostic::new(proc_macro::Level::Warning, stream.to_string()).emit();
	TokenStream::new()
}
