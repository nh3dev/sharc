use std::cell::RefCell;
use super::hir::{Type, TypeKind};
use crate::span::{Spannable, Sp, Span};

pub static CORE_PATH: &[Sp<&str>] = &[Sp { span: Span::new(0), elem: "core" }];

pub struct StaticTypes<'src, 'b> {
	pub u8:    &'b RefCell<Type<'src, 'b>>,
	pub usize: &'b RefCell<Type<'src, 'b>>,
	pub isize: &'b RefCell<Type<'src, 'b>>,
	pub none:  &'b RefCell<Type<'src, 'b>>,
	pub never: &'b RefCell<Type<'src, 'b>>,
	pub type_: &'b RefCell<Type<'src, 'b>>,
}

impl StaticTypes<'_, '_> {
	pub fn new(bump: &bump::Bump) -> Self {
		let alloc_ty = |kind| bump.alloc(RefCell::new(Type::new(kind)));

		Self { 
			u8:    alloc_ty(TypeKind::U(8)),
			usize: alloc_ty(TypeKind::Usize),
			isize: alloc_ty(TypeKind::Isize),
			none:  alloc_ty(TypeKind::None),
			never: alloc_ty(TypeKind::Never),
			type_: alloc_ty(TypeKind::Type),
		}
	}
}
