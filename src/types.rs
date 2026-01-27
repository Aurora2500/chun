pub mod prelude;

use std::{cell::RefCell, sync::Arc};

use ecow::EcoString;

#[derive(Debug)]
pub enum Type {
	Named {
		name: EcoString,
	},
	Fn {
		args: Vec<Arc<Self>>,
		ret: Arc<Self>,
	},
	Unit,
	Tuple {
		items: Vec<Arc<Self>>,
	},
	Ptr {
		pointee: Arc<Self>,
	},
	Optional {
		inner: Arc<Self>,
	},
	Var {
		type_: Arc<RefCell<TypeVar>>,
	},
}

#[derive(Debug)]
pub enum TypeVar {
	Unbound(u64),
	Link(Arc<Type>),
	Generic(u64),
}

#[derive(Debug)]
pub enum TypeConstraint {
	Integral,
	Floating,
	Boolean,
}
