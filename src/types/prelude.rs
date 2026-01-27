use std::{cell::RefCell, sync::Arc};

use ecow::EcoString;

use crate::types::{Type, TypeVar};

pub fn named(name: EcoString) -> Arc<Type> {
	Arc::new(Type::Named { name })
}

pub fn fun(args: Vec<Arc<Type>>, ret: Arc<Type>) -> Arc<Type> {
	Arc::new(Type::Fn { args, ret })
}

pub fn unit() -> Arc<Type> {
	Arc::new(Type::Unit)
}

pub fn ptr(pointee: Arc<Type>) -> Arc<Type> {
	Arc::new(Type::Ptr { pointee })
}

pub fn optional(inner: Arc<Type>) -> Arc<Type> {
	Arc::new(Type::Optional { inner })
}

pub fn unitype(id: u64) -> Arc<Type> {
	Arc::new(Type::Var {
		type_: Arc::new(RefCell::new(TypeVar::Unbound(id))),
	})
}
