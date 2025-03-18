#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Integral {
	I32,
	I64,
	U32,
	U64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Float {
	Single,
	Double,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Numeric {
	Integral(Integral),
	Float(Float),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
	Numeric(Numeric),
}
