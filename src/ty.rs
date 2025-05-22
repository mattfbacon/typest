use crate::problem::Problem;

// TODO interning to avoid cloning.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
	String,
	Int,
	Float,
	Decimal,
	Any,
	Unknown,
	None,
	Content,
	Auto,
	Bool,
	Function {
		args: Vec<Self>,
		ret: Box<Self>,
	},
	Array(Vec<Self>),
	/// Only used internally.
	NotKnownYet,
	// make sure to add new variants to the parser impl if necessary.
}

pub enum Containment {
	True,
	False,
	NotKnownYet,
}

impl From<bool> for Containment {
	fn from(x: bool) -> Self {
		if x {
			Self::True
		} else {
			Self::False
		}
	}
}

impl Containment {
	pub fn from_iterator(iter: impl IntoIterator<Item = Containment>) -> Self {
		for sub in iter {
			if !matches!(sub, Self::True) {
				return sub;
			}
		}
		Self::True
	}

	pub fn unwrap(self, span: Span) -> Result<bool, Problem> {
		match self {
       Self::True => Ok(true),
       Self::False => Ok(false),
       Self::NotKnownYet => Err(Problem { message: "type is being evaluated and this usage is cyclic. try adding an explicit type annotation.".to_string(), span }),
    }
	}
}

trait IntoContainment {
	fn into_containment(self) -> Containment;
}

impl IntoContainment for Containment {
	fn into_containment(self) -> Containment {
		self
	}
}

impl IntoContainment for bool {
	fn into_containment(self) -> Containment {
		self.into()
	}
}

// Hack to allow `..iter` in `containment_and`.
impl<T: Iterator<Item = Containment>> IntoContainment for std::ops::RangeTo<T> {
	fn into_containment(self) -> Containment {
		Containment::from_iterator(self.end)
	}
}

macro_rules! containment_and {
	($single:expr) => { IntoContainment::into_containment($single) };
	($iter:expr, $($rest:expr),* $(,)?) => {{
		let v = IntoContainment::into_containment($iter);
		if !matches!(v, Containment::True) {
			v
		} else {
			containment_and!($($rest),*)
		}
	}};
	($first:expr, $($rest:expr),* $(,)?) => {{
		let v = Containment::from_containment($first);
		if !matches!(v, Containment::True) {
			v
		} else {
			containment_and!($($rest),*)
		}
	}};
}
use std::str::FromStr;

pub(crate) use containment_and;
use typst::syntax::Span;

impl Type {
	pub fn not_known_yet(&self) -> bool {
		matches!(self, Self::NotKnownYet)
	}

	pub fn contains(&self, other: &Self) -> Containment {
		// TODO maybe `any`/`unknown` should have precedence over this.
		if self.not_known_yet() || other.not_known_yet() {
			return Containment::NotKnownYet;
		}
		// `any` is the escape hatch and disables all checking.
		// `unknown` is like `any` but can only be widened,
		// i.e., all types are contained in `unknown` but not vice versa.
		if matches!(self, Self::Any | Self::Unknown) || matches!(other, Self::Any) {
			return Containment::True;
		}
		match self {
			Self::Function { args, ret } => {
				let Self::Function {
					args: other_args,
					ret: other_ret,
				} = other
				else {
					return Containment::False;
				};
				containment_and!(
					args.len() == other_args.len(),
					// Args are contravariant.
					..args
						.iter()
						.zip(other_args)
						.map(|(arg, other_arg)| other_arg.contains(arg)),
					// Ret is covariant.
					ret.contains(other_ret),
				)
			}
			Self::Array(els) => {
				let Self::Array(other_els) = other else {
					return Containment::False;
				};
				containment_and!(
					els.len() == other_els.len(),
					..els
						.iter()
						.zip(other_els)
						.map(|(el, other_el)| el.contains(other_el)),
				)
			}
			_ => (self == other).into(),
		}
	}
}

fn parse_type(raw: &str) -> Result<(Type, &str), &'static str> {
	if let Some(mut raw) = raw.strip_prefix('(') {
		let mut array_tys = Vec::new();
		while !raw.starts_with(')') {
			let el_ty;
			// TODO dicts / named fields.
			(el_ty, raw) = parse_type(raw)?;
			array_tys.push(el_ty);
			raw = raw.trim_start();
			if let Some(new_raw) = raw.strip_prefix(',') {
				raw = new_raw.trim_start();
			} else {
				if !raw.starts_with(')') {
					return Err("expected )");
				}
				break;
			}
		}
		raw = raw.strip_prefix(')').unwrap().trim_start();
		let (ty, raw) = if let Some(mut raw) = raw.strip_prefix("=>") {
			raw = raw.trim_start();
			let ret_ty;
			(ret_ty, raw) = parse_type(raw)?;
			let ty = Type::Function {
				args: array_tys,
				ret: Box::new(ret_ty),
			};
			(ty, raw)
		} else {
			(Type::Array(array_tys), raw)
		};
		return Ok((ty, raw.trim_start()));
	}

	let ty = match raw {
		"string" => Type::String,
		"int" => Type::Int,
		"float" => Type::Float,
		"decimal" => Type::Decimal,
		"any" => Type::Any,
		"unknown" => Type::Unknown,
		"none" => Type::None,
		"content" => Type::Content,
		"auto" => Type::Auto,
		"bool" => Type::Bool,
		_ => return Err("invalid type"),
	};
	// Above `match` checks the entire `raw` so cannot leave trailing input.
	Ok((ty, ""))
}

impl FromStr for Type {
	type Err = &'static str;

	fn from_str(raw: &str) -> Result<Self, Self::Err> {
		let (res, rest) = parse_type(raw)?;
		if !rest.is_empty() {
			return Err("extra input");
		}
		Ok(res)
	}
}

#[test]
fn test_parse_type() {
	assert_eq!("int".parse::<Type>().unwrap(), Type::Int);
	assert_eq!(
		"(int, (content, (int, int)) => none)"
			.parse::<Type>()
			.unwrap(),
		Type::Array(vec![
			Type::Int,
			Type::Function {
				args: vec![Type::Content, Type::Array(vec![Type::Int, Type::Int])],
				ret: Box::new(Type::None),
			}
		])
	);
}
