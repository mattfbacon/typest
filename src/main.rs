#![deny(
	absolute_paths_not_starting_with_crate,
	keyword_idents,
	macro_use_extern_crate,
	meta_variable_misuse,
	missing_abi,
	missing_copy_implementations,
	non_ascii_idents,
	nonstandard_style,
	noop_method_call,
	rust_2018_idioms,
	unused_qualifications
)]
#![warn(clippy::pedantic)]
#![forbid(unsafe_code)]

use std::collections::HashMap;

use typst::diag::FileResult;
use typst::ecow::EcoString;
use typst::foundations::{Bytes, Datetime};
use typst::syntax::ast::{AstNode as _, Expr};
use typst::syntax::{ast, FileId, Source, Span, SyntaxKind, SyntaxNode};
use typst::text::{Font, FontBook};
use typst::utils::LazyHash;
use typst::{Library, World, WorldExt};

use crate::problem::Problem;
use crate::ty::Type;

mod problem;
mod ty;

struct Scope<'a> {
	parent: Option<&'a Scope<'a>>,
	types: HashMap<EcoString, Type>,
}

impl<'a> Scope<'a> {
	fn new() -> Self {
		Self {
			parent: None,
			types: HashMap::new(),
		}
	}

	fn sub<'b: 'a>(&'b self) -> Scope<'b> {
		Self {
			parent: Some(self),
			types: HashMap::new(),
		}
	}

	fn get_type(&self, name: &str) -> Option<&Type> {
		self
			.types
			.get(name)
			.or_else(|| self.parent.and_then(|parent| parent.get_type(name)))
	}

	fn insert_type(&mut self, name: EcoString, ty: Type) -> Option<Type> {
		self.types.insert(name, ty)
	}
}

struct Checker {
	problems: Vec<Problem>,
	last_type_annotation: Option<Type>,
}

enum CheckMarkupResponse {
	Normal,
	NextCode,
}

macro_rules! throw_problem {
	($self:ident, $problem:expr) => {
		$self.problems.push($problem);
		return Err(ProblemsReported);
	};
}

macro_rules! unwrap_problem {
	($self:ident, $expr:expr) => {
		match $expr {
			Ok(v) => v,
			Err(problem) => {
				throw_problem!($self, problem);
			}
		}
	};
}

struct ProblemsReported;

impl Checker {
	fn new() -> Self {
		Self {
			problems: Vec::new(),
			last_type_annotation: None,
		}
	}

	fn get_type_binop(
		&mut self,
		scope: &Scope<'_>,
		binary: ast::Binary<'_>,
	) -> Result<Type, ProblemsReported> {
		let lhs = self.get_type(scope, &binary.lhs())?;
		let rhs = self.get_type(scope, &binary.rhs())?;
		Ok(match binary.op() {
			ast::BinOp::Add => match (lhs, rhs) {
				(a, Type::None) => a,
				(Type::None, b) => b,
				(Type::Int, Type::Int) => Type::Int,
				(Type::Int | Type::Float, Type::Int | Type::Float) => Type::Float,
				(Type::Int | Type::Decimal, Type::Int | Type::Decimal) => Type::Decimal,
				_ => todo!(),
			},
			ast::BinOp::Sub => todo!(),
			ast::BinOp::Mul => todo!(),
			ast::BinOp::Div => todo!(),
			ast::BinOp::And => todo!(),
			ast::BinOp::Or => todo!(),
			ast::BinOp::Eq => todo!(),
			ast::BinOp::Neq => todo!(),
			ast::BinOp::Lt => todo!(),
			ast::BinOp::Leq => todo!(),
			ast::BinOp::Gt => todo!(),
			ast::BinOp::Geq => todo!(),
			ast::BinOp::Assign => todo!(),
			ast::BinOp::In => todo!(),
			ast::BinOp::NotIn => todo!(),
			ast::BinOp::AddAssign => todo!(),
			ast::BinOp::SubAssign => todo!(),
			ast::BinOp::MulAssign => todo!(),
			ast::BinOp::DivAssign => todo!(),
		})
	}

	fn get_type(&mut self, scope: &Scope<'_>, expr: &Expr<'_>) -> Result<Type, ProblemsReported> {
		Ok(match expr {
			Expr::Ident(ident) => {
				if let Some(ty) = scope.get_type(ident.get()) {
					ty.clone()
				} else {
					self.problems.push(Problem {
						message: "unknown identifier".to_string(),
						span: expr.span(),
					});
					return Err(ProblemsReported);
				}
			}
			Expr::None(..) => Type::None,
			Expr::Auto(..) => Type::Auto,
			Expr::Bool(..) => Type::Bool,
			Expr::Int(..) => Type::Int,
			Expr::Float(..) => Type::Float,
			Expr::Str(..) => Type::String,
			Expr::Parenthesized(parenthesized) => self.get_type(scope, &parenthesized.expr())?,
			Expr::Content(content) => {
				self.check_markup(&mut scope.sub(), content.body().to_untyped());
				Type::Content
			}
			Expr::Binary(binary) => self.get_type_binop(scope, *binary)?,
			Expr::Numeric(..) => todo!(),
			Expr::Code(..) => todo!(),
			Expr::Array(..) => todo!(),
			Expr::Dict(..) => todo!(),
			Expr::Unary(..) => todo!(),
			Expr::FieldAccess(..) => todo!(),
			Expr::FuncCall(func_call) => {
				let callee_expr = func_call.callee();
				let callee = self.get_type(scope, &callee_expr)?;
				let Type::Function {
					args: expected_args,
					ret: expected_ret,
				} = callee
				else {
					throw_problem!(
						self,
						Problem {
							message: format!("called non-function: {callee:?}"),
							span: callee_expr.span(),
						}
					);
				};

				let args_expr = func_call.args();
				// TODO: add and use `parameters` type.
				let actual_args = args_expr
					.items()
					.map(|arg| match arg {
						ast::Arg::Pos(expr) => expr,
						_ => todo!(),
					})
					.map(|arg| Ok((arg.span(), self.get_type(scope, &arg)?)))
					.collect::<Result<Vec<_>, _>>()?;

				if expected_args.len() != actual_args.len() {
					throw_problem!(
						self,
						Problem {
							message: format!(
								"expected {} argument{}, got {}",
								expected_args.len(),
								if expected_args.len() == 1 { "" } else { "s" },
								actual_args.len()
							),
							span: args_expr.span(),
						}
					);
				}
				for (expected_arg, (actual_arg_span, actual_arg)) in expected_args.iter().zip(actual_args) {
					let contained = expected_arg.contains(&actual_arg);
					let contained = unwrap_problem!(self, contained.unwrap(actual_arg_span));
					if !contained {
						throw_problem!(
							self,
							Problem {
								message: format!("invalid type. expected {expected_arg:?}, got {actual_arg:?}"),
								span: actual_arg_span,
							}
						);
					}
				}

				*expected_ret
			}
			Expr::Closure(..) => todo!(),
			Expr::Let(..) => todo!(),
			Expr::DestructAssign(..) => todo!(),
			Expr::Contextual(..) => todo!(),
			Expr::Conditional(..) => todo!(),
			Expr::While(..) => todo!(),
			Expr::For(..) => todo!(),
			Expr::Import(..) => todo!(),
			Expr::Include(..) => todo!(),
			Expr::Break(..) => todo!(),
			Expr::Continue(..) => todo!(),
			Expr::Return(..) => todo!(),
			_ => unreachable!(),
		})
	}

	fn check_type_expr(
		&mut self,
		scope: &Scope<'_>,
		expected_ty: &Type,
		expr: &Expr<'_>,
	) -> Result<(), ProblemsReported> {
		let actual_ty = self.get_type(scope, expr)?;
		self.check_type(expected_ty, &actual_ty, expr.span())
	}

	fn check_type(
		&mut self,
		expected_ty: &Type,
		actual_ty: &Type,
		expr_span: Span,
	) -> Result<(), ProblemsReported> {
		let contained = expected_ty.contains(actual_ty);
		let contained = unwrap_problem!(self, contained.unwrap(expr_span));
		if !contained {
			self.problems.push(Problem {
				message: format!("type mismatch. expected {expected_ty:?}, got {actual_ty:?}"),
				span: expr_span,
			});
			return Err(ProblemsReported);
		}
		Ok(())
	}

	fn parse_comment(raw: &str) -> Option<Type> {
		let ty = raw.strip_prefix("@type ")?;
		Some(ty.trim().parse().unwrap())
	}

	fn parse_ty_annot_node(node: &SyntaxNode) -> Option<Type> {
		let text = match node.kind() {
			SyntaxKind::LineComment => node.text().strip_prefix("//").unwrap().trim(),
			SyntaxKind::BlockComment => node
				.text()
				.strip_prefix("/*")
				.unwrap()
				.strip_suffix("*/")
				.unwrap()
				.trim(),
			_ => return None,
		};
		Self::parse_comment(text)
	}

	fn comment(&mut self, node: &SyntaxNode) {
		let Some(ty) = Self::parse_ty_annot_node(node) else {
			return;
		};
		let old = self.last_type_annotation.replace(ty);
		if let Some(old) = old {
			panic!(
				"un tipo già n'hai annotato uno ({old:?}). forse non è stato usato come ti sei aspettato/a"
			);
		}
	}

	fn check_code(
		&mut self,
		scope: &mut Scope<'_>,
		root: &SyntaxNode,
	) -> Result<(), ProblemsReported> {
		match root.kind() {
			SyntaxKind::LetBinding => {
				let let_binding = root.cast::<ast::LetBinding<'_>>().unwrap();
				match let_binding.kind() {
					ast::LetBindingKind::Normal(pattern) => match pattern {
						ast::Pattern::Normal(expr) => match expr {
							Expr::Ident(ident) => {
								let ident = ident.get().clone();
								let expected_ty = self
									.last_type_annotation
									.take()
									.unwrap_or(Type::NotKnownYet);
								scope.insert_type(ident.clone(), expected_ty.clone());
								if expected_ty.not_known_yet() {
									// Type is not known.
									// If there is an initializer, we can get the type from there.
									let ty = if let Some(init) = let_binding.init() {
										self.get_type(scope, &init)?
									} else {
										Type::Any
									};
									scope.insert_type(ident, ty);
								} else {
									// Type is known.
									// If there is an initializer, we can check its type.
									if let Some(init) = let_binding.init() {
										self.check_type_expr(scope, &expected_ty, &init)?;
									}
								}
							}
							_ => todo!(),
						},
						_ => todo!(),
					},
					// TODO migrate closure-specific logic to `get_type`.
					ast::LetBindingKind::Closure(ident) => {
						let ident = ident.get().clone();

						let expected_ret_ty = self
							.last_type_annotation
							.take()
							.unwrap_or(Type::NotKnownYet);

						let mut expected_arg_tys = Vec::new();
						let closure = let_binding
							.to_untyped()
							.cast_first_match::<ast::Closure<'_>>()
							.unwrap();
						let mut last_ty = None;
						for child in closure.params().to_untyped().children() {
							if let Some(ty_annot) = Self::parse_ty_annot_node(child) {
								let old = last_ty.replace(ty_annot);
								if old.is_some() {
									throw_problem!(
										self,
										Problem {
											span: child.span(),
											message: "multiple type annotations".to_string(),
										}
									);
								}
							} else if let Some(param) = child.cast::<ast::Param<'_>>() {
								let ty = last_ty.take().unwrap_or(Type::Any);
								let ast::Param::Pos(ast::Pattern::Normal(Expr::Ident(name))) = param else {
									todo!();
								};
								expected_arg_tys.push((name, ty));
							}
						}

						let closure_ty = Type::Function {
							args: expected_arg_tys
								.iter()
								.map(|(_name, ty)| ty)
								.cloned()
								.collect(),
							// This may be `NotKnownYet`.
							ret: Box::new(expected_ret_ty.clone()),
						};
						scope.insert_type(ident.clone(), closure_ty);

						let mut body_scope = scope.sub();
						for (name, ty) in &expected_arg_tys {
							body_scope.insert_type(name.get().clone(), ty.clone());
						}

						let body = closure.body();
						let actual_ret_ty = self.get_type(&body_scope, &body);
						let ret_ty = actual_ret_ty.unwrap_or(Type::Any);

						// If return type was not explicitly provided, infer it from the body.
						if expected_ret_ty.not_known_yet() {
							let closure_ty = Type::Function {
								args: expected_arg_tys
									.iter()
									.map(|(_name, ty)| ty)
									.cloned()
									.collect(),
								ret: Box::new(ret_ty),
							};
							scope.insert_type(ident.clone(), closure_ty);
						} else {
							self.check_type(&expected_ret_ty, &ret_ty, body.span())?;
						}
					}
				}
			}
			_ => todo!(),
		}
		Ok(())
	}

	fn check_math(&mut self, _scope: &mut Scope<'_>, root: &SyntaxNode) {
		eprintln!("todo check math {root:?}");
	}

	fn check_markup(&mut self, scope: &mut Scope<'_>, node: &SyntaxNode) -> CheckMarkupResponse {
		match node.kind() {
			SyntaxKind::Error => unreachable!(),
			SyntaxKind::LineComment | SyntaxKind::BlockComment => self.comment(node),
			SyntaxKind::Equation => {
				let math = node
					.children()
					.find(|child| child.kind() == SyntaxKind::Math)
					.unwrap();
				self.check_math(scope, math);
			}
			SyntaxKind::Hash => return CheckMarkupResponse::NextCode,
			_ => {
				let mut is_code = false;
				for child in node.children() {
					if is_code {
						_ = self.check_code(scope, child);
						is_code = false;
					} else {
						let resp = self.check_markup(scope, child);
						is_code = match resp {
							CheckMarkupResponse::Normal => false,
							CheckMarkupResponse::NextCode => true,
						};
					}
				}
			}
		}
		CheckMarkupResponse::Normal
	}
}

fn check_types(world: &dyn World) -> Vec<Problem> {
	let main = world.source(world.main()).unwrap();

	let content = main.root();
	dbg!(content);

	let mut checker = Checker::new();
	let mut scope = Scope::new();

	checker.check_markup(&mut scope, content);

	checker.problems
}

const SOURCE: &str = r"
// @type int
#let a = 3
// @type string
#let b = 4

// @type int
#let c(
	// @type int
	x
) = x + 2
#let d(
	// @type string
	x
) = c(x)
";

struct TestWorld {
	library: LazyHash<Library>,
	book: LazyHash<FontBook>,
	main: Source,
}

impl TestWorld {
	fn new() -> Self {
		Self {
			library: LazyHash::new(Library::default()),
			book: LazyHash::new(FontBook::new()),
			main: Source::detached(SOURCE),
		}
	}
}

impl World for TestWorld {
	fn library(&self) -> &LazyHash<Library> {
		&self.library
	}

	fn book(&self) -> &LazyHash<FontBook> {
		&self.book
	}

	fn main(&self) -> FileId {
		self.main.id()
	}

	fn source(&self, id: FileId) -> FileResult<Source> {
		if id == self.main.id() {
			Ok(self.main.clone())
		} else {
			todo!()
		}
	}

	fn file(&self, _id: FileId) -> FileResult<Bytes> {
		todo!()
	}

	fn font(&self, _index: usize) -> Option<Font> {
		todo!()
	}

	fn today(&self, _offset: Option<i64>) -> Option<Datetime> {
		todo!()
	}
}

fn main() {
	let world = TestWorld::new();
	let problems = check_types(&world);
	for Problem { span, message } in problems {
		let span = world.range(span).unwrap();
		let text = &world.main.text()[span.clone()];
		let line = world.main.byte_to_line(span.start).unwrap();
		let column = world.main.byte_to_column(span.start).unwrap();
		eprintln!("problem:\n  span: {line}:{column} (span:?) {text:?}\n  message: {message}");
	}
}
