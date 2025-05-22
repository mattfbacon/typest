use typst::syntax::Span;

#[derive(Debug)]
pub struct Problem {
	pub span: Span,
	pub message: String,
}
