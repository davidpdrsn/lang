mod env_stack;
mod stack;

pub use env_stack::EnvStack;
pub use stack::Stack;

use pest::Span;

pub trait LineAndCol {
    fn line_and_col(&self) -> (usize, usize);
}

impl<'a> LineAndCol for Span<'a> {
    fn line_and_col(&self) -> (usize, usize) {
        self.start_pos().line_col()
    }
}
