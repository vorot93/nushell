use crate::parser::TokenNode;
use crate::prelude::*;
use crate::traits::{DebugDocBuilder, PrettyDebugWithSource};
use crate::{Span, Spanned};
use derive_new::new;
use getset::Getters;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Getters, new)]
pub struct Pipeline {
    #[get = "pub"]
    pub(crate) parts: Vec<Spanned<PipelineElement>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Getters, new)]
pub struct PipelineElement {
    pub pipe: Option<Span>,
    #[get = "pub"]
    pub tokens: Spanned<Vec<TokenNode>>,
}

impl PrettyDebugWithSource for Spanned<Pipeline> {
    fn pretty_debug(&self, source: &str) -> DebugDocBuilder {
        b::intersperse(
            self.parts.iter().map(|token| token.pretty_debug(source)),
            b::operator(" | "),
        )
    }
}

impl PrettyDebugWithSource for Spanned<PipelineElement> {
    fn pretty_debug(&self, source: &str) -> DebugDocBuilder {
        b::intersperse(
            self.tokens.iter().map(|token| match token {
                TokenNode::Whitespace(_) => b::blank(),
                token => token.pretty_debug(source),
            }),
            b::space(),
        )
    }
}
