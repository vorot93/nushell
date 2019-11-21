use crate::parser::hir::Expression;
use crate::prelude::*;
use crate::traits::{DebugDocBuilder as b, PrettyDebug};
use derive_new::new;
use getset::{Getters, MutGetters};
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum RawPathMember {
    String(String),
    Int(BigInt),
}

pub type PathMember = Spanned<RawPathMember>;

impl PrettyDebug for &PathMember {
    fn pretty(&self) -> DebugDocBuilder {
        match &self.item {
            RawPathMember::String(string) => b::primitive(format!("{:?}", string)),
            RawPathMember::Int(int) => b::primitive(format!("{}", int)),
        }
    }
}

#[derive(
    Debug, Hash, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq, Getters, Clone, new,
)]
pub struct ColumnPath {
    #[get = "pub"]
    members: Vec<PathMember>,
}

impl ColumnPath {
    pub fn iter(&self) -> impl Iterator<Item = &PathMember> {
        self.members.iter()
    }

    pub fn split_last(&self) -> (&PathMember, &[PathMember]) {
        self.members.split_last().unwrap()
    }
}

impl PrettyDebug for ColumnPath {
    fn pretty(&self) -> DebugDocBuilder {
        let members: Vec<DebugDocBuilder> =
            self.members.iter().map(|member| member.pretty()).collect();

        b::delimit(
            "(",
            b::description("path") + b::equals() + b::intersperse(members, b::space()),
            ")",
        )
        .nest()
    }
}

impl HasFallibleSpan for ColumnPath {
    fn maybe_span(&self) -> Option<Span> {
        if self.members.len() == 0 {
            None
        } else {
            Some(span_for_spanned_list(self.members.iter().map(|m| m.span)))
        }
    }
}

impl fmt::Display for RawPathMember {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RawPathMember::String(string) => write!(f, "{}", string),
            RawPathMember::Int(int) => write!(f, "{}", int),
        }
    }
}

impl PathMember {
    pub fn string(string: impl Into<String>, span: impl Into<Span>) -> PathMember {
        RawPathMember::String(string.into()).spanned(span.into())
    }

    pub fn int(int: impl Into<BigInt>, span: impl Into<Span>) -> PathMember {
        RawPathMember::Int(int.into()).spanned(span.into())
    }
}

#[derive(
    Debug,
    Clone,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Getters,
    MutGetters,
    Serialize,
    Deserialize,
    new,
)]
#[get = "pub(crate)"]
pub struct Path {
    head: Expression,
    #[get_mut = "pub(crate)"]
    tail: Vec<PathMember>,
}

impl PrettyDebugWithSource for Path {
    fn pretty_debug(&self, source: &str) -> DebugDocBuilder {
        self.head.pretty_debug(source)
            + b::operator(".")
            + b::intersperse(self.tail.iter().map(|m| m.pretty()), b::operator("."))
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.head)?;

        for entry in &self.tail {
            write!(f, ".{}", entry.item)?;
        }

        Ok(())
    }
}

impl Path {
    pub(crate) fn parts(self) -> (Expression, Vec<PathMember>) {
        (self.head, self.tail)
    }
}
