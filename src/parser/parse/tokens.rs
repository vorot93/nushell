use crate::parser::Operator;
use crate::prelude::*;
use crate::Text;
use std::fmt;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum RawToken {
    Number(RawNumber),
    Operator(Operator),
    String(Span),
    Variable(Span),
    ExternalCommand(Span),
    ExternalWord,
    GlobPattern,
    Bare,
}

impl RawToken {
    pub fn type_name(&self) -> &'static str {
        match self {
            RawToken::Number(_) => "number",
            RawToken::Operator(..) => "operator",
            RawToken::String(_) => "string",
            RawToken::Variable(_) => "variable",
            RawToken::ExternalCommand(_) => "syntax error",
            RawToken::ExternalWord => "syntax error",
            RawToken::GlobPattern => "glob pattern",
            RawToken::Bare => "string",
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum RawNumber {
    Int(Span),
    Decimal(Span),
}

impl PrettyDebugWithSource for RawNumber {
    fn pretty_debug(&self, source: &str) -> DebugDocBuilder {
        match self {
            RawNumber::Int(span) => b::primitive(span.slice(source)),
            RawNumber::Decimal(span) => b::primitive(span.slice(source)),
        }
    }
}

impl RawNumber {
    pub fn int(span: impl Into<Span>) -> Spanned<RawNumber> {
        let span = span.into();

        RawNumber::Int(span).spanned(span)
    }

    pub fn decimal(span: impl Into<Span>) -> Spanned<RawNumber> {
        let span = span.into();

        RawNumber::Decimal(span).spanned(span)
    }

    pub(crate) fn to_number(self, source: &Text) -> Number {
        match self {
            RawNumber::Int(tag) => Number::Int(BigInt::from_str(tag.slice(source)).unwrap()),
            RawNumber::Decimal(tag) => {
                Number::Decimal(BigDecimal::from_str(tag.slice(source)).unwrap())
            }
        }
    }
}

pub type Token = Spanned<RawToken>;

impl PrettyDebugWithSource for Token {
    fn pretty_debug(&self, source: &str) -> DebugDocBuilder {
        match self.item {
            RawToken::Number(number) => number.pretty_debug(source),
            RawToken::Operator(operator) => operator.pretty(),
            RawToken::String(_) => b::primitive(self.span.slice(source)),
            RawToken::Variable(_) => b::var(self.span.slice(source)),
            RawToken::ExternalCommand(_) => b::primitive(self.span.slice(source)),
            RawToken::ExternalWord => b::typed("external", b::description(self.span.slice(source))),
            RawToken::GlobPattern => b::typed("pattern", b::description(self.span.slice(source))),
            RawToken::Bare => b::primitive(self.span.slice(source)),
        }
    }
}

impl Token {
    pub fn debug<'a>(&self, source: &'a Text) -> DebugToken<'a> {
        DebugToken {
            node: *self,
            source,
        }
    }

    pub fn extract_number(&self) -> Option<Spanned<RawNumber>> {
        match self.item {
            RawToken::Number(number) => Some(number.spanned(self.span)),
            _ => None,
        }
    }

    pub fn extract_int(&self) -> Option<(Span, Span)> {
        match self.item {
            RawToken::Number(RawNumber::Int(int)) => Some((int, self.span)),
            _ => None,
        }
    }

    pub fn extract_decimal(&self) -> Option<(Span, Span)> {
        match self.item {
            RawToken::Number(RawNumber::Decimal(decimal)) => Some((decimal, self.span)),
            _ => None,
        }
    }

    pub fn extract_operator(&self) -> Option<Spanned<Operator>> {
        match self.item {
            RawToken::Operator(operator) => Some(operator.spanned(self.span)),
            _ => None,
        }
    }

    pub fn extract_string(&self) -> Option<(Span, Span)> {
        match self.item {
            RawToken::String(span) => Some((span, self.span)),
            _ => None,
        }
    }

    pub fn extract_variable(&self) -> Option<(Span, Span)> {
        match self.item {
            RawToken::Variable(span) => Some((span, self.span)),
            _ => None,
        }
    }

    pub fn extract_external_command(&self) -> Option<(Span, Span)> {
        match self.item {
            RawToken::ExternalCommand(span) => Some((span, self.span)),
            _ => None,
        }
    }

    pub fn extract_external_word(&self) -> Option<Span> {
        match self.item {
            RawToken::ExternalWord => Some(self.span),
            _ => None,
        }
    }

    pub fn extract_glob_pattern(&self) -> Option<Span> {
        match self.item {
            RawToken::GlobPattern => Some(self.span),
            _ => None,
        }
    }

    pub fn extract_bare(&self) -> Option<Span> {
        match self.item {
            RawToken::Bare => Some(self.span),
            _ => None,
        }
    }
}

pub struct DebugToken<'a> {
    node: Token,
    source: &'a Text,
}

impl fmt::Debug for DebugToken<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.node.span.slice(self.source))
    }
}
