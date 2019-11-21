use crate::data::base::Primitive;
use crate::prelude::*;
use crate::traits::DebugDocBuilder as b;
use pretty::{BoxAllocator, DocAllocator};

impl PrettyType for Primitive {
    fn pretty_type(&self) -> DebugDocBuilder {
        match self {
            Primitive::Nothing => ty("nothing"),
            Primitive::Int(_) => ty("integer"),
            Primitive::Decimal(_) => ty("decimal"),
            Primitive::Bytes(_) => ty("bytesize"),
            Primitive::String(_) => ty("string"),
            Primitive::ColumnPath(_) => ty("column-path"),
            Primitive::Pattern(_) => ty("pattern"),
            Primitive::Boolean(_) => ty("boolean"),
            Primitive::Date(_) => ty("date"),
            Primitive::Duration(_) => ty("duration"),
            Primitive::Path(_) => ty("path"),
            Primitive::Binary(_) => ty("binary"),
            Primitive::BeginningOfStream => b::keyword("beginning-of-stream"),
            Primitive::EndOfStream => b::keyword("end-of-stream"),
        }
    }
}

impl PrettyDebug for Primitive {
    fn pretty(&self) -> DebugDocBuilder {
        match self {
            Primitive::Nothing => b::primitive("nothing"),
            Primitive::Int(int) => prim(format_args!("{}", int)),
            Primitive::Decimal(decimal) => prim(format_args!("{}", decimal)),
            Primitive::Bytes(bytes) => primitive_doc(bytes, "bytesize"),
            Primitive::String(string) => prim(string),
            Primitive::ColumnPath(path) => path.pretty(),
            Primitive::Pattern(pattern) => primitive_doc(pattern, "pattern"),
            Primitive::Boolean(boolean) => match boolean {
                true => b::primitive("$yes"),
                false => b::primitive("$no"),
            },
            Primitive::Date(date) => primitive_doc(date, "date"),
            Primitive::Duration(duration) => primitive_doc(duration, "seconds"),
            Primitive::Path(path) => primitive_doc(path, "path"),
            Primitive::Binary(_) => b::opaque("binary"),
            Primitive::BeginningOfStream => b::keyword("beginning-of-stream"),
            Primitive::EndOfStream => b::keyword("end-of-stream"),
        }
    }
}

impl PrettyDebug for Value {
    fn pretty(&self) -> DebugDocBuilder {
        match self {
            Value::Primitive(p) => p.pretty(),
            Value::Row(row) => row.pretty_builder().nest(1).group().into(),
            Value::Table(table) => BoxAllocator
                .text("[")
                .append(
                    BoxAllocator
                        .intersperse(table.iter().map(|v| v.item.to_doc()), BoxAllocator.space())
                        .nest(1)
                        .group(),
                )
                .append(BoxAllocator.text("]"))
                .into(),
            Value::Error(_) => b::error("error"),
            Value::Block(_) => b::opaque("block"),
        }
    }
}

fn prim(name: impl std::fmt::Debug) -> DebugDocBuilder {
    b::primitive(format!("{:?}", name))
}

fn ty(name: impl std::fmt::Debug) -> DebugDocBuilder {
    b::kind(format!("{:?}", name))
}

fn primitive_doc(name: impl std::fmt::Debug, ty: impl Into<String>) -> DebugDocBuilder {
    b::primitive(format!("{:?}", name)) + b::delimit("(", b::kind(ty.into()), ")")
}
