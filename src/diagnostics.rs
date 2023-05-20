use codemap::{CodeMap, Span};
use codemap_diagnostic::{ColorConfig, Emitter, SpanLabel, SpanStyle};
use std::{fmt, process::ExitCode};

pub struct Error(pub codemap_diagnostic::Diagnostic);

impl fmt::Debug for Error {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

impl std::error::Error for Error {}

impl Error {
    pub fn emit(&self, code_map: &CodeMap) -> ExitCode {
        let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(code_map));
        emitter.emit(std::slice::from_ref(&self.0));
        ExitCode::FAILURE
    }
}

pub const fn error(message: String, spans: Vec<SpanLabel>) -> Error {
    Error(codemap_diagnostic::Diagnostic {
        level: codemap_diagnostic::Level::Error,
        message,
        code: None,
        spans,
    })
}

pub fn primary_label(span: Span, label: impl Into<String>) -> SpanLabel {
    SpanLabel {
        span,
        label: Some(label.into()),
        style: SpanStyle::Primary,
    }
}

pub fn secondary_label(span: Span, label: impl Into<String>) -> SpanLabel {
    SpanLabel {
        span,
        label: Some(label.into()),
        style: SpanStyle::Secondary,
    }
}
