use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Emitter};
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

impl Error {
    pub fn emit(&self, code_map: &CodeMap) -> ExitCode {
        let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(code_map));
        emitter.emit(std::slice::from_ref(&self.0));
        ExitCode::FAILURE
    }
}
