use super::Value;
use std::collections::BTreeMap;

#[derive(Default)]
pub struct Renames(BTreeMap<Value, Value>);

impl Renames {
    pub fn insert(&mut self, from: Value, to: Value) {
        debug_assert!(self.0.insert(from, to).is_none());
    }

    pub fn take(&mut self, mut value: Value) -> Value {
        while let Some(renamed) = self.0.remove(&value) {
            value = renamed;
        }
        value
    }

    pub fn apply_to_slice(&mut self, slice: &mut [Value]) {
        for value in slice {
            *value = self.take(*value);
        }
    }
}

impl Extend<(Value, Value)> for Renames {
    fn extend<T: IntoIterator<Item = (Value, Value)>>(&mut self, iter: T) {
        for (from, to) in iter {
            self.insert(from, to);
        }
    }
}
