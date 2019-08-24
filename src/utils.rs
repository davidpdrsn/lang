use std::collections::HashMap;
use std::hash::Hash;

pub struct EnvStack<K, V> {
    bottom: HashMap<K, V>,
    stack: Vec<HashMap<K, V>>,
}

impl<K: Hash + Ord + Eq, V> EnvStack<K, V> {
    pub fn new() -> Self {
        EnvStack {
            bottom: HashMap::new(),
            stack: Vec::new(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if let Some(current_value) = self.get_mut(&key) {
            let prev = std::mem::replace(current_value, value);
            return Some(prev);
        }

        if let Some(env) = self.stack.last_mut() {
            env.insert(key, value)
        } else {
            self.bottom.insert(key, value)
        }
    }

    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq,
    {
        for env in self.stack.iter().rev() {
            if let Some(value) = env.get(key) {
                return Some(value);
            }
        }
        self.bottom.get(key)
    }

    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq,
    {
        for env in self.stack.iter_mut().rev() {
            if let Some(value) = env.get_mut(key) {
                return Some(value);
            }
        }
        self.bottom.get_mut(key)
    }

    pub fn contains_key<Q: ?Sized>(&self, key: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq,
    {
        self.get(key).is_some()
    }

    pub fn push_env(&mut self) {
        self.stack.push(HashMap::new())
    }

    pub fn pop_env(&mut self) -> Option<HashMap<K, V>> {
        if !self.stack.is_empty() {
            Some(self.stack.remove(self.stack.len() - 1))
        } else {
            None
        }
    }
}
