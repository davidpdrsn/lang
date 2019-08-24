macro_rules! stack {
    () => {
        Stack::new()
    };

    ( $($elem:expr),* ) => {
        Stack::from_vec(vec![ $($elem),* ])
    };
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    pub fn new() -> Self {
        Stack(Vec::new())
    }

    pub fn size(&self) -> usize {
        self.0.len()
    }

    pub fn push(&mut self, item: T) {
        self.0.push(item)
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.0.is_empty() {
            None
        } else {
            Some(self.0.remove(self.0.len() - 1))
        }
    }

    pub fn peek(&self) -> Option<&T> {
        if self.0.is_empty() {
            None
        } else {
            Some(&self.0[self.0.len() - 1])
        }
    }

    pub fn from_vec(vec: Vec<T>) -> Self {
        Stack(vec)
    }
}

impl<T> IntoIterator for Stack<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Stack<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a, T> IntoIterator for &'a mut Stack<T> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_pushing_and_poping() {
        let mut stack = Stack::<String>::new();
        assert_eq!(stack.size(), 0);

        stack.push("1".to_string());
        assert_eq!(stack.size(), 1);
        stack.push("2".to_string());
        assert_eq!(stack.size(), 2);

        assert_eq!(stack.pop(), Some("2".to_string()));
        assert_eq!(stack.size(), 1);

        assert_eq!(stack.peek(), Some(&"1".to_string()));
        assert_eq!(stack.size(), 1);

        assert_eq!(stack.pop(), Some("1".to_string()));
        assert_eq!(stack.size(), 0);

        assert_eq!(stack.pop(), None);
        assert_eq!(stack.size(), 0);
    }

    #[test]
    fn macro_and_iter() {
        let stack = stack![1, 2, 3];
        let as_vec = stack.into_iter().collect::<Vec<_>>();
        assert_eq!(vec![1, 2, 3], as_vec);
    }
}
