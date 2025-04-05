use std::fmt::Display;

/// Presents iterators in a comma separated format.
pub struct IteratorFormatter<I, D>(pub I)
where
    I: Iterator<Item = D> + Clone,
    D: Display;

impl<I, D> Display for IteratorFormatter<I, D>
where
    I: Iterator<Item = D> + Clone,
    D: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.clone().peekable();

        loop {
            match (iter.next(), iter.peek().is_some()) {
                (Some(next), true) => write!(f, "{}, ", next)?,
                (Some(next), false) => write!(f, "{}", next)?,
                (None, _) => break Ok(()),
            }
        }
    }
}
