fn main() {
    eprintln!("Run `cargo test` to execute the unit tests in this example.");
}

mod foo {
    /*
     * Define a custom data structure.
     */
    pub struct Bar<D> {
	data: D,
    }

    /*
     * Implement functions (known as methods in other languages) for the custom data structure.
     */
    impl<D> Bar<D> {
	pub fn new(data: D) -> Bar<D> {
	    Bar{data}
	}

	pub fn get_data(&self) -> & D {
	    &self.data
	}
    }

    /*
     * Implement Display for i32 generics.
     */
    impl std::fmt::Display for Bar<i32> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	    write!(f, "{}", self.data)
        }
    }

    /*
     * Implement Display for String generics.
     *
     * Also, is there a way to implement traits for multiple types at the same time?
     */
    impl std::fmt::Display for Bar<std::string::String> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	    write!(f, "{}", self.data)
        }
    }

    /*
     * Define another custom data structure.
     */
    pub struct Stack<D> {
	next: Option<Box<Stack<D>>>,
	data: Option<D>,
    }

    /*
     * Implement the generic Stack data structure.
     */
    impl<D> Stack<D> {
	pub fn new() -> Self {
            Self { next: None, data: None }
	}

	pub fn push(&mut self, d: D) -> () {
	    let mut new_stack = Stack::new();
	    std::mem::swap(&mut new_stack.next, &mut self.next);
	    std::mem::swap(&mut new_stack.data, &mut self.data);

	    self.data = Some(d);
	    self.next = Some(Box::new(new_stack));
	}

	pub fn pop (&mut self) -> Option<D> {
	    if let Some(next_stack) = self.next.take() {
		let popped_data = self.data.take();
		let mut temp_stack = *next_stack;
		std::mem::swap(&mut self.next, &mut temp_stack.next);
		std::mem::swap(&mut self.data, &mut temp_stack.data);
		return popped_data;
	    }
	    None
	}

	pub fn clear(&mut self) -> () {
	    self.next = None;
	    self.data = None;
	}

	pub fn size(&self) -> usize {
	    let mut count = 0;
	    let mut current = self;
	    while current.data.is_some() {
		count += 1;
		if let Some(ref next_stack) = current.next {
		    current = next_stack.as_ref();
		} else {
		    break;
		}
	    }
	    count
	}

	pub fn is_empty(&self) -> bool {
	    self.data.is_none()
	}
    }

    /*
     * Run tests with `cargo test`.
     */
    #[cfg(test)]
    mod tests {
	use super::*;

	#[test]
	fn test_foo_bar() {
	    let foo = Bar::new(String::from("ligma"));
	    assert_eq!(foo.data, "ligma".to_string());

	    let bar = Bar::new(69i32);
	    assert_eq!(*bar.get_data(), 69);
	}

	#[test]
	fn test_stack() {
	    let mut stack: Stack<i32> = Stack::new();
	    assert!(stack.is_empty());
	    assert_eq!(stack.size(), 0);

	    stack.push(1);
	    assert_eq!(stack.size(), 1);
	    assert_eq!(stack.pop(), Some(1));
	    assert!(stack.is_empty());

	    stack.push(1);
	    stack.push(2);
	    stack.push(3);
	    assert_eq!(stack.size(), 3);
	    stack.clear();
	    assert!(stack.is_empty());
	    assert_eq!(stack.size(), 0);

	    let mut str_stack: Stack<String> = Stack::new();
	    str_stack.push("hello".to_string());
	    assert_eq!(str_stack.pop(), Some("hello".to_string()));
	}
    }
}


