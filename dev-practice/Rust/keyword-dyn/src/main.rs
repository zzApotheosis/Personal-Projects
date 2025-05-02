trait Animal {
    fn sound(&self) -> String;
}

struct Dog;

impl Animal for Dog {
    fn sound(&self) -> String {
        "Woof!".to_string()
    }
}

struct Cat;

impl Animal for Cat {
    fn sound(&self) -> String {
        "Meow!".to_string()
    }
}

fn main() {
    let dog = Dog;
    let cat = Cat;

    let animals: Vec<&dyn Animal> = vec![&dog, &cat];

    for animal in animals {
        println!("{}", animal.sound());
    }
}
