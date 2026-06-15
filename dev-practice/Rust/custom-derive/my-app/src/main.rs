use my_macros::Describe;

#[derive(Describe)]
struct User {
    username: String,
    email: String,
    age: u32,
}

#[derive(Describe)]
struct EmptyStruct;

fn main() {
    let user = User {
        username: String::from("Alice"),
        email: String::from("alice@example.com"),
        age: 700,
    };

    // Call the manually-generated method
    user.describe();

    let empty = EmptyStruct;
    empty.describe();
}
