//use ::std::default::Default; // Already part of the prelude; no need to import explicitly

fn main() -> ()
{
    let mut my_struct: StructA = StructA::default();
    let mut other_struct: StructB = StructB::default();
    my_struct.x += 1;
    println!("my_struct.x = {}", my_struct.x);
    println!("other_struct.x = {}", other_struct.x);
    my_struct.work(1).unwrap();
    other_struct.work(1).unwrap();
    do_the_work(&mut my_struct);
    do_the_work(&mut other_struct);
}

fn do_the_work(w: &mut dyn Workable) {
    w.work(0).unwrap();
}

#[allow(dead_code)]
mod c
{
    
}

trait Workable
{
    fn work(&mut self, _: i32) -> Result<(), i32>;
}

//#[derive(Default)] // If we want to just automatically create default values for the struct on initialization
struct StructA
{
    x: i32
}

struct StructB
{
    x: bool
}

impl Default for StructA
{
    fn default() -> Self
    {
        Self
        {
            x: 0
        }
    }
}

impl Default for StructB
{
    fn default() -> Self
    {
        Self
        {
            x: false
        }
    }
}

impl Workable for StructA
{
    fn work(&mut self, x: i32) -> Result<(), i32>
    {
        println!("I just worked!");
        self.x = x;
        Ok(())
    }
}

impl Workable for StructB
{
    fn work(&mut self, x: i32) -> Result<(), i32>
    {
        self.x = x != 0;
        println!("I also just worked!");
        Ok(())
    }
}
