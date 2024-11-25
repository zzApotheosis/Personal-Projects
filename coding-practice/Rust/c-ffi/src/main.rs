fn main() -> Result<(), i32> {
    let s1 = CoolStruct { x: 24, y: 11 };
    let mut s2 = CoolStruct { x: 42, y: 0 };

    s1.twenty_five();
    s2.twenty_five();

    s1.forty_two();
    s2.forty_two();

    s1.hi("Steeben").unwrap();
    s2.look_of_disapproval().unwrap();

    unsafe {
        cool_function(s2.x, 'z' as i8, &s1);
        cool_function(s1.x, 'S' as i8, &s2);
    }

    let mut x: i32 = 24;
    println!("Rust x = {}", x);
    println!("Calling unsafe C functions!");
    unsafe {
        println!("From Rust, C static x = {}", get_x());
        set_x(123 as i32);
        println!("From Rust, C static x = {}", get_x());
        x = get_x();
    }
    println!("Rust x = {}", x);
    return Ok(());
}

extern "C" {
    fn cool_function(i: i32, c: i8, cs: *const CoolStruct);
    fn set_x(new_x: i32);
    fn get_x() -> i32;
}

#[repr(C)]
struct CoolStruct {
    pub x: i32,
    pub y: i32,
}

trait CoolTrait {
    fn hi(&self, name: &str) -> Result<(), i32>;
    fn look_of_disapproval(&mut self) -> Result<(), i32>;
}

trait GuardianOfTheGalaxy {
    fn forty_two(&self);
}

trait TwentyFive {
    fn twenty_five(&self);
}

impl CoolTrait for CoolStruct {
    fn hi(&self, name: &str) -> Result<(), i32> {
        println!("Why hello there, {name}!");
        Ok(())
    }

    fn look_of_disapproval(&mut self) -> Result<(), i32> {
        self.x += 1;
        self.y += 1;
        println!("ಠ_ಠ");
        Ok(())
    }
}

impl GuardianOfTheGalaxy for CoolStruct {
    fn forty_two(&self) {
        print!("The answer to life, the universe, and everything is ");
        if self.x == 42 {
            println!("{}", self.x);
        } else {
            println!("definitely not {}", self.x);
        }
    }
}

impl TwentyFive for CoolStruct {
    fn twenty_five(&self) {
        match self.x {
            24 => {
                println!(
                    "What's funnier than {}? {}. HEEEHEEHEHEHEHE",
                    self.x,
                    self.x + 1
                );
            }
            _ => {
                println!("{} isn't really funny at all :/", self.x);
            }
        }
    }
}
