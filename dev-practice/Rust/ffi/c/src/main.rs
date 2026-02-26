// Must match the same defined size in the C code
const MSG_SIZE: usize = 1024;

fn main() -> Result<(), i32> {
    let s1 = CoolStruct {
        x: 24,
        y: 11,
        msg: [0; MSG_SIZE],
    };
    let mut s2 = CoolStruct {
        x: 42,
        y: 0,
        msg: [0; MSG_SIZE],
    };

    unsafe {
        let s3: *mut CoolStruct = CoolStruct_new();
        CoolStruct_set_x(s3, 5);
        CoolStruct_set_y(s3, 7);
        let msg: String = String::from("This is one cool string\0");
        CoolStruct_set_msg(s3, msg.as_ptr());
        set_static_coolstruct(s3);
    }

    unsafe {
        let s4: *mut CoolStruct = get_static_coolstruct();
        println!("s4->x = {}", CoolStruct_get_x(s4));
        println!("s4->y = {}", CoolStruct_get_y(s4));
        let mut buffer: [u8; MSG_SIZE] = [0u8; MSG_SIZE];
        CoolStruct_get_msg(s4, buffer.as_mut_ptr());

        /* Finding the NUL-terminator, since this is a C-style string */
        let nul_index = buffer
            .iter()
            .position(|&c| c == b'\0')
            .unwrap_or(buffer.len());
        println!(
            "s4->msg = {}",
            ::std::str::from_utf8_unchecked(&buffer[0..nul_index])
        );

        (*s4).twenty_five();
        (*s4).forty_two();
        (*s4).look_of_disapproval().unwrap();

        CoolStruct_destroy(&s4);
    }

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

    let s3: *mut CoolStruct;
    let mut x: i32;
    let mut y: i32;

    unsafe {
        s3 = CoolStruct_new();
        x = CoolStruct_get_x(s3);
        y = CoolStruct_get_y(s3);
    }
    println!("s3->x = {x}");
    println!("s3->y = {y}");

    unsafe {
        CoolStruct_set_x(s3, 25);
        CoolStruct_set_y(s3, 42);
        x = CoolStruct_get_x(s3);
        y = CoolStruct_get_y(s3);
    }

    println!("s3->x = {x}");
    println!("s3->y = {y}");

    println!("Before destroy: s3 = {s3:?}");
    unsafe {
        CoolStruct_destroy(&s3);
    }
    println!("After destroy: s3 = {s3:?}");

    /* Use GMP to print a huge number */
    unsafe {
        let huge_num: String = String::from("135792468000000690000004206900000133700000001\0");
        print_bignum(huge_num.as_ptr());
    }

    /* Ye, boi */
    Ok(())
}

extern "C" {
    fn CoolStruct_new() -> *mut CoolStruct;
    fn CoolStruct_destroy(instance: *const *mut CoolStruct);
    fn CoolStruct_get_x(instance: *const CoolStruct) -> i32;
    fn CoolStruct_get_y(instance: *const CoolStruct) -> i32;
    fn CoolStruct_get_msg(instance: *const CoolStruct, msg: *const u8);
    fn CoolStruct_set_x(instance: *mut CoolStruct, x: i32);
    fn CoolStruct_set_y(instance: *mut CoolStruct, y: i32);
    fn CoolStruct_set_msg(instance: *mut CoolStruct, msg: *const u8);
    fn get_static_coolstruct() -> *mut CoolStruct;
    fn set_static_coolstruct(instance: *mut CoolStruct);
    fn cool_function(i: i32, c: i8, cs: *const CoolStruct);
    fn set_x(new_x: i32);
    fn get_x() -> i32;
    fn print_bignum(s: *const u8);
}

#[repr(C)]
struct CoolStruct {
    pub x: i32,
    pub y: i32,
    pub msg: [u8; MSG_SIZE],
}

trait CoolTrait {
    fn hi(&self, name: &str) -> Result<(), i32>;
    fn look_of_disapproval(&mut self) -> Result<(), i32>;
}

trait GuardianOfTheGalaxy {
    fn forty_two(&self);
}

trait SpongeBob {
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

impl SpongeBob for CoolStruct {
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
