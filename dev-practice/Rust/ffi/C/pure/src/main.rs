use std::ffi::CString;
use std::os::raw::c_char;

#[link(name = "my_lib")]
unsafe extern "C" {
    fn c_greet(_: *const c_char);
    fn c_calc_distance(_: *const CPoint, _: *const CPoint) -> ::std::ffi::c_double;
}

#[repr(C)]
struct CPoint {
    x: ::std::ffi::c_int,
    y: ::std::ffi::c_int,
}

fn main() {
    let rust_string = CString::new("Rustacean").expect("CString::new failed");

    unsafe {
        c_greet(rust_string.as_ptr());
    }

    let mut points: Vec<CPoint> = Vec::new();
    points.push(CPoint { x: 0, y: 0 });
    points.push(CPoint { x: 6, y: 7 });
    points.push(CPoint { x: -6, y: -7 });

    let origin: &CPoint = points.get(0).unwrap();
    let point: &CPoint = points.get(1).unwrap();
    let distance: ::std::ffi::c_double;
    unsafe {
        distance = c_calc_distance(origin, point);
    }
    eprintln!(
        "Distance between {} and {} = {}",
        points.get(0).unwrap(),
        points.get(1).unwrap(),
        distance
    );
    eprintln!("Wait a second... That's not what I passed into C world!");
}

impl ::std::fmt::Display for CPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{{{}, {}}}", self.x, self.y))
    }
}
