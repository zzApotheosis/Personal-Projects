var mydata: c_uint = 0;

pub export fn zig_foo(n: c_uint) callconv(.C) c_uint {
    mydata = mydata + n;
    return mydata;
}
