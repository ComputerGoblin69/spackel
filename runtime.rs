#![no_std]

extern "C" {
    fn write(fd: i32, buf: *const core::ffi::c_void, len: usize) -> i32;
}

#[no_mangle]
pub unsafe extern "C" fn spkl_print_char(n: u32) {
    let mut buf = [0; 4];
    let s = char::from_u32(n)
        .unwrap_or(char::REPLACEMENT_CHARACTER)
        .encode_utf8(&mut buf);
    unsafe {
        write(1, s.as_ptr().cast(), s.len());
    }
}
