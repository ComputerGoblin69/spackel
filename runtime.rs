#![no_std]

extern "C" {
    fn fwrite(
        ptr: *const core::ffi::c_void,
        size: usize,
        nmemb: usize,
        stream: *mut core::ffi::c_void,
    ) -> i32;
    fn printf(fmt: *const core::ffi::c_char, ...) -> i32;

    static stdout: *mut core::ffi::c_void;
}

#[no_mangle]
pub unsafe extern "C" fn spkl_print_char(n: u32) {
    let mut buf = [0; 4];
    let s = char::from_u32(n)
        .unwrap_or(char::REPLACEMENT_CHARACTER)
        .encode_utf8(&mut buf);
    unsafe {
        fwrite(s.as_ptr().cast(), 1, s.len(), stdout);
    }
}

#[no_mangle]
pub unsafe extern "C" fn spkl_print_i32(n: i32) {
    printf(b"%d\0".as_ptr().cast(), n);
}

#[no_mangle]
pub unsafe extern "C" fn spkl_println_i32(n: i32) {
    printf(b"%d\n\0".as_ptr().cast(), n);
}

#[no_mangle]
pub unsafe extern "C" fn spkl_print_f32(n: f32) {
    printf(b"%g\0".as_ptr().cast(), n as f64);
}

#[no_mangle]
pub unsafe extern "C" fn spkl_println_f32(n: f32) {
    printf(b"%g\n\0".as_ptr().cast(), n as f64);
}
