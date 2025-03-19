pub fn this_lifetime_change_is_totally_safe<'a, T: ?Sized>(x: &T) -> &'a T {
    unsafe { std::mem::transmute(x) }
}

pub fn this_lifetime_change_is_totally_safe_mut<'a, T: ?Sized>(x: &mut T) -> &'a mut T {
    unsafe { std::mem::transmute(x) }
}
