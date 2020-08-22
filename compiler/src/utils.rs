use typed_arena::{Arena};

pub(crate) trait ArenaExt<T> {
    fn alloc_fixed<'a, I>(&'a self, iter: I) -> &'a mut [T]
    where
        I: IntoIterator<Item = T>,
        T: Default;
}

impl<T> ArenaExt<T> for Arena<T> {
    fn alloc_fixed<'a, I>(&'a self, iter: I) -> &'a mut [T]
    where
        I: IntoIterator<Item = T>,
        T: Default,
    {
        use std::{mem::MaybeUninit, ptr};

        let iter = iter.into_iter();

        unsafe {
            struct FillRemainingOnDrop<U: Default> {
                ptr: *mut U,
                end: *mut U,
            }

            impl<U: Default> Drop for FillRemainingOnDrop<U> {
                fn drop(&mut self) {
                    unsafe {
                        while self.ptr != self.end {
                            ptr::write(self.ptr, U::default());
                            self.ptr = self.ptr.add(1);
                        }
                    }
                }
            }
            let (len, max) = iter.size_hint();
            assert!(Some(len) == max);

            let elems = self.alloc_uninitialized(len);

            {
                let elems = elems as *mut _ as *mut MaybeUninit<T>;
                let mut fill = FillRemainingOnDrop {
                    ptr: elems as *mut T,
                    end: elems.add(len) as *mut T,
                };

                for elem in iter {
                    assert!(fill.ptr != fill.end);
                    ptr::write(fill.ptr, elem);
                    fill.ptr = fill.ptr.add(1);
                }
            }

            let elems = elems as *mut _ as *mut [T];
            &mut *elems
        }
    }
}

