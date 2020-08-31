use typed_arena::{Arena};
use gluon::base::pos::{BytePos,Span};
use ast::types::{SourceSpan,SourcePos};

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



pub fn source_span_to_byte_span(source_span:&SourceSpan) -> Span<BytePos>  {
    let start = source_pos_to_byte_pos(&source_span.start);
    let end =source_pos_to_byte_pos(&source_span.end);
    Span::new(start, end)
}

pub fn source_pos_to_byte_pos(source_pos:&SourcePos) -> BytePos  {
    let uline = source_pos.line as u32;
    let ucol = source_pos.col as u32;
    let hight16 = (uline << 16) & 0xffff0000;
    let low16 = ucol & & 0x0000ffff;
    let value = hight16 | low16;
    BytePos(value)
}