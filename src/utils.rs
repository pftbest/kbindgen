/// Thin wrapper around `&[u8]`
///
/// Implements `Debug` print using `from_utf8_lossy` and adds some convenience
/// methods for slicing and trimming as if it's an ascii string.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ByteStr<'a>(pub &'a [u8]);

impl<'a> From<&'a [u8]> for ByteStr<'a> {
    fn from(value: &'a [u8]) -> Self {
        ByteStr(value)
    }
}

impl<'a> core::ops::Deref for ByteStr<'a> {
    type Target = [u8];

    fn deref(&'_ self) -> &'a Self::Target {
        self.0
    }
}

impl core::fmt::Debug for ByteStr<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let str = String::from_utf8_lossy(self.0);
        write!(f, "{:?}", str)
    }
}

impl<'a> ByteStr<'a> {
    /// Divides the string into two at an index where the predicate is false.
    ///
    /// The first slice will contain bytes from the start of a string to the point where
    /// the predicate failed (excluding the byte which failed the predicate).
    /// The second slice will contain the rest (including the byte which failed the predicate).
    ///
    /// # Examples
    ///
    /// ```
    /// use kbindgen::utils::ByteStr;
    /// let s = ByteStr(&[1, 2, 3, 4, 5, 6]);
    ///
    /// {
    ///    let (left, right) = s.slice_where(|c| c == 0);
    ///    assert_eq!(left, ByteStr(&[]));
    ///    assert_eq!(right, ByteStr(&[1, 2, 3, 4, 5, 6]));
    /// }
    ///
    /// {
    ///     let (left, right) = s.slice_where(|c| c < 3);
    ///     assert_eq!(left, ByteStr(&[1, 2]));
    ///     assert_eq!(right, ByteStr(&[3, 4, 5, 6]));
    /// }
    ///
    /// {
    ///     let (left, right) = s.slice_where(|c| c > 0);
    ///     assert_eq!(left, ByteStr(&[1, 2, 3, 4, 5, 6]));
    ///     assert_eq!(right, ByteStr(&[]));
    /// }
    /// ```
    pub fn slice_where<P>(self, mut predicate: P) -> (Self, Self)
    where
        P: FnMut(u8) -> bool,
    {
        let slice = self.0;
        let len = slice.len();
        let mut pos = len;
        for i in 0..len {
            if !predicate(slice[i]) {
                pos = i;
                break;
            }
        }
        let (start, end) = slice.split_at(pos);
        (Self(start), Self(end))
    }

    /// Returns a slice with leading ascii whitespace removed.
    ///
    /// # Examples
    ///
    /// ```
    /// use kbindgen::utils::ByteStr;
    /// let s = ByteStr(b"\t \r\n abc  ");
    /// assert_eq!(s.trim_start(), ByteStr(b"abc  "));
    /// ```
    pub fn trim_start(self) -> Self {
        let (_, end) = self.slice_where(|c| c.is_ascii_whitespace());
        end
    }

    /// Returns a slice without the first byte.
    ///
    /// If the slice is empty, returns an empty slice.
    ///
    /// # Examples
    ///
    /// ```
    /// use kbindgen::utils::ByteStr;
    /// assert_eq!(ByteStr(b"abc").skip_one(), ByteStr(b"bc"));
    /// assert_eq!(ByteStr(b"").skip_one(), ByteStr(b""));
    /// ```
    pub fn skip_one(self) -> Self {
        if self.0.is_empty() {
            self
        } else {
            Self(&self.0[1..])
        }
    }

    /// Parse the string using `FromStr` trait.
    pub fn parse<F>(self) -> Result<F, ()>
    where
        F: core::str::FromStr,
    {
        let s = core::str::from_utf8(self.0).map_err(|_| ())?;
        s.parse().map_err(|_| ())
    }

    /// This specialization of `starts_with` is at least 10x faster than normal
    /// `slice::starts_with` for small strings. On some large input it reduced
    /// the total execution time from 10.0s to 0.8s
    pub fn starts_with<T>(self, needle: T) -> bool where T: AsRef<[u8]> {
        let needle: &[u8] = needle.as_ref();
        let slice = self.0;
        let len = needle.len();
        if len > slice.len() {
            return false;
        }
        for i in 0..len {
            if slice[i] != needle[i] {
                return false;
            }
        }
        true
    }
}
