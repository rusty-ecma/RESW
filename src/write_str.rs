use std::{
    io::{
        Write, Result as IoResult
    },
};

pub struct WriteString {
    buf: Vec<u8>,
}

pub struct ChildWriter<'a> {
    parent: &'a mut WriteString
}

impl WriteString {
    pub fn new() -> Self {
        Self {
            buf: vec![],
        }
    }
    pub fn generate_child(&mut self) -> ChildWriter {
        ChildWriter {
            parent: self
        }
    }
    pub fn get_string(&self) -> Result<String, ::std::string::FromUtf8Error> {
        String::from_utf8(self.buf.clone())
    }

    pub fn get_string_lossy(&self) -> String {
        String::from_utf8_lossy(&self.buf).to_owned().to_string()
    }
}

impl Write for WriteString {
    fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
        self.buf.extend(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> IoResult<()> {
        Ok(())
    }
}

impl<'a> Write for ChildWriter<'a> {
    fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
        self.parent.write(buf)
    }

    fn flush(&mut self) -> IoResult<()> {
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn write_test() {
        let mut w = WriteString::new();
        for _ in 0..100 {
            let ct = w.write(b"asdf").unwrap();
            assert_eq!(ct, 4);
        }
        let s = w.get_string().unwrap();
        assert_eq!(s, "asdf".repeat(100));
    }
}