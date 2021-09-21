use std::io::Write;

pub fn write_varint(mut x: i32, writer: &mut dyn Write) -> std::io::Result<usize> {
    let mut read = 0;
    loop {
        let mut temp = (x & 0b0111_1111) as u8;
        x >>= 7;
        if x != 0 {
            temp |= 0b1000_0000;
        }

        read += writer.write(&[temp])?;

        if x == 0 {
            break;
        }
    }
    Ok(read)
}