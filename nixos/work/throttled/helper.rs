use std::fs::OpenOptions;
use std::io::prelude::*;
use std::{thread, time};

fn main() -> std::io::Result<()> {
    let file_name = "/sys/devices/virtual/powercap/intel-rapl-mmio/intel-rapl-mmio:0/constraint_0_power_limit_uw";

    loop {
        let mut file = OpenOptions::new().write(true).open(&file_name)?;
        file.write_all(b"20000000")?; //20 watts
        let ten_seconds = time::Duration::from_secs(10);
        thread::sleep(ten_seconds);
    }
}
