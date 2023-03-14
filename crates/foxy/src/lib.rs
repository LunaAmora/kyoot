use std::io::Read;

use anyhow::Result;
use tracing::info;

pub fn compile(_buff: impl Read) -> Result<()> {
    info!("Hello from the Foxy compiler!");
    Ok(())
}
