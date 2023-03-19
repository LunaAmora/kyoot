use std::{fs::File, io::BufReader, path::PathBuf};

use clap::{Parser, Subcommand};
use clap_verbosity_flag::{InfoLevel, Verbosity};
use tracing::error;
use tracing_log::AsTrace;
use tracing_subscriber::FmtSubscriber;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
    #[command(flatten)]
    verbose: Verbosity<InfoLevel>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Compile a `.ky` file.
    Com {
        #[arg(value_name = "FILE")]
        path: PathBuf,
    },
}

fn main() {
    let args = Cli::parse();
    let level = args.verbose.log_level().map(|l| l.as_trace());

    FmtSubscriber::builder()
        .with_max_level(level)
        .without_time()
        .with_target(false)
        .try_init()
        .expect("setting default subscriber failed");

    if let Err(err) = try_main(args) {
        error!("{:?}", err);
        std::process::exit(1);
    }
}

fn try_main(args: Cli) -> anyhow::Result<()> {
    let Commands::Com { path } = args.command;
    let file = File::open(&path)?;
    let source_name = path.to_string_lossy();

    foxy::compile(BufReader::new(file), &source_name)
}

#[cfg(test)]
mod tests {
    use crate::Cli;

    #[test]
    fn verify_cli() {
        use clap::CommandFactory;
        Cli::command().debug_assert();
    }
}
