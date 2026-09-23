use clap::Parser;
use matc::Result;
use matc::cli::Cli;

fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    let cli = Cli::parse();
    if let Err(err) = cli.run() {
        eprintln!("{:?}", err);
        std::process::exit(1);
    }

    Ok(())
}
