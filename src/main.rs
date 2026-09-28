use clap::Parser;
use matc::cli::Cli;
use miette::{MietteHandlerOpts, Report};

fn main() {
    tracing_subscriber::fmt::init();

    miette::set_hook(Box::new(|_| {
        Box::new(
            MietteHandlerOpts::new()
                .color(true)
                .tab_width(4)
                .build(),
        )
    }))
    .expect("failed to set miette hook");

    let cli = Cli::parse();
    if let Err(err) = cli.run() {
        eprintln!("{:?}", Report::new(err));
        std::process::exit(1);
    }
}
