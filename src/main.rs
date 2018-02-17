use clap::{App, SubCommand};
use std::process;
use std::io;

extern crate serde_json;
extern crate ansi_term;
extern crate clap;

mod link;

fn build_cli<'a>() -> App<'a, 'a> {
    let link = SubCommand::with_name("link")
        .about("Creates config file symlinks");

    return App::new("dotfiles").subcommand(link);
}

fn execute_cmd(command: &str) -> io::Result<()> {
    if command == "link" {
        link::make_symlinks()?;
    }

    Ok(())
}

fn main() {
    let app = build_cli();
    let matches = app.get_matches();

    let name = match matches.subcommand_name() {
        Some(name) => name,
        _ => {
            build_cli()
                .print_help()
                .expect("Tried to write a help page, but the universe exploded.");

            return ();
        },
    };

    match execute_cmd(name) {
        Err(reason) => {
            println!("Failed: {}", reason);
            process::exit(1);
        },
        _ => (),
    }
}
