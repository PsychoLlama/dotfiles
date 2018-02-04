use clap::{App, SubCommand};
use std::process;

extern crate serde_json;
extern crate clap;

mod link;

fn main() {
    let link = SubCommand::with_name("link")
        .about("Creates config file symlinks");

    let matches = App::new("dotfiles")
        .subcommand(link)
        .get_matches();

    match matches.subcommand() {
        ("link", Some(_)) => {
            match link::make_symlinks() {
                Err(_) => process::exit(1),
                _ => (),
            };
        },
        _ => {
            println!("You gotta pass an arg. \
                     Someday I'll learn how to print a help page.");
        },
    };
}
