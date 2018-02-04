use clap::{App, SubCommand};

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
            link::create_plan();
        },
        _ => {
            println!("You gotta pass an arg. \
                     Someday I'll learn how to print a help page.");
        },
    };
}
