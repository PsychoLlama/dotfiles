use std::io::{self, Error, ErrorKind};
use std::os::unix::fs::{symlink};
use std::collections::{HashMap};
use std::fs::{self, File};
use std::path::Path;
use std::env;

use serde_json as serde;
use ansi_term::Color::{Yellow, Green};

// Reads the symlink manifest.
fn get_symlink_manifest(manifest_file: &str) -> io::Result<HashMap<String, String>> {

    // Make sure the manifest exists.
    if !Path::new(&manifest_file).is_file() {
        let msg = "Can't find the manifest.json file. Add it, then try again.";
        let error = Error::new(ErrorKind::Other, msg);
        return Err(error);
    }

    // Parse it as JSON.
    let file = File::open(manifest_file).expect("Unable to read manifest file.");
    let data: HashMap<String, String> = serde::from_reader(&file)?;

    Ok(data)
}

fn get_dotfiles_dir() -> io::Result<String> {
    let current_exe = env::current_exe()?;
    let bin_str = fs::canonicalize(current_exe)?
        .into_os_string()
        .into_string()
        .expect("Couldn't resolve a path to the dotfiles executable.");

    let exe_path = bin_str.split('/');
    let mut path: Vec<&str> = vec![];

    for value in exe_path {
        path.push(value);
        if value == "dotfiles" {
            break;
        }
    }

    Ok(path.join("/"))
}

fn normalize_destination(path: &str) -> Result<String, env::VarError> {
    let home_path = env::home_dir().expect("Couldn't locate home directory.");

    let home = home_path
        .into_os_string()
        .into_string()
        .expect("Weird. Couldn't parse the home directory.");

    let normalized = path.replace('~', &home);

    Ok(normalized)
}

fn create_symlink(source: &str, destination: &str) -> io::Result<()> {
    assert!(Path::new(&source).exists());

    if Path::new(&destination).is_file() {
        fs::remove_file(&destination)?;
    }

    symlink(&source, &destination)?;

    Ok(())
}

pub fn make_symlinks() -> io::Result<()> {
    let dotfiles_dir = get_dotfiles_dir()?;
    let mut linked_dir = String::from(dotfiles_dir);
    linked_dir.push_str("/linked/");

    let mut manifest_file = String::clone(&linked_dir);
    manifest_file.push_str("/manifest.json");

    let manifest = get_symlink_manifest(&manifest_file)?;

    let arrow = Green.paint("=>");
    let opening_brace = Yellow.paint("{");
    let closing_brace = Yellow.paint("}");

    for (key, value) in manifest.iter() {
        let source = value.replace("./", linked_dir.as_ref());
        let destination = normalize_destination(&key).unwrap();

        create_symlink(&source, &destination)?;

        let pretty_source = value.replace("./", "linked/");

        println!("{} {} {} {} {}",
                 opening_brace,
                 key,
                 arrow.to_string(),
                 pretty_source,
                 closing_brace);
    }

    Ok(())
}
