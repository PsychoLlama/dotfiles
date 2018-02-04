use std::io::{self, Error, ErrorKind};
use std::collections::{HashMap};
use std::path::Path;
use std::fs::File;

use serde_json as serde;

fn get_symlink_manifest() -> io::Result<HashMap<String, String>> {
    let path = "linked/manifest.json";

    // Make sure the manifest exists.
    if !Path::new(path).is_file() {
        println!("Can't find the manifest.json file. Add it, then try again.");

        let error = Error::new(ErrorKind::Other, "Can't find symlink manifest.");
        return Err(error);
    }

    // Parse it as JSON.
    let file = File::open(path).expect("Unable to read manifest file.");
    let data: HashMap<String, String> = serde::from_reader(&file)?;

    return Ok(data);
}

// TODO: implement.
pub fn make_symlinks() -> io::Result<()> {
    let _plan = get_symlink_manifest()?;

    return Ok(());
}
