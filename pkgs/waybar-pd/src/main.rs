mod dunst;
mod idle;
mod river;

fn main() {
    let mut args = std::env::args();
    let cmd = args.next().unwrap();

    let result = match args.next().as_deref() {
        Some("river-mode") => river::monitor(),
        Some("dunst") => dunst::monitor(),
        Some("idle") => idle::monitor(),
        _ => {
            eprintln!("usage: {cmd} [river-mode | dunst | idle]");
            std::process::exit(1);
        }
    };

    if let Err(e) = result {
        eprintln!("{cmd}: {e}");
        std::process::exit(1);
    }
}
