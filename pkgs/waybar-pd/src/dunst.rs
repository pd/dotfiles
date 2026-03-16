use std::io::{self, Write};
use zbus::blocking::Connection;

#[zbus::proxy(
    interface = "org.dunstproject.cmd0",
    default_service = "org.freedesktop.Notifications",
    default_path = "/org/freedesktop/Notifications",
    gen_async = false
)]
trait Dunst {
    #[zbus(property, name = "paused")]
    fn paused(&self) -> zbus::Result<bool>;
}

fn emit(paused: bool) -> io::Result<()> {
    let state = if paused { "paused" } else { "unpaused" };
    let mut stdout = io::stdout().lock();
    writeln!(
        stdout,
        r#"{{"text":"{state}", "alt":"{state}", "class":"{state}"}}"#
    )?;
    stdout.flush()
}

pub fn monitor() -> Result<(), Box<dyn std::error::Error>> {
    let conn = Connection::session()?;
    let proxy = DunstProxy::new(&conn)?;

    for _signal in proxy.receive_paused_changed() {
        emit(proxy.paused()?)?;
    }

    Ok(())
}
