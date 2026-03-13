use std::io::{self, Write};
use zbus::blocking::fdo::PropertiesProxy;
use zbus::blocking::Connection;

fn emit(paused: bool) -> io::Result<()> {
    let state = if paused { "paused" } else { "unpaused" };
    let mut stdout = io::stdout().lock();
    writeln!(
        stdout,
        r#"{{"text":"{state}", "alt":"{state}", "class":"{state}"}}"#
    )?;
    stdout.flush()
}

fn is_paused(proxy: &PropertiesProxy) -> Result<bool, Box<dyn std::error::Error>> {
    let iface = zbus::names::InterfaceName::from_static_str("org.dunstproject.cmd0")?;
    let value = proxy.get(iface, "paused")?;
    Ok(value.try_into()?)
}

pub fn monitor() -> Result<(), Box<dyn std::error::Error>> {
    let conn = Connection::session()?;

    let proxy = PropertiesProxy::builder(&conn)
        .destination("org.freedesktop.Notifications")?
        .path("/org/freedesktop/Notifications")?
        .build()?;

    emit(is_paused(&proxy)?)?;

    let signals = proxy.receive_properties_changed()?;
    for _signal in signals {
        emit(is_paused(&proxy)?)?;
    }

    Ok(())
}
