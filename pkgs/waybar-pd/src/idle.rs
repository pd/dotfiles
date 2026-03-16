use std::io::{self, BufRead, BufReader, Write};
use std::os::fd::OwnedFd;
use std::process::{Child, Command, Stdio};
use std::sync::mpsc::{self, Sender};
use std::thread;
use std::time::Duration;

use zbus::blocking::Connection;
use zbus::zvariant::OwnedFd as ZbusFd;

#[zbus::proxy(
    interface = "org.freedesktop.login1.Manager",
    default_service = "org.freedesktop.login1",
    default_path = "/org/freedesktop/login1",
    gen_async = false
)]
trait LoginManager {
    fn inhibit(&self, what: &str, who: &str, why: &str, mode: &str) -> zbus::Result<ZbusFd>;

    fn list_inhibitors(&self) -> zbus::Result<Vec<(String, String, String, String, u32, u32)>>;

    fn list_sessions(
        &self,
    ) -> zbus::Result<Vec<(String, u32, String, String, zbus::zvariant::OwnedObjectPath)>>;

    #[zbus(property)]
    fn block_inhibited(&self) -> zbus::Result<String>;

    #[zbus(signal)]
    fn session_new(&self, id: &str, path: zbus::zvariant::ObjectPath<'_>) -> zbus::Result<()>;

    #[zbus(signal)]
    fn session_removed(&self, id: &str, path: zbus::zvariant::ObjectPath<'_>) -> zbus::Result<()>;
}

#[zbus::proxy(
    interface = "org.freedesktop.login1.Session",
    default_service = "org.freedesktop.login1",
    gen_async = false
)]
trait LoginSession {
    #[zbus(property)]
    fn remote(&self) -> zbus::Result<bool>;
}

enum Event {
    Audio(bool),
    SaiiDied,
    Signal,
    SSH(bool),
    InhibitChanged,
}

fn watch_audio(tx: Sender<Event>) -> io::Result<Child> {
    let mut child = Command::new("sway-audio-idle-inhibit")
        .args(["--dry-print-both"])
        .stdout(Stdio::piped())
        .spawn()
        .map_err(|e| io::Error::new(e.kind(), format!("sway-audio-idle-inhibit: {e}")))?;

    let stdout = child.stdout.take().unwrap();
    thread::spawn(move || {
        let reader = BufReader::new(stdout);
        for line in reader.lines() {
            let Ok(line) = line else { break };
            let _ = tx.send(Event::Audio(line.trim() == "RUNNING"));
        }
        let _ = tx.send(Event::SaiiDied);
    });

    Ok(child)
}

fn has_remote_sessions(proxy: &LoginManagerProxy) -> bool {
    let Ok(sessions) = proxy.list_sessions() else {
        return false;
    };
    sessions.iter().any(|(_, _, _, _, path)| {
        LoginSessionProxy::builder(proxy.inner().connection())
            .path(path.as_ref())
            .ok()
            .and_then(|b| b.build().ok())
            .and_then(|p| p.remote().ok())
            .unwrap_or(false)
    })
}

fn watch_ssh(tx: Sender<Event>) {
    let tx_new = tx.clone();
    thread::spawn(move || {
        let conn = Connection::system().unwrap();
        let proxy = LoginManagerProxy::new(&conn).unwrap();
        let iter = proxy.receive_session_new().unwrap();
        for _ in iter {
            let active = has_remote_sessions(&proxy);
            let _ = tx_new.send(Event::SSH(active));
        }
    });

    thread::spawn(move || {
        let conn = Connection::system().unwrap();
        let proxy = LoginManagerProxy::new(&conn).unwrap();
        let iter = proxy.receive_session_removed().unwrap();
        for _ in iter {
            let active = has_remote_sessions(&proxy);
            let _ = tx.send(Event::SSH(active));
        }
    });
}

fn watch_inhibitors(tx: Sender<Event>) {
    thread::spawn(move || {
        let conn = Connection::system().unwrap();
        let proxy = LoginManagerProxy::new(&conn).unwrap();

        for _ in proxy.receive_block_inhibited_changed() {
            let _ = tx.send(Event::InhibitChanged);
        }
    });
}

fn watch_signals(tx: Sender<Event>) {
    thread::spawn(move || {
        let mut signals =
            signal_hook::iterator::Signals::new([signal_hook::consts::SIGUSR1]).unwrap();
        for _ in signals.forever() {
            let _ = tx.send(Event::Signal);
        }
    });
}

fn take_inhibit(proxy: &LoginManagerProxy, what: &str, why: &str) -> Option<OwnedFd> {
    let fd = proxy.inhibit(what, "waybar-pd", why, "block").ok()?;
    Some(fd.into())
}

fn list_inhibitors(proxy: &LoginManagerProxy) -> Vec<(String, String)> {
    let Ok(inhibitors) = proxy.list_inhibitors() else {
        return vec![];
    };
    inhibitors
        .into_iter()
        .filter(|(what, who, ..)| {
            who == "waybar-pd" && what.split(':').any(|w| w == "idle" || w == "sleep")
        })
        .flat_map(|(what, _, why, ..)| {
            what.split(':')
                .filter(|w| *w == "idle" || *w == "sleep")
                .map(|w| (w.to_string(), why.clone()))
                .collect::<Vec<_>>()
        })
        .collect()
}

fn tooltip(inhibitors: &[(String, String)]) -> String {
    let mut lines: Vec<String> = vec![];
    for kind in ["idle", "sleep"] {
        let mut reasons: Vec<&str> = inhibitors
            .iter()
            .filter(|(what, _)| what == kind)
            .map(|(_, why)| why.as_str())
            .collect();
        if reasons.is_empty() {
            continue;
        }
        reasons.sort_unstable();
        reasons.dedup();
        lines.push(format!("{kind} inhibited ({})", reasons.join(", ")));
    }
    lines.join("\\n")
}

fn emit(inhibitors: &[(String, String)]) -> io::Result<()> {
    let (alt, tooltip) = if inhibitors.is_empty() {
        ("uninhibited".to_string(), "idle uninhibited".to_string())
    } else {
        ("inhibited".to_string(), tooltip(inhibitors))
    };

    let mut stdout = io::stdout().lock();
    writeln!(
        stdout,
        r#"{{"text":"", "alt":"{alt}", "tooltip":"{tooltip}", "class":"{alt}"}}"#
    )?;
    stdout.flush()
}

pub fn monitor() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, rx) = mpsc::channel();

    let conn = Connection::system()?;
    let proxy = LoginManagerProxy::new(&conn)?;

    let mut saii = watch_audio(tx.clone())?;
    watch_ssh(tx.clone());
    watch_inhibitors(tx.clone());
    watch_signals(tx.clone());

    let mut audio_inhibitor: Option<OwnedFd> = None;
    let mut manual_inhibitor: Option<OwnedFd> = None;
    let mut ssh_inhibitor: Option<OwnedFd> = None;
    for event in rx {
        match event {
            Event::InhibitChanged => {}
            Event::Audio(playing) => {
                if playing && audio_inhibitor.is_none() {
                    audio_inhibitor = take_inhibit(&proxy, "idle", "audio");
                } else if !playing {
                    audio_inhibitor = None;
                }
            }
            Event::SSH(active) => {
                if active && ssh_inhibitor.is_none() {
                    ssh_inhibitor = take_inhibit(&proxy, "sleep", "ssh");
                } else if !active {
                    ssh_inhibitor = None;
                }
            }
            Event::Signal => {
                if manual_inhibitor.is_none() {
                    manual_inhibitor = take_inhibit(&proxy, "idle", "manual");
                } else {
                    manual_inhibitor = None;
                }
            }
            Event::SaiiDied => {
                let status = saii.wait();
                eprintln!("sway-audio-idle-inhibit exited: {status:?}");
                audio_inhibitor = None;
                thread::sleep(Duration::from_secs(1));
                saii = watch_audio(tx.clone())?;
            }
        }
        emit(&list_inhibitors(&proxy))?;
    }

    Ok(())
}
