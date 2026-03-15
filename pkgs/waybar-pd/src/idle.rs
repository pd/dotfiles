use std::io::{self, BufRead, BufReader, Write};
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::thread;

fn emit(audio: bool, manual: bool) -> io::Result<()> {
    let (alt, tooltip) = match (audio, manual) {
        (_, true) => ("inhibited", "idle inhibited (manual)"),
        (true, false) => ("inhibited", "idle inhibited (audio)"),
        (false, false) => ("uninhibited", "idle uninhibited"),
    };
    let mut stdout = io::stdout().lock();
    writeln!(
        stdout,
        r#"{{"text":"", "alt":"{alt}", "tooltip":"{tooltip}", "class":"{alt}"}}"#
    )?;
    stdout.flush()
}

enum Event {
    Audio(bool),
    Signal,
}

pub fn monitor() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, rx) = mpsc::channel();

    // saii handles inhibiting when audio is on, and prints an
    // update for us to track when it changes state
    let mut child = Command::new("sway-audio-idle-inhibit")
        .stdout(Stdio::piped())
        .spawn()?;

    let stdout = child.stdout.take().unwrap();
    let tx_audio = tx.clone();
    thread::spawn(move || {
        let reader = BufReader::new(stdout);
        for line in reader.lines() {
            let Ok(line) = line else { break };
            let audio = line.trim() == "IDLE INHIBITED";
            let _ = tx_audio.send(Event::Audio(audio));
        }
    });

    // on click we send a USR1 to toggle on/off
    let tx_signal = tx;
    thread::spawn(move || {
        let mut signals =
            signal_hook::iterator::Signals::new([signal_hook::consts::SIGUSR1]).unwrap();
        for _ in signals.forever() {
            let _ = tx_signal.send(Event::Signal);
        }
    });

    let mut audio = false;
    let mut manual = false;
    let mut inhibitor: Option<std::process::Child> = None;

    // Initial state
    emit(false, false)?;

    for event in rx {
        match event {
            Event::Audio(a) => audio = a,
            Event::Signal => {
                manual = !manual;
                if manual {
                    inhibitor = Command::new("systemd-inhibit")
                        .args(["--what=idle", "--who=waybar-pd", "--why=manual", "--mode=block", "sleep", "infinity"])
                        .stdin(Stdio::null())
                        .stdout(Stdio::null())
                        .stderr(Stdio::null())
                        .spawn()
                        .ok();
                } else if let Some(mut child) = inhibitor.take() {
                    let _ = child.kill();
                    let _ = child.wait();
                }
            }
        }
        emit(audio, manual)?;
    }

    Ok(())
}
