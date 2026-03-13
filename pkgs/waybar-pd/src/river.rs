use std::io::{self, Write};
use wayland_client::protocol::{wl_registry, wl_seat};
use wayland_client::{Connection, Dispatch, QueueHandle};

mod proto {
    #[allow(clippy::single_component_path_imports)]
    use wayland_client;
    use wayland_client::protocol::*;

    pub mod __interfaces {
        use wayland_client::backend as wayland_backend;
        use wayland_client::protocol::__interfaces::*;
        wayland_scanner::generate_interfaces!("protocol/river-status-unstable-v1.xml");
    }

    use self::__interfaces::*;
    wayland_scanner::generate_client_code!("protocol/river-status-unstable-v1.xml");
}

use proto::zriver_seat_status_v1::ZriverSeatStatusV1;
use proto::zriver_status_manager_v1::ZriverStatusManagerV1;

struct State {
    status_manager: Option<ZriverStatusManagerV1>,
    seats: Vec<wl_seat::WlSeat>,
}

impl Dispatch<wl_registry::WlRegistry, ()> for State {
    fn event(
        state: &mut Self,
        registry: &wl_registry::WlRegistry,
        event: wl_registry::Event,
        _: &(),
        _: &Connection,
        qh: &QueueHandle<Self>,
    ) {
        if let wl_registry::Event::Global {
            name,
            interface,
            version,
        } = event
        {
            match interface.as_str() {
                "zriver_status_manager_v1" => {
                    state.status_manager = Some(registry.bind(name, version.min(4), qh, ()));
                }
                "wl_seat" => {
                    state
                        .seats
                        .push(registry.bind(name, version.min(9), qh, ()));
                }
                _ => {}
            }
        }
    }
}

impl Dispatch<wl_seat::WlSeat, ()> for State {
    fn event(
        _: &mut Self,
        _: &wl_seat::WlSeat,
        _: wl_seat::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
    }
}

impl Dispatch<ZriverStatusManagerV1, ()> for State {
    fn event(
        _: &mut Self,
        _: &ZriverStatusManagerV1,
        _: proto::zriver_status_manager_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
    }
}

impl Dispatch<ZriverSeatStatusV1, ()> for State {
    fn event(
        _: &mut Self,
        _: &ZriverSeatStatusV1,
        event: proto::zriver_seat_status_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        if let proto::zriver_seat_status_v1::Event::Mode { name } = event {
            let mut stdout = io::stdout().lock();
            if name != "normal" {
                writeln!(
                    stdout,
                    r#"{{"text":"{name}", "alt":"{name}", "class":"{name}"}}"#
                )
                .unwrap();
            } else {
                writeln!(stdout, "{{}}").unwrap();
            }
            stdout.flush().unwrap();
        }
    }
}

pub fn monitor() -> Result<(), Box<dyn std::error::Error>> {
    let conn = Connection::connect_to_env()?;
    let display = conn.display();
    let mut event_queue = conn.new_event_queue();
    let qh = event_queue.handle();

    let _registry = display.get_registry(&qh, ());

    let mut state = State {
        status_manager: None,
        seats: Vec::new(),
    };

    event_queue.roundtrip(&mut state)?;

    let status_manager = state
        .status_manager
        .as_ref()
        .ok_or("river status manager not advertised")?;

    let _seat_statuses: Vec<_> = state
        .seats
        .iter()
        .map(|seat| status_manager.get_river_seat_status(seat, &qh, ()))
        .collect();

    loop {
        event_queue.blocking_dispatch(&mut state)?;
    }
}
