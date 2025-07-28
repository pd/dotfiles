_default:
    @just --list

# nixos-rebuild test all hosts
[group('ops')]
test-all: (all "test")

# nixos-rebuild switch all hosts
[group('ops')]
switch-all: (all "switch")

[group('ops')]
build host:
    @just {{ host }} build

[group('ops')]
test host:
    @just {{ host }} test

[group('ops')]
switch host:
    @just {{ host }} switch

[group('ops')]
repl host:
    #!/usr/bin/env bash
    if [[ "{{ host }}" == "span" ]]; then
      nix repl .#darwinConfigurations.span
    else
      nix repl .#nixosConfigurations.{{ host }}
    fi

# nixos-rebuild <op> all hosts
[group('hosts')]
all op="test": (desk op) (htpc op) (pi op) (donix op) (orb op)

[group('hosts')]
desk op="test":
    @just _nixos_rebuild {{ op }} desk

[group('hosts')]
htpc op="test":
    @just _nixos_rebuild {{ op }} htpc

[group('hosts')]
pi op="test":
    @just _nixos_rebuild {{ op }} pi

[group('hosts')]
donix op="test":
    @just _nixos_rebuild {{ op }} donix

[group('hosts')]
orb op="test":
    @just _nixos_rebuild {{ op }} orb

_nixos_rebuild op host:
    #!/usr/bin/env bash
    if [[ "{{ host }}" == "$HOSTNAME" ]]; then
      nixos-rebuild {{ op }} --flake '.#{{ host }}' --use-remote-sudo --fast
    else
      nixos-rebuild {{ op }} --flake '.#{{ host }}' --target-host {{ host }} --build-host {{ host }} --use-remote-sudo --fast
    fi

# can only really be run on span
[group('hosts')]
span op="switch":
    sudo darwin-rebuild {{ op }} --flake '.#span'


[group('routers')]
routers:
    nix build '.#routers'

[group('routers')]
wrt op="reload": routers
    @just _deploy_router wrt {{ op }}

[group('routers')]
rpt op="reload": routers
    @just _deploy_router rpt {{ op }}

_deploy_router name op:
    if [[ "{{ op }}" == "build" ]]; then true; \
    elif [[ "{{ op }}" == "restart" ]]; then ./result/bin/deploy-{{ name }}; \
    else ./result/bin/deploy-{{ name }} --reload; \
    fi
