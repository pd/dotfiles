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
    @just {{ host }} repl

# nixos-rebuild <op> all hosts
[group('hosts')]
all op="test": (desk op) (htpc op) (pi op) (donix op)

[group('hosts')]
desk op="test":
    sudo nixos-rebuild {{ op }} --flake '.#desk' --fast

[group('hosts')]
htpc op="test":
    nixos-rebuild {{ op }} --flake '.#htpc' --target-host htpc --build-host htpc --use-remote-sudo --fast

[group('hosts')]
pi op="test":
    nixos-rebuild {{ op }} --flake '.#pi' --target-host pi --build-host pi --use-remote-sudo --fast

[group('hosts')]
donix op="test":
    nixos-rebuild {{ op }} --flake '.#donix' --target-host donix --build-host donix --use-remote-sudo --fast

# can only really be run on span, doesn't have test or repl, etc
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
