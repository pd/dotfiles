_default:
    @just --list

# nixos-rebuild test all hosts
test-all: (all "test")

# nixos-rebuild switch all hosts
switch-all: (all "switch")

build host:
    @just {{host}} build

test host:
    @just {{host}} test

switch host:
    @just {{host}} switch

repl host:
    @just {{host}} repl

# nixos-rebuild <op> all hosts
all op="test": (desk op) (htpc op) (pi op) (donix op)

desk op="test":
    sudo nixos-rebuild {{op}} --flake '.#desk' --fast

htpc op="test":
    nixos-rebuild {{op}} --flake '.#htpc' --target-host htpc --build-host htpc --use-remote-sudo --fast

pi op="test":
    nixos-rebuild {{op}} --flake '.#pi' --target-host pi --build-host pi --use-remote-sudo --fast

donix op="test":
    nixos-rebuild {{op}} --flake '.#donix' --target-host donix --build-host donix --use-remote-sudo --fast
