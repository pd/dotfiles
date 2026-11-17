_default:
    @just --list

# nixos-rebuild test all hosts
[group('ops')]
test-all: (all "test")

# nixos-rebuild switch all hosts
[group('ops')]
switch-all: (all "switch")

[group('ops')]
diff host:
    @just {{ host }} diff

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
    if [[ "{{ host }}" == "armspan" ]]; then
      nix repl 'path:.#darwinConfigurations.armspan'
    elif [[ "{{ host }}" == "mini" ]]; then
      nix repl 'path:.#darwinConfigurations.mini'
    elif [[ "{{ host }}" == "hm" ]]; then
      just hm repl
    else
      nix repl 'path:.#nixosConfigurations.{{ host }}'
    fi

homeConfiguration := x'$USER@$(hostname)'
[group('ops')]
hm op="switch":
    #!/usr/bin/env bash
    if [[ "{{ op }}" == "diff" ]]; then
      just hm build
      nvd diff $(home-manager generations | head -1 | cut -d' ' -f7) ./result
    else
      home-manager {{ op }} --flake "path:$(pwd)#{{ homeConfiguration }}"
    fi

[group('hosts')]
all op="test": (desk op) (htpc op) (pi op) (donix op)

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

_nixos_rebuild op host:
    #!/usr/bin/env bash
    set -euo pipefail

    is_local() {
      test "{{ host }}" == "$HOSTNAME"
    }

    if [[ "{{ op }}" == "diff" ]]; then
      if is_local; then
        just build {{ host }}
        nvd diff /run/current-system ./result
      else
        build_log="$(mktemp -p "${TMPDIR:-/tmp}" nixos-rebuild-log.XXXXXX)"
        trap "rm -f $build_log" EXIT
        just build {{ host }} | tee "$build_log"
        ssh "{{ host }}" nix run nixpkgs#nvd diff /run/current-system "$(cat "$build_log")"
      fi

    elif is_local; then
      nixos-rebuild {{ op }} --flake 'path:.#{{ host }}' --sudo --no-reexec
    else
      nixos-rebuild {{ op }} --flake 'path:.#{{ host }}' --target-host {{ host }} --build-host {{ host }} --sudo --no-reexec
    fi

[group('hosts')]
armspan op="switch":
    @just _darwin_rebuild {{ op }} armspan

[group('hosts')]
mini op="switch":
    @just _darwin_rebuild {{ op }} mini

_darwin_rebuild op host:
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ "{{ host }}" == "$(hostname -s)" ]]; then
      if [[ "{{ op }}" == "diff" ]]; then
        just build {{ host }}
        nvd diff /run/current-system ./result
      else
        sudo darwin-rebuild {{ op }} --flake 'path:.#{{ host }}'
      fi
      exit
    fi

    out=$(nix build --no-link --print-out-paths --eval-store auto --store "ssh-ng://{{ host }}" 'path:.#darwinConfigurations.{{ host }}.system')
    case "{{ op }}" in
      build) echo "$out" ;;
      diff) ssh "{{ host }}" nix run nixpkgs#nvd diff /run/current-system "$out" ;;
      switch) ssh -t "{{ host }}" "sudo nix-env -p /nix/var/nix/profiles/system --set $out && sudo $out/activate" ;;
      activate) ssh -t "{{ host }}" "sudo $out/activate" ;;
      *) echo "unsupported remote op: {{ op }}" >&2; exit 1 ;;
    esac

[group('routers')]
routers:
    nix build 'path:.#routers'

[group('routers')]
wrt op="reload": routers
    @just _deploy_router wrt {{ op }}

[group('routers')]
rpt op="restart": routers
    @just _deploy_router rpt {{ op }}

_deploy_router name op:
    #!/usr/bin/env bash
    set -euo pipefail

    # ensures GNU cp/getopt are available when running dewclaw's generated deploy script
    run() { nix shell --inputs-from . nixpkgs#coreutils nixpkgs#util-linux --command "$@"; }

    case "{{ op }}" in
    build)   true;;
    restart) run ./result/bin/deploy-{{ name }};;
    *)       run ./result/bin/deploy-{{ name }} --reload;;
    esac

# no args: dns + flakes; `flakes`: all inputs; `dns`: blocklist SHAs; else: named inputs
[group('dev')]
bump *inputs:
    #!/usr/bin/env bash
    set -euo pipefail
    case "{{ inputs }}" in
      "")     just bump dns && just bump flakes;;
      flakes) nix flake update;;
      dns)    just _bump_dns;;
      *)      nix flake update {{ inputs }};;
    esac

_bump_dns:
    #!/usr/bin/env bash
    set -euo pipefail
    pin() {
      local repo=$1
      local branch=$2
      local sha
      sha=$(git ls-remote "https://github.com/$repo" "refs/heads/$branch" | cut -f1)
      [[ $sha =~ ^[0-9a-f]{40}$ ]] || { echo "bad sha for $repo: '$sha'" >&2; exit 1; }
      sed -i -E "s#(raw\.githubusercontent\.com/$repo/)[0-9a-f]{40}/#\1$sha/#" flake.nix
    }
    pin StevenBlack/hosts master
    pin hagezi/dns-blocklists main
    nix flake update stevenblack-blocklist hagezi-lgwebos

# basic linting
[group('dev')]
check:
    fd -e nix -X nixfmt --check
    deadnix --fail
    statix check
    (cd pkgs/waybar-pd && cargo check)

# format
[group('dev')]
fmt:
    fd -e nix -X nixfmt

# autofix some lint errors
[group('dev')]
fix:
    deadnix --edit
    statix fix
    fd -e nix -X nixfmt

[group('dev')]
rekey:
    agenix --rekey
