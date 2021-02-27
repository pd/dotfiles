terraform {
  backend "local" {
    path = "./.terraform.tfstate"
  }
}

variable "user" {
  type    = string
  default = "pd"
}

variable "nuc_ip" {
  type    = string
  default = "192.168.0.110"
}

variable "ssh_private_key" {
  type = string
}

# We connect exactly once as root to create my personal user
# that everything else will run as.
module "bootstrap" {
  source = "./exec"
  user   = "root"
  host   = var.nuc_ip
  key    = var.ssh_private_key

  commands = [
    "id pd >/dev/null || useradd -G adm,wheel,audio,optical,video --create-home pd",
    "mkdir -p /home/pd/.ssh && chmod 700 /home/pd/.ssh",
    "[[ ! -f /home/pd/.ssh/authorized_keys ]] && cp /root/.authorized_keys /home/pd/.ssh/authorized_keys",
    "chown -R pd:pd /home/pd/.ssh",
    "chmod 600 /home/pd/.ssh/authorized_keys",
  ]
}

# So I can freely mix core vs AUR packages, get paru in place
# before anything else
module "paru" {
  depends_on = [module.bootstrap]

  source = "./exec"
  user   = var.user
  host   = var.nuc_ip
  key    = var.ssh_private_key

  commands = [
    "sudo pacman -Sy --noconfirm --needed base-devel git",
    "mkdir -p ~/vendor",
    "[[ -d ~/vendor/paru ]] || (cd ~/vendor && git clone https://aur.archlinux.org/paru.git)",
    "which paru >/dev/null || (cd ~/vendor/paru && makepkg --noconfirm -si && paru --gendb)",
  ]
}

module "base" {
  depends_on = [module.paru]

  source = "./pkgs"
  user   = var.user
  host   = var.nuc_ip
  key    = var.ssh_private_key

  packages = [
    "curl",
    "emacs",
    "jq",
    "mandoc",
    "net-tools",
    "rsync",
    "tree",
    "zsh",
  ]

  commands = [
    "sudo chsh -s /usr/bin/zsh pd"
  ]
}
