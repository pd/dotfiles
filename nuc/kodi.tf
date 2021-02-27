module "kodi" {
  depends_on = [module.base]

  source = "./pkgs"
  user   = var.user
  host   = var.nuc_ip
  key    = var.ssh_private_key

  packages = [
    "kodi-x11",
    "kodi-standalone-service",
    "kodi-eventclients",
    "pulseaudio",
    "alsa-firmware",
  ]

  commands = [
    "sudo mkdir -p /srv/kodi/{TV,Movies}",
    "sudo chown -R kodi:kodi /srv/kodi",
    "sudo chmod -R g+w /srv/kodi",
    "sudo usermod -a -G kodi ${var.user}",
    "sudo systemctl enable kodi-x11.service",
    "sudo systemctl start kodi-x11.service",
  ]
}

module "kodi_transmission" {
  depends_on = [module.kodi, module.transmission]

  source = "./exec"
  user   = var.user
  host   = var.nuc_ip
  key    = var.ssh_private_key

  commands = [
    "sudo usermod -a -G kodi transmission",
    "sudo usermod -a -G transmission kodi",
  ]
}
