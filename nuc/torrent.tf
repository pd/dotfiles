module "transmission" {
  depends_on = [module.base]

  source = "./pkgs"
  user   = var.user
  host   = var.nuc_ip
  key    = var.ssh_private_key

  packages = [
    "transmission-cli",
  ]

  commands = [
    "sudo mkdir -p /srv/transmission/{done,dots,incoming,wip}",
    "sudo chown -R transmission:transmission /srv/transmission",
    "sudo chmod -R g+w /srv/transmission",
    "sudo usermod -a -G transmission ${var.user}",
  ]
}

locals {
  transmission_settings = jsonencode({
    download-dir   = "/srv/transmission/done",
    incomplete-dir = "/srv/transmission/wip"

    rpc-whitelist-enabled = true
    rpc-whitelist         = "127.0.0.1,::1,192.168.*.*"

    watch-dir-enabled = true
    watch-dir         = "/srv/transmission/incoming"

    script-torrent-done-enabled  = true
    script-torrent-done-filename = "/srv/transmission/stage-for-kodi.sh"
  })
}

module "transmission_config" {
  depends_on = [module.transmission]

  source = "./upload"
  user   = var.user
  host   = var.nuc_ip
  key    = var.ssh_private_key

  files = {
    "${path.module}/scripts/stage-for-kodi.sh" = {
      dest  = "/srv/transmission/stage-for-kodi.sh"
      chown = "transmission:transmission"
      chmod = "774"
    }
  }

  post_commands = [
    "sudo cat /var/lib/transmission/.config/transmission-daemon/settings.json | jq '. + ${local.transmission_settings}' > /tmp/transmission-settings.json",
    "sudo mv /tmp/transmission-settings.json /var/lib/transmission/.config/transmission-daemon/settings.json",
    "sudo chown transmission:transmission /var/lib/transmission/.config/transmission-daemon/settings.json",
    "sudo chmod 664 /var/lib/transmission/.config/transmission-daemon/settings.json",
    "sudo systemctl enable transmission.service",
    "sudo systemctl start transmission.service",
  ]
}
