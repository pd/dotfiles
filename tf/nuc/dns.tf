module "dns" {
  depends_on = [module.base]

  source = "./pkgs"
  user   = var.user
  host   = var.nuc_ip
  key    = var.ssh_private_key

  packages = [
    "unbound",
  ]

  commands = [
    "sudo systemctl enable unbound.service",
    "sudo systemctl start unbound.service",
  ]
}

module "dns_config" {
  depends_on = [module.dns]

  source = "./upload"
  user   = "root"
  host   = var.nuc_ip
  key    = var.ssh_private_key

  files = {
    "${path.module}/files/unbound.conf" = {
      dest  = "/etc/unbound/unbound.conf"
      chown = "root:root"
      chmod = "664"
    }
  }

  post_commands = [
    "systemctl reload unbound"
  ]
}
