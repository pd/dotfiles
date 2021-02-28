module "nginx" {
  depends_on = [module.base]

  source = "./pkgs"
  user   = var.user
  host   = var.nuc_ip
  key    = var.ssh_private_key

  packages = [
    "nginx",
  ]

  commands = [
    "sudo systemctl enable nginx.service",
    "sudo systemctl start nginx.service",
  ]
}

module "nginx_config" {
  depends_on = [module.nginx]

  source = "./upload"
  user   = "root"
  host   = var.nuc_ip
  key    = var.ssh_private_key

  files = {
    "${path.module}/files/nginx.conf" = {
      dest  = "/etc/nginx/nginx.conf"
      chown = "root:root"
      chmod = "644"
    }
  }

  post_commands = [
    "systemctl reload nginx"
  ]
}
