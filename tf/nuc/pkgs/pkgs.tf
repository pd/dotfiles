variable "user" {
  type = string
}

variable "host" {
  type = string
}

variable "key" {
  type = string
}

variable "packages" {
  type = list(string)
}

variable "commands" {
  type    = list(string)
  default = []
}

module "install" {
  source = "../exec"
  user   = var.user
  host   = var.host
  key    = var.key

  commands = [
    "paru -S --noconfirm --needed ${join(" ", var.packages)}"
  ]
}

module "postinstall" {
  depends_on = [module.install]

  count    = length(var.commands) > 0 ? 1 : 0
  source   = "../exec"
  user     = var.user
  host     = var.host
  key      = var.key
  commands = var.commands
}
