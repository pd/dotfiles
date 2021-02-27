variable "user" {
  type = string
}

variable "host" {
  type = string
}

variable "key" {
  type = string
}

variable "pre_commands" {
  type    = list(string)
  default = []
}

variable "files" {
  type = map(object({
    dest  = string
    chown = string
    chmod = string
  }))
}

variable "post_commands" {
  type    = list(string)
  default = []
}

module "pre" {
  source = "../exec"
  user   = var.user
  host   = var.host
  key    = var.key

  commands = var.pre_commands
}

resource "null_resource" "upload" {
  for_each   = var.files
  depends_on = [module.pre]

  connection {
    type        = "ssh"
    user        = var.user
    host        = var.host
    private_key = file(var.key)
  }

  provisioner "file" {
    source      = each.key
    destination = each.value.dest
  }
}

module "post" {
  depends_on = [null_resource.upload.0]

  source = "../exec"
  user   = var.user
  host   = var.host
  key    = var.key

  commands = concat(flatten([
    for src, file in var.files : [
      "sudo chown ${file.chown} ${file.dest}",
      "sudo chmod ${file.chmod} ${file.dest}",
    ]
  ]), var.post_commands)
}
