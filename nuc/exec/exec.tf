variable "user" {
  type = string
}

variable "host" {
  type = string
}

variable "key" {
  type = string
}

variable "commands" {
  type = list(string)
}

resource "null_resource" "exec" {
  triggers = {
    script = join("\n", var.commands)
  }

  connection {
    type        = "ssh"
    user        = var.user
    host        = var.host
    private_key = file(var.key)
  }

  provisioner "remote-exec" {
    inline = var.commands
  }
}
