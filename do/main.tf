terraform {
  backend "s3" {
    bucket                      = "pdtfstate"
    key                         = "do.json"
    skip_credentials_validation = true
    skip_metadata_api_check     = true
    skip_region_validation      = true
    skip_requesting_account_id  = true
    skip_s3_checksum            = true
    region                      = "us-east-005"

    endpoints = {
      s3 = "https://s3.us-east-005.backblazeb2.com"
    }
  }

  required_providers {
    b2           = { source = "Backblaze/b2" }
    digitalocean = { source = "digitalocean/digitalocean" }
  }
}

resource "b2_bucket" "tfstate" {
  bucket_name = "pdtfstate"
  bucket_type = "allPrivate"

  default_server_side_encryption {
    algorithm = "AES256"
    mode      = "SSE-B2"
  }
}

resource "digitalocean_droplet" "do" {
  name   = "do.krh.me"
  region = "nyc3"
  size   = "s-1vcpu-1gb"
  image  = "53893572"
}

data "digitalocean_image" "donix" {
  name   = "donix"
  source = "user"
}

data "digitalocean_ssh_keys" "ssh" {
}

resource "digitalocean_droplet" "donix" {
  name     = "donix.krh.me"
  region   = "nyc3"
  size     = "s-1vcpu-1gb"
  image    = data.digitalocean_image.donix.id
  ssh_keys = data.digitalocean_ssh_keys.ssh.ssh_keys[*].id
}

output "do" {
  value = {
    name = digitalocean_droplet.do.name
    ip   = digitalocean_droplet.do.ipv4_address
  }
}

output "donix" {
  value = {
    name = digitalocean_droplet.donix.name
    ip   = digitalocean_droplet.donix.ipv4_address
  }
}
