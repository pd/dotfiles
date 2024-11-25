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

data "digitalocean_ssh_keys" "ssh" {
}

resource "digitalocean_droplet" "do" {
  name   = "do.krh.me"
  region = "nyc3"
  size   = "s-1vcpu-1gb"
  image  = "53893572"
  ipv6   = true
}

data "digitalocean_image" "ubuntu" {
  slug = "ubuntu-22-04-x64"
}

resource "digitalocean_droplet" "donix" {
  name     = "donix.krh.me"
  region   = "nyc3"
  size     = "s-1vcpu-1gb"
  image    = data.digitalocean_image.ubuntu.id
  ipv6     = true
  ssh_keys = data.digitalocean_ssh_keys.ssh.ssh_keys[*].id

  # https://github.com/elitak/nixos-infect/blob/5ef3f953d32ab92405b280615718e0b80da2ebe6/README.md#digital-ocean
  # starting from ubuntu 22.04 because of:
  # https://github.com/elitak/nixos-infect/issues/199
  user_data = <<EOF
#cloud-config
write_files:
- path: /etc/nixos/host.nix
  permissions: '0644'
  content: |
    {pkgs, ...}:
    { environment.systemPackages = [ pkgs.vim ]; }
runcmd:
- curl https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect | PROVIDER=digitalocean NIXOS_IMPORT=./host.nix NIX_CHANNEL=nixos-24.05 bash 2>&1 | tee /tmp/infect.log
EOF
}

output "do" {
  value = {
    name = digitalocean_droplet.do.name
    ip4  = digitalocean_droplet.do.ipv4_address
    ip6  = digitalocean_droplet.do.ipv6_address
  }
}

output "donix" {
  value = {
    name = digitalocean_droplet.donix.name
    ip4  = digitalocean_droplet.donix.ipv4_address
    ip6  = digitalocean_droplet.donix.ipv6_address
  }
}
