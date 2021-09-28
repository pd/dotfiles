terraform {
  required_providers {
    digitalocean = {
      source  = "digitalocean/digitalocean"
      version = "~> 2.0"
    }

    http = {
      source = "hashicorp/http"
    }
  }
}

data "http" "ifconfig" {
  url = "http://ifconfig.co"
}

resource "digitalocean_record" "home" {
  domain = "krh.me"
  name   = "home"
  ttl    = 300
  type   = "A"
  value  = trimspace(data.http.ifconfig.body)
}
