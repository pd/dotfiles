locals {
  records = {
    "internetsfamo.us" = [
      { type = "NS", name = "@", value = "ns1.digitalocean.com." },
      { type = "NS", name = "@", value = "ns2.digitalocean.com." },
      { type = "NS", name = "@", value = "ns3.digitalocean.com." },
      { type = "A", name = "@", value = digitalocean_droplet.donix.ipv4_address },
      { type = "A", name = "www", value = digitalocean_droplet.donix.ipv4_address },
      { type = "AAAA", name = "@", value = digitalocean_droplet.donix.ipv6_address },
      { type = "AAAA", name = "www", value = digitalocean_droplet.donix.ipv6_address },
      { type = "CAA", name = "@", value = "letsencrypt.org." },
    ]

    "krh.me" = [
      { type = "NS", name = "@", value = "ns1.digitalocean.com." },
      { type = "NS", name = "@", value = "ns2.digitalocean.com." },
      { type = "NS", name = "@", value = "ns3.digitalocean.com." },

      { type = "A", name = "do", value = digitalocean_droplet.do.ipv4_address },
      { type = "AAAA", name = "do", value = digitalocean_droplet.do.ipv6_address },
      { type = "A", name = "donix", value = digitalocean_droplet.donix.ipv4_address },
      { type = "AAAA", name = "donix", value = digitalocean_droplet.donix.ipv6_address },

      { type = "CNAME", name = "mail", value = "ghs.googlehosted.com." },
      { type = "MX", name = "@", value = "aspmx.l.google.com.", priority = 1 },
      { type = "MX", name = "@", value = "alt1.aspmx.l.google.com.", priority = 5 },
      { type = "MX", name = "@", value = "alt2.aspmx.l.google.com.", priority = 5 },
      { type = "MX", name = "@", value = "alt3.aspmx.l.google.com.", priority = 10 },
      { type = "MX", name = "@", value = "alt4.aspmx.l.google.com.", priority = 10 },
    ]
  }
}

resource "digitalocean_domain" "domains" {
  for_each = toset(keys(local.records))
  name     = each.value
}

resource "digitalocean_record" "dns" {
  for_each = merge([
    for domain, records in local.records : {
      for r in records :
      "${domain}/${r.type}/${r.name}/${r.value}" => merge(r, { domain = domain })
    }
  ]...)

  domain = each.value.domain
  type   = each.value.type
  name   = each.value.name
  value  = each.value.value

  tag   = each.value.type == "CAA" ? "issue" : null
  flags = each.value.type == "CAA" ? 0 : null

  priority = lookup(each.value, "priority", null)
}
