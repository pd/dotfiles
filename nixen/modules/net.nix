# home 192.168.1.0/24,  gw .254
# wg   10.100.100.0/24, gw .1
#
# computers:
#   do:    .1, wan only
#   desk:  .10
#   span:  .11
#   htpc:  .12
#   pi:    .13
#   air:   .14
#
# phones:
#   pd:    .50
#   erin:  .51
#
# appliances:
#   nas:   .100
#   tv:    .101
#   hera:  .120
{
  nets = {
    wg0 = {
      server = "donix";
      endpoint = "donix.krh.me:51820";
      cidr = "10.100.100.0/24";
    };
  };

  hosts = {
    donix = {
      wg0 = {
        ip = "10.100.100.1";
        publicKey = "WZgf+DC6SBQeatqOgpC2j6tvIu5VxKi/WgdbIU/m7wg=";
      };
    };

    desk = {
      lan = {
        ip = "192.168.1.10";
      };

      wg0 = {
        ip = "10.100.100.10";
        publicKey = "CA7HMcgdgxixmpikVA15Bxydi7pFriBbR3A3W7mE2BU=";
      };
    };

    span = {
      lan = {
        ip = "192.168.1.11";
      };

      wg0 = {
        ip = "10.100.100.11";
        publicKey = "ifiRznc81W75NIIq53+8BH6uJ3iJODbXdAk+ND1J+3U=";
      };
    };

    htpc = {
      cnames = [
        "kodi"
        "torrent"
      ];

      lan = {
        ip = "192.168.1.12";
      };

      wg0 = {
        ip = "10.100.100.12";
        publicKey = "gd9CT0OuYZP/kaFTP26uiIkXa+D/uvkAL0Z98X68Jx4=";
      };
    };

    pi = {
      lan = {
        ip = "192.168.1.13";
      };

      # wg0 = {
      #   ip = "10.100.100.13";
      # };
    };

    nas = {
      lan = {
        ip = "192.168.1.100";
      };
    };

    tv = {
      lan = {
        ip = "192.168.1.101";
      };
    };

    hera = {
      lan = {
        ip = "192.168.1.120";
      };
    };
  };
}
