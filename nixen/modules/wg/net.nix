{
  hostname = "donix.krh.me";
  port = 51820;
  cidr = "10.100.100.0/24";

  server = {
    publicKey = "WZgf+DC6SBQeatqOgpC2j6tvIu5VxKi/WgdbIU/m7wg=";
    ip = "10.100.100.1/32";
  };

  clients = {
    desk = {
      publicKey = "CA7HMcgdgxixmpikVA15Bxydi7pFriBbR3A3W7mE2BU=";
      ip = "10.100.100.10/32";
    };

    span = {
      publicKey = "ifiRznc81W75NIIq53+8BH6uJ3iJODbXdAk+ND1J+3U=";
      ip = "10.100.100.11/32";
    };

    htpc = {
      publicKey = "gd9CT0OuYZP/kaFTP26uiIkXa+D/uvkAL0Z98X68Jx4=";
      ip = "10.100.100.12/32";
    };
  };
}
