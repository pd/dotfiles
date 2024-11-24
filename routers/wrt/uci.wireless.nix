{ }:
let
  device = {
    type = "mac80211";
    htmode = "HE40";
    cell_density = false;
  };

  iface = {
    network = "lan";
    mode = "ap";
    ssid = "bazqux";
    encryption = "psk2";
    key._secret = "wifi_password";

    # roaming
    wds = true;
    ieee80211r = true;
    mobility_domain = "dead";
    ft_over_ds = false;
    ft_psk_generate_local = true;
  };
in
{
  wifi-device.radio0 = device // {
    channel = 1;
    band = "2g";
    path = "platform/soc/18000000.wifi";
  };

  wifi-iface.default_radio0 = iface // {
    device = "radio0";
  };

  wifi-device.radio1 = device // {
    channel = "auto";
    band = "5g";
    path = "platform/soc/18000000.wifi+1";
  };

  wifi-iface.default_radio1 = iface // {
    device = "radio1";
  };
}
