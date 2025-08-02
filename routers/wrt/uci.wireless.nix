{ uci }:
with uci.wifi;
{
  wifi-device.radio0 = device "platform/soc/18000000.wifi" // bands."2g";
  wifi-device.radio1 = device "platform/soc/18000000.wifi+1" // bands."5g";

  wifi-iface.default_radio0 = ap "radio0" // wds;
  wifi-iface.default_radio1 = ap "radio1" // wds;
}
