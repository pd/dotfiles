{ lib, pd, ... }:
let
  homeHosts = lib.map (h: "${h}.home") (
    [
      "wrt"
      "rpt"
    ]
    ++ pd.net.hosts.htpc.cnames
  );

  amo =
    slug: extra:
    {
      installation_mode = "normal_installed";
      install_url = "https://addons.mozilla.org/firefox/downloads/latest/${slug}/latest.xpi";
    }
    // extra;
in
{
  programs.firefox.policies = {
    # get ff to stop showing "this connection is not secure ..."
    # warnings DIRECTLY ATOP MY PASSWORD MANAGER'S FILL PASSWORD UI
    Preferences."dom.securecontext.allowlist".Value = lib.concatStringsSep "," homeHosts;

    DisableTelemetry = true;
    DisableFirefoxStudies = true;
    DisablePocket = true;

    FirefoxHome = {
      Pocket = false;
      Stories = false;
      SponsoredPocket = false;
      SponsoredStories = false;
      SponsoredTopSites = false;
    };

    FirefoxSuggest = {
      SponsoredSuggestions = false;
      ImproveSuggest = false;
    };

    UserMessaging = {
      WhatsNew = false;
      ExtensionRecommendations = false;
      FeatureRecommendations = false;
      UrlbarInterventions = false;
      MoreFromMozilla = false;
      SkipOnboarding = true;
    };

    AIControls = {
      Default.Value = "blocked";
      Translations.Value = "available";
    };

    ExtensionSettings = {
      "uBlock0@raymondhill.net" = amo "ublock-origin" { private_browsing = true; };
      "{446900e4-71c2-419f-a6a7-df9c091e268b}" = amo "bitwarden-password-manager" {
        default_area = "navbar";
      };
      "{e6e36c9a-8323-446c-b720-a176017e38ff}" = amo "torrent-control" { };
      "{d07ccf11-c0cd-4938-a265-2a4d6ad01189}" = amo "view-page-archive" { };
    };
  };
}
