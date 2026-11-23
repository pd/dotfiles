{ lib, pd, ... }:
let
  homeHosts = lib.map (h: "${h}.home") (
    [
      "wrt"
      "rpt"
    ]
    ++ pd.net.hosts.htpc.cnames
  );
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
  };
}
