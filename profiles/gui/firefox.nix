{
  programs.firefox = {
    enable = true;
    policies = {
      DisablePocket = true;
      DisableTelemetry = true;
      PasswordManagerEnabled = false;
      Preferences = {
        "browser.urlbar.suggest.remotetab" = {
          Value = false;
          Status = "user";
        };
      };
      SearchEngines = {
        Add = [
          {
            Name = "NixOS Package Search";
            Alias = "nixp";
            URLTemplate = "https://search.nixos.org/packages?query={searchTerms}";
            Method = "GET"; # "POST"
            IconURL = "https://search.nixos.org/favicon.png";
          }
          {
            Name = "NixOS Option Search";
            Alias = "nixo";
            URLTemplate = "https://search.nixos.org/options?query={searchTerms}";
            Method = "GET"; # "POST"
            IconURL = "https://search.nixos.org/favicon.png";
          }
          {
            Name = "Searx English";
            Alias = "sx";
            URLTemplate = "https://searx.rpi4.beastade.top/search?q={searchTerms}&language=en&&safesearch=2&";
            Method = "GET";
            IconURL = "https://searx.rpi4.beastade.top/favicon.ico";
          }
          {
            Name = "Searx Lithuanian";
            Alias = "slt";
            URLTemplate = "https://searx.rpi4.beastade.top/search?q={searchTerms}&language=lt&&safesearch=2&";
            Method = "GET";
            IconURL = "https://searx.rpi4.beastade.top/favicon.ico";
          }
        ];
        Default = "Searx English";
      };
    };
  };
}
