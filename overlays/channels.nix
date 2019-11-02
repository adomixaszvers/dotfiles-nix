self: super: {
  channels = {
    all-hies = import <all-hies> { };
    lorri = import <lorri> { };
    nixos-unstable = import <nixos-unstable> { };
  };
}
