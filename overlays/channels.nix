self: super: {
  channels = {
    all-hies = import <all-hies> { };
    nixos-unstable = import <nixos-unstable> { };
  };
}
