{
  highline = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0yclf57n2j3cw8144ania99h1zinf8q3f5zrhqa754j6gl95rp9d";
      type = "gem";
    };
    version = "2.0.3";
  };
  json_pure = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0kd19s22clqfbjdj6ni1sv7bwmrfdqbcdaa52vi1djmcpnib48wx";
      type = "gem";
    };
    version = "2.3.0";
  };
  thor = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1xbhkmyhlxwzshaqa7swy2bx6vd64mm0wrr8g3jywvxy7hg0cwkm";
      type = "gem";
    };
    version = "1.0.1";
  };
  vimgolf = {
    dependencies = ["highline" "json_pure" "thor"];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1qp6fdycxb65ivi3szb9d9xh2cvv36bpkcw3k0a2ggg27l9afbq4";
      type = "gem";
    };
    version = "0.4.8";
  };
}