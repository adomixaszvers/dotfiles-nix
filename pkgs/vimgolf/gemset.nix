{
  highline = {
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "01ib7jp85xjc4gh4jg0wyzllm46hwv8p0w1m4c75pbgi41fps50y";
      type = "gem";
    };
    version = "1.7.10";
  };
  json_pure = {
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "12yf9fmhr4c2jm3xl20vf1qyz5i63vc8a6ngz9j0f86nqwhmi2as";
      type = "gem";
    };
    version = "2.1.0";
  };
  thor = {
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0nmqpyj642sk4g16nkbq6pj856adpv91lp4krwhqkh2iw63aszdl";
      type = "gem";
    };
    version = "0.20.0";
  };
  vimgolf = {
    dependencies = [ "highline" "json_pure" "thor" ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1qp6fdycxb65ivi3szb9d9xh2cvv36bpkcw3k0a2ggg27l9afbq4";
      type = "gem";
    };
    version = "0.4.8";
  };
}
