{
  pkgs_otp27 = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
  }) { };
  name = "redmine_tracker";
  pname = "redmine_tracker";
  version = "0.0.1";
}
