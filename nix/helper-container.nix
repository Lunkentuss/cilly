{pkgs, extraPaths}:
with pkgs;
dockerTools.buildImage {
  name = "cilly-helper";
  config = {
    Cmd = [ "${bashInteractive}/bin/bash" ];
  };
  copyToRoot = buildEnv {
    name = "cilly-helper-root";
    pathsToLink = [ "/bin" ];
    paths = [
      bashInteractive
      coreutils
      git
      gnused
    ] ++ extraPaths;
  };

  runAsRoot=''
  mkdir -p /tmp
  chmod 1777 /tmp
  cat << EOF > /.gitconfig
  [safe]
    directory = *
  EOF
  '';
}
