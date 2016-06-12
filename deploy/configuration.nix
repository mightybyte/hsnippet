{ hostname, hsnippetConfig, versionInfo }:
{ config, pkgs, ... }:
{
  imports = [ ./virtualisation/amazon-image.nix ];
  services.journald.rateLimitBurst = 0;
  ec2.hvm = true;

  nix.trustedBinaryCaches = [ "https://ryantrinkle.com:5443" ];

  environment.systemPackages = with pkgs; [
    rxvt_unicode.terminfo
    git
    tmux
  ];

  networking = {
    hostName = hostname;
    firewall.allowedTCPPorts = [
      80
    ];
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql94;
    initialScript = builtins.toFile "init.sql" ''
      create user hsnippet with password 'hsnippet';
      create database hsnippet owner hsnippet;
      \connect hsnippet
      alter schema public owner to hsnippet;
    '';
  };

  systemd.services.hsnippet = {
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    restartIfChanged = true;
    script =
      let hsnippetPkg = import ../. {};
      in ''
      set -x
      export HOME="/var/lib/hsnippet"
      cd "$HOME"
      pwd
      ln -sft . "${hsnippetPkg}"/*
      echo "${versionInfo}" > version
      ln -sfT "${hsnippetConfig}" devel.cfg
      mkdir -p log
      mkdir -p userbuild/snippets
      cp -Rf --no-preserve=mode userbuild-template/* userbuild
      echo Got to here
      ./bin/main -p 80 >> log/stdout.log 2>> log/stderr.log
    '';
  };
  users.extraUsers.hsnippet = {
    group = "hsnippet";
    description = "HSnippet server user";
    home = "/var/lib/hsnippet";
    createHome = true;
    isSystemUser = true;
    openssh.authorizedKeys.keys =
      [ "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAgEA9bu1Gb7OBTczDPQ5ZiH5wHZotyDT5MYA5Z8sEpswCHEzeBN4wCt4KuLS2XoJ8vCORvCwyzVY6CQ+voxo/m/Shm92pPqxNtHdam5X+QEzsVHRzUVNJzVPbTF7sbAAQ6TrENVoeB0BpoOpOxzhDyDugeVDd9m+SC3HS2eyw4sGHIobdKpM/CVRMe3YjwTFYsDGb3QnmW506hlUN5+HKfKtjF1g8wkPHBRFAtjIWc/91ycRAKUEwPkRSbmHIdlCAqrk13yhTF190D+zkbbOfb2TXbwMqJFtRh5/aRaTyW/wEWJUhyjgQz2Ws03Q6gfWa1ary6k1pM0O4F1Pv3PPihTxYBdVKEI2O93cZE+DW1VQ7leh+TnLcQygmyDE4QkFKlQe9azF7Sss4P0MDW3L48T5y9bIyC2W2QE/NX8n7FbsywZQ1t/7+XJErN1BXLO0tZw8ka7NWPsLrkOox3D+I0dD61To1KcUlDf7i1bCMC8URyTf77wI2R0N43Gj9weLkncHF2ziv9Gje0mG6iXd8VSn/94Qepn50zc1wVoiziCb8jug032q9Jf9bwff6MTjDxkEhVH95u07R3v+TkmXGV56gcKSvsRzV6ddLMO7ox0rMLxyrDf5mfbcD83bDYqtBCvf5fd6qwivmcUEZNZZGPaiQmSilDVVPq0FQhEyYttNR2k= mightybyte@achilles" ];
  };
  users.extraGroups.hsnippet = {
  };
}
