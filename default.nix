let
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  reflex-platform = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).reflex-platform;
in (import reflex-platform {}).project ({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;
  packages = {
    ldgv = ./.;
  };
  overrides = self: super: {
    validation-selective = self.callHackage "validation-selective" "0.1.0.0" {};
  };
  shells = {
    ghc = ["ldgv"];
    ghcjs = ["ldgv"];
  };
})
