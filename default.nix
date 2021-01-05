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
  shells = {
    ghc = ["ldgv"];
    ghcjs = ["ldgv"];
  };
})
