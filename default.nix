{
  lib,
  melpaBuild,
}:
melpaBuild {
  pname = "nix-converter";
  version = "1.0.0";

  src = ./.;

  meta = {
    homepage = "https://github.com/theobori/nix-converter.el";
    description = "A Emacs conversion tool based on nix-converter";
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ theobori ];
  };
}
