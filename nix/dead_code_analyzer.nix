{
  fetchFromGitHub,
  buildDunePackage,
  cppo,
}:

buildDunePackage (finalAttrs: {
  pname = "dead_code_analyzer";
  version = "1.3.0";

  src = fetchFromGitHub {
    owner = "LexiFi";
    repo = finalAttrs.pname;
    rev = finalAttrs.version;
    sha256 = "sha256-LVZmUzN7p9HvZkWAAluimo46fB0Uj+fzjfHlIoaitJ8=";
  };

  nativeBuildInputs = [ cppo ];
})
