{ mkDerivation, ansi-terminal, base, containers, directory
, filepath, process, stdenv
}:
mkDerivation {
  pname = "-snip";
  version = "0.1";
  src = ./local.tgz;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    ansi-terminal base containers directory filepath process
  ];
  license = stdenv.lib.licenses.unfree;
}
