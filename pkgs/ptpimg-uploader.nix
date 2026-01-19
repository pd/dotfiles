{
  fetchFromGitHub,
  python3Packages,
  ...
}:
python3Packages.buildPythonApplication {
  pname = "ptpimg-uploader";
  version = "0.13";

  src = fetchFromGitHub {
    owner = "theirix";
    repo = "ptpimg-uploader";
    rev = "83214249daba29f65953b89d9b3a0f11c2135c54";
    hash = "sha256-MM36F+zrXgOczRuCa9DCzcrcPvFu8ideQs9vlfR+OWk=";
  };

  pyproject = true;
  build-system = [ python3Packages.setuptools ];
  dependencies = [ python3Packages.requests ];

  meta = {
    homepage = "https://github.com/theirix/ptpimg-uploader";
    description = "Upload images to ptpimg.";
  };
}
