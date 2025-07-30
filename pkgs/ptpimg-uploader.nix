{
  fetchFromGitHub,
  python3Packages,
  ...
}:
python3Packages.buildPythonApplication {
  pname = "ptpimg-uploader";
  version = "";

  src = fetchFromGitHub {
    owner = "theirix";
    repo = "ptpimg-uploader";
    rev = "aa0133506a2982fbd6bebbe6e31d7810b1433e73";
    hash = "sha256-Li8zwpssR1Y6TRkJGQwe5yAbZiZyDD5FCe6bkKJCfX8=";
  };

  dependencies = with python3Packages; [
    requests
  ];

  meta = {
    homepage = "https://github.com/theirix/ptpimg-uploader";
    description = "Upload images to ptpimg.";
  };
}
