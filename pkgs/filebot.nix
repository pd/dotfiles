{ pkgs, ... }:
with pkgs;
pkgs.filebot.overrideAttrs (
  final: prev: {
    # https://github.com/NixOS/nixpkgs/pull/429413
    installPhase = with pkgs; ''
      mkdir -p $out/opt $out/bin
      # Since FileBot has dependencies on relative paths between files, all required files are copied to the same location as is.
      cp -r filebot.sh lib/ jar/ $out/opt/
      cp -r filebot.sh jar/ $out/opt/
      # Copy lib based on platform and force filebot to use libmediainfo.so from nix
      local platformDir
      case "${stdenv.hostPlatform.system}" in
        "x86_64-linux")
          platformDir="Linux-x86_64"
          ;;
        "aarch64-linux")
          platformDir="Linux-aarch64"
          ;;
      esac
      if [ -n "$platformDir" ]; then
        mkdir -p "$out/opt/lib"
        cp -r "lib/$platformDir" "$out/opt/lib/"
        rm "$out/opt/lib/$platformDir/libmediainfo.so"
        ln -s "${libmediainfo}/lib/libmediainfo.so" "$out/opt/lib/$platformDir/"
      fi
      # Filebot writes to $APP_DATA, which fails due to read-only filesystem. Using current user .local directory instead.
      substituteInPlace $out/opt/filebot.sh \
        --replace 'APP_DATA="$FILEBOT_HOME/data/$(id -u)"' 'APP_DATA=''${XDG_DATA_HOME:-$HOME/.local/share}/filebot/data' \
        --replace '$FILEBOT_HOME/data/.license' '$APP_DATA/.license' \
        --replace '-jar "$FILEBOT_HOME/jar/filebot.jar"' '-Dcom.googlecode.lanterna.terminal.UnixTerminal.sttyCommand=${coreutils}/bin/stty -jar "$FILEBOT_HOME/jar/filebot.jar"'
      wrapProgram $out/opt/filebot.sh \
        --prefix PATH : ${lib.makeBinPath [ openjdk17 ]}
      # Expose the binary in bin to make runnable.
      ln -s $out/opt/filebot.sh $out/bin/filebot
    '';
  }
)
