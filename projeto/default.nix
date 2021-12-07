{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    config.android_sdk.accept_license = true;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
project ./. ({ hackGet, ... }: {
  android.applicationId = "systems.fatecrl.ads.haskell.livrariabrasileira";
  android.displayName = "Livraria Brasileira";
  ios.bundleIdentifier = "systems.fatecrl.ads.haskell.livrariabrasileira";
  ios.bundleName = "Livraria Brasileira";
  packages = {
  psql = hackGet ./dep/psql;
  };
})
