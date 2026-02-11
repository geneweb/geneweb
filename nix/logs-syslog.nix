{ buildDunePackage, logs, syslog-message, ptime
}:

buildDunePackage (finalAttrs: {
  pname = "logs-syslog";
  version = "0.5.0";
  duneVersion = "3";

  src = fetchTarball {
    url = "https://github.com/hannesm/logs-syslog/archive/refs/tags/v${finalAttrs.version}.tar.gz";
    sha256 = "sha256:1p60nsqfsg1igxv8l0bmwsrw3zvvv9d15gi5sf7hcj4lfdcf711s";
  };

  buildInputs = [ logs ptime ];
  propagatedBuildInputs = [ syslog-message ];
})
