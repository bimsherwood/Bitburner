
## Thief

Runs a basic grow/weaken/hack loop

thief.js targetServer

## InstallThief

Nukes a host server if need be, (re-)installs a Thief on a given server, and
starts the Thief

install-thief.js install hostServer targetServer
install-thief.js uninstall hostServer

## Thiefnet

Raises or tears down the configured Thief network

thiefnet.js update
thiefnet.js uninstall

## TargetScanner

Crawls the internet. Limit the scan length. Identifies servers that match
specific conditions, such as open port requirements.

Keep the results in a global database.

Report mode will print the targets to the log.

Add a programatic interface to the scan results so that a script can nurse a
botnet.