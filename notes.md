

## Invert control of VPS and Reach

This will allow the reach code to access a consitent cache.

Currently Inside Reach:
  - Schedule cache
  - Schedule builder

Currenlty Outside Reach:
  - VPS upgrades
  - Server finder
  - Crawler
  - Installer
  - Rooter
  

## Fix the fukn buy price bug

## Better Thief

Run concurrent scripts for:
  - weaken (constantly)
  - grow (keeping track of a target weakness)
  - hack (keep track of a target weekness and size)

Make the scripts run on only a few threads each so that there is a smoother
growth and hack rate, which will prevent wasted effort on servers that have
tighter limits.

## Update VPS manager

Make the VPS manager automatically call out to reach.js to commission a rebuilt
server.

Synchronise server decommissions with completion of the thief script somehow.

## Contract solver

Automatic contract solver

## Telemetry collector

Collect useful stats? Decide what might be useful. We don't get timing
information by the looks.

## Targeted transmission between servers

Somehow multiplex a port so that messages can be sent over the port to
particular servers.

Read all messages and then immediately re-write messages not intended for the
reader? With exponential backoff to help the intended recipient to get the
message. Limit the backoff period though.

Give the messages a poison counter, which is larger than the total number of
instances possibly subscribed to the channel. Poison-queue messages that are
read over and over by the one same server, even when the reader is backing off.

Maybe abuse the exposure of the message array.