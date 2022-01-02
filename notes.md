
## Stock market script

Make a simple script to buy and sell stock in the long market order position.

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