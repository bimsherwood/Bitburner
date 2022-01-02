
## Update find-targets

Inject the server list instead of using the Crawler directly.

Inject query options, such as root access status and money availability.

Include the host finder in this library, along with the target finder. Add an
option to pick by power.

Add options for the sort order to this module.

Remove the unused main function.

## Update reach.js 

Dependency-inject a Cache.

Split out the scheduler into its own script. Dependency-inject a scheduler.

Parameterise the max target count.

New functions:
- getAccessibleServers()
- rootServers(hostnames)
- deploy(host, target)

## Change the scheduler

Concentrate resources to the top few best targets.

Rank targets by (max money / min security), or something of the like, to
approximately compare yeild potentials.

Parameterise the target count until a good value can be automatically chosen.
The optimal number may not be 1, as this server could completely dry up and
refill every cycle, wasting some calls to hack().

## Update VPS manager

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