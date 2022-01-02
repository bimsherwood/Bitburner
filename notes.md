
## Update reach.js

Implement a new option that only schedules unscheduled hosts, and does not
disrupt other hosts.

Pass crawler to the discover() function, as it is not needed for the
single-server update function.

## Automatic VPS management

Fix the shitty mess of code.

Functions:
- onVpsCreate() // install thief using the new reach.js function
- class ManagedVPS()
- size()
- upgrade()

## Update find-targets

Inject the server list instead of using the Crawler directly.

Inject query options, such as root access status and money availability.

Remove the sorting from this module.

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