
## Planit: Serialisable Expert Systems

Planit is an expert system designed to help you build expert systems-- for graphics, live-tinkering, serialisation, and distribution.

To make Planit your own, you can call `save_source(YourDir)` to produce a backup, and then you can start hacking as much as you want! Assert and retract dynamically, then again call `save_source` when you're done. You can alter the save predicate to add new modules.

You can also make use of `save_bin(YourDir)`, which I recommend.

Currently there is no versioning system, personally I use git, maybe there'll be one in the future.

Be aware that the documented system comments won't be preserved! So if you want comments, find your own way around that. My comments exist to document the core system and will eventually be removed in favour of database documentation.

## TODO

Figure out dynamic recognisers (use global objects?)

Colour-coding on calendar

Sort out the management of overlaps (Automatic columns?)

See if we can figure out serialising PCE objects

Code analysis tools

Improve page editor (It needs it)
