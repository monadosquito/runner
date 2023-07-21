# Create tracks

- Define top-level bindings of tracks one of which is `defaultTrack` in the
  `Core.Script.Track.DefaultTrack` module by connecting the [`Core.Track.Track`
  smart constructors](#table-1) with the `*>`, `<*`, or `>>` operators or the
  `do` notation.
- Add not `defaultTrack` ones into the `Core.Script.Track.tracks` list as
  tuples of their names and themselves.

## Hints

- Create tracks under the `Core.Script.Track` module.

# Configure

Set some [options](#table-2) to appropriate [values](#table-2) in a
configuration file or as command arguments.

## Notes

- Options passed as arguments to the command overwrite ones read from a
  configuration file.
- If a configuration file whose particular key is set to a value is present,
  then the key is used from it, otherwise a [default value](#table-2) is used.

---

## Table 1

track smart constructor effects

|Smart constructor|Effect                                     |
|-----------------|-------------------------------------------|
|`part`           |A given number of track lines is generated.|

## Table 2

options

|Option                 |Default value         |Command option                 |Configuration key   |Description                                                    |
|-----------------------|----------------------|-------------------------------|--------------------|---------------------------------------------------------------|
|Configuration file path|`"configuration.json"`|`--configuration` (`-c`)       |-                   |a configuration file path                                      |
|Track name             |`"default"`           |`--track-name` (`-t`)          |`trackName`         |a name of a track to interpret                                 |
|Track piece capacity   |`10`                  |`--track-piece-capacity` (`-p`)|`trackPieceCapacity`|a number of track lines rendered at a time                     |
|Track width            |`5`                   |`--track-width` (`-w`)         |`trackWidth`        |a number of the `Core.Track.Cell` values in a single track line|
