# Create tracks

- Define top-level bindings of tracks one of which is `defaultTrack` in the
  `Core.Script.Track.DefaultTrack` module by connecting the [`Core.Track.Track`
  smart constructors](#table-1) with the `*>`, `<*`, or `>>` operators or the
  `do` notation.
- Add not `defaultTrack` ones into the `Core.Script.Track.tracks` list as
  tuples of their names and themselves.

## Notes

- A current difficulty level set within a particular track overwrites starting
  one there.
- A difficulty level amount must be a floating number from `-1` to `1`,
  otherwise it is a nearest boundary.
- A difficulty level difference must be an integer having absolute value not
  greater than a track width, otherwise it is a nearest boundary.
- Predefined track lines lengths must not be empty and greater than a track
  width, otherwise they are ignored.

## Hints

- A probability should be a floating number from `0` to `1`.
- Create tracks under the `Core.Script.Track` module.
- In order that a track parts can be generated after a predefined one is
  appended, its last line must have at least one `Core.Track.Cell.TrailPart`
  value.
- In order that either track sequences are selected with the given
  probabilities, their sum must be `1`, otherwise they are equal.

# Configure

Set some [options](#table-2) to appropriate [values](#table-2) in a
configuration file or as command arguments.

## Notes

- A configured difficulty level is a starting one for all tracks.
- A difficulty level must be a natural number from `0` to a track width,
  otherwise it is a nearest boundary.
- Options passed as arguments to the command overwrite ones read from a
  configuration file.
- If a configuration file whose particular key is set to a value is present,
  then the key is used from it, otherwise a [default value](#table-2) is used.

---

## Table 1

track smart constructor effects

|Smart constructor                          |Effect                                                                                                                |
|-------------------------------------------|----------------------------------------------------------------------------------------------------------------------|
|`dynamicLengthFinitePart`                  |A number, selected within a given range, of track lines is generated.                                                 |
|`eitherSequenceWhere`                      |A following track sequence up to the same or `eitherSequenceEnd` smart constructor can be selected to be generated.   |
|`infiniteTailWhere`                        |A following track sequence is repeated infinitely.                                                                    |
|`leftPredefinedPart`                       |Given track lines offsetted from the left with a given `Core.Track.Cell` value are appended to a track.               |
|`middlePredefinedPart`                     |Given track lines offsetted from both sides with a given `Core.Track.Cell` value are appended to a track.             |
|`rightPredefinedPart`                      |Given track lines offsetted from the right with a given `Core.Track.Cell` value are appended to a track.              |
|`repeatedSequenceWhere`                    |A following track sequence is repeated a given number of times.                                                       |
|`sequenceEnd`                              |A previous sequence is generated.                                                                                     |
|`staticLengthFinitePart`                   |A given number of track lines is generated.                                                                           |
|`withAlteredDifficultyLevel`               |A given integer is added to a current difficulty level.                                                               |
|`withAmountAlteredDifficultyLevel`         |A given maximum difficulty level amount is added to a current difficulty level.                                       |
|`withDifficultyLevel`                      |A given track difficulty level is set as a current one.                                                               |
|`withDifficultyLevelAmount`                |A given maximum difficulty level amount is set as a current difficulty level.                                         |
|`withGradualDifficultyLevelAmountRiseSlope`|A following track sequence is generated altering a difficulty level by a its amount every given number of lines.      |
|`withGradualDifficultyLevelSlope`          |A following track sequence is generated altering a difficulty level by a given difference every given number of lines.|
|`withProbability`                          |An either sequence within which it is present is selected with a given probability.                                   |
|`withSteepDifficultyLevelSlope`            |A following sequence has a steep difficulty level.                                                                    |

# Table 2

options

<table>
    <tr>
        <th>Option</th>
        <th>Default value</th>
        <th>Command option</th>
        <th>Configuration key</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>Configuration file path</td>
        <td><code>"configuration.json"</code></td>
        <td><code>--configuration</code> (<code>-c</code>)</td>
        <td>-</td>
        <td>a configuration file path</td>
    </tr>
    <tr>
        <td>Track difficulty level</td>
        <td><code>2</code></td>
        <td><code>--track-difficulty-level</code> (<code>-d</code>)</td>
        <td><code>trackDifficultyLevel</code></td>
        <td>
            a number defining maximum numbers of
            <code>Core.Track.Cell.Pass</code> values in a single track line and
            track trails in a track
        </td>
    <tr>
        <td>Track name</td>
        <td><code>"default"</code></td>
        <td><code>--track-name</code> (<code>-t</code>)</td>
        <td><code>trackName</code></td>
        <td>a name of a track to interpret</td>
    </tr>
    </tr>
    <tr>
        <td>Track piece capacity</td>
        <td><code>10</code></td>
        <td><code>--track-piece-capacity</code> (<code>-p</code>)</td>
        <td><code>trackPieceCapacity</code></td>
        <td>a number of track lines rendered at a time</td>
    </tr>
    <tr>
        <td>Track start part length</td>
        <td><code>3</code></td>
        <td><code>--track-start-part-length</code> (<code>-p</code>)</td>
        <td><code>trackStartPartLength</code></td>
        <td>a number of track start lines contained in all start parts</td>
    </tr>
    <tr>
        <td>Track width</td>
        <td><code>5</code></td>
        <td><code>--track-width</code> (<code>-w</code>)</td>
        <td><code>trackWidth</code></td>
        <td>
            a number of the <code>Core.Track.Cell</code> values in a single
            track line
        </td>
    </tr>
</table>
