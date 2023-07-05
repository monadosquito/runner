# Create tracks

Define top-level bindings of tracks by connecting the [`Core.Track.Track` smart
constructors](#table-1) with the `*>`, `<*`, or `>>` operators or the `do`
notation.

## Hints

- Create tracks under the `Core.Script.Track` module.

---

## Table 1

track smart constructor effects

|Smart constructor|Effect                                     |
|-----------------|-------------------------------------------|
|`part`           |A given number of track lines is generated.|
