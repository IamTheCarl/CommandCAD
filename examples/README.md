# Examples

These are examples of what can currently be done with Command CAD.
Please note that most files contain multiple examples you can run.

## Basic Math

Examples of mathmatical operations, and some control flow demos. Really demonstraights the CLI serialization and deserialization features.

### Addition

Performs addition of two numbers.

```
cargo run -- run basic_math.ccm addition -- 2 3
5.0
```

### Recursive Addition

A recursive function that will count down from the specified number of cycles, and add the remaning number of cycles to the total sum.
For example, if I were to input 3, the output would be the sum of 3 + 2 + 1, resulting in 6.

```
cargo run -- run basic_math.ccm recursive_addition -- 3
6.0
```

### Convert to Meters

Converts lengths to meters.

```
cargo run -- run basic_math.ccm convert_to_meters -- 3yd
2.7432
```

### Default value

Functions can have default values provided, but you can also override the default value from the CLI.

```
cargo run -- run basic_math.ccm default_value -- default
42.0
```

```
cargo run -- run basic_math.ccm default_value -- 3yd
2.7432
```

## Bearing

The file `bearing.ccm` is actually pseudo code that was used to drive the design of Command CAD. Eventually it will be a working example but for the moment it is not.

## Fornjot Demos

Copies the demos from the Fornjot repository (Fornjot being the CAD kernel this project is built on)
Currently only one of the demos are implemented.

### Cuboid

Create a box structure by providing its measurements.
```
cargo run -- form examples/fornjot_demos.ccm cuboid ~/cuboid.stl -- 1cm 1cm 1cm
```

## Sketches

Demonstrates the creation of 2D objects.
Please note that export of 2D objects is not yet supported, so the output of these is not very exciting.

### Circle

Creates a circle with the provided diameter.

```
cargo run -- form examples/sketches.ccm circle -- 1cm
```

### Square

Creates a square with the provided side length.

```
cargo run -- form examples/sketches.ccm square -- 1cm
```

### Square with Circle

Creates a sketch with both a square and a circle with the provided dimensions.

```
cargo run -- form examples/sketches.ccm squre_with_circle -- 1cm 1cm
```
