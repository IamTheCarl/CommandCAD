# Command CAD

Command CAD is the product of my frustrations with the current state of CAD software and my curiosity of what would happen if OpenSCAD was given heavy type safety, functional programming paradigms, and fully declarative.

_Please note that Command CAD is in a highly experimental state, do not expect stability, even in the short term_

Command CAD is still highly experimental and therefore not well documented, but at least you can find examples in [the examples directory](./examples/README.md).

The following is an fictional but practical example of a model:
```
let
    nominal_angle_to_sun = 60deg;

    # 30watts per square meter multiplied by square meters results in watts.
    nominal_power_output = (main_body: std.scalar.Length) -> std.scalar.Power:
      let 
        # A length * by a length gives an area.
        solar_panel_size = main_body * 1m;
      in
        solar_panel_size * nominal_angle_to_sun::sin() * (30W/1 'm^2');

    # We need 3cm of cooling surface for each watt this produces
    cooling_fin_size = nominal_power_output(main_body = 15cm) / 1W * 3cm;
in
    std.mesh3d.cylinder(
        diameter = cooling_fin_size,
        height = 1cm,
        sectors = 360u,
        stacks = 1u
    )::to_stl(name = "cooling_plate")
```

## Current Features

The following features are currently available:
* Dimensional analysis
  * Dimensional type safety enforced at method calls
* Mesh based modeling
  * Note that there are plans to experiment with imperative modeling techniques


