# Command CAD

In my frustration with the state of CAD software as a whole, I lost my mind and wrote my own CAD program. Out of all of the ones I've used, the only ones that are really palatable are OpenSCAD and CAD Query. Command CAD takes most of its inspiration from these two. Command CAD has some critical differences though, like a stronger and convenient type safety for its scripting language and a more modular design.

_Please note that Command CAD is in a highly experimental state, do not expect stability, even in the short term_

Observe the following example:
```
main_body = 15;
```
This is valid syntax for both OpenSCAD and CAD Query.
The following critical information is missing:
 * What is the dimension? Is this a length? An area? It could even be Power or Time.
 * Even if we assume the dimension is length, what is the unit? Meters? Millimeters?

In the past I've worked around this issue with naming conventions:
```
main_body_length_cm = 15;
```
We now know the dimension and the unit, but there are problems: 
 * STL files are unaware of the units you use, and will just copy the numbers you input
   * If you were to feed this into a slicer and it assumed that 15 is millimeters (this is the default behavior of most slicers), the body's length would be an entire magnitude too short. You would have to rewrite the script to produce millimeters for all final products.
   * If you then use a different slicer that doesn't assume millimeters as the unit for STL files, you would have to again change the script to support that slicer.
 * This naming convention is tedious to use, and many developers simply won't use it

Command CAD's solution to this problem is to make dimension and unit a part of the language:
```
let main_body: Length = 15cm;
let nominal_angle_to_sun: Angle = 60deg;
let solar_panel_size: Area = main_body * 1m; // A length * by a length gives an area.
let nominal_power_output: Power = solar_panel_size * nominal_angle_to_sun.sin() * (30w/1m); // 30watts per meter multiplied by meters results in watts.
```
Not only is the dimension of measurements tracked and verified for you (preventing common mathematical errors), but the units are automatically converted as well.
If I were to do all my calculations using meters and then specify that my STL file should use millimeters, Command CAD will convert all of the final results to millimeters when exporting the STL file.

## Design Goals

* Scripting language for user to define models with
  * Deterministic evaluation
  * Convenient type safety
    * Measurement units are automatically converted to the correct output when exporting
  * Code Reuse
    * Code is organized into projects for easy and robust inclusion into other projects as libraries
    * Sketches, Solids, and Tasks can be imported from other projects
	* A package manager automatically resolves project dependencies
  * Self-Documentation
    * Model designs usually have extensive mathematics behind them
	  * Typically this documentation is kept in separate files or even in a physical notebook. If these files or physical notebook are lost, so is the reasoning behind the model
	  * The model not only contains the math and reasoning, but is defined by it. Changing the math will automatically update the model, removing opportunities for human error
	  * If you use git, you will have a commentated history of how you've changed your thinking over time
* Modular
  * Code Reuse isn't just for importing parts into your assembly. Other projects can add functionality, such as gcode generation techniques.

## Current features

 * CLI interfaces
   * Functions to perform calculation and generate models can be called from the command line
   * Arguments provided to functions are serialized from Json or Yaml (detection of format is automatic)
   * Automatic unit conversion is applied to provided arguments
   * The keyword `default` can be used to invoke a function's default arguments
   * Values returned are serialized and printed to stdout or saved to a file
 * Scripting Language
   * Functions
     * Default arguments can be specified. Use the keyword `default` when calling a function to indicate you wish to use the default value
	 * Constraints can be applied to arguments
   * Structs
     * Type safe
	 * Constraints can be applied to struct members
	 * While there is no polymorphic behavior, there is syntax to copy the values of one struct into another
   * Basic types
     * Strings
	   * A text format, UTF8 encoded.
	   * Can be formatted using similar syntax to Python's format method.
     * Lists
	   * Lists of objects
	   * Not all values in the list must be the same type
	 * Number
	   * Guaranteed to never be NaN (Dividing by zero produces a run time error)
	 * Measurement
	   * Guaranteed to never be NaN (Dividing by zero produces a run time error)
	   * Dimensional analysis enforced through type safety
	 * Automatic unit conversions
	   * Unit conversions provided by the well tested [Units of Measurement](https://github.com/iliekturtles/uom) crate
	 * Boolean
 * CAD Kernel
   * Built off the impressive [Fornjot](https://fornjot.app/) kernel, which is just as experimental as Command CAD is
	
## Alternatives

Other code based CAD programs exist, and with how experimental Command CAD currently is, you should probably prefer them.

### CAD Query
 * Technical overhead
   * To install Cad Query you must first install Python and Pip, and then you can install CAD Query. A typical Mechanical Engineer on Windows is unwilling to go through this.
 * Breaking Changes
   * There is still vast numbers of outdated tutorials and advice on the internet for Python. There aren't just the breaking changes between Python 2 and 3, but the breaking changes between 3.5, 3.6, 3.7, etc. Coming back to a project after a few months can be very frustrating to beginner who does not know how to use Python environments.
 * Does not provide measurement type safety

### OpenSCAD
 * Limited language features
   * Does not provide structs
 * Lack of package management
 * Does not provide measurement type safety
