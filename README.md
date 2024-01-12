# Command CAD

In my frustration with the state of CAD software as a whole, I lost my mind and wrote my own CAD program. Out of all of the ones I've used, the only one that was really palatable is OpenSCAD, which Command CAD takes most of its inspiration from. Command CAD has some critical differences though, like a stronger and convenient type safety for its scripting language and a more modular design.

## Design Goals

* Scripting language for user to define models with
  * Deterministic execution
  * Convenient type safety
    * Metric and Empirical measurements are automatically converted to the correct output
	* Degrees are automatically converted to radians when passed to tragicomic functions
  * Code Reuse
    * Code is organized into projects for easy and robust inclusion into other projects as libraries
    * Sketches, Meshes, and Functions can be imported from other projects
	* Package manager automatically resolves dependencies
  * Documentation
    * Model designs usually have extensive mathematics behind them
	  * This proof of logic and reasoning is often a legal necessity
	  * Typically this documentation is kept in separate files or even in a physical notebook. If these files or physical notebook are lost, so is the reasoning behind the model
	  * Commented code can contain all of this information, and since the code is your model, it is inseparable from the model
	  * The model will automatically update when you make changes to the code removing opportunities for human error during data entry
	  * If you use git, you will have a commentated history of how you've changed your thinking over time
* Modular
  * Instead of packing all the features into one program, make several small programs that excel at one task
  * Build Program
    * Consumes a script and converts it into a mesh (STL, AMF, etc) and exports
  * CAM Program
    * Consumes a script and converts it into GCode for laser cutters or CNC machines
	  * Scripts can attach properties to surfaces, such as what kind of machining operation should be used for it.
	* For now the build program can export STLs for slicers, but a custom slicer may become a goal in the future.
  * LSP-server
    * Between Emacs and VSCode, there really isn't a point in making a custom text editor for the scripting language. Instead an LSP server and live preview application can be provided to assist established high quality editors.

## Why this and not CADQuery?

* Technical overhead
  * To install Cad Query you must first install Python and Pip, and then you can install CadQuery. A typical Mechanical Engineer on Windows is unwilling to go through this.
* Breaking Changes
  * There is still vast numbers of outdated tutorials and advice on the internet. There aren't just the breaking changes between Python 2 and 3, but the breaking changes between 3.5, 3.6, 3.7, etc. Python is very bad about breaking changes.
* The scripting language itself
  * Additional unit type safety

# Planned changes

* Global functions can no longer be called externally
* Tasks are global functions that can be called externally
* Functions can be defined from all scopes, but only be access from child scopes
* Structs can be defined from all scopes, but only be accessed from child scopes
* Struct construction should not require the keyword `struct` in front
  * Struct construction will instead be a trailer operator type
* It should be possible to deserialize measurements, but not to serialize measurements
  * Serializing a measurement is problematic because we don't know what unit to output in (meters, inches, kilometers?)
  * Losing precision in ways outside of the user's control could be very dangerous
* Error handling can be improved. Instead of pushing errors to the log, they should be part of the `failure` type.
