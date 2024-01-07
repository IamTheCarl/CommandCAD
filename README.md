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
 
