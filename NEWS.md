# oecdplot 2022.12.21

* adds functionality to save plots in PDF format (issue #100)

# oecdplot 2022.12.19

* Provides a workaround to load Arial Narrow when the VPN is disconnected
* Uses Cairo when saving in EPS format

# oecdplot 2022.12.14

* Re-adds EPS exporting

# oecdplot 2022.12.11

* fixes transparency problems in legends when exporting as EMF

# oecdplot 2022.11.27

* uses Cairo for PNG, therefore forces to use Arial Narrow
* sets minimal ggplot2 and devEMF version
* allows optional parameters for EMF

# oecdplot 2022.11.16

* drops argument for office 365
* the typography internals were redone
* drops as many dependencies as possible to ease bug tracking

# oecdplot 2022.11.08

* provides single argument for office 365 compatibility

# oecdplot 2022.11.07

* complete support for SVG (i.e., for Office 365)
* adds formal tests for different outputs

# oecdplot 2022.11.04

* Drops cairo requirement and exports directly

# oecdplot 2022.10.28

* EMF output uses EMF instead of EMF+ as default
* If no statlink_dataframe is specified, it uses the data passed to ggplot as is

# oecdplot 2022.10.24

* Saves as PNG unless you specific EMF as `format = "emf"`

# oecdplot 2022.10.17

* Adds SVG support
* Uses OECDMAIN for fonts, replacing P:/ as path

# oecdplot 2022.09.27

* Adds wrapper for column and diamond plot
* Saves statlinks plots/data under `./statlinks` instead of 
  `./produced_data/statlinks`

# oecdplot 2022.09.19

* Automatically adds trailing "/""to the output folder in
  `save_oecd_chart()`.
* Doesn't fail on load if it can't find Arial Narrow
  
# oecdplot 2022.09.12

* Includes wrappers to produce plots in one line (i.e., see
  `oecd_col()`, `oecd_line()`, etc.)
* Fixes issues with text size when exporting to EMF
  
# oecdplot 2022.07.06

* Thinner legend margin
* Starting to work on the 'What If' section in the documentation

# oecdplot 2022.07.05

* Starts using semantic versioning
* All personalization to legends were disabled, except for the gray background
  (i.e., see issues #56 to #62)
* The tick marks were adjusted to mimic Excel charts
* BREAKING CHANGE: Allows saving to any path, without the restriction of writing 
  to the current project
  
# oecdplot 0.1.8

* Based on #56, now we treat EMF as a special case with 96 DPI for the 
  coordinate system. This is NOT a good solution.
* BREAKING CHANGE: oecd_load_fonts() changes to load_oecd_fonts() for consistent
  naming

# oecdplot 0.1.7

* Based on #55 now the DPI on save is 96*2 = 192 and the gray theme changes to
  another gray copied from ONE AUTHOR

# oecdplot 0.1.6

* Thinner grid lines
* rearrange_whitespace changes to fct_whitespace_to_newline, which returns
  factors and changes usage, now you can use
  mutate + across + fct_whitespace_to_newline

# oecdplot 0.1.5

* Simple as 0.1.4 but removes any need for RTools or to compile code
* Mentions units in the documentation (i.e. it's explicit that everything is in 
  CM, not IN)
* Provides option to change the x-axis text angle (see #41)
* Replaces pop data with protected terrestrial areas (i.e., to show stat links)

# oecdplot 0.1.4

* Needs RTools (i.e., it includes C code)
* Drops oecdfonts and devEMF dependency
* Allows to use centimeters with EMF outputs
* Features vignettes with use cases (work in progress)

# oecdplot 0.1.3

* 1st version with complete documentation
* save Statlinks defaults to FALSE

# oecdplot 0.1.2

* First attempt to fix JPEG text size
* Allows to use Arial Narrow on Windows
* Initial SQL connection
* Drops EPS support (labelled as obsolete)

# oecdplot 0.1.1

* Exports to EPS by using system Cairo
* Corrects JPEG export (i.e., uses JPEG, not PNG)
* Use cli:: to provide messages

# oecdplot 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Initial version, uses showtextdb instead of extrafont to use Arial Narrow.
