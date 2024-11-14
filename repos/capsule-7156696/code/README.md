# Interactive modeling of effective light fluence at depths into the skin for PDT dose comparison

## Code
Interactive Jupyter notebooks written in Python3 are provided. 
Both code examples can be run from the code directory using the interactive Jupyter interface. 
Output files are stored in the results directory.

### GAMOS Geometry Helper.ipynb
This notebook can be used to convert an Excel file of a specific format into a working GAMOS geometry input file. 
This is necessary to help reduce errors when specifying complex wavelength-dependent optical properties ($\mu_a, \mu_s, g, n$, etc.) for volumes. 
The code allows for material properties to be defined for BOX volumes over an arbitrary range of optical wavelengths. 
The user will still need to create a GAMOS input file to interact with this geometry.

### Monte Carlo Analysis.ipynb
This code is used to read the output fluence of the Monte Carlo simulations and generates figures and lookup tables provided in the manuscript.

# Running GAMOS
The Mote Carlo simulations used in this analysis were run using GAMOS 5.2 with the Dartmouth tissue optics plugin.
The tissue optics plugin was originally created by [Adam Glaser for GAMOS 4.0](https://dx.doi.org/10.1364%2FBOE.4.000741). 
I have updated his changes to remain compatible with GAMOS 5.2 and these changes are viewable on GitHub (GAMOS [4.0->5.1](https://github.com/ethanlarochelle/GamosCore/commits/master), [5.1->5.2](https://github.com/ethanlarochelle/GamosCore/commits/5_2)). 
Additionally, a [Docker image](https://hub.docker.com/r/ethanlarochelle/gamos5_2_dartmouth_tissue_optics/) with these changes and pre-installed dependencies is also available. The [code used to create this container](https://github.com/ethanlarochelle/gamos5_2_docker) is also avaialble.
The inputs and outputs of the Monte Carlo simulations are available in the zip file in the data directory.
The interested reader is encouraged to try using the conatiner and provided GAMOS input files, however this functionality is not provided in the current code capsule. 

# Changelog
## Version 3
* Functions were added to calculate pro-drug and PpIX concentration as a function of tissue depth and time
* A function has been added to calculate wavelength-dependent photobleaching constant ($\beta$)
* A function has been added to convert molecules of PpIX per volume into molar concentration
* Analysis of PpIX concentration as a function of incubation time, treatment time, and light fluence has been introduced.
* A cytotoxic threshold is intriduced, which combines the previous light fluence threshold with a minimum PpIX concentration
* Addidional analysis has been added to reflect this new threshold.
## Version 2
* A comparison of beam sizes has been added to the Monte Carlo analysis.
* The primary beam size used for analysis is now 25mm, whereas before it was 2.25mm
* A figure showing the tissue optical properties has been added.
* The depth of activation now reports the intersection with the threshold fluence at the deepest point, not the first point.
* Information about the Monte Carlo run-time has been added.
* The code can now be run interactively with JupyterLab [(help with JupyterLab)](https://help.codeocean.com/interactive-sessions/jupyterlab-interactive-sessions)
  * Open the JupyterLab interactive session 
  * Browse the code folder
  * Open Monte Carlo Analysis.ipynb
  * Run each cell

