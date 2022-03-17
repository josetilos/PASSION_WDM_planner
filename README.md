WORK PUBLISHED IN (Please Cite): 

Authors: Mariangela Rapisarda, José Alberto Hernández, Alberto Gatto, Paola Parolari, Pierpaolo Boffi, Michela Svaluto Moreolo, Josep Maria Fábrega, Laia Nadal, Ricardo Martínez, Víctor López, Juan-Pedro Fernández-Palacios, Gabriel Otero, and David Larrabeiti

Title: All-optical aggregation and distribution of traffic in large metropolitan area networks using multi-Tb/s S-BVTs

Reference: J. Opt. Commun. Netw. 14(5), 316-326 (2022)

DOI: https://doi.org/10.1364/JOCN.448115

This software tool aims at designing and planning optical WDM networks employing low-cost VCSEL-based S-BVTs for multi-Tb/s transmission in next-generation Metro networks. Further details on the PASSION technology can be found in the EU project's website: https://www.passion-project.eu/ 

The tool is fed with the optical characterization of PASSION hardware (for OSNR calculations), hierarchical network topology and traffic demands, and delivers as outputs, the primary and backup optical lightpaths, wavelength assignment per fiber link, and techno-economic metrics of the designed network, including per-node design and network cost.

The Passion’s network planning tool is based on the R programming language for obtaining the network configuration, in particular, the igraph library, version 0.9.3. In this sense, the easiest way to use the tool is by installing R (v 3.0.1+) and Rstudio IDE (v 1.4+), available for MAC OS, Linux and Windows systems. The easiest way to install them is by following the instructions in https://www.rstudio.com/products/rstudio/download/

Once these are installed and configured, the latest version available of the igraph package is needed to be installed, using install.packages(“igraph”,dependences=TRUE) 

The software tool is programmed in files: 
- R_passion_tool2.R (main R code) 
- Passion_tool.Rmd (Rmarkdown file)

The former contains all the code and logic for designing the Passion network, while the later uses the information provided by the main R code to produce techno-economic reports in PDF format. 

To execute the main R code, only a click to Source button in Rstudio is needed. To generate the techno-economic PDF file using the Rmd code, only a click to Knit button in Rstudio is necessary. 

In a nutshell, the planning tool receives as inputs: 

- Network topology characterised as files: nodesLabeling.csv and crossMatrix.csv. The first file shows the main features of each node. The second is a matrix that provides connectivity between the nodes (0 if not connected or a number of km if connected). 
- Traffic matrix: this is included in nodesLabeling.csv as the amount of traffic injected (in Gb/s) to the MAN per HL4, HL3 and HL12 nodes. 
- OSNR requirements for the support of 25, 40 and 50 Gb/s per wavelength, as files osnr_25_oh_fec.csv, osnr_40_oh_fec.csv and osnr_50_oh_fec.csv. 
- Normalised equipment cost (in Cost Units), provided in file Passion_cost_components.csv. 

Output:  

- lightpaths.csv includes the primary and backup paths from each HL4 towards the closest HL1/2. The backup path is both link and node disjoint if possible; if the topology connectivity does not allow it, the backup path shares the minumum number of links and nodes with the primary path. This file also includes details regarding the end-to-end OSNR, bitrate and route. 

- FFlightpaths.csv provides a description of the wavelength and fiber allocation per lightpath 

- NodeDesign.csv contains the design of each node (number of ROADMs and S-BVTs) along with its cost in normalised CU. 
