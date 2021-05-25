{\rtf1\ansi\ansicpg1252\cocoartf2580
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red255\green255\blue255;\red0\green0\blue0;
}
{\*\expandedcolortbl;;\cssrgb\c0\c0\c0\c84706;\cssrgb\c100000\c100000\c100000;\cssrgb\c0\c0\c0;
}
{\*\listtable{\list\listtemplateid1\listhybrid{\listlevel\levelnfc23\levelnfcn23\leveljc0\leveljcn0\levelfollow0\levelstartat1\levelspace360\levelindent0{\*\levelmarker \{disc\}}{\leveltext\leveltemplateid1\'01\uc0\u8226 ;}{\levelnumbers;}\fi-360\li720\lin720 }{\listname ;}\listid1}}
{\*\listoverridetable{\listoverride\listid1\listoverridecount0\ls1}}
\paperw11900\paperh16840\margl1440\margr1440\vieww13140\viewh13080\viewkind0
\deftab720
\pard\pardeftab720\sl388\qj\partightenfactor0

\f0\fs24 \AppleTypeServices\AppleTypeServicesF65539 \cf2 \cb3 \expnd0\expndtw0\kerning0
\outl0\strokewidth0 \strokec2 The Passion\'92s network planning tool is based on the R programming language for obtaining the network configuration, in particular, the\'a0igraph\'a0library, version 0.9.3\'a0[igraph\ul ].\ulnone \'a0Following the URL description, \'93igraph, the network analysis package, is a collection of network analysis tools with the emphasis on efficiency, portability and ease\'a0of\'a0use;\'a0igraph\'a0is open-source and free, and can be programmed in R, Python, Mathematica and C/C++\'94.\AppleTypeServices \'a0\cb1 \
\pard\pardeftab720\sl345\qj\partightenfactor0
\AppleTypeServices\AppleTypeServicesF65539 \cf2 \cb3 In this sense, the easiest way to use the tool is by installing\'a0R\'a0(v 3.0.1+)\'a0and\'a0Rstudio\'a0IDE\'a0(v 1.4\ul +),\ulnone \'a0available for MAC OS, Linux and Windows systems. The easiest way to install them is by following the instructions in\'a0[Rstudio].\AppleTypeServices \'a0\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 Once\'a0these are installed and configured, the\'a0latest version available of the\'a0igraph\'a0package is needed to be installed, using\'a0install.packages(\'93igraph\'94,dependences=TRUE)\AppleTypeServices \'a0\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 Finally, the software tool is programmed in files:\AppleTypeServices \'a0\
\pard\tx220\tx720\pardeftab720\li720\fi-720\sl345\qj\partightenfactor0
\ls1\ilvl0\AppleTypeServices\AppleTypeServicesF65539 \cf2 \kerning1\expnd0\expndtw0 \outl0\strokewidth0 \
\ls1\ilvl0\expnd0\expndtw0\kerning0
\outl0\strokewidth0 \strokec2 - R_passion_tool2.R\'a0(main\'a0R code)\AppleTypeServices \'a0\
- \AppleTypeServices\AppleTypeServicesF65539 Passion_tool.Rmd\AppleTypeServices  \AppleTypeServices\AppleTypeServicesF65539 (Rmarkdown\'a0file)\AppleTypeServices \'a0\cb1 \uc0\u8232 \cf4 \strokec4 \
\pard\pardeftab720\sl345\qj\partightenfactor0
\AppleTypeServices\AppleTypeServicesF65539 \cf2 \cb3 \strokec2 The former contains all the code and logic for designing the Passion network, while the later uses the information provided by the main R code to produce techno-economic reports in PDF format.\AppleTypeServices \'a0\
\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 To execute the main R code, only a click to Source button in\'a0Rstudio\'a0is needed. To generate the techno-economic PDF file using the\'a0Rmd\'a0code, only a click to Knit button in\'a0Rstudio\'a0is necessary.\AppleTypeServices \'a0\cb1 \
\pard\pardeftab720\sl345\qj\partightenfactor0
\cf2 \
\pard\pardeftab720\sl388\qj\partightenfactor0
\AppleTypeServices\AppleTypeServicesF65539 \cf2 \cb3 In a nutshell, the planning tool receives as inputs:\AppleTypeServices \'a0\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 \'a0- Network topology\'a0characterised\'a0as files: nodesLabeling.csv and crossMatrix.csv. The first file shows the main features of each node. The second is a matrix that provides connectivity between the nodes (0 if not connected or a number of km if connected).\AppleTypeServices \'a0\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 - Traffic matrix: this is included in nodesLabeling.csv as the amount of traffic injected (in Gb/s) to the MAN per HL4, HL3 and HL12 nodes.\AppleTypeServices \'a0\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 - OSNR requirements for the support of 25, 40 and 50 Gb/s per wavelength, as files osnr_25_oh_fec.csv, osnr_40_oh_fec.csv and osnr_50_oh_fec.csv.\AppleTypeServices \'a0\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 -\'a0Normalised\'a0equipment cost (in Cost Units), provided in file Passion_cost_components.csv.\AppleTypeServices \'a0\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 \'a0Output:\'a0\AppleTypeServices \'a0\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 - lightpaths.csv includes the primary and backup paths from each HL4 towards the closest HL1/2. The backup path is both link and node disjoint if possible; if the topology connectivity does not allow it, the backup path shares the\'a0minumum\'a0number of links and nodes with the primary path. This file also includes details regarding the end-to-end OSNR, bitrate and route.\AppleTypeServices \'a0\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 - FFlightpaths.csv provides a description of the wavelength and fiber allocation per\'a0lightpath\AppleTypeServices \'a0\cb1 \
\AppleTypeServices\AppleTypeServicesF65539 \cb3 - NodeDesign.csv contains the design of each node (number of ROADMs and S-BVTs) along with its cost in\'a0normalised\'a0CU.\AppleTypeServices \'a0\cb1 \
}