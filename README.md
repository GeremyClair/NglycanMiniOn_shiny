# NglycanMiniOn_shiny
 Shiny app for the package NglycanMiniOn


## Requirements
To use the shiny app, we recommend to use R (version 4.1.0 or superior) and to use Studio (Build 561 or superior). 

The installation of the NglycanMinion package is required R please first make sure devtools is installed
> install.packages("devtools")

When devtools is installed, run the following command to install RomicsProcessor and its dependencies
> devtools::install_github("GeremyClair/NglycanMiniOn")

Finally R shiny (v1.8.1.1 or superior) has to be installed
> install.packages("shiny")

## Usage
In R studio open the file named [app.R](https://github.com/GeremyClair/NglycanMiniOn_shiny/blob/main/app.R). Then in the tab data ingestion, select between List and Ranked mode.

The List Mode enable to compare a Query to a Universe list (e.g., list of detected N-Glycans, or list of all detectable N-Glycan by MALDI-MSI). Functions for enrichments are Fisher exact, EASE, Binomial, and hypergeometric. Load a .csv file for the query mode, an example of file is provided [query.csv](https://github.com/GeremyClair/NglycanMiniOn_shiny/blob/main/query.csv) for the Universe you can either choose to use all known N-glycan structures detectable by MSI or you can upload your own N-glycan list, an example of such list is provided [universe.csv](https://github.com/GeremyClair/NglycanMiniOn_shiny/blob/main/universe.csv).

The Ranked Mode enable to perform enrichment analysis based on the rank of the N-glycans within a list (e.g., p-value, fold-change, intensity order, etc.) An example of file is provided : [ranked table.csv](https://github.com/GeremyClair/NglycanMiniOn_shiny/blob/main/rank%20table.csv). Kolmoronov-Smirnoff test will be used to perform enrichment

When the data is loaded you can reach the second tab titled "Enrichment". Based on the type of data last loaded, if you selected "List mode" different enrichment options will be offered only KS test will be offered for "Ranked mode" 

Finally the Visualization table will enable to visualize the number of Nglycans belonging to each class and other structural attributes.

## Contacts
Written by Geremy Clair for the Department of Energy (PNNL, Richland, WA) \
E-mail: geremy.clair@pnnl.gov or proteomics@pnnl.gov \
Website: https://www.pnnl.gov/integrative-omics or https://panomics.pnnl.gov/

## License
NglycanMiniOn is licensed under the 2-Clause BSD License; 
you may not use this file except in compliance with the License.  You may obtain 
a copy of the License at https://opensource.org/licenses/BSD-2-Clause

Copyright 2024 Battelle Memorial Institute
