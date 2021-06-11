# Avant-garde: fully automated, cloud-based workflow on Terra

Avant-garde is a tool for automated signal refinement of data-independent acquisition (DIA) and other targeted mass spectrometry data (Vaca Jacome et al. 2020). Avant-garde can be used alongside existing tools for peptide detection to refine chromatographic peak traces of peptides identified in DIA data by these tools. Avant-garde ensures confident and accurate quantitation by removing interfering transitions, adjusting integration boundaries, and scoring peaks to control the false discovery rate. 

Avant-garde was originally developed as an external tool in [Skyline](https://skyline.ms/project/home/begin.view?) for easy and efficient chromatogram refinement directly within Skyline documents for small datasets. To enable robust analysis of large DIA datasets and remove dependency on the Skyline software, we present here an improved workflow adapting the [Avant-garde refinement algorithm](https://github.com/SebVaca/Avant_garde) for compatibility with [Terra](https://app.terra.bio/), a Google cloud-based platform for large-scale data analysis and sharing. The new workflow, deployed on Terra at [Avant-garde_Production_v1_0](https://app.terra.bio/#workspaces/lincs-phosphodia/Avant-garde_Production_v1_0), is fully-automated, robust, scalable, user-friendly, and more efficient for large DIA datasets. It is written in the Workflow Description Language (WDL) and calls dockerized R and python functions.

## Contents of this repository

This reposoitory contains all scripts that support the cloud-based Avant-garde workflow deployed on Terra.

* `Avantgarde_WDL.wdl`: full workflow written in the Worfkflow Description Language (WDL) that is executed by the [Cromwell](https://github.com/broadinstitute/cromwell) Workflow Management System on Terra.
* `Dockerfile`: Dockerfile containing all commands to assemble the Avant-garde docker image. The publically available Avant-garde docker image, containing all programs and scripts necessary to run Avant-garde, is hosted on Docker Hub and can be pulled from `broadlincsproteomics/avant-garde:v1_0`. 

The Dockerfile contains a command to copy all contents of the `src` directory into the docker image:
* `AvG_final_report.R`: R script that executes the final task of the WDL workflow: `final_r_reports`.
* `AvG_for_Terra.R`: R script that executes the Avant-garde algorithm adapted for Terra. It is called during the second task of the WDL workflow: `run_avg`.
* `avg_utils-0.0.0.tar.gz`: source distribution file for the python package in the [avg_utils](https://github.com/SebVaca/avg_utils) GitHub repository. This is installed by the Dockerfile using pip, and the functions contained in this python package are called in the first and second tasks of the WDL workflow.

The `templates` directory includes:
* `AvG_Params.R`: the Avant-garde parameters file (see [Parameters documentation](https://github.com/broadinstitute/Avant-garde-Terra/wiki/Parameters) for more info).
* `AvantGardeDIA_Export_Template.csv`: example CSV input file showing the required fields.

The `tutorial` directory includes:
* `Tutorial_AvantGardeDIA_Export.zip`: zip file containing the input dataset to the [tutorial](https://github.com/broadinstitute/Avant-garde-Terra/wiki/Tutorial).
* `images` subdirectory contains the image files displayed in the tutorial.

## Running Avant-garde on Terra

To get started with using Terra and running the Avant-garde workflow, follow the [tutorial](https://github.com/broadinstitute/Avant-garde-Terra/wiki/Tutorial). Detailed documentation for the workflow can be found on the [Avant-garde-Terra wiki page](https://github.com/broadinstitute/Avant-garde-Terra/wiki).


## References

Vaca Jacome, A.S. et al. Avant-garde: an automated data-driven DIA data curation tool. Nat Methods 17, 1237–1244 (2020). https://doi.org/10.1038/s41592-020-00986-4. 