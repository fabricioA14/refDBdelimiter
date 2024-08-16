<p align="center">
  <img src="https://github.com/fabricioA14/refDBdelimiter/assets/73892283/3a436338-be1e-4c3d-b893-cb6aacd63eb6" alt="refdb" width="230"/>
</p>

<h1 align="center">refDBdelimiter: An R-Based Web Application for Molecular Identification with Geographically Curated eDNA Databases</h1>

## Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Usage](#usage)
- [Overview](#overview)
- [Contributing](#contributing)
- [License](#license)

## Introduction

<p align="justify">
The integration of multiple biodiversity repositories such as the Global Biodiversity Information Facility (GBIF) and National Center for Biotechnology Information (NCBI) has become relevant for optimizing ecological research (<a href="https://doi.org/10.1002/edn3.489">Curd et al., 2024</a>). However, the disparate nature of these multiple data sources poses significant challenges for researchers seeking to conduct comprehensive analyses, particularly in the context of metabarcoding studies (<a href="https://doi.org/10.1111/1755-0998.13741">Jeunen et al., 2023</a>). Metabarcoding, a high-throughput sequencing approach, allows for the simultaneous analysis of multiple taxa within environmental samples, providing insights into community composition and diversity (<a href="https://doi.org/10.1371/journal.pone.0285674">Gold et al., 2023</a>).
</p>

<p align="justify">
The proposed R package aims to streamline the metabarcoding analysis workflow by providing a cohesive framework for accessing, cleaning, and integrating GBIF occurrence data and NCBI sequences.
</p>

## Installation

The `refDBdelimiter` package is designed to be used on Windows systems. To ensure a smooth installation process, please follow these steps to meet the necessary system requirements:

1. **Windows Subsystem for Linux (WSL)**: This package relies on WSL for various underlying processes. To install WSL, follow these simple steps:
   
   - Open Command Prompt as Administrator and run the following command:
     ```cmd
     wsl --install
     ```
   - Restart your computer if prompted.

For more detailed instructions, refer to the [Microsoft](https://docs.microsoft.com/en-us/windows/wsl/install) documentation.

2. **Install `ncbi-blast+` on WSL**:

   - First, run the `wsl` command, update and upgrade your system by running:
     ```bash
     wsl
     sudo apt-get update
     sudo apt-get upgrade
     ```
  
   - Next, install the `ncbi-blast+` package:
     ```bash
     sudo apt install ncbi-blast+
     ```

3. **GNparser**: The package requires `gnparser` for taxonomic name parsing. To resolve installation issues on Windows:
   - Download the appropriate binary file from the [GNparser](https://github.com/gnames/gnparser/releases) releases page.
   - Extract the binary file, then:
     ```cmd
     cd C:\
     mkdir C:\bin
     copy path_to\gnparser.exe C:\bin
     ```
     After running these commands, you need to add the `C:\bin` directory to your PATH user and/or system environment variables. This ensures that `gnparser` can be accessed from any command prompt or script. For instructions on how to add a directory to your PATH environment variable, refer to this tutorial: [How to set the path and environment variables in Windows](https://www.computerhope.com/issues/ch000549.htm).

     These steps are described in this [GitHub](https://github.com/brunobrr/bdc/issues/265) issue.

4. Install `Biostrings` package from Bioconductor:

     ```r
     if (!requireNamespace("BiocManager", quietly = TRUE))
     install.packages("BiocManager")
     BiocManager::install("Biostrings")
     ```

5. Install `rnaturalearthhires` package from GitHub:

    ```r
    remotes::install_github("ropensci/rnaturalearthhires")
    library(rnaturalearthhires)
    ```

6. To install the `refDBdelimiter` package, you can use the following commands in R:

    ```r
    # Install devtools if not already installed
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }

    # Install refDBdelimiter from GitHub
    devtools::install_github("fabricioA14/refDBdelimiter")
    ```

## Usage

### Acquiring Data From Sources

### GBIF

1. **Create an Account**:
   - Before starting, you must create an account and log in to GBIF. Visit [GBIF](https://www.gbif.org) and sign up if you don't have an account yet.

2. **Log In**:
   - Log in to your account to be able to access the data download features.

3. **Search**:
   - Apply additional filters as needed, such as location. In this example, "Siluriformes" is searched within the "South America" location filter.

<p align="center">
  <img src="https://github.com/user-attachments/assets/4d6cb543-64cf-4670-8ac7-9d0fac6f084f" width="900"/>
</p>

4. **Select the Download Option"**:
   - Click on the `Download` tab.
   - Choose the download type as `Simple`, which provides a simplified version in a tab-delimited CSV format. The tool only works with this simplified version.

<p align="center">
  <img src="https://github.com/user-attachments/assets/218ba6e6-0758-4f40-b099-03f6b47b4cd1" width="900"/>
</p>

5. **Process and Wait for Download Availability"**:
   - Depending on the size of the data, GBIF may take some time to process the request.
   - You will receive an e-mail notification once your download is ready. In this example, 659,081 occurrences are being downloaded.
   - Once the download is available, click the `Download` button to retrieve your file.

<p align="center">
  <img src="https://github.com/user-attachments/assets/148af978-3663-4152-86b8-1b8492d7322a" width="900"/>
</p>

### NCBI

1. **Access the NCBI website**: Navigate to [NCBI](https://www.ncbi.nlm.nih.gov).
2. **Select the database**:
   - In the search bar, select `All Databases` from the dropdown menu.
   - Type **"Siluriformes COI"** (or your desired search term) in the search bar.
   - Click the `Search` button to start the search.

<p align="center">
  <img src="https://github.com/user-attachments/assets/2db5f63d-37c2-4c56-8e5b-be23caf814b2" width="900"/>
</p>

3. **Filter by "Nucleotide"**:
   - On the results page, locate the `Genomes` section and click on **"Nucleotide"**. This will filter the results to show only relevant nucleotide sequences.

<p align="center">
  <img src="https://github.com/user-attachments/assets/777567b7-1cc4-4d3f-a931-c00a6e2faff9" width="900"/>
</p>

4. **Select the sequences**:
   - In the list of search results, you will see multiple entries for sequences related to your search term.
   - To download all the sequences, go to the `Send to` menu located in the top right corner of the page.

5. **Configure the download**:
   - Select **"Complete Record"** to download all details of the sequences.
   - Choose **"File"** as the destination.
   - In the "Format" field, select **"FASTA"**.
   - Under "Sort by", leave it as **"Default order"**.
   - Click **"Create File"** to start downloading the FASTA file containing all sequences.

<p align="center">
  <img src="https://github.com/user-attachments/assets/05221fe8-b3ce-4733-a838-dffc379b9efc" width="900"/>
</p>

### Preprocessing

1. **Input File Path:**
   - Specify the file path to your occurrence input dataset (CSV file downloaded previously from GBIF).
   - Example: `C:/Data/occurrence_data.csv`

2. **Number of Rows to Read:**
   - Define how many rows from the file should be read during preprocessing.
   - Default: `All` (reads the entire dataset)

3. **Additional Columns to Import:**
   - Indicate any additional columns from the input file that you want to include in the preprocessing.
   - Example: `habitat, climate`
   
   ⚠️ **Warning:** Be cautious when adding many additional columns, especially if you are working with a very large dataset. This is particularly important if your goal is to work only with occurrence data and not to build a metabarcoding database.

4. **Select Taxonomic Level for Visualization:**
   - Choose the taxonomic level that will be used for visualization in the graph on the right.
   - Example: `Family`

5. **Save Path for Preprocess:**
   - Define the path where the preprocessed data file will be saved.
   - Default: `1_PreProcess_cleaned`

6. **Distance for Inconsistent Coordinates (dist):**
   - Adjust the distance in decimal degrees to identify and handle inconsistent coordinates.
   - Default: `0.1`

7. **Save Coordinates Output:**
   - Check this option if you want to save the processed coordinates separately.

8. **Select Output Formats:**
   - Choose the output formats for the preprocessed data file. Options include:
     - `shp`
     - `geojson`
     - `gpkg`
     - `kml`
     - `csv`

9. **Run Preprocess:**
   - Click this button to start preprocessing with the specified settings.

<p align="center">
  <img src="https://github.com/user-attachments/assets/ec35b074-8506-4626-94cb-32d7f30e449d" width="600"/>
</p>

- Useful Links
  
  **bdc:** [bdc_coordinates_country_inconsistent](https://brunobrr.github.io/bdc/reference/bdc_coordinates_country_inconsistent.html)

### Taxonomic Filter

This section explains the parameters configured in the Taxonomic Filters tab of your Shiny app for taxonomic data filtering.

1. **Replace synonyms by accepted names (`replace_synonyms`):**
   - Check this option to automatically replace any synonyms in your dataset with their accepted taxonomic names.

2. **Suggest names for misspelled names (`suggest_names`):**
   - Enable this feature to allow the system to suggest corrections for any misspelled taxonomic names in your dataset.

3. **Distance between the searched and suggested names (`suggestion_distance`):**
   - Adjust the slider to set the maximum allowable distance between the entered name and the suggested corrected name.
   - Example: `0.9` (A higher value increases the tolerance for differences between names.)

4. **Taxonomic Database (`db`):**
   - Specify the taxonomic database to be used for validating and retrieving taxonomic information.
   - The only one available at the moment is `gbif`.

5. **Taxonomic Rank Name (`rank_name`):**
   - Enter the specific taxonomic rank name that you want to focus on during filtering.
   - Example: `Chordata`

6. **Taxonomic Rank (`rank`):**
   - Select the level of taxonomic rank you wish to filter or analyze within your data.
   - Example: `Phylum`

⚠️ **Warning:** It is very important to ensure that your Taxonomic Rank Name and Taxonomic Rank parameters are synchronized.
Examples:
  - If your Taxonomic Rank Name is `Chordata`, your Taxonomic Rank should be `Phylum`.
  - If your Taxonomic Rank Name is `Gymnotiformes`, your Taxonomic Rank should be `Order`.

7. **Select Taxonomic Level for Visualization:**
   - Choose the taxonomic level that will be used for visualization in the graph on the right.
   - Example: `Family`

8. **Use parallel processing (`parallel`):**
   - Check this option to enable parallel processing, which can speed up the filtering process by using multiple cores.

9. **Number of cores (`ncores`):**
   - Specify the number of CPU cores to be used if parallel processing is enabled.
   - Example: `4`

10. **Export accepted names (`export_accepted`):**
    - Enable this option if you want to export the accepted taxonomic names to a separate file.

11. **Save Path for Taxonomy (`output_file`):**
    - Define the path where the filtered and cleaned taxonomic data will be saved.
    - Default: `2_taxonomy_cleaned`

12. **Select Output Formats:**
    - Choose the output formats for the filtered taxonomic data file. Options include:
      - `shp`
      - `geojson`
      - `gpkg`
      - `kml`
      - `csv`

13. **Run Taxonomic Filters:**
    - Click this button to start the taxonomic filtering process with the specified settings.

<p align="center">
  <img src="https://github.com/user-attachments/assets/215551ee-1980-4990-a820-f51be65ed20b" width="600"/>
</p>

- Useful Links
  
  **bdc:** [bdc_query_names_taxadb](https://brunobrr.github.io/bdc/reference/bdc_query_names_taxadb.html)

### Spatial Filter

This section explains the parameters configured in the Spatial Filters tab of your Shiny app for spatial data filtering.

1. **Number of Decimals to be Tested (`ndec`):**
   - Specify the number of decimal places to be tested for precision in the coordinates.
   - Example: `3`

2. **Clustering Based on (`species`):**
   - Choose the taxonomic level for clustering, such as species, genus, etc.
   - Example: `Species`

3. **Select Spatial Tests (`tests`):**
   - Select the spatial tests to be applied during the cleaning process. Options include:
     - `capitals`: Check for points near capital cities.
     - `centroids`: Identify records near centroids.
     - `duplicates`: Find duplicate records.
     - `equal`: Test for equal coordinates.
     - `gbif`: Flag records based on GBIF headquarters.
     - `institutions`: Identify points near biodiversity institutions.
     - `outliers`: Detect outliers in spatial data.
     - `zeros`: Find records with zero coordinates.
     - `urban`: Identify records in urban areas.

4. **Radius for Capitals (`capitals_rad`):**
   - Define the radius (in meters) around capital cities within which records will be flagged.
   - Example: `3000` meters

5. **Radius for Centroids (`centroids_rad`):**
   - Set the radius (in meters) around centroids within which records will be flagged.
   - Example: `10000` meters

6. **Detail Level for Centroids (`centroids_detail`):**
   - Choose the detail level for identifying centroid-based issues:
     - `country`
     - `provinces`
     - `both`

7. **Radius Around Biodiversity Institutions Coordinates (`inst_rad`):**
   - Define the radius (in meters) around biodiversity institutions within which records will be flagged.
   - Example: `100` meters

8. **Multiplicative Factor for Outliers (`outliers_mtp`):**
   - Set the multiplicative factor to adjust the sensitivity of the outlier detection.
   - Example: `5`

9. **Minimum Distance for Outliers (`outliers_td`):**
   - Specify the minimum distance (in kilometers) between a record and all other records of the same species to be considered an outlier.
   - Example: `1000` kilometers

10. **Minimum Number of Records for Outlier Test (`outliers_size`):**
    - Define the minimum number of records required in the dataset for the taxon-specific outlier test to be applied.
    - Example: `10`

11. **Range Radius (`range_rad`):**
    - Set the range radius (in decimal degrees) to consider when identifying records that fall outside the expected range.
    - Example: `0`

12. **Radius for Zeros (`zeros_rad`):**
    - Define the radius (in decimal degrees) around zero coordinates to flag records as potential errors.
    - Example: `0.5` decimal degrees

13. **Select Taxonomic Level for Visualization:**
    - Choose the taxonomic level that will be used for visualization in the graph on the right.
    - Example: `Family`

14. **Save Path for Space:**
    - Specify the path where the spatially filtered data will be saved.
    - Default: `3_space_cleaned`

15. **Select Output Formats:**
    - Choose the output formats for the spatially filtered data file. Options include:
      - `shp`
      - `geojson`
      - `gpkg`
      - `kml`
      - `csv`

16. **Run Spatial Filters:**
    - Click this button to start the spatial filtering process with the specified settings.

<p align="center">
  <img src="https://github.com/user-attachments/assets/1c68ee1b-22f1-4574-8842-b5fec5a0db93" width="600"/>
</p>

- Useful Links
  
  **bdc:** [bdc_coordinates_precision](https://brunobrr.github.io/bdc/reference/bdc_coordinates_precision.html)
  
  **CoordinateCleaner:** [clean_coordinates](https://www.rdocumentation.org/packages/CoordinateCleaner/versions/3.0.1/topics/clean_coordinates)

### Temporal Filter

This section explains the parameters configured in the Temporal Filters tab of your Shiny app for filtering data based on temporal criteria.

1. **Year Threshold (`year_threshold`):**
   - Set the year threshold to filter out records that fall outside the specified range. Any records with a year earlier than this threshold will be flagged or removed.
   - Example: `1950`

2. **Select Taxonomic Level for Visualization:**
   - Choose the taxonomic level that will be used for visualization in the graph on the right.
   - Example: `Family`

3. **Save Path for Time:**
   - Specify the path where the temporally filtered data will be saved.
   - Default: `4_time_cleaned`

4. **Select Output Formats:**
   - Choose the output formats for the temporally filtered data file. Options include:
     - `shp`
     - `geojson`
     - `gpkg`
     - `kml`
     - `csv`

5. **Run Temporal Filters:**
   - Click this button to start the temporal filtering process with the specified settings.

<p align="center">
  <img src="https://github.com/user-attachments/assets/9f599568-01bf-44b7-9bd9-9bc5b408754c" width="600"/>
</p>

- Useful Links
  
  **bdc:** [bdc_year_outOfRange](https://brunobrr.github.io/bdc/reference/bdc_year_outOfRange.html)

### Interactive Map

<p align="center">
  <img src="https://github.com/user-attachments/assets/2e04b2cf-c774-44dd-a0a1-2b5c1151504f" width="600"/>
</p>

### Make Database

To create a database, you need to define these parameters:

1. **Mandatory Parameters**:
   - **Raw Database**: This is the database you will download in FASTA format from the NCBI website. Example: `ncbiChordata.fasta`. To download, go to the NCBI site, use "Send to - Complete Record - Choose Destination: Send to File - Format: FASTA".
   - **GBIF Database**: Set this parameter only if you have run the previous map. The output will generate this file. Example: `gbif_database.txt`. If not set, simply provide a FASTA file and the database will be built without any geographic filter.
   - **Output Database**: This file will contain the sequences from the selected region based on your GBIF database. Example: `Chordata_Ncbi_Gbif.fasta`.
   - **Database Name**: This is the name of your final database. Example: `ChordataBrazilCOX1`.
   - **Minimum Sequence Length**: This parameter sets a threshold for the minimum length of sequences to be included in the database. Example: `100` bp. This ensures that only sequences with a length greater than or equal to the specified threshold are included in the database, improving the quality and reliability of the data.
2. **Optional Parameters**:
   - **Genus Flexibility**: This parameter allows the database to include all species within a genus if any species of that genus occurs in the area of interest. This helps to address the lack of sequences available in NCBI molecular reference databases. 
   - **Exclude UNVERIFIED Sequences**: This parameter allows you to exclude sequences labeled as UNVERIFIED from the database. This can improve the reliability and accuracy of the data by ensuring that only verified sequences are included.
   - Additional parameters associated with the `makeblastdb` function can be set as needed. For a complete list of these parameters and their descriptions, refer to the [`makeblastdb`](https://www.ncbi.nlm.nih.gov/books/NBK279684/table/appendices.T.makeblastdb_application_opt/) application options.

<p align="center">
  <img src="https://github.com/user-attachments/assets/8846abb5-26c4-45a2-b5d9-deada60902f1" width="600"/>
</p>

The image above illustrates the creation of a new database, displaying the current time, the path of the new database, its title, sequence type, and other relevant details. In this example, 17,507 sequences were added in 1.5962 seconds. This output will be printed to the screen in the RStudio session upon completion.

<p align="center">
  <img src="https://github.com/user-attachments/assets/cee67c0a-cd46-49f4-8a48-1ee0eae1e1aa" width="600"/>
</p>

- Useful Links
  
  **NCBI:** [makeblastdb application options](https://www.ncbi.nlm.nih.gov/books/NBK279684/table/appendices.T.makeblastdb_application_opt/)

### Taxonomic Assignment

The **Taxonomic Assignment** tab provides a user interface for configuring and running a BLAST (Basic Local Alignment Search Tool) analysis for taxonomic assignment of sequences. Below is a detailed explanation of each input parameter and its function:

- **Directory**: Specify the directory where your input files are located.
- **Database File**: Enter the path to the database file you want to use for the BLAST search.
- **Query File**: Provide the path to the query file containing the sequences to be analyzed, typically in FASTA format (default: `otus.fasta`).
- **OTU Table**: Specify the path to the OTU (Operational Taxonomic Unit) table.
- **Task**: Select the BLAST task to be performed (default: `megablast`).
- **Output File**: Enter the path for the output file where the BLAST results will be saved (default: `blast.txt`).
- **Max Target Seqs**: Set the maximum number of target sequences to keep for each query (default: 50).
- **Percentage Identity**: Use the slider to set the minimum percentage identity for matches (default: 95%).
- **Query Coverage HSP Percentage**: Set the minimum query coverage as a percentage of the HSP (High-scoring Segment Pair) (default: 95%).
- **Specie Threshold**: Use the slider to set the species-level threshold percentage. All OTUs identified above this cutoff will have valid species-level identification (default: 99%).
- **Genus Threshold**: Use the slider to set the genus-level threshold percentage. All OTUs identified above this cutoff will have valid genus-level identification (default: 97%).
- **Family Threshold**: Use the slider to set the family-level threshold percentage. All OTUs identified above this cutoff will have valid family-level identification (default: 95%).
- **Number of Threads**: Specify the number of threads to use for the BLAST search (default: 6).
- For additional parameters and detailed descriptions, refer to the official [BLASTn](https://www.ncbi.nlm.nih.gov/books/NBK279684/table/appendices.T.blastn_application_options/) manual.

When you click the **"Run BLAST"** button, the input parameters are used to configure and execute a BLAST search using the `refDB_Blast` function. The results of the BLAST search will be outputted according to the specified parameters.

This section of the interface provides a comprehensive set of options to customize the BLAST search, making it highly adaptable to different research needs and data types.

<p align="center">
  <img src="https://github.com/user-attachments/assets/8a75b9e9-10ee-4560-84ad-dd55287eb450" width="600"/>
</p>

- Useful Links
  
  **NCBI:** [blastn application options](https://www.ncbi.nlm.nih.gov/books/NBK279684/table/appendices.T.blastn_application_options/)

### GBIF Data Submission

This section outlines the necessary files and steps for preparing your eDNA data for submission to GBIF, following the Darwin Core (DwC) standard as described [here](https://docs.gbif-uat.org/edna-tool-guide/en/#dwc-standard). This process also adheres to the FAIR data principles, ensuring that your data is Findable, Accessible, Interoperable, and Reusable.

### 1. **Uploading the OTU Table**
   - **Objective:** Start by uploading the OTU table, which contains the occurrence data for the species detected.
   - **Steps:**
     - Use the "Browse..." button to select and upload your OTU table file (e.g., `otu_table.txt`).
     - Click "Load Data" to display the contents of the table and ensure it is loaded correctly.

### 2. **Uploading Taxonomic Assignment and OTU Sequence Data**
   - **Objective:** Link the taxonomic assignment with the OTU sequences, which are essential for identifying species.
   - **Steps:**
     - Upload the taxonomic assignment file (e.g., `taxonomic_assignment.txt`).
     - Upload the OTU sequence file (e.g., `otus.fasta`).
     - Click "Load Data" to visualize the data and confirm the correct mapping between OTUs and taxonomic assignments.

### 3. **Selecting Optional Fields for Metadata**
   - **Objective:** Add additional metadata fields that are relevant for the submission, such as geographic coordinates and occurrence status.
   - **Steps:**
     - Choose from available fields like `decimalLatitude`, `occurrenceStatus`, `locality`, and others.
     - Click "Add Selected Columns" to include these fields in your data.

### 4. **Defining Default Values for Metadata**
   - **Objective:** Set default values for specific metadata terms that are consistent across the dataset.
   - **Steps:**
     - Input default values for terms like `env_medium`, `target_gene`, and `otu_db`.
     - Ensure that these values align with the requirements for GBIF submission.

### 5. **Saving the Prepared Table**
   - **Objective:** Save the prepared OTU table, taxonomic information, and associated metadata as required files for GBIF submission.
   - **Steps:**
     - Click "Save Table" after each preparation step to ensure all data is properly saved.
     - The final output should be a set of files that include the OTU table, taxonomic assignments, and metadata, formatted according to the DwC standard.

### Important Note
The files formats described above are essential for preparing your metabarcoding dataset for GBIF submission. They are aligned with the Darwin Core (DwC) standard, which ensures that your eDNA data is correctly formatted and ready for integration into GBIF’s global biodiversity database. Additionally, by following these guidelines, you ensure that your data complies with the FAIR principles (Findable, Accessible, Interoperable, Reusable), making it more valuable and usable by the broader scientific community.

<p align="center">
  <img src="https://github.com/user-attachments/assets/9b9ae398-53ba-4aa2-bc2d-e85eba053e19" width="600"/>
</p>

For more details on the DwC standard and GBIF submission requirements, refer to the [GBIF eDNA Tool Guide](https://docs.gbif-uat.org/edna-tool-guide/en/#dwc-standard).

## Overview

<p align="center">
  <img src="https://github.com/user-attachments/assets/62b03877-6653-44be-83be-8fa468aa69c8" width="600"/>
</p>

- **Shiny Integration**: All functionalities run within an interactive Shiny app for seamless analysis and visualization.
- **Preprocessing**: Preprocess biodiversity data using various cleaning and validation steps.
- **Taxonomic Filter**: Advanced taxonomic name parsing and cleaning using multiple sources.
- **Spatial Filter**: Perform spatial validations and clean coordinates.
- **Temporal Filter**: Filter data based on temporal thresholds.
- **Interactive Map**: This stage allows for manual curation based on iterative visualization. You can curate the data manually based on a predefined area from GBIF download or by uploading a SHP file.
- **Data Saving**: Save cleaned data in multiple formats from each tab. Available formats include:
  - **Shapefile (SHP)**: Ideal for spatial data and widely used in GIS applications. Use this format when you need to perform spatial analyses in GIS software like ArcGIS or QGIS. For example, mapping species distribution across different regions.
  - **GeoJSON**: A lightweight data format that is easy to share and integrate with web mapping applications. Use this format for online visualization of species distribution on platforms like Leaflet or Mapbox.
  - **GPKG (GeoPackage)**: An open format that supports large datasets and stores both vector and raster data in a single file. Use this format for comprehensive datasets that include multiple layers of biological data, such as habitats, species distributions, and environmental variables.
  - **KML**: Suitable for visualizing geographic data in Google Earth. Use this format to create interactive maps for presentations or reports, showing migration patterns or habitat ranges of species.
  - **CSV**: Versatile and widely used for tabular data. Ideal for statistical analysis and data sharing. Use this format to export cleaned occurrence data for further analysis in software like R or Excel, or to share datasets with collaborators.
- **Database Creation**: Once the data is cleaned and filtered, it is organized into a well-structured database, ready for use in various analyses.
- **Molecular Identification**: Following database creation, the data is used for molecular identification processes, enabling the accurate classification of species based on genetic information.
- **FAIR Submission**: The final processed data is submitted following the FAIR principles (Findable, Accessible, Interoperable, Reusable), ensuring that it is well-documented, easily accessible, and can be reused in future research.

Each of these formats has its unique advantages depending on the type of analysis, visualization, or data sharing needs, allowing for flexible use of the processed biodiversity data.

## Contributing

Contributions are welcome and appreciated! To contribute to this project, please follow these steps:

1. **Install Git**: Make sure you have Git installed on your system. You can download it from [here](https://git-scm.com/). To install Git on Windows, follow these steps:
   - Download the Git installer from [Git](https://gitforwindows.org/) for Windows.
   - Run the installer and follow the default settings.
   - Open Command Prompt and verify the installation by running:
     ```cmd
     git --version
     ```
2. **Fork the repository**: Click the "Fork" button on the upper right corner of the repository page.
3. **Clone your fork**: 
     ```cmd
     git clone https://github.com/fabricioA14/refDBdelimiter.git
     ```

## License

MIT License

© 2024 Fabricio dos Anjos Santa Rosa

<p align="justify">
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
</p>

<p align="justify">
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
</p>

<p align="justify">
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
</p>
