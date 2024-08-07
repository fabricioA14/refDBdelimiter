<p align="center">
  <img src="https://github.com/fabricioA14/refDBdelimiter/assets/73892283/3a436338-be1e-4c3d-b893-cb6aacd63eb6" alt="refdb" width="230"/>
</p>

<h1 align="center">refDBdelimiter: An R-Based Web Application for Molecular Identification with Geographically Curated eDNA Databases</h1>

## Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Usage](#usage)
- [Features](#features)
- [Examples](#examples)
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

2. **Install the bdc package**:
   - First, install the `bdc` package from CRAN:
     ```r
     install.packages("bdc")
     ```

3. **DuckDB**: The `refDBdelimiter` package also depends on DuckDB for efficient data handling during the taxonomic cleaning stage. To address related issues:
   - Ensure you have the latest version of DuckDB installed.
   - If you encounter an error when trying to read a database file with an incompatible version, follow these steps. This error typically appears in functions like `bdc_query_names_taxadb`, which you might see mentioned in related issues:
     - Delete the existing database directory:
       ```r
       fs::dir_delete(taxadb:::taxadb_dir())
       ```
     - Rerun your database query:
       ```r
       resolvedspplistitis <- bdc_query_names_taxadb(sci_name=specieslist, db = "itis", suggestion_distance = 0.9)
       ```
   These steps are described in the [GitHub](https://github.com/brunobrr/bdc/issues/233) issue.

4. **GNparser**: The package requires `gnparser` for taxonomic name parsing. To resolve installation issues on Windows:
   - Download the appropriate binary file from the [GNparser](https://github.com/gnames/gnparser/releases) releases page.
   - Extract the binary file and move it to the `AppData` directory. This can be done using R:
     ```r
     unzip("path/to/gnparser.zip", exdir = "path/to/destination")
     AppData_path <- Sys.getenv("AppData")
     file.copy("path/to/destination/gnparser", file.path(AppData_path, "gnparser"), recursive = TRUE)
     ```
     
   These steps are described in this [GitHub](https://github.com/brunobrr/bdc/issues/233) issue.

Once these dependencies are met, you can install the `refDBdelimiter` package using the following commands in R:

To install the `refDBdelimiter` package, you can use the following commands in R:

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install refDBdelimiter from GitHub
devtools::install_github("fabricioA14/refDBdelimiter")
```
## Usage

### Preprocessing

<p align="center">
  <img src="https://github.com/user-attachments/assets/ec35b074-8506-4626-94cb-32d7f30e449d" width="600"/>
</p>

### Taxonomic Filter

<p align="center">
  <img src="https://github.com/user-attachments/assets/215551ee-1980-4990-a820-f51be65ed20b" width="600"/>
</p>

### Spatial Filter

<p align="center">
  <img src="https://github.com/user-attachments/assets/1c68ee1b-22f1-4574-8842-b5fec5a0db93" width="600"/>
</p>

### Temporal Filter

<p align="center">
  <img src="https://github.com/user-attachments/assets/9f599568-01bf-44b7-9bd9-9bc5b408754c" width="600"/>
</p>

### Interactive Map

<p align="center">
  <img src="https://github.com/user-attachments/assets/2e04b2cf-c774-44dd-a0a1-2b5c1151504f" width="600"/>
</p>

### Make Database

To create a database, you need to acquire\define these files:

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

## Features

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
     git clone https://github.com/your-username/refDBdelimiter.git
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
