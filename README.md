
# GIGEM

GIGEM (Group Isolation Gauge Effect Monitoring) is a user-friendly Shiny application developed in R for analyzing and visualizing data from Drosophila Activity Monitors (DAM, TriKinetics). The application is designed to streamline data processing and enable researchers to focus on insights and analyses.

## Features

### Automated Detection and Exclusion

- Automatically detects and excludes data from deceased animals, ensuring cleaner datasets and accurate analyses.

### Customizable Analysis Options

- **User-defined Time Ranges:** Specify time ranges to focus on specific periods of interest.
- **Monitor Exclusion:** Easily exclude data from malfunctioning monitors.
- **Treatment Group Comparison:** Compare activity data between different treatment groups.

### Interactive Data Management

- **Filtering Options:** Intuitive filtering tools to tailor datasets according to your needs.
- **Data Selection:** Dynamically select and explore subsets of the data for targeted analysis.

## Current Status

This project is still a work in progress. We are actively working to enhance features and improve user experience.

## Requirements

To use GIGEM, you need:

- R (version 4.0 or later)
- The `shiny` package

Additional required packages will be listed in the project files.

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/GIGEM.git
   ```
2. Open the project in RStudio.
3. Run the Shiny application:
   ```R
   shiny::runApp()
   ```

## Usage

1. Upload your DAM data in the specified format.
2. Use the interface to apply filters, exclude monitors, and customize time ranges.
3. Compare treatment groups and visualize results interactively.

## Included Data

- **Monitor_Files:** Contains all current files related to the monitoring setup.
- **generated_files:** Files generated during application use, including processed outputs.
- **uploaded_files:** User-uploaded files for analysis.
- **Practice Data:** All `.txt` files included are practice datasets derived from DAM monitors.

## Roadmap

- **Documentation:**

  - Develop a comprehensive user manual, including example outputs, plots, and explanations of generated data.
  - Create a detailed `README.md` or `README.html` to guide users on using GIGEM.

- **Enhancements to Normalization and Analysis:**

  - Normalize data based on group types (e.g., sleep time, n bouts, latency) without combining incompatible variables.
  - Address current issues:
    - Enable functionality for downloading Activity and Sleep bar plots.
    - Assist users in selecting appropriate normalization groups.
    - Ensure changes in manually omitted cuvettes are reflected in analyses.
    - Fix the non-functional "Normalize" radio button in the Plot Data tab.

- **Visualization Improvements:**

  - Allow customization of sleep population plots by variables such as sex, treatment, or environment.
  - Improve plot readability by removing gray grid backgrounds and adjusting plot dimensions.
  - Update x-axis labels to "Zeitgeber Time (h)."

- **Metadata Management:**

  - Add options to upload and process multiple files simultaneously.
  - Enable users to omit specific cuvettes or apply consistent metadata settings across monitors.
  - Allow metadata files to be saved with user-defined names.

- **Future Features:**

  - Implement functionality to omit regions for specific monitors.
  - Develop methods for custom dead-animal removal with flexible analysis timeframes.
  - Expand plotting options for normalized and statistical values, tailored to manuscript requirements.

## Acknowledgments

Special thanks to the developers and maintainers of R and Shiny for providing powerful tools for data visualization and application development.

We gratefully acknowledge the contributions of **Esther Doria** and the guidance of our research lab principal investigator, **Wanhe Li**.
