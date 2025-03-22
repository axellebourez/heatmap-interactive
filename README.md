# Heatmap Interactive Shiny App

## Overview
This interactive Shiny application allows you to visualize Excel data as a dynamic heatmap. With a user-friendly interface, you can upload an Excel file, select the desired sheet, samples, and annotations, and customize correlation and clustering methods. The app provides multiple color palette options and enables you to download your heatmap as PNG or PDF.

## Features
- **Data Upload:** Easily import Excel (.xlsx) files.
- **Sheet & Sample Selection:** Choose the worksheet, samples, and annotation columns for visualization.
- **Customization:** Select correlation methods (Pearson, Spearman, Kendall) and clustering options (e.g., ward.D, complete, etc.).
- **Visualization:** Generate interactive heatmaps using `heatmaply` and `plotly`.
- **Download Options:** Export your heatmap as PNG or PDF.

## Requirements
- R (version 4.x or higher recommended)
- Required R packages:
  - shiny
  - ggplot2
  - dplyr
  - tidyr
  - readxl
  - heatmaply
  - plotly
  - tibble

Install the packages using:
```R
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "readxl", "heatmaply", "plotly", "tibble"))
