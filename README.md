# VECTR-Clasp  
*Automated DeepLabCut data processing and visualisation toolkit for hind-limb clasping analysis*

---

## Overview  
**VECTR-Clasp** is an R package designed to streamline the analysis of DeepLabCut tracking data from mouse hind-limb clasping assays.  
It provides a unified workflow for:

- Extracting and cleaning coordinate data  
- Assessing tracking likelihood quality  
- Quantifying angular dynamics and movement metrics  
- Generating high-quality static and animated visualisations  

This package was developed for reproducible, modular analysis of behavioural datasets.

## Core Commands

Each function can be run independently and accepts a full path to your data directory.

| Function | Purpose | Example |
|-----------|----------|----------|
| `analyse_deeplabcut_data()` | Full pipeline for extraction and preprocessing | `analyse_deeplabcut_data("C:/Users/X/Example Data")` |
| `check_files()` | Checks DeepLabCut tracking quality based on likelihood thresholds | `check_files("C:/Users/X/Example Data")` |
| `analyse_angles()` | Computes circular statistics and swing metrics from angle data | `analyse_angles("C:/Users/X/Example Data/Angle CSVs")` |
| `analyse_movement()` | Calculates total distance, displacement, and frame movement (requires pixel-cm ratio) | `analyse_movement("C:/Users/X/Example Data/Relative CSVs", cm_per_10px = 0.5)` |
| `generate_trackplot()` | Creates an animated trajectory for a single file | `generate_trackplot("C:/Users/X/Example Data/Relative CSVs/KO-1_extracted_snout_and_base-of-tail_relative.csv")` |
| `generate_trackplots_all()` | Generates animated trackplots for all relative CSVs in a folder | `generate_trackplots_all("C:/Users/X/Example Data/Relative CSVs")` |
| `visualise_data()` | Produces static trackplots, 2D density heatmaps, and polar histograms for all mice | `visualise_data("C:/Users/X/Example Data")` |

---

## Output

All results and visualisations are automatically saved in subfolders:

- **Filtered CSVs/** – processed outputs  
- **Analysis/** – angle and movement summaries  
- **Visualisation/** – TIFF images and trackplots  

All plots are exported as **600 dpi TIFFs**, suitable for publication-quality figures.

---
