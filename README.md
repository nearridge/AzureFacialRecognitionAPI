# AzureFacialRecognition

GUI for the Microsoft Azure Face API. Focuses on emotion analysis. Emotion analysis created for Echelon Insights in Fall 2020 by Neeraj Sharma.

### Required Packages

```
library(tidyverse)
library(httr)
library(scales)
library(here)
library(glue)
library(svMisc)
library(patchwork)
library(jsonlite)
library(ggpubr)
library(jpeg)
```

### Required Software

[FFmpeg](https://ffmpeg.org)

After downloading, it is necessary to modify the filepath in line 30 of `main.R` to properly reflect the location of the FFmpeg download. 

### Compatible Media Types

.mp4 and .MOV

Modification of the `legal_filetypes` parameter will allow for additional media formats to be explored.

### File Structure

This application assumes a subfolder called `Video` where all media resources will lie. All application resouces are hosted from `app.R`. All functions are nested in `main.R`.
