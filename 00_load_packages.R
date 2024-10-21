# -------------------------------------------------------------------------
## Script for load packages
## Author: Amanda Pena
## Project: Gráficos Super Salários
## Date: 05/09/2024
# -------------------------------------------------------------------------
# 1- Load necessary libraries ----------------------------------------------


# install.packages("pacman")

packages <- c("readr", "here", "data.table", "dplyr", "stringr", "tidyverse", "magrittr",
              "writexl", "readr", "rdrobust", "ggplot2", "MASS",  "haven", "pBrackets",
              "rdd","splines", "Hmisc", "huxtable", "janitor", "modelsummary", "lubridate","latex2exp",
              "forcats", "ggthemes", "stringdist", "scales", "openxlsx", "scales"
)
# If the package is not installed, then install it 
if (!require("pacman")) install.packages("pacman")

# Change to install = TRUE to install the required packages 
pacman::p_load(packages, character.only = TRUE, install = TRUE)
