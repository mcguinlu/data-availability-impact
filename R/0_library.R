# INSTALL REQUIRED PACKAGES -----------------------------------------------
if (!require("devtools")) install.packages("devtools") 
if (!require("medrxivr")) devtools::install_github("mcguinlu/medrxivr")
if (!require("rvest")) install.packages("rvest")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("rio")) install.packages("rio")

# LIBRARY CALLS -----------------------------------------------------------
library(medrxivr) # Access MedRxiv Preprint Data
library(rvest) # Easily Harvest (Scrape) Web Pages
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(dplyr) # A Grammar of Data Manipulation
library(rio) # A Swiss-Army Knife for Data I/O
