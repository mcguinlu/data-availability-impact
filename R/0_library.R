# INSTALL REQUIRED PACKAGES -----------------------------------------------
if (!require("devtools")) install.packages("devtools") # not installed on this machine 
if (!require("medrxivr")) devtools::install_github("mcguinlu/medrxivr") # not installed on this machine
if (!require("grateful")) devtools::install_github("Pakillo/grateful") # not installed on this machine
if (!require("rvest")) install.packages("rvest") # not installed on this machine
if (!require("stringr")) install.packages("stringr") # not installed on this machine
if (!require("dplyr")) install.packages("dplyr") # not installed on this machine
if (!require("rio")) install.packages("rio") # not installed on this machine
if (!require("here")) install.packages("here") # not installed on this machine
if (!require("flextable")) install.packages("flextable") # not installed on this machine
if (!require("ggplot2")) install.packages("ggplot2") # not installed on this machine
if (!require("patchwork")) install.packages("patchwork") # not installed on this machine
if (!require("RColorBrewer")) install.packages("RColorBrewer") # not installed on this machine

# LIBRARY CALLS -----------------------------------------------------------
library(medrxivr) # Access MedRxiv Preprint Data
library(rvest) # Easily Harvest (Scrape) Web Pages
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(dplyr) # A Grammar of Data Manipulation
library(rio) # A Swiss-Army Knife for Data I/O
library(here) # A Simpler Way to Find Your Files
library(flextable) # Functions for Tabular Reporting
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(patchwork) # The Composer of Plots
library(RColorBrewer) # ColorBrewer Palettes

# GENERATE PACKAGE CITATIONS ----------------------------------------------
# Doesn't need to run everytime
# Running this as part of the 

# pkgs_bib <- grateful::get_citations(grateful::scan_packages())
# 
# pkgs_bib <- unlist(pkgs_bib)
# pkgs_bib <- gsub("Müller","Müller", pkgs_bib)
# pkgs_bib <- gsub("Puspendra Singh <puspendra.pusp22@gmail.com>","and Puspendra Singh", pkgs_bib)
# 
# writeLines(pkgs_bib,
#            here("report","references_packages.bib"),
#            useBytes = TRUE) # Needed to parse special characters
# 
# '%notin%' <- Negate('%in%')
# 
# pkg_cite <- grateful::scan_packages()
# pkg_cite_md <- pkg_cite[pkg_cite %notin% c("medrxivr","rvest","rcrossref")] %>%
#   paste0(collapse = "; @") %>%
#   paste0("[@",.,"]")
# 
# writeClipboard(pkg_cite_md)

