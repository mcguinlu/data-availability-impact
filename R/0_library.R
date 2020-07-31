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



# LOCAL SCRIPTS -----------------------------------------------------------
# This is a  modifed version of the medrxvir::mx_download() function, which
# saves PDFs using their ID as the filename rather than their DOI and version
# number.

mx_download_ID <- function(mx_results,
                        directory,
                        create = TRUE,
                        print_update = 10){
  
  mx_results$filename <- mx_results$ID
  mx_results$filename <- gsub("/","-",mx_results$filename)
  
  print(paste0("Estimated time to completion: ",
               round(length(mx_results$link_pdf)*13/60/60, 2), " hours"))
  
  if(!file.exists(directory)  && create){
    dir.create(file.path(directory))
  }
  
  # Add trailing forward slash to the directory path
  if(substr(directory,nchar(directory), nchar(directory)) != "/"){
    directory <- paste(directory,"/",sep="")
  }
  
  number <- 1
  
  for (file_location in mx_results$link_pdf) {
    if (file.exists(paste0(directory,
                           mx_results$filename[which(mx_results$link_pdf ==
                                                     file_location)],
                           ".pdf"))) {
      message(paste0("PDF for ID ",
                     mx_results$filename[which(mx_results$link_pdf ==
                                                 file_location)],
                     " already downloaded."))
      
      number <- number + 1
      
      next
    }
    
    while (TRUE) {
      message(paste0("Downloading PDF ",
                     number,
                     " of ",
                     length(mx_results$link_pdf),
                     " (DOI: ",
                     mx_results$filename[which(mx_results$link_pdf ==
                                                 file_location)],
                     "). . . "))
      
      sleep_time <- runif(1, 10, 13)
      Sys.sleep(sleep_time)
      
      pmx_results <-
        try(download.file(
          url = file_location,
          destfile = paste0(directory, mx_results$filename[number], ".pdf"),
          method = "auto",
          mode = "wb"
        ))
      if (!is(pmx_results, 'try-error'))
        break
    }
    
    
    
    if ((number %% print_update == 0) == TRUE) {
      message(paste0(
        "PDF ",
        number,
        " of ",
        length(mx_results$link_pdf),
        " downloaded! (",
        round(number / length(mx_results$link_pdf) * 100, 0),
        "%) "
      ))
    }
    
    number <- number + 1
    
  }
}

