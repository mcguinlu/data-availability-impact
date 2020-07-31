# Set parameters

source("R/0_library.R")

n_rev <- 2

# Take sample of 10%, or total number eligible if smaller
n_subset <- 400

# Create notin function
'%notin%' <- Negate("%in%")

###########################################################################
# PILOTING SUBSET ---------------------------------------------------------

# Set seed for reproducibility
set.seed(99)

# Take random sample of 101 records, from total dataset of 4101
df <- read.csv("data/data-avail.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(ID, data_avail) %>%
  mutate(decision = as.character(""),
         tmp = 1:nrow(.)) %>%
  dplyr::filter(tmp %in% sample(1:nrow(.), 101)) %>%
  select(-tmp)

for (rev in 1:n_rev) {
write.csv(df, 
          here("data","raw","subset",paste0("subset_101_rev",rev,".csv")),
          row.names = FALSE)
}

###########################################################################
# PRIMARY DATASET ---------------------------------------------------------

# Requires whole dataset (4101 records), but reviewers must be blind to all
# metadata (title, abstract, authors, institution, preprint DOI, published DOI,
# etc)

df <- read.csv("data/data-avail.csv", stringsAsFactors = FALSE)

df_blind <- df %>%
  dplyr::select(ID, data_avail) %>%
  mutate(decision = as.character(""))

for (rev in 1:n_rev) {
  rio::export(df_blind,
              paste0("data/raw/reviewer",rev,"/full_blind_reviewer",rev,".xlsx"),
              row.names = FALSE)
}

# ADD DATA ON PUBLISHED PREPRINTS -----------------------------------------

# Read in data
df <- read.csv("data/data-avail.csv", stringsAsFactors = FALSE)

# Add empty holding variables
df$publish.print <- character(nrow(df))
df$publish.online <- character(nrow(df))
df$journal <- character(nrow(df))
df$publisher <- character(nrow(df))

# Cycle through records
for (doi in seq_len(nrow(df))) {
  
  # If there is no published DOI, skip that record
  if (is.na(df$published[doi])) {
    next
  }
  
  message(paste0("Adding info for DOI ", doi))
  
  # Pass published DOI to rcrossref to extract information
  tmp <- rcrossref::cr_works(dois=df$published[doi])
  
  # 
  df$publish.print[doi] <- ifelse(is.null(tmp$data$published.print), "",tmp$data$published.print)
  df$publish.online[doi] <- ifelse(is.null(tmp$data$published.online), "",tmp$data$published.online)
  df$journal[doi] <- ifelse(is.null(tmp$data$container.title), "",tmp$data$container.title)
  df$publisher[doi] <- ifelse(is.null(tmp$data$publisher), "",tmp$data$publisher)
  
}

# Save final dataframe to CSV file
write.csv(df, "data/data-avail-published.csv", row.names = FALSE)


###########################################################################
# SECONDARY DATASETS ------------------------------------------------------
# Secondary Question 1 ----------------------------------------------------
# Does coding of DAS differ between preprint and published versions?
df <- read.csv("data/raw/published_DAS_extraction/data-avail_with_pub_DAS.csv",
               stringsAsFactors = FALSE)

data_s1 <- df %>%
  # Filter to those that were published
  dplyr::filter(!is.na(published)) 

if (nrow(data_s1)>n_subset) {
  set.seed(42)
  df_s1 <- data_s1 %>%
    mutate(tmp = 1:nrow(.)) %>%
    dplyr::filter(tmp %in% sample(1:nrow(.), n_subset)) %>%
    select(-tmp)
} 

df_s1 <- df_s1 %>%
 # Filter to those with a data availability statement
  dplyr::filter(published_DAS != "9 - None") %>%
  mutate(published_decision = as.character("")) %>%
  select(ID, published_DAS, published_decision)

for (rev in 1:n_rev) {
  rio::export(df_s1,
              paste0("data/raw/reviewer",rev,"/s1_blind_reviewer",rev,".xlsx"),
              row.names = FALSE)
}

# Secondary Question 2 ----------------------------------------------------
# Code availability
# For random sample (n=400), how many refer to code availability in full-text PDF
# but do not mention it in DAS?

df <- read.csv("data/data-avail.csv",
               stringsAsFactors = FALSE)

# Set seed for reproducibility
# Set to different to above, for diversity of records
set.seed(42)

df_s2_full <- df %>%
  mutate(code_DAS_decision = as.character(""), 
         code_PDF_decision = as.character(""), 
         preprint_pdf = paste0("https://www.medrxiv.org/content/",doi,"v",version,".full.pdf"),
         tmp = 1:nrow(.)) %>%
  dplyr::filter(tmp %in% sample(1:nrow(.), n_subset)) 

df_s2 <- df_s2_full %>%
  select(ID, title, data_avail, preprint_pdf, code_DAS_decision, code_PDF_decision)

class(df_s2$preprint_pdf) <- "hyperlink"  

for (rev in 1:n_rev) {
  rio::export(df_s2,
              paste0("data/raw/reviewer",rev,"/s2_blind_reviewer",rev,".xlsx"),
              row.names = FALSE)
}

# Download copies of PDFs for the 400 records in this set. Means that reviewer's
# can work offline if needs be. Use modified mx_download_ID() function to save
# by unique ID rather than DOI and version number (default
# medrxivr::mx_download() behaviour). mx_download_ID() is defined in the
# 0_library.R file
mx_download_ID(df_s2_full,
               directory = here("data","s2_pdf"),
               create = TRUE,
               print_update = 50)
