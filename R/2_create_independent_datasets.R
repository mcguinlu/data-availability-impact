# Set parameters

n_rev <- 2
n_subset <- 200

# PRIMARY DATASET ---------------------------------------------------------

# Analyses: P1 & P2
# Requires whole dataset (4101 records), but reviewers must be blind to metadata

df <- read.csv("data/data-avail.csv", stringsAsFactors = FALSE)

df_blind <- df %>%
  dplyr::select(ID, data_avail) %>%
  mutate(decision = as.character(""))

for (rev in 1:n_rev) {
  rio::export(df_blind,
              paste0("data/raw/reviewer",rev,"/full_blind_reviewer",rev,".xlsx"),
              row.names = FALSE)
}


# SECONDARY DATASETS ------------------------------------------------------
# Independent - do not depend on decisions from primary dataset

# S1
# Does coding of DAS differ between preprint and published?
df <- read.csv("data/data-avail.csv",
               stringsAsFactors = FALSE)

# Set seed for reproducibility
set.seed(42)

df_s1 <- df %>%
  # Filter to those that were published
  dplyr::filter(!is.na(published)) %>%
  mutate(published_decision = as.character(""), 
         published_url = paste0("https://www.doi.org/", published),
         tmp = 1:nrow(.)) %>%
  dplyr::filter(tmp %in% sample(1:nrow(.), n_subset)) %>%
  select(ID, title, published_url, published_decision)

class(df_s1$published_url) <- "hyperlink"  

for (rev in 1:n_rev) {
  rio::export(df_s1,
              paste0("data/raw/reviewer",rev,"/s1_blind_reviewer",rev,".xlsx"),
              row.names = FALSE)
}

# S2
# Code availability
# For random sample, how many make code available in PDF but do not mention in DAS?

df <- read.csv("data/data-avail.csv",
               stringsAsFactors = FALSE)

# Set seed for reproducibility
# Set to different to above, for diversity of records
set.seed(41)

df_s2 <- df %>%
  # Filter to those that were published
  dplyr::filter(!is.na(published)) %>%
  mutate(code_DAS_decision = as.character(""), 
         code_PDF_decision = as.character(""), 
         preprint_pdf = paste0("https://www.medrxiv.org/content/",doi,"v",version,".full.pdf"),
         tmp = 1:nrow(.)) %>%
  dplyr::filter(tmp %in% sample(1:nrow(.), n_subset)) %>%
  select(ID, title, data_avail, preprint_pdf, code_DAS_decision, code_PDF_decision)

class(df_s2$preprint_pdf) <- "hyperlink"  

for (rev in 1:n_rev) {
  rio::export(df_s2,
              paste0("data/raw/reviewer",rev,"/s2_blind_reviewer",rev,".xlsx"),
              row.names = FALSE)
}