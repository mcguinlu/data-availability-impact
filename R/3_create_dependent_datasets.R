# Set parameters

n_rev <- 2
n_subset <- 200

# SECONDARY DATASETS ------------------------------------------------------
# Dependent - depend on decisions from primary dataset

# s3
# Do preprints make data available on publication?
# Requires complete and consolidated version of the P1 dataset, as this dataset 
# includes only those with a label of 3 ("Available in the future/on publication")

data <- read.csv("data/data-avail.csv",
                 stringsAsFactors = FALSE) 

# Replace with dataframe containing decisions when ready
data_tmp <- rio::import("data/raw/reviewer1/full_blind_reviewer1.xlsx") %>%
  select(ID, decision_consolidated) 

# Merge decisions with full datasets
data_s3 <- data %>%
  merge(data_tmp, by = "ID" )

# Get records with a "available in future" decision based on initial screening, 
# which have subsequently been published

data_s3 <- data_s3 %>%
  filter(decision_consolidated == 3 & !is.na(published)) %>%
  mutate(avail_on_publication_decision = as.character(""),
         published_url = paste0("https://www.doi.org/", published),
  )

# If number of records is > n_subset, select random n_subset
if (nrow(data_s3)>n_subset) {
  set.seed(42)
  df_s3 <- data_s3 %>%
    mutate(tmp = 1:nrow(.)) %>%
    dplyr::filter(tmp %in% sample(1:nrow(.), n_subset)) %>%
    select(-tmp)
}

class(df_s3$preprint_pdf) <- "hyperlink"

data_s3 <- data_s3 %>%
  select(ID, title, published_url, NA_decision)

for (rev in 1:2) {
  rio::export(df_s3,
              paste0("data/raw/reviewer",rev,"/s3_blind_reviewer",rev,".csv"),
              row.names = FALSE)
}
