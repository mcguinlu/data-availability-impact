# GET RECORDS -------------------------------------------------------------

# Limit to those records posted on medRxiv up to & including 1st May 2020
# Using medRxiv snapshot 2020-05-26 14:16
# Returns 4101 records

snap_url <- paste0("https://raw.githubusercontent.com/mcguinlu/medrxivr-data/",
                   "795081b8895faba1868e4978acfc725c456d0449/",
                   "snapshot.csv") 
             
df <- medrxivr::mx_search(
  data = read.csv(snap_url,
                  stringsAsFactors = FALSE),
  query = "*",
  to_date = 20200501
)


# SCRAPE DATA AVAILABILITY STATEMENTS -------------------------------------

# Generate new empty variable to hold availability statements
df$data_avail <- character(nrow(df))

# Extract availability statements by browsing to the webpage for each record
for (link in 1:nrow(df)) {
  message(paste0("Record ",link," of ", nrow(df)))
  
  # ?versioned=TRUE ensures that the link resolves to the correct version
  # As the availability statements don't seem to vary by version, remove this
  base_url <- gsub("\\?versioned=TRUE","",df$link_page[link])
  
  # Download ".external-links" page for the record, allowing for 503 errors
  try <- 1
  while (try <= 5) {
    message(paste0("Try ", try))
    Sys.sleep(runif(1,10,13))
    page <- try(read_html(paste0(base_url,".external-links")))
    if (!is(page, 'try-error')) break
    try <- try + 1
  }
  
  if (is(page, 'try-error')) next
  
  # Capture, clean and coerce text in ".external-links-view" node to single cell
  df$data_avail[link] <- page %>%
    html_node(".external-links-view") %>%
    gsub("\n"," ", .) %>%
    str_extract_all(., "(?<=\\>)[^<\n]+", ) %>%
    .[[1]] %>% 
    paste(., collapse = " ") %>%
    gsub("Data Availability ","", ., ignore.case = FALSE) %>%
    trimws()
  
  # Process takes a while, due to delay between scrapes
  # Every 100 records, save to a temporary file
  if (link%%100 == 0) {
    write.csv(df, "data/data-avail-tmp.csv", row.names = FALSE)
  }
  
}

# Save final dataframe to CSV file
write.csv(df, "data/data-avail.csv", row.names = FALSE)

# ADD MANUALLY EXTRACTED PUBLISHED DAS ------------------------------------
# This is used to create the S2 datasets in "2_create_independent_datasets.R"

df <- read.csv("data/data-avail.csv", stringsAsFactors = FALSE)

# Read in manually extracted published DAS
rev1 <- rio::import("data/complete/published_DAS_extraction/published_DAS_extraction_rev1.xlsx")

rev2 <- rio::import("data/complete/published_DAS_extraction/published_DAS_extraction_rev2.xlsx")

pub_df <- rbind(rev1,rev2) %>%
  mutate(published_DAS = ifelse(published_DAS == "9", "9 - None", published_DAS)) %>%
  select(ID,published_DAS)

# Perform checks on published DAS:
# add publisher info and group by journal, to check that all have/don't have a
# DAS (helps to validate extraction)

df <- merge(df, pub_df, all.x = TRUE)

# Save 
write.csv(df, "data/complete/published_DAS_extraction/data-avail_with_pub_DAS.csv", row.names = FALSE)
