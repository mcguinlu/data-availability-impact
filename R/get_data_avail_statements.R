# SET-UP ------------------------------------------------------------------

# Library calls
library(medrxivr)
library(rvest)
library(stringr)
library(dplyr)

# Limit to those records posted on medRxiv up to & including 1st May 2020
# Using medRxiv snapshot - 2020-05-26 14:16
# Returns 4101 records
df <- mx_search("*", to.date = 20200501)

# Generate new variable to hold availability statements
df$data_avail <- character(nrow(df))

# EXTRACT -----------------------------------------------------------------

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
  
  # Try "#p-13" as identifier of availability statements on the web-page
  df$data_avail[link] <- page %>%
    html_node(".external-links-view") %>%
    str_extract_all(., "(?<=\\>)[^<\n]+", ) %>% 
    .[[1]] %>% 
    paste(., collapse = " ") %>%
    gsub("Data Availability ","",.) 
  
  if (link%%100 == 0) {
    write.csv(df, "data/data-avail-tmp.csv", row.names = FALSE)
  }
  
}


# SAVE --------------------------------------------------------------------

# Save to CSV file
write.csv(df, "data/data-avail.csv", row.names = FALSE)


# ADD DATA ON PUBLISHED PREPRINTS -----------------------------------------

df <- read.csv("data/data-avail.csv", stringsAsFactors = FALSE)

df$publish.print <- character(nrow(df))
df$publish.online <- character(nrow(df))
df$journal <- character(nrow(df))
df$publisher <- character(nrow(df))

for (doi in 1:100) {
  
  if (is.na(df$published[doi])) {
    next
  }
    
  message(paste0("Adding info for DOI ", doi))

  tmp <- rcrossref::cr_works(dois=df$published[doi])
  
  df$publish.print[doi] <- ifelse(is.null(tmp$data$published.print), "",tmp$data$published.print)
  df$publish.online[doi] <- ifelse(is.null(tmp$data$published.online), "",tmp$data$published.online)
  df$journal[doi] <- ifelse(is.null(tmp$data$container.title), "",tmp$data$container.title)
  df$publisher[doi] <- ifelse(is.null(tmp$data$publisher), "",tmp$data$publisher)

}

# SAVE --------------------------------------------------------------------

# Save to CSV file
write.csv(df, "data/data-avail-published.csv", row.names = FALSE)


df <- read.csv("data/data-avail-published.csv", stringsAsFactors = FALSE)

# ADD DATE OF PUBLICATION OF FIRST PREPRINT VERSION -----------------------

mx_results <- mx_search("*", to.date = 20200501, deduplicate = FALSE)
mx_results <- mx_results %>%
  dplyr::group_by(doi) %>%
  dplyr::slice(which.min(version))  %>% 
  mutate(first.date = date) %>% 
  select(doi, first.date)

df <- merge(df, mx_results, by = "doi")


# CLEAN DATES AND CREATE DATE DIFFERENCE VARIABLES -------------------------

df <- df %>% 
  
  # Add day to dates that are missing it
  mutate_at(.vars = vars("publish.print", "publish.online"),
            ~ case_when(
              nchar(.) == 0 ~ "",
              . == "2020" ~ "2020-01-01",
              . == "2019" ~ "2019-01-01",
              nchar(.) > 0 & nchar(.) < 9 ~ paste0(.,"-01"), 
              nchar(.) >= 9 ~ .)) %>%
  
  # Convert date columns to Date format
  mutate_at(.vars = vars("publish.print", "publish.online"),
            ~as.Date(.,format="%Y-%m-%d")) %>%
  mutate_at(.vars = vars("date", "first.date"),
            ~as.Date.character(., format = "%Y%m%d")) %>%
  
  # Replace impossible dates
  # For some of the publish.* dates, they were before the preprint was published
  mutate(publish.print = if_else(publish.print<first.date,publish.online,publish.print),
         publish.online = if_else(publish.online<first.date,publish.print,publish.online)) %>%
  
  # Create minimum date of publication and time to publication variables
  # first.date = date of publication of first version of preprint
  # date = date of publication of last/current version of preprint
  mutate(min_pub_date = pmin(publish.print,publish.online, na.rm = TRUE),
         date_diff = as.numeric(min_pub_date - first.date))


# CHECKS ------------------------------------------------------------------

# Check how many have negative delay to publication
View(df[which(df$date_diff<0),])

# Check how many have have both published and online dates that preceed the preprint date
# The mutate func above results in both being set to NA in this case
# These will need to be manually coded
View(df[which(!is.na(df$published) & is.na(df$publish.print) & is.na(df$publish.online)),])




