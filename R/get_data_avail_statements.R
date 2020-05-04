# SET-UP ------------------------------------------------------------------

# Library calls
library(medrxivr)
library(rvest)


# Limit to those records posted on medRxiv up to & including 31st Dec 2019
df <- mx_search("*", to.date = 20191231)

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
    html_node("#p-13") %>%
    html_text()
  
  # If "#p-13" holds data statement, moves to next record
  # Otherwise repeat this process for "#p-14", "#p-15", "#p-16"
  if (!is.na(df$data_avail[link])) {
    next
  }
  
  # Try "#p-14"
  df$data_avail[link] <- page %>%
    html_node("#p-14") %>%
    html_text()
  
  if (!is.na(df$data_avail[link])) {
    next
  }
  
  # Try "#p-15"
  df$data_avail[link] <- page %>%
    html_node("#p-15") %>%
    html_text()
  
  if (!is.na(df$data_avail[link])) {
    next
  }
  
  # Try "#p-16"
  df$data_avail[link] <- page %>%
    html_node("#p-16") %>%
    html_text()
  
  if (!is.na(df$data_avail[link])) {
    next
  }
  
}


# SAVE --------------------------------------------------------------------

# Save to CSV file
write.csv(df, "data/data-avail.csv", row.names = FALSE)
