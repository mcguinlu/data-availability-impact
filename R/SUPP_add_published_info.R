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



# ADD DATE OF PUBLICATION OF FIRST PREPRINT VERSION -----------------------

df <- read.csv("data/data-avail-published.csv", stringsAsFactors = FALSE)

#################
#I think this should be latest version not first version!

mx_results <- mx_search(query = "*", to.date = 20200501, deduplicate = FALSE)
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

write.csv(df,"data/full_raw.csv", row.names = FALSE)