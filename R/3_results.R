# Import data -------------------------------------------------------------

# Read in preprint file
df_pre <- rio::import(here("data",
                       "complete",
                       "consolidated",
                       "full_blind_both.xlsx")) %>%
  dplyr::select(ID, data_avail, consolidated)

colnames(df_pre)[3] <- "preprint_decision"

# Read in published file
df_pub <- rio::import(here("data",
                           "complete",
                           "consolidated",
                           "s1_blind_both.xlsx")) %>%
  dplyr::filter(published_DAS!="XXXX - Abstract") %>%
  dplyr::select(ID, published_DAS, consolidated)

colnames(df_pub)[3] <- "published_decision"

# Read in details file
df_reference <- read.csv(here("data",
                        "data-avail-published.csv"), stringsAsFactors = FALSE)

# INITIAL DATA WRANGLING --------------------------------------------------

# Calculate Inter Rater Reliability (IRR)
df_irr <-rio::import(here("data",
                                     "complete",
                                     "consolidated",
                                     "full_blind_both.xlsx")) %>%
  dplyr::select(rev1_decision, rev2_decision)

kappa <- round(irr::kappa2(df_irr)$value,2)



df <- merge(df_pub, df_pre, all.x=TRUE) %>%
  # Create 0/1 variable to indicate an "open" vs "closed" statement
  mutate(
    preprint_decision_group = case_when(
    preprint_decision %in% c(1,2,3,4,5,6) ~ 0, # Closed
    preprint_decision %in% c(7,8) ~ 1, # Open
    preprint_decision == 0 ~ 9 # Not applicable
  ),
  published_decision_group = case_when(
    published_decision %in% c(1,2,3,4,5,6) ~ 0, # 
    published_decision %in% c(7,8) ~ 1,
    published_decision == 0 ~ 9
  )
  )  %>%
  filter(preprint_decision_group!=9,
         published_decision_group!=9) %>%
  mutate(change = published_decision_group - preprint_decision_group)

df_both <- merge(df_pub, df_pre, all.y=TRUE)

n_records <- df_reference %>%
  nrow()

n_pub_no_format <- df_reference %>%
  filter(is.na(published)==FALSE) %>%
  nrow()

n_pub <- n_pub_no_format %>%
  paste0(" (", round(./n_records*100,1),"%)")

n_pub_das <- df_reference %>%
  merge(df_pub, all.x = TRUE) %>%
  filter(is.na(published_DAS)==FALSE & published_DAS != "XXXX - Abstract") %>%
  nrow() %>%
  paste0(" (", round(./n_pub_no_format*100,1),"%)")

n_excluded_pre <- df_pre %>%
  filter(preprint_decision == 0) %>%
  nrow() %>%
  paste0(" (", format(round(./n_records*100,1),nsmall = 1),"%)")

n_total_pre <- df_pre %>%
  filter(preprint_decision != 0) %>%
  nrow()

n_available_pre <- df_pre %>%
  filter(preprint_decision != 0 & preprint_decision %in% c(7,8)) %>%
  nrow() %>%
  paste0(" (", round(./n_total_pre*100,1),"%)")

n_available_pre_abstract <- df_pre %>%
  filter(preprint_decision != 0 & preprint_decision %in% c(7,8)) %>%
  nrow() %>%
  paste0(" (", round(./n_records*100,1),"%)")

n_not_available_pre <- df_pre %>%
  filter(preprint_decision != 0 & preprint_decision %in% c(1:6)) %>%
  nrow() %>%
  paste0(" (", round(./n_total_pre*100,1),"%)")

n_not_available_abstract <- df_pre %>%
  filter(preprint_decision != 0 & preprint_decision %in% c(1:6)) %>%
  nrow() %>%
  paste0(" (", round(./n_records*100,1),"%)")

# Published

df_pub <- df_pub %>%
  filter(published_DAS != "XXXX - Abstract")

n_pub_denominator <- df_pub %>%
  nrow()

n_pub_denominator_format <- df_pub %>%
  nrow() %>%
  paste0(" (", format(round(./n_pub_no_format*100,1), nsmall = 1),"%)")

n_pub_denominator_abstract <- df_pub %>%
  nrow() %>%
  paste0(" (", round(./n_records*100,1),"%)")

n_excluded_pub <- df_pub %>%
  filter(published_decision == 0) %>%
  nrow() %>%
  paste0(" (", round(./n_pub_denominator*100,1),"%)")

n_total_pub <- df_pub %>%
  filter(published_decision != 0) %>%
  nrow()

n_available_pub <- df_pub %>%
  filter(published_decision != 0 & published_decision %in% c(7,8)) %>%
  nrow() %>%
  paste0(" (", round(./n_total_pub*100,1),"%)")

n_available_pub_abstract <- df_pub %>%
  filter(published_decision != 0 & published_decision %in% c(7,8)) %>%
  nrow() %>%
  paste0(" (", round(./n_pub_denominator*100,1),"%)")

n_not_available_pub <- df_pub %>%
  filter(published_decision != 0 & published_decision %in% c(1:6)) %>%
  nrow() %>%
  paste0(" (", round(./n_total_pub*100,1),"%)")

n_not_available_pub_abstract <- df_pub %>%
  filter(published_decision != 0 & published_decision %in% c(1:6)) %>%
  nrow() %>%
  paste0(" (", round(./n_pub_denominator*100,1),"%)")


n_comparison <- df %>%
  nrow() %>%
  paste0(" records (", round(./n_records*100,1),"% of the total sample of ",n_records," records)")  

n_comparison_abstract <- df %>%
  nrow()


# MAIN TABLE AND LONG FORM SUPPLEMENTARY TABLE ----------------------------

# Read in policy details
df_policy <- rio::import(here("data",
                              "complete",
                              "consolidated",
                              "journal_data_policies_both.xlsx")) %>%
  mutate(policy_code = consolidated) %>%
  dplyr::select(journal, policy_code)

# Main S1 table
df_s1 <- merge(df, df_reference, all.x = TRUE) %>%
  merge(df_policy, by = "journal") %>%
  group_by(policy_code) %>% 
  summarise(number_journals = length(unique(journal)),
            number = n(),
            pub_open = sum(published_decision_group==1),
            preprint_open = sum(preprint_decision_group==1),
            pub_closed = sum(published_decision_group==0),
            preprint_closed = sum(preprint_decision_group==0),
            preprint_prop = paste0(round(preprint_open/number*100, 1),"% (",preprint_open,")"),
            pub_prop =  paste0(round(pub_open/number*100,1),"% (",pub_open,")"),
            change_open = sum(change>0),
            change_closed = sum(change<0),
            change_none = sum(change==0)) %>%
  mutate(policy_code = ifelse(policy_code == 1, "Mandates open data", "Does not mandate open data")) %>%
  dplyr::select(-c(pub_open, pub_closed, preprint_open, preprint_closed))

colnames(df_s1) <-
  c("Journal data\n sharing policy",
    "number_journals",
    "Preprinted records subsequently published\n (N)",
    "Open DAS in\n preprinted version\n % (N)",
    "Open DAS in\n published version\n % (N)",
    "More open\n (N)",
    "More closed\n (N)",
    "No change\n (N)"
  )

ft <- df_s1 %>%
  dplyr::select(-number_journals) %>%
  flextable() %>%
  add_header(
    "Journal data\n sharing policy" = "Journal data\n sharing policy",
    "Preprinted records subsequently published\n (N)" = "Preprinted records subsequently published\n (N)",
    "Open DAS in\n preprinted version\n % (N)" = "Open DAS in\n preprinted version\n % (N)",
    "Open DAS in\n published version\n % (N)" = "Open DAS in\n published version\n % (N)",
    "More open\n (N)" = "Change in DAS from preprint to publication",
    "More closed\n (N)" = "Change in DAS from preprint to publication",
    "No change\n (N)" = "Change in DAS from preprint to publication"
  ) %>%
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  border(
    part = "header",
    border.top = officer::fp_border(color = "black", width = 1),
    border.bottom = officer::fp_border(color = "black", width = 1)
  ) %>%
  border(
    part = "header",
    j = 1:4,
    border.right = officer::fp_border(color = "#A6A6A6", width = 1)
  ) %>%
  bg(bg = "#A6A6A6", part = "header") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  align(align = "center", part = "all") %>%
  bg(i = ~ seq(from = 1, to = nrow(df_s1)) %% 2 == 0,
     bg = "#DDDDDD",
     part = "body") %>%
  add_footer_lines("DAS: Data availability statement") %>%
  fontsize(size = 8, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  align(j = 1, align = "left", part = "all") %>%
  border(
    part = "all",
    border.right = officer::fp_border(
      color = "black",
      style = "dotted",
      width = 0.25
    ),
    j = c(1, 2, 4)
  )

(ft_s1 <- ft)

# Create values for S1 for use in manuscript
n_policy_open <- df_s1[2,3] %>%
  paste0(" (", round(./n_comparison_abstract*100,1),"%)")

n_policy_closed <- df_s1[1,3] %>%
  paste0(" (", round(./n_comparison_abstract*100,1),"%)")

open_at_pre_not <- gsub(" \\([[:digit:]]+\\)","", as.character(df_s1[1,4]))
open_at_pub_not <- gsub(" \\([[:digit:]]+\\)","", as.character(df_s1[1,5]))
open_at_pre_req <- gsub(" \\([[:digit:]]+\\)","", as.character(df_s1[2,4]))
open_at_pub_req <- gsub(" \\([[:digit:]]+\\)","", as.character(df_s1[2,5]))

n_more_closed <- as.character(df_s1[1,7])


# First supplemental table for S1
df_s1_supp1 <- merge(df, df_reference, all.x = TRUE) %>%
  merge(df_policy, by = "journal") %>%
  mutate(policy_code = ifelse(policy_code == 1, "Mandates open data","Does not mandate open data")) %>%
  group_by(journal) %>%
  summarise(number = n(),
            pub_open = sum(published_decision_group==1),
            preprint_open = sum(preprint_decision_group==1),
            preprint_prop = paste0(round(preprint_open/number*100,1),"% (",preprint_open,")"),
            pub_prop =  paste0(round(pub_open/number*100,1),"% (",pub_open,")"),
            change_open = sum(change>0),
            change_closed = sum(change<0),
            change_none = sum(change==0), 
            policy_code = first(policy_code)) %>%
  dplyr::select(-c(pub_open,preprint_open)) %>%
  arrange(journal) %>%
  dplyr::select(journal, policy_code, everything())

colnames(df_s1_supp1) <-
  c("Journal",
    "Open data policy",
    "Number of records\n (N)",
    "Open at preprint\n % (N)",
    "Open at publication\n % (N)",
    "More open\n (N)",
    "More closed\n (N)",
    "No change\n (N)"
    
  )

ft <- flextable(df_s1_supp1) %>%
  add_header(
    "Journal" = "Journal",
    "Open data policy" = "Open data policy",
    "Number of records\n (N)" = "Number of records\n (N)",
    "Open at preprint\n % (N)" = "Open at preprint\n % (N)",
    "Open at publication\n % (N)" = "Open at publication\n % (N)",
    "More open\n (N)" = "Change from preprint to publication",
    "More closed\n (N)" = "Change from preprint to publication",
    "No change\n (N)" = "Change from preprint to publication"
  ) %>%
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  border(
    part = "header",
    border.top = officer::fp_border(color = "black", width = 1),
    border.bottom = officer::fp_border(color = "black", width = 1)
  ) %>%
  border(
    part = "header",
    j = 1:5,
    border.right = officer::fp_border(color = "#A6A6A6", width = 1)
  ) %>%
  bg(bg = "#A6A6A6", part = "header") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  align(align = "center", part = "all") %>%
  bg(i = ~ seq(from = 1, to = nrow(df_s1_supp1)) %% 2 == 0,
     bg = "#DDDDDD",
     part = "body") %>%
  fontsize(size = 8, part = "all") %>%
  border(
    part = "body",
    border.right = officer::fp_border(
      color = "black",
      style = "dotted",
      width = 0.25
    ),
    j = c(1, 2, 3, 5)
  ) %>%
  set_table_properties(layout = "autofit") %>%
  align(j = 1:2, align = "left", part = "all")


(ft_s1_supp <- ft)

# Second supplemental table for S1
df_categories <- rio::import(here("report","table-data","categories.xlsx")) %>%
  replace(is.na(.), "") %>%
  mutate(preprint_decision = Key) %>%
  dplyr::select(preprint_decision, 'Sub-category')

df_s1_supp2 <- merge(df, df_reference, all.x = TRUE) %>%
  merge(df_policy, by = "journal") %>%
  filter(published_DAS != "XXXX - Abstract") %>%
  group_by(preprint_decision, policy_code) %>%
  summarise(
    number_journals = length(unique(journal)),
    number = n(),
    pub_open = sum(published_decision_group == 1),
    preprint_open = sum(preprint_decision_group == 1),
    pub_closed = sum(published_decision_group == 0),
    preprint_closed = sum(preprint_decision_group == 0),
    preprint_prop = paste0(round(preprint_open / number * 100, 1), "% (", preprint_open, ")"),
    pub_prop =  paste0(round(pub_open / number * 100, 1), "% (", pub_open, ")"),
    change_open = sum(change > 0),
    change_closed = sum(change < 0),
    change_none = sum(change == 0)
  ) %>%
  ungroup(preprint_decision) %>%
  left_join(df_categories) %>%
  dplyr::rename_all( ~ gsub("-", "_", .x, fixed = TRUE)) %>%
  mutate(
    policy_code = ifelse(policy_code == 1, "Mandates open data", "Does not mandate open data"),
    Sub_category = paste0("(", preprint_decision, ") ", Sub_category)
  ) %>%
  dplyr::select(
    Sub_category,
    everything(),
    -c(
      preprint_decision,
      pub_open,
      pub_closed,
      preprint_open,
      preprint_closed
    )
  )

colnames(df_s1_supp2) <-
  c("Preprint decision",
    "Open data policy",
    "Number of journals\n (N)",
    "Number of records\n (N)",
    "Open at preprint\n % (N)",
    "Open at publication\n % (N)",
    "More open\n (N)",
    "More closed\n (N)",
    "No change\n (N)"
  )

ft <- flextable(df_s1_supp2) %>%
  add_header(
    "Preprint decision" = "Preprint decision",
    "Open data policy" = "Open data policy",
    "Number of journals\n (N)" = "Number of journals\n (N)",
    "Number of records\n (N)" = "Number of records\n (N)",
    "Open at preprint\n % (N)" = "Open at preprint\n % (N)",
    "Open at publication\n % (N)" = "Open at publication\n % (N)",
    "More open\n (N)" = "Change from preprint to publication",
    "More closed\n (N)" = "Change from preprint to publication",
    "No change\n (N)" = "Change from preprint to publication"
  ) %>%
  merge_v(part = "header") %>%
  merge_v(j = 1, part = "body") %>%
  merge_h(part = "header") %>%
  border(
    part = "header",
    border.top = officer::fp_border(color = "black", width = 1),
    border.bottom = officer::fp_border(color = "black", width = 1)
  ) %>%
  border(
    part = "header",
    j = 1:6,
    border.right = officer::fp_border(color = "#A6A6A6", width = 1)
  ) %>%
  bg(bg = "#A6A6A6", part = "header") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  align(align = "center", part = "all") %>%
  bg(i = ~ seq(from = 1, to = nrow(df_s1_supp2)) %% 2 == 0,
     bg = "#DDDDDD",
     part = "body") %>%
  border(
    part = "body",
    border.bottom = officer::fp_border(color = "black", width = 0.5),
    i = ~ seq(from = 1, to = nrow(df_s1_supp2)) %% 2 == 0
  ) %>%
  border(
    part = "body",
    border.right = officer::fp_border(color = "black", width = 0.5),
    j=1
  ) %>%
  border(
    part = "body",
    border.right = officer::fp_border(color = "black",style = "dotted", width = 0.25),
    j= c(2,4,6)
  ) %>%
  border(
    part = "body",
    border.bottom = officer::fp_border(color = "black", width = 0.5),
    j=1
  ) %>%
  width(j=1,width = 1) %>%
  fontsize(size = 8, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  align(j = 1:2, align = "left", part = "body")

(ft_s1_supp2 <- ft)


# S2: CODE AVAILABILITY ---------------------------------------------------

# Read in code
df_s2 <- rio::import(here("data",
                          "complete",
                          "reviewer 2",
                          "s2_blind_reviewer2.xlsx")) %>%
  dplyr::select(code_DAS_decision, code_PDF_decision) %>%
  mutate(code_DAS_decision = ifelse(code_DAS_decision==1,"Code availability described", "Code availability not described"),
         code_PDF_decision = ifelse(code_PDF_decision==1,"Code availability described", "Code availability not described"))


df_s2 <- table(df_s2$code_DAS_decision, df_s2$code_PDF_decision) %>%
  as.data.frame.matrix() %>%
  tibble::rownames_to_column() %>%
  mutate("DAS Category" = "Data availability statement") %>%
  dplyr::select("DAS Category", rowname, "Code availability described", "Code availability not described")


code_pdf_total <- df_s2$`Code availability described`[1] + df_s2$`Code availability described`[2]

code_captured <- df_s2$`Code availability described`[1] %>% 
  paste0(" (", round(./code_pdf_total*100,1),"%)")

colnames(df_s2)[1]<- " "  
colnames(df_s2)[2]<- "_"  

ft_s2 <- flextable::flextable(df_s2) %>%
  merge_v() %>%
  add_header(
    " " = "",
    "_" = "",
    "Code availability described" = "Full text",
    "Code availability not described" = "Full text",
  ) %>%
  merge_h(part = "header") %>%
  flextable::border_remove() %>%
  align(align = "center", part = "all") %>%
  border(
    part = "header",
    border.bottom = officer::fp_border(color = "black", width = 1),
    j = 3:4
  ) %>%
  border(
    part = "body",
    border.right = officer::fp_border(color = "black", width = 1),
    j = 1:2
  ) %>%
  border(
    part = "header",
    border.right = officer::fp_border(color = "black", width = 1),
    j = 2:4,
    i = 2
  ) %>%
  border(
    part = "body",
    border = officer::fp_border(color = "black", width = 1),
    j = 2:4,
    i = 1:2
  ) %>%
  bg( bg = "#A6A6A6", part = "header", i=2,j=3:4) %>%
  bg( bg = "#A6A6A6", part = "body", i=1:2,j=2) %>%
  bold(part = "header") %>%
  bold(j = 1:2, part = "body") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  color(color = "white", part = "header", j = 2)


# S3: Do those who promise to make it available, actually do so? --------------

n_future <- df_pre %>%
  filter(preprint_decision %in% c(3,4)) %>%
  nrow() %>%
  paste0(" (", round(./n_records*100,1),"%)")

n_future_pub <- df_both %>%
  filter(preprint_decision %in% c(3,4) & is.na(published_DAS) == FALSE & published_DAS != "XXXX - Abstract") %>%
  nrow()

n_future_pub_open <- df_both %>%
  filter(preprint_decision %in% c(3,4) & is.na(published_DAS) == FALSE & published_DAS != "XXXX - Abstract" ) %>%
  nrow()

n_onpub_link <- df_pre %>%
  filter(preprint_decision %in% c(3)) %>%
  nrow() %>%
  paste0(" (", round(./n_records*100,1),"%)")

n_onpub_nolink <- df_pre %>%
  filter(preprint_decision %in% c(4)) %>%
  nrow() %>%
  paste0(" (", round(./n_records*100,1),"%)")

n_future <- df_pre %>%
  filter(preprint_decision %in% c(3,4)) %>%
  nrow() %>%
  paste0(" (", round(./n_records*100,1),"%)")


df4 <- merge(df, df_reference, all.x = TRUE) %>%
  filter(preprint_decision %in% c(3,4)) %>%
  group_by(preprint_decision) %>%
  mutate(pre_total = n())  %>%
  group_by(preprint_decision, published_decision) %>%
  mutate(pub_total = n()) %>%
  mutate(Group = case_when(preprint_decision == 3 ~ "Data available in the future, with a link to an embargoed repository provided",
                           preprint_decision == 4 ~ "Data available in the future, with no details of embargoed repository given")) %>%
  dplyr::select(Group, published_decision, pub_total, pre_total) %>%
  distinct() %>%
  arrange(Group, published_decision) %>%
  mutate('Preprint Category' = Group,
        'Number of published studies' = paste0(pub_total," (",paste0(round(pub_total/pre_total*100,1),"%"),")"),
         'Published Category' = published_decision,
         'Number of preprints' = pre_total) %>%
  ungroup() %>%
  dplyr::select('Preprint Category', 'Number of preprints','Published Category', 'Number of published studies') %>%
  mutate('Published Category' = as.character(.$'Published Category')) %>%
  mutate(
    'Published Category' = case_when(
      .$'Published Category' == "1" ~ "1. Data not made available",
      .$'Published Category' == "2" ~ "2. Data available on request to authors",
      .$'Published Category' == "5" ~ "5. Data available from central repository (access-controlled or open access), but insufficient detail available to find specific dataset",
      .$'Published Category' == "7" ~ "7. Data available in the manuscript/supplementary files",
      .$'Published Category' == "8" ~ "8. Data available via a online repository that is not access-controlled e.g. Dryad, Zenodo",
      TRUE ~ "9"
    )
  )

ft <- flextable(df4) %>%
  merge_v(j=1:2) %>%
  bg(bg = "#A6A6A6", part = "header") %>%
  bold(part = "header") %>%
  bold(j = 1, part = "body") %>%
  border(i = 1:6, j = 3:4, border.bottom = fp_border(color = "black")) %>%
  border(i = 1:7, j = 2, border.right = fp_border(color = "black")) %>%
  border(i = 3, j = 1:4, border.bottom = fp_border(color = "black", width = 2)) %>%
  hline_bottom(part = "all",border = fp_border(width = 2))%>%
  align(align = "center", part = "all" ) %>%
  align(align = "left", part = "body", j=3 ) %>%
  fontsize(size = 8, part = "all") %>%
  set_table_properties( layout = "autofit") %>%
  fix_border_issues(part = "all") %>%
  align(j = 1, align = "left", part = "all")

(ft_s3 <- ft)


# S4: What happened to those papers published in journals with an open policy 
# but which did not share data? --------------

df_s4 <- merge(df, df_reference, all.x = TRUE) %>%
  merge(df_policy, by = "journal") %>%
  filter(published_decision_group == 0 & policy_code == 1) %>%
  group_by(published_decision) %>% 
  summarise('Number of records' = n()) %>%
  left_join(df_categories, by = c("published_decision" = "preprint_decision")) %>%
  mutate("Key" = published_decision) %>%
  dplyr::select("Key","Sub-category","Number of records")

n_not_open <- sum(df_s4$`Number of records`)

ft <- flextable(df_s4) %>%
  border(
    part = "header",
    border.top = officer::fp_border(color = "black", width = 1),
    border.bottom = officer::fp_border(color = "black", width = 1)
  ) %>%
  bg(bg = "#A6A6A6", part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  bg(i = ~ seq(from = 1, to = nrow(df_s4)) %% 2 == 0,
     bg = "#DDDDDD",
     part = "body") %>%
  bold(part = "body", j=1) %>%
  width(j = 1, width = 2) %>%
  fontsize(size = 8, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  align(j = 1:2, align = "left", part = "body")

(ft_s4 <- ft)

# #  GET PERCENTAGE OF COVID PAPERS -----------------------------------------
# 
cv_query = c("[Cc][Oo][Vv][Ii][Dd]", "[Cc]oronavir", "\\b[Nn][Cc][Oo][Vv]\\b")

mx_data <- medrxivr::mx_snapshot("795081b8895faba1868e4978acfc725c456d0449")
                   
cv_num <- medrxivr::mx_search(
  data = mx_data,
  query = cv_query,
  to_date = "2020-05-01"
) %>%
  nrow()

# FIGURES -----------------------------------------------------------------
df_pre_plot <- df_pre %>%
  group_by(preprint_decision) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  mutate(
    preprint_decision_group = case_when(
      preprint_decision %in% c(1,2,3,4,5,6) ~ "Closed", # Closed
      preprint_decision %in% c(7,8) ~ "Open", # Open
      preprint_decision == 0 ~ "Not applicable" # Not applicable
    )
  )

pre_plot <- ggplot(df_pre_plot, aes(x = preprint_decision, y= freq, fill = factor(preprint_decision_group, levels = c("Not applicable", "Closed", "Open")))) +
  geom_col() +
  theme_minimal() +
  labs(x = "Category",
       y = "Percentage of preprints",
       fill = "Group")  +
  scale_fill_manual(values = c( "#8DA0CB", "#FC8D62","#66C2A5")) +
  scale_x_continuous(labels = as.character(df_pre_plot$preprint_decision), breaks = df_pre_plot$preprint_decision) +
  scale_y_continuous(labels = c("0%","10%","20%","30%"), limits = c(0,30)) +
  ggtitle(paste0("Preprint (N = ",n_records,")")) + 
  theme(plot.title = element_text(size = 10),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
  NULL


df_pub_plot <- df_pub %>%
  group_by(published_decision) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  mutate(
    published_decision_group = case_when(
      published_decision %in% c(1,2,3,4,5,6) ~ "Closed", # Closed
      published_decision %in% c(7,8) ~ "Open", # Open
      published_decision == 0 ~ "Not applicable" # Not applicable
    )
  )

pub_plot <- ggplot(df_pub_plot, aes(x = published_decision, y = freq, fill = factor(published_decision_group, levels = c("Not applicable","Closed", "Open")))) +
  geom_col() +
  theme_minimal() +
  labs(x = "Category",
       y = "Percentage of published articles",
       fill = "Group") +
  scale_fill_manual(values = c("#8DA0CB", "#FC8D62","#66C2A5")) +
  scale_x_continuous(labels = as.character(df_pub_plot$published_decision), breaks = df_pub_plot$published_decision) +
  scale_y_continuous(labels = c("0%","10%","20%","30%"), limits = c(0,30)) + 
  ggtitle(paste0("Published (N = ",n_pub_denominator,")")) + 
  theme(plot.title = element_text(size = 10),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
  NULL

plot_s1 <- pre_plot + pub_plot + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') &
  theme(
    legend.position = 'bottom',
    legend.background = element_rect(
      size = 0.5,
      linetype = "solid",
      colour =
        "black"
    )
  ) & labs(fill = "Group:")

ggplot2::ggsave(here("report","Fig1.tiff"),
                plot_s1,
                dpi = 400, units = "in",
                width = 5.1, height = 4)


