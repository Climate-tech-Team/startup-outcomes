# Original code by Kavita Surana
# Updated by Kathleen Kennedy

library(tidyverse)
library(stringr)
library(readr)

patents <- read.csv(paste0(getwd(),'/output/patent_df_2.csv'),encoding="UTF-8", stringsAsFactors = FALSE)  %>%
  filter((validUTF8(assignee_organization))==TRUE) %>% #removing special characters 
  mutate_all(tolower) %>%
  select(-company)

#-------------------

# patents and assignees

assignees<-read.csv(paste(getwd(),"/input/assignees_new.csv",sep=""),fileEncoding = "UTF-8") %>%
  dplyr::rename("company.othernames" = "company") %>%
  select(-X)

#-------------------

# complete firm list

startups.1<-read_csv(paste(getwd(),"/input/companies_from_investors_20220504.csv",sep="")) %>% #, fileEncoding = "UTF-8")  %>%
  select(company,country,year.founded,investor.type.edited) %>% unique() %>%
  filter(country=="united states" & year.founded>=2005) %>%
  filter(!investor.type.edited %in% c("not an investor","other")) %>%
  filter(!company %in% c("ashland","aspiration")) %>% #manual removal
  select(-investor.type.edited) %>%
  unique() %>%
  mutate(company.othername = company) %>%
  select(-country,-year.founded) %>%
  unique()

#additional file from manual checks on patent data
startups.2<-read.csv(paste(getwd(),"/input/companies_from_patent_data.csv",sep=""),fileEncoding = "UTF-8")

companies.match<-left_join(startups.1,startups.2) %>%
  unique() %>%
  mutate(company.patentname = ifelse(is.na(company.patentname)==TRUE, as.character(company.othername), as.character(company.patentname))) %>%
  filter(company.patentname!=0) %>%
  pivot_longer(2:3) %>%
  select(-name) %>%
  dplyr::rename("company.othernames" = value) %>%
  unique() %>%
  full_join(assignees) %>%
  select(company, assignee_original) %>%
  na.omit() %>%
  unique() %>%
  mutate(assignee_organization = assignee_original)

#match patent assignee
patents <- patents %>%
  left_join(companies.match, by="assignee_organization") %>%
  select(company, assignee_organization, assignee_key_id, patent_number, app_date) %>%
  unique() %>%
  na.omit()

get_year <- function(x){
  y <- substr(x,1,4)
  return(y)
}
#find the patent year
patents$year<-as.numeric(lapply(patents$app_date, get_year))

patents<-patents %>%
  group_by(company,year) %>%
  mutate(pat_count = n_distinct(patent_number)) %>%
  ungroup() %>%
  select(company,year,pat_count) %>%
  unique() %>%
  filter(!company %in% c("exxonmobil","chrysler", "hrl laboratories", "phillips 66")) #NOTE remember to remove these; how about enabling technologies and NA sector?

#export data
write.csv(patents,paste0(getwd(),'/output/patent_counts_2.csv'), row.names = FALSE)


