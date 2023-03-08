# Original code by Kavita Surana
# Updated by Kathleen Kennedy

library(tidyverse)
library(stringr)

# load patents file and list of firms 
# first combine all derwent files into one txt, import into excel, save as csv
# the file from 04242015 has patents from a few that were incorrect or missing
patents <- read.csv(paste0(getwd(),'/output/patent_df_2.csv'),encoding="UTF-8", stringsAsFactors = FALSE)  %>%
  filter((validUTF8(assignee_organization))==TRUE) #removing special characters 

#matching assignees and firms -----
assignees <- patents %>%
  select(assignee_organization,assignee_country,company) %>%
  mutate(assignee_original = assignee_organization) %>%
  unique() %>%
  mutate_all(tolower) %>%
  #cleaning up assignee names
  mutate(assignee_organization = gsub(", inc", " ", assignee_organization)) %>%
  mutate(assignee_organization = gsub(" s.r.l.", " ", assignee_organization)) %>%
  mutate(assignee_organization = gsub(" inc.", " ", assignee_organization)) %>%
  mutate(assignee_organization = gsub(" llc", " ", assignee_organization)) %>%
  mutate(assignee_organization = gsub(" inc", " ", assignee_organization)) %>%
  mutate(assignee_organization = gsub(" ltd", " ", assignee_organization)) %>%
  mutate(assignee_organization = gsub("[[:punct:]]", " ", assignee_organization)) %>%
  mutate(assignee_organization = trimws(assignee_organization)) %>%
  #cleaning up company names
  mutate(company_tmp = gsub(", inc", " ", company)) %>%
  mutate(company_tmp = gsub(" inc.", " ", company_tmp)) %>%
  mutate(company_tmp = gsub(" llc", " ", company_tmp)) %>%
  mutate(company_tmp = gsub(" inc", " ", company_tmp)) %>%
  mutate(assignee_organization = gsub(" ltd", " ", assignee_organization)) %>%
  mutate(company_tmp = gsub("[[:punct:]]", " ", company_tmp)) %>%
  mutate(company_tmp = trimws(company_tmp)) %>%
  #number of assignees matched for each startup
  group_by (company) %>%
  mutate(count_assignees = n_distinct(assignee_organization)) %>%
  ungroup() %>%
  mutate(check = ifelse(assignee_organization == company_tmp, "include", NA)) %>%
  #matching the words in the i3 company name with the assignee
  ##number of words in the company name = n
  mutate(company_words = str_count(company_tmp, '\\w+')) %>%
  ##matching first n words with the assignee words
  mutate(assignee_words = word(assignee_organization, 1,company_words)) %>%
  mutate(assignee_words = gsub("[[:punct:]]", " ", assignee_words)) %>%
  mutate(assignee_words = trimws(assignee_words)) %>%
  mutate(check.loc = stringr::str_locate(assignee_words, company)[, 1]) %>%
  mutate(check = ifelse((is.na(check)==TRUE & check.loc == 1), 
                         "include", 
                         check)) %>%
 # mutate(check = ifelse(str_locate(assignee_words, company) == 1, "include", "exclude")) %>%
  ##removing those where first n words don't match
  filter(check == "include") %>%
  filter(is.na(check)==FALSE)

#verifying those where we didn'`t get a 1:1 match -----
  assignees_tmp <- assignees %>%
  filter(count_assignees>2) %>%
  select(company, assignee_organization, assignee_country) %>%
  unique() 

write.csv(assignees_tmp,paste0(getwd(),'/input/assignees_tmp2.csv')) 


#manual removal / keep
keep.list<- data.frame ("assignee_organization" = c("andium", "ingenu","kelvin","malta","nauto","notco delaware","notco deleware", "nuvia","pearl","rokid corporation","stion corporation"),
                        "type" = "include")

rem.tmp <- data.frame ("assignee_organization" = c("intelenz","nanografix corporation","overair proximity technologies","rachiotek","second nature brands","skyreader media","advanorigin co","veeva systems","solidian gmbh", "nexark"),
                       "type" = "remove")

remove.list<-assignees_tmp %>%
  left_join(keep.list) %>%
  #filter(is.na(type)==TRUE) %>%
  select(assignee_organization,  type) %>%
  mutate(type="remove") %>%
  rbind(rem.tmp)


#bringing it together / removing manual data
assignees <- assignees %>%
  left_join(remove.list) %>%
  filter(is.na(type)==TRUE) %>%
  select(assignee_organization,assignee_original, company) %>%
  unique()
  

write.csv(assignees,paste0(getwd(),'/input/assignees_new.csv')) 
#repeating the above as an iterative step with , with manually edited list present in input/companies_from_patent_data.csv
