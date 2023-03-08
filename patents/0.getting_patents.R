library(patentsview) 
library(plyr)
library(dplyr)
library(tidyr)

Sys.setlocale("LC_ALL", "en_US.UTF-8")


#FUNCTION for patent search------------
patent.search.fun<-function(start.date, end.date,company)
{
  pat1<-search_pv(
    query =  with_qfuns(and(
      qry_funs$gte(app_date=start.date), #check patent_date or app_date
      qry_funs$lte(app_date=end.date),   
      #qry_funs$eq(patent_type="utility"),
      qry_funs$contains(assignee_organization = company)
    )),
    method = "GET",
    #see this link so we can select the fields we are interested in (also for other papers/ analyses)
    # https://patentsview.org/apis/api-endpoints/assignees
    fields = c("patent_number", "patent_title", "assignee_organization", "assignee_country", "assignee_type", "app_date"),
    per_page = 10000
    #sort = NULL, # I don't remember what these are, we should check if we need them
    #subent_cnts = TRUE,
    #mtchd_subent_only = TRUE,
    #all_pages=TRUE
  )
  return(pat1)
}

#FUNCTION for unnesting patent search table-----
#patentsview generates a list based on endpoints that is difficult to process. 
#this function will unnest the list into dataframes, and then merges those dataframes by patent_id
table.unnest<-function(pat.temp, pat.type)
{
  pat.summary<-unnest_pv_data(data = pat.temp$data, pk = "patent_number")
  data.table<-join_all(pat.summary, by="patent_number", type="inner")
  data.table$type<-pat.type
  data.table$cpc_subgroup_id<-NULL
  data.table<-unique(data.table)
  return(data.table)
}



#MAIN------
#------
##startup patents-----
#based on https://patentsview.org/
#------
## startup names------
startups.1<-read.csv(paste(getwd(),"/input/companies_from_investors_20220504.csv",sep=""),fileEncoding = "UTF-8") %>%
  select(company,country,year.founded,investor.type.edited) %>% unique() %>%
  filter(country=="united states" & year.founded>=2005) %>%
  filter(!investor.type.edited %in% c("not an investor","other")) %>%
  filter(!company %in% c("ashland","aspiration")) %>% #manual removal
  select(-investor.type.edited) %>%
  unique() %>%
  mutate(company.othername = company)

#additional file from manual checks on patent data
startups.2<-read.csv(paste(getwd(),"/input/companies_from_patent_data.csv",sep=""),fileEncoding = "UTF-8")

companies.match<-left_join(startups.1,startups.2) %>%
  unique() %>%
  mutate(company.patentname = ifelse(is.na(company.patentname)==TRUE, as.character(company.othername), as.character(company.patentname))) %>%
  filter(company.patentname!=0) %>%
  select(-company.othername)
  
companies<-companies.match$company.patentname %>%
  unique()

start.date<-"2005-01-01"
end.date<-"2021-12-31" 

patent_df<-data.frame(matrix(ncol = 9, nrow = 0)) 
colnames(patent_df) <- c("patent_number", "assignee_organization", "assignee_country",
                         "assignee_type","assignee_key_id", "app_date", "app_id", 
                         "patent_title", "type")

for(i in companies){
  patentsearch<-patent.search.fun(start.date,end.date,i)
  if(!is.null(patentsearch$data$patents)){
    pat.summary<-table.unnest(patentsearch,i) 
    patent_df<-rbind(patent_df,pat.summary)
  }
}

patent_df<-patent_df %>% 
  dplyr::rename("company"="type")

write.csv(patent_df,paste0(getwd(),'/output/patent_df_2.csv'), row.names = FALSE)

