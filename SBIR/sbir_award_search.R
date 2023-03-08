library(httr)

##Functions----

find.sbir.firm <- function(keyword = NULL, name = NULL, duns = NULL){
  #input at least one search field as character vector
  ######################requires httr
  
  #make matrix of search criteria
  params <- as.list(environment())
  params <- params[-which(sapply(params,is.null))]
  params <- as.matrix(expand.grid(params))
  fields <- colnames(params)
  
  #write urls for combinations of search parameters by field
  url <- write_urls(params, endpoint = "firm")
  
  #make requests for each url and concatenate
  content <- api_requests(url)
  
  #return list of API content
  return(content)
}

find.sbir.award <- function(agency = NULL, firm = NULL, year = NULL, ri = NULL){
  #input at least one search field as character vector
  ######################requires httr
  
  #make matrix of search criteria
  params <- as.list(environment())
  params <- params[-which(sapply(params,is.null))]
  params <- as.matrix(expand.grid(params))
  fields <- colnames(params)
  
  #write urls for combinations of search parameters by field
  url <- write_urls(params, endpoint = "awards")
  
  #make requests for each url and concatenate
  content <- api_requests(url)
  
  #return list of API content
  return(content)
}

write_urls <- function(params, endpoint, filetype = "json"){
  #takes matrix of search parameters and returns list of api urls
  #endpoint can be either "firm
  
  #write urls for combinations of search parameters by field
  #the API can handle only one parameter of each field per search
  #one url is needed for each row in the parameter matrix
  fields <- colnames(params)
  url <- character(nrow(params))
  url[] <- paste0("https://www.sbir.gov/api/", endpoint, ".json?")
  for (p in 1:nrow(params)){
    crit <- ""
    for (f in 1:ncol(params)){
      #concatenate search parameters
      crit <- paste(crit, fields[f], "=", params[p,f], "&", sep = "")
    }
    #concatenate url with search parameters
    url[p] <- paste(url[p], crit, sep = "")
  }
  
  return(url)
}

api_requests <- function(url, suppressOutput = F){
  #takes list of api urls and returns content
  
  #iterate through list of urls
  #organize all API content in a list
  content <- list()
  for (u in url){
    #the API only returns 100 results
    #request content until API stops returning content
    start = 0
    count_url <- paste(u, "start=", start, sep = "") #specify start award number
    if (!suppressOutput) print(paste("Requesting", count_url))
    resp <- GET(count_url) #gets response object, see httr
    c <- content(resp) #gets content of response object, see httr
    while (is.null(names(c))){
      #if c is a named list, there are no more awards
      content <- append(content, c)
      start = start + length(c)
      count_url <- paste(u, "start=", start, sep = "")
      if (!suppressOutput) print(paste("Requesting", count_url))
      resp <- GET(count_url)
      c <- content(resp)
    }
    content <- append(content, c)
  }
  
  return(content)
}

clean.sbir <- function(content){
  #make content list into dataframe, remove duplicates
  
  #make each set of elements in content list a row and bind to dataframe 
  award_data <- as.data.frame(do.call(rbind, content))
  
  #removes any duplicated rows
  dups <- which(duplicated(award_data))
  if (length(dups)>0){
    award_data <- award_data[-dups,]
  }
  
  return(award_data)
}

scrape.sbir <- function(award_data, suppressOutput = F){
  #scrape additional award details from SBIR website
  #this takes a long time to run
  #award_data <- apply(award_data, 2, as.character)
  scrapes<-data.frame()
  for (l in award_data$award_link){
    if (!suppressOutput) print(paste("Scraping", l))
    search <- try(readLines(l))
    if(inherits(search,"try-error")) next
    #tryCatch(search <- readLines(l), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    content <- data.frame(Label = search[grep("open-label", search)],
                          Desc = search[grep("open-description", search)], stringsAsFactors = FALSE)
    content$Label <- gsub(".*<span class=\"open-label\">", "", content$Label)
    content$Label <- gsub(":</span>.*","",content$Label)
    content$Desc <- gsub(".*<span class=\"open-description\">", "", content$Desc)
    content$Desc <- gsub("</span>.*","",content$Desc)
    row.names(content) <- content$Label
    content <- data.frame(t(content[-1]), stringsAsFactors = FALSE)
    content$award_link <- l
    scrapes <- rbind(scrapes, content)
  }
  award_data <- merge(award_data, scrapes, by = "award_link")
  
  award_data[award_data == "N/A"] <- NA
  award_data$Amount <- gsub("\\..*", "", award_data$Amount)
  award_data$Amount <- gsub("[[:punct:]]", "", award_data$Amount)
  award_data$Amount <- as.numeric(award_data$Amount)
  award_data$Award.Start.Date..Proposal.Award.Date. <- as.Date(award_data$Award.Start.Date..Proposal.Award.Date.)
  award_data$Award.End.Date..Contract.End.Date. <- as.Date(award_data$Award.End.Date..Contract.End.Date.)
  
  
  return(award_data)
}

clean.names <- function(names){
  #check out fuzzy match
  #break gsub into individual replacements
  names <- gsub("[^[:alnum:]]", " ", names)
  names <- gsub("\\W*\\sinc.*|\\W*\\scorp.*|group|\\W*\\sltd.*|\\ssolutions|\\scompany|\\W*\\sllp.*|\\W*\\sllc.*",
                "",names,ignore.case=TRUE)
  names <- trimws(names)
  return(names)
}


##Search for firms by keyword----

# #list of keywords to search for
# keywords<-c("renewable","energy","electricity","power","CO2","climate","greenhouse gas","green house gas",
#             "GHG","carbon neutral","cleantech","clean tech","green","low emission","low carbon",
#             "hybrid vehicle","zero emissions", "perovskite")
# #range of years to search for
years <- 2010:2015
# 
# #search list of firms based on keywords
# sbir_firms <- find.sbir.firm(keyword = keywords)
# sbir_firms <- clean.sbir(sbir_firms)
#firms <- unlist(sbir_firms$company_name)

##Search for award data by firm name----

#Search for firms in i3 database
#for test case this only searches for companies that receive DOE funding; NASA and other agencies may provide SBIR funding
i3_fname <- "./i3 full data/investments.csv"
i3_invest <- read.csv(i3_fname, stringsAsFactors = F, strip.white = T)
i3_doe <- i3_invest[grep("DOE", i3_invest$Investor),]


#prep firm names for URL
firms <- clean.names(i3_doe$Company)
firms <- gsub(" ", "%20", firms)

#search list of awards to these firms
sbir_awards <- find.sbir.award(firm = firms, year = years)
sbir_awards <- clean.sbir(sbir_awards)
sbir_awards <- apply(sbir_awards, 2, as.character)
sbir_awards <- as.data.frame(sbir_awards)

#scrape SBIR website for additional award data
sbir_awards <- scrape.sbir(sbir_awards)

filename <- paste0("sbir_awards", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv")
write.csv(sbir_awards, filename, row.names = F)

