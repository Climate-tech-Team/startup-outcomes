##Functions----


new.sbir <- function(sbir_awards, i3_invest){
  sbir_awards <- clean.sbir(sbir_awards)
  i3_invest <- clean.i3.invest(i3_invest)

  #idenfity sbir awards in i3 by matching firm name, investor name, award year, and award amount rounded to 2 significant digits
  sbir_in_i3 <- merge(sbir_awards, i3_invest, by = "clean.firm")
  sbir_in_i3 <- sbir_in_i3[sbir_in_i3$clean.amount.x == sbir_in_i3$clean.amount.y & sbir_in_i3$clean.year.x == sbir_in_i3$clean.year.y & sbir_in_i3$clean.agency==sbir_in_i3$clean.investor,]
  
  new_sbir <- sbir_awards[!sbir_awards$award_link %in% sbir_in_i3$award_link,]
  
  return(new_sbir)
}

clean.sbir <- function(sbir_awards, scraped = T){
  
  sbir_awards$clean.year <- as.integer(sbir_awards$award_year)
  
  #round amount to 2 significant digits
  sbir_awards$clean.amount <- signif(sbir_awards$award_amount, 2)
  
  sbir_awards$clean.firm <- clean.names(sbir_awards$firm)
  
  #no change to agency needed
  sbir_awards$clean.agency <- sbir_awards$agency
  
  return(sbir_awards)
}

clean.i3.invest <- function(i3_invest){
  #ignore "Other.Investors," they all have their own row anyway
  i3_invest <- i3_invest[,1:grep("Other.Investors", names(i3_invest))-1]
  
  i3_invest$clean.year <- as.integer(i3_invest$Investment.Year)
  
  #round amount to 2 significant digits
  i3_invest$clean.amount <- signif(i3_invest$Investment.Amount...., 2)
  
  #replace Investor names with acronyms only (for comparison with SBIR agency)
  i3_invest$clean.investor <- gsub("\\).*", "", i3_invest$Investor)
  i3_invest$clean.investor <- gsub(".*\\(", "", i3_invest$clean.investor)
  
  i3_invest$clean.firm <- clean.names(i3_invest$Company)
  
  return(i3_invest)
}

clean.names <- function(names){
  #remove non alphanumeric characters, remove common company names, remove whitespace
  names <- gsub("[^[:alnum:]]", " ", names)
  names <- gsub("\\W*\\sinc.*|\\W*\\scorp.*|group|\\W*\\sltd.*|\\ssolutions|\\scompany|\\W*\\sllp.*|\\W*\\sllc.*",
                "",names,ignore.case=TRUE)
  names <- trimws(names)
  return(names)
}

##Main----

#Input data

sbir_fname <- "./sbir_awards.csv"
sbir_awards <- read.csv(sbir_fname, stringsAsFactors = F, strip.white = T)

i3_fname <- "./i3 full data/investments.csv"
i3_invest <- read.csv(i3_fname, stringsAsFactors = F, strip.white = T)

#Identify sbir awards not already recorded in i3 by matching firm name, investor name, award year, and award amount rounded to 2 significant digits

sbir_not_in_i3 <- new.sbir(sbir_awards, i3_invest)

filename <- paste0("new_sbir_awards", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv")
write.csv(sbir_awards, filename, row.names = F)
