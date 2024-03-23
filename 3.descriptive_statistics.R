library(tidyverse)
library(stargazer)
library(corrplot)
library(gtsummary)
library(flextable)

#=================================
# Function for Correlation Matrix 
#=================================
# Taken from https://github.com/kavsurana/tech-complexity-project/blob/master/stats.R
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-
  function(x,
           method = c("pearson", "spearman"),
           removeTriangle = c("upper", "lower"),
           result = c("none", "html", "latex")) {
    #Compute correlation matrix
    require(Hmisc)
    x <- as.matrix(x)
    correlation_matrix <- rcorr(x, type = method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value
    
    ## Define notions for significance levels; spacing is important.
    mystars <-
      ifelse(p < .0001, "***", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(
      x
    )), R), 3))[, -1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep = "")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep = "")
    
    ## remove upper triangle of correlation matrix
    if (removeTriangle[1] == "upper") {
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if (removeTriangle[1] == "lower") {
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    Rnew <- cbind(Rnew[1:length(Rnew) - 1])
    if (result[1] == "none")
      return(Rnew)
    else{
      if (result[1] == "html")
        print(xtable(Rnew), type = "html")
      else
        print(xtable(Rnew), type = "latex")
    }
  }

#=======================
# Descriptive statistics
#=======================
# Model 1 - all startups with at least one funding source
haz_df1 <- read.csv(paste0(getwd(),'/outputs/hazard_data.csv'))

statdata.tmp <- haz_df1 %>%
  select(
    Success,
    Failure,
    IPO,
    MA,
    CVC_bin,
    Other_inv_bin,
    Public_grant_bin,
    Location,
    patent_count
  ) %>%
  na.omit()
correlations <- corstars(statdata.tmp)
descriptions <- psych::describe(statdata.tmp)

summary.stats <- cbind(descriptions, correlations)

write.table(summary.stats,
            paste(getwd(), "/tables/model1_correlations.csv", sep = ""),
            sep = ",",
            row.names = FALSE)

tbl_summary(statdata.tmp) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/model1_summary_stats.docx'))