# Code to perform robustness checks and generate table output for regressions
# Must be run after 3.cox_proportional_hazards.R
# Kathleen Kennedy 2022
########################################################################

# Proportion tests for Table 1 outcomes, Model 2 vs 3 and Model 4 vs 5
# Comparison of binary variables from two different populations
# Model 1 - all startups with at least one funding source
haz_df1 <- read.csv(paste0(getwd(),'/outputs/hazard_data.csv')) %>%
  mutate(age = 2021 - Year.Founded) %>%
  dplyr::select(Company,Industry.Group,Sector,Year.Founded,age,patent_count,
                CVC_bin,Public_grant_bin,Other_inv_bin,Success,Failure,
                IPO,MA,survival_10)

# Model 2 vs 3 - Exits
mod2_starts <- haz_df1 %>% filter(Public_grant_bin == 1)
mod3_starts <- haz_df1 %>% filter(Public_grant_bin == 0)
prop.test(c(sum(mod2_starts$Success),sum(mod3_starts$Success)),
          c(nrow(mod2_starts),nrow(mod3_starts)))
prop.test(c(sum(mod2_starts$IPO),sum(mod3_starts$IPO)),
          c(nrow(mod2_starts),nrow(mod3_starts)))
prop.test(c(sum(mod2_starts$MA),sum(mod3_starts$MA)),
          c(nrow(mod2_starts),nrow(mod3_starts)))

# Model 2 vs 3 - Failure
prop.test(c(sum(mod2_starts$Failure),sum(mod3_starts$Failure)),
          c(nrow(mod2_starts),nrow(mod3_starts)))

# Model 2 vs 3 - Survival
prop.test(c(sum(mod2_starts$survival_10),sum(mod3_starts$survival_10)),
          c(nrow(mod2_starts),nrow(mod3_starts)))

# Model 2 vs 3 - age of start-ups
P <- haz_df1[,"Public_grant_bin"]
A <- haz_df1[,"age"]
Ap <- A[P==1]  
Anp <- A[P==0]  
t.test(Ap,Anp)

# Model 2 vs 3 - number of patents
P <- haz_df1[,"Public_grant_bin"]
Pat <- haz_df1[,"patent_count"]
Patp <- Pat[P==1]  
Patnp <- Pat[P==0]  
t.test(Patp,Patnp)

# Model 4 vs 5 - Exits
mod4_starts <- haz_df1 %>% filter(CVC_bin == 1)
mod5_starts <- haz_df1 %>% filter(CVC_bin == 0)
prop.test(c(sum(mod4_starts$Success),sum(mod5_starts$Success)),
          c(nrow(mod4_starts),nrow(mod5_starts)))
prop.test(c(sum(mod4_starts$IPO),sum(mod5_starts$IPO)),
          c(nrow(mod4_starts),nrow(mod5_starts)))
prop.test(c(sum(mod4_starts$MA),sum(mod5_starts$MA)),
          c(nrow(mod4_starts),nrow(mod5_starts)))

# Model 4 vs 5 - Failure
prop.test(c(sum(mod4_starts$Failure),sum(mod5_starts$Failure)),
          c(nrow(mod4_starts),nrow(mod5_starts)))

# Model 4 vs 5 - Survival
prop.test(c(sum(mod4_starts$survival_10),sum(mod5_starts$survival_10)),
          c(nrow(mod4_starts),nrow(mod5_starts)))

# Model 4 vs 5 - age of start-ups
C <- haz_df1[,"CVC_bin"]
A <- haz_df1[,"age"]
Ac <- A[C==1]  
Anc <- A[C==0]  
t.test(Ac,Anc)

# Model 2 vs 3 - number of patents
C <- haz_df1[,"CVC_bin"]
Pat <- haz_df1[,"patent_count"]
Patc <- Pat[C==1]  
Patnc <- Pat[C==0]  
t.test(Patc,Patnc)

########################################################################

cleantech1 <- newhaz1 %>% filter(Year.Founded <= 2011)
cleantech2 <- newhaz1 %>% filter(Year.Founded >= 2012)

quantile(base$patent_count, probs = seq(.1, .9, by = .1))
patentlow <- newhaz1 %>% filter(patent_count < 12)
patenthigh <- newhaz1 %>% filter(patent_count >= 12)

quantile(base$total_capital, probs = seq(.1, .9, by = .1))
invlow <- newhaz1 %>% filter(total_capital < 80863249)
invhigh <- newhaz1 %>% filter(total_capital >= 80863249)

###################################################
#                                                 #
# Model 1: Full population, Exits                 #
#                                                 #
###################################################

# Check cleantech 1.0
cleantech1_exits <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                            Pub_gr + patent_time + Location + strata(Sector) +
                            strata(Year.Founded), 
                          data=cleantech1, cluster=ID)
summary(cleantech1_exits)
# Check assumptions
temp <- cox.zph(cleantech1_exits) 
print(temp)
tbl_regression(cleantech1_exits,exponentiate = TRUE, 
               label = c(CVC ~ 'Corporate Investment',
                         Oth_inv ~ "Other Private Investment",
                         Pub_gr ~ "Public Grant",
                         Location ~ "Location",
                         patent_time ~ "Patent Count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/cleantech1_exits.docx'))

# Check cleantech 2.0
cleantech2_exits <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                            Pub_gr + patent_time + Location + strata(Sector) +
                            strata(Year.Founded), 
                          data=cleantech2, cluster=ID)
summary(cleantech2_exits)
# Check assumptions
temp <- cox.zph(cleantech2_exits) 
print(temp)
tbl_regression(cleantech2_exits,exponentiate = TRUE, 
               label = c(CVC ~ 'Corporate investment',
                         Oth_inv ~ "Other equity investment",
                         Pub_gr ~ "Public grant",
                         Location ~ "Location",
                         patent_time ~ "Patent count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/cleantech2_exits.docx'))


# Check companies with high level of patenting ( > 12 total)
patenthigh_exits <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                            Pub_gr + Location + strata(Sector) +
                            strata(Year.Founded), 
                          data=patenthigh, cluster=ID)
summary(patenthigh_exits)
# Check assumptions
temp <- cox.zph(patenthigh_exits) 
print(temp)
tbl_regression(patenthigh_exits,exponentiate = TRUE, 
               label = c(CVC ~ 'Corporate investment',
                         Oth_inv ~ "Other equity investment",
                         Pub_gr ~ "Public grant",
                         Location ~ "Location")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/patenthigh_exits.docx'))

# Check companies with low level of patenting ( < 50 total)
patentlow_exits <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                            Pub_gr + Location + strata(Sector) +
                            strata(Year.Founded), 
                          data=patentlow, cluster=ID)
summary(patentlow_exits)
# Check assumptions
temp <- cox.zph(patentlow_exits) 
print(temp)
tbl_regression(patentlow_exits,exponentiate = TRUE, 
               label = c(CVC ~ 'Corporate investment',
                         Oth_inv ~ "Other equity investment",
                         Pub_gr ~ "Public grant",
                         Location ~ "Location")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/patentlow_exits.docx'))


###################################################
#                                                 #
# Model 1: Full population, Failure               #
#                                                 #
###################################################

# Check cleantech 1.0
cleantech1_fail <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                            Pub_gr + patent_time + Location + strata(Sector) +
                            strata(Year.Founded), 
                          data=cleantech1, cluster=ID)
summary(cleantech1_fail)
# Check assumptions
temp <- cox.zph(cleantech1_fail) 
print(temp)
tbl_regression(cleantech1_fail,exponentiate = TRUE, 
               label = c(CVC ~ 'Corporate investment',
                         Oth_inv ~ "Other equity investment",
                         Pub_gr ~ "Public grant",
                         Location ~ "Location",
                         patent_time ~ "Patent count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/cleantech1_fail.docx'))

# Check cleantech 2.0
cleantech2_fail <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                           Pub_gr + patent_time + Location + strata(Sector) +
                           strata(Year.Founded), 
                         data=cleantech2, cluster=ID)
summary(cleantech2_fail)
# Check assumptions
temp <- cox.zph(cleantech2_fail) 
print(temp)
tbl_regression(cleantech2_fail,exponentiate = TRUE, 
               label = c(CVC ~ 'Corporate investment',
                         Oth_inv ~ "Other equity investment",
                         Pub_gr ~ "Public grant",
                         Location ~ "Location",
                         patent_time ~ "Patent count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/cleantech2_fail.docx'))

# Check companies with high level of patenting ( > 12 total)
patenthigh_fail <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                            Pub_gr + Location + strata(Sector) +
                            strata(Year.Founded), 
                          data=patenthigh, cluster=ID)
summary(patenthigh_fail)
# Check assumptions
temp <- cox.zph(patenthigh_fail) 
print(temp)
tbl_regression(patenthigh_fail,exponentiate = TRUE, 
               label = c(CVC ~ 'Corporate investment',
                         Oth_inv ~ "Other equity investment",
                         Pub_gr ~ "Public grant",
                         Location ~ "Location")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/patenthigh_fail.docx'))

# Check companies with low level of patenting ( < 12 total)
patentlow_fail <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                           Pub_gr + Location + strata(Sector) +
                           strata(Year.Founded), 
                         data=patentlow, cluster=ID)
summary(patentlow_fail)
# Check assumptions
temp <- cox.zph(patentlow_fail) 
print(temp)
tbl_regression(patentlow_fail,exponentiate = TRUE, 
               label = c(CVC ~ 'Corporate investment',
                         Oth_inv ~ "Other equity investment",
                         Pub_gr ~ "Public grant",
                         Location ~ "Location")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/patentlow_fail.docx'))
