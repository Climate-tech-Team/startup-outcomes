# Code to perform robustness checks and generate table output for regressions
# Must be run after 3.cox_proportional_hazards.R
# Kathleen Kennedy 2022
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


# Check companies with high level of patenting ( > 50 total)
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

# Check companies with high level of patenting ( > 50 total)
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

# Check companies with low level of patenting ( < 50 total)
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
