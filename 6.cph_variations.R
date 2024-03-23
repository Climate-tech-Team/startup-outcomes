# Code to perform alternate form of CPH regressions
# Should be run after 3.cox_proportional_hazards.R
# Kathleen Kennedy 2022
########################################################################

CVC_inv <- haz_df1 %>% 
  select(ID,t_inv, CVC) %>%
  filter(CVC > 0) %>%
  unique()
Oth_inv <- haz_df1 %>% 
  select(ID,t_inv, Other_investors) %>%
  filter(Other_investors > 0) %>%
  unique()
Pub_gr <- haz_df1 %>%
  select(ID,t_inv, Pub_grant) %>%
  filter(Pub_grant > 0) %>%
  unique()
nation <- haz_df1 %>%
  select(ID,t_inv, national) %>%
  filter(national > 0) %>%
  unique()
subnat <- haz_df1 %>%
  select(ID,t_inv, subnational) %>%
  filter(subnational > 0) %>%
  unique()
inter <- haz_df1 %>%
  select(ID,t_inv, international) %>%
  filter(international > 0) %>%
  unique()
cvc_amt <- haz_df1 %>%
  select(ID,t_inv,cvc_amt) %>%
  filter(cvc_amt > 0) %>%
  mutate(cvc_amt = cvc_amt/1000000) %>%
  unique()
oth_amt <- haz_df1 %>%
  select(ID,t_inv,other_amt) %>%
  filter(other_amt > 0) %>%
  mutate(other_amt = other_amt/1000000) %>%
  unique()
pub_amt <- haz_df1 %>%
  select(ID,t_inv,pub_amt) %>%
  filter(pub_amt > 0) %>%
  mutate(pub_amt = pub_amt/1000000) %>%
  unique()

newhaz <- tmerge(data1=base,data2=end_times,id=ID,tstop=end_period)
newhaz <- tmerge(newhaz,patent_df,id=ID, patent_time=tdc(t_pat,pat_count)) 
newhaz <- tmerge(newhaz,CVC_inv,id=ID, CVC=tdc(t_inv,CVC))
newhaz <- tmerge(newhaz,Oth_inv,id=ID, Oth_inv=tdc(t_inv,Other_investors))
newhaz <- tmerge(newhaz,Pub_gr,id=ID, Pub_gr=tdc(t_inv,Pub_grant))
#newhaz <- tmerge(newhaz,nation,id=ID, nation=tdc(t_inv,national))
#newhaz <- tmerge(newhaz,subnat,id=ID, subnat=tdc(t_inv,subnational))
#newhaz <- tmerge(newhaz,inter,id=ID, inter=tdc(t_inv,international))

newhaz <- tmerge(newhaz,patent_df,id=ID, patent_time_cum=cumtdc(t_pat,pat_count)) 
newhaz <- tmerge(newhaz,CVC_inv,id=ID, CVC_cum=cumtdc(t_inv))
newhaz <- tmerge(newhaz,Oth_inv,id=ID, Oth_inv_cum=cumtdc(t_inv,Other_investors))
newhaz <- tmerge(newhaz,Pub_gr,id=ID, Pub_gr_cum=cumtdc(t_inv,Pub_grant))

newhaz <- tmerge(newhaz,success,id=ID, success=event(time_to_success))
newhaz <- tmerge(newhaz,failure,id=ID, failure=event(time_to_failure))
newhaz <- tmerge(newhaz,ipo,id=ID, ipo=event(time_to_ipo))
newhaz <- tmerge(newhaz,ma,id=ID, ma=event(time_to_ma))
newhaz <- tmerge(newhaz,outcome,id=ID, outcome=event(time_to_outcome))

newhaz[is.na(newhaz)] <- 0

newhaz <- newhaz %>%
  mutate(Sector = ifelse(Sector == "", "uncategorized", Sector))

# Version with dollar investment amount
newhaz2 <- tmerge(data1=base,data2=end_times,id=ID,tstop=end_period)
newhaz2 <- tmerge(newhaz2,patent_df,id=ID, patent_time=tdc(t_pat,pat_count)) 
newhaz2 <- tmerge(newhaz2,cvc_amt,id=ID, cvc_amt=tdc(t_inv,cvc_amt)) 
newhaz2 <- tmerge(newhaz2,oth_amt,id=ID, oth_amt=tdc(t_inv,other_amt))
newhaz2 <- tmerge(newhaz2,pub_amt,id=ID, pub_amt=tdc(t_inv,pub_amt))

newhaz2 <- tmerge(newhaz2,success,id=ID, success=event(time_to_success))
newhaz2 <- tmerge(newhaz2,failure,id=ID, failure=event(time_to_failure))
newhaz2 <- tmerge(newhaz2,ipo,id=ID, ipo=event(time_to_ipo))
newhaz2 <- tmerge(newhaz2,ma,id=ID, ma=event(time_to_ma))
newhaz2 <- tmerge(newhaz2,outcome,id=ID, outcome=event(time_to_outcome))

newhaz2[is.na(newhaz2)] <- 0

newhaz2 <- newhaz2 %>%
  mutate(Sector = ifelse(Sector == "", "uncategorized", Sector))

###################################################
#                                                 #
# Model 1: Full population model of all start-ups #
#                                                 #
###################################################

#################################################
# Exits - Cox proportional hazards regression   #
#################################################

# Count investment variables
Cph1s <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                   Pub_gr + patent_time + Location + strata(Sector) + 
                   strata(Year.Founded),
                 data=newhaz, cluster=ID)

summary(Cph1s)

# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(Cph1s) 
print(temp)    
plot(temp, var=1)
ggcoxzph(temp, font.main = 9, font.submain = 9,font.y=9,font.x=9, font.tickslab=9) 

# If dfbeta values are small compared to regression coefficient,
# there are no influential outliers
ggcoxdiagnostics(Cph1s, type = "dfbeta", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 

tbl_regression(Cph1s,exponentiate = TRUE, label = c(CVC ~ 'Corporate Investment',
                                                      Oth_inv ~ "Other Private Investment",
                                                      Pub_gr ~ "Public Grant",
                                                      Location ~ "Location",
                                                      patent_time ~ "Patent Count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/Table2_exits_investmentcounts.docx'))

# Dollar investment amounts 
Cph1s_amt <- coxph(Surv(tstart,tstop,success) ~ cvc_amt + oth_amt +
                     pub_amt + patent_time + Location + strata(Sector) + 
                     strata(Year.Founded),
                   data=newhaz2, cluster=ID)

summary(Cph1s_amt)

# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(Cph1s_amt) 
print(temp)    
plot(temp, var=1)
ggcoxzph(temp, font.main = 9, font.submain = 9,font.y=9,font.x=9, font.tickslab=9) 

# If dfbeta values are small compared to regression coefficient,
# there are no influential outliers
ggcoxdiagnostics(Cph1s_amt, type = "dfbeta", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 
#################################################
# Failure - Cox proportional hazards regression #
#################################################
# Count variables
Cph1f <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                   Pub_gr + patent_time + Location + strata(Sector) +
                   strata(Year.Founded), 
                 data=newhaz, cluster=ID)

summary(Cph1f)
# Chech assumptions
# If no variables are significant, 
# proportional hazards assumption is valid
temp2 <- cox.zph(Cph1f) 
print(temp2)            
plot(temp2, var=4)

# If dfbeta values are small compared to regression coefficient,
# there are no influential outliers
ggcoxdiagnostics(Cph1f_amt, type = "dfbeta", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 

tbl_regression(Cph1f,exponentiate = TRUE, label = c(CVC ~ 'Corporate investment',
                                                      Oth_inv ~ "Other equity investment",
                                                      Pub_gr ~ "Public grant",
                                                      Location ~ "Location",
                                                      patent_time ~ "Patent count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/Table2_failure_investmentcounts.docx'))

# Dollar investment amounts
Cph1f_amt <- coxph(Surv(tstart,tstop,failure) ~ cvc_amt + oth_amt +
                 pub_amt + patent_time + Location + strata(Sector) +
                 strata(Year.Founded), 
               data=newhaz2, cluster=ID)

summary(Cph1f_amt)
# Chech assumptions
# If no variables are significant, 
# proportional hazards assumption is valid
temp2 <- cox.zph(Cph1f_amt) 
print(temp2)            
plot(temp2, var=4)
###################################
# Model 2                         #
###################################
mod2 <- haz_df1 %>%
  dplyr::filter(Pub_grant_bin == 1) %>%
  unique()
newhaz_sub2 <- newhaz %>%
  dplyr::filter(ID %in% mod2$ID)
newhaz2_sub2 <- newhaz2 %>%
  dplyr::filter(ID %in% mod2$ID)

# Count variable variation for model 2 success
Cph2s <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                 patent_time + Location + strata(Sector) + 
                 strata(Year.Founded), 
               data=newhaz_sub2, cluster=ID)

summary(Cph2s)

# Count variable variation for model 2 failure
Cph2f <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                 patent_time + Location + strata(Sector) + 
                 strata(Year.Founded), 
               data=newhaz_sub2, cluster=ID)

summary(Cph2f)

# Dollar investment amounts for model 2 success
Cph2s_amt <- coxph(Surv(tstart,tstop,success) ~ cvc_amt + oth_amt +
                     patent_time + Location + strata(Sector) + 
                     strata(Year.Founded),
                   data=newhaz2_sub2, cluster=ID)

summary(Cph2s_amt)

# Dollar investment amounts for model 2 failure
Cph2f_amt <- coxph(Surv(tstart,tstop,failure) ~ cvc_amt + oth_amt +
                     patent_time + Location + strata(Sector) + 
                     strata(Year.Founded),
                   data=newhaz2_sub2, cluster=ID)

summary(Cph2f_amt)

###################################
# Model 3                         #
###################################
newhaz_sub3 <- newhaz %>%
  dplyr::filter(!(ID %in% mod2$ID))
newhaz2_sub3 <- newhaz2 %>%
  dplyr::filter(!(ID %in% mod2$ID))

# Count variable variation for model 3 success
Cph3s <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                 patent_time + Location + strata(Sector) + 
                 strata(Year.Founded), 
               data=newhaz_sub3, cluster=ID)

summary(Cph3s)

# Count variable variation for model 3 failure
Cph3f <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                 patent_time + Location + strata(Sector) + 
                 strata(Year.Founded), 
               data=newhaz_sub3, cluster=ID)

summary(Cph3f)

# Dollar investment amounts for model 3 success
Cph3s_amt <- coxph(Surv(tstart,tstop,success) ~ cvc_amt + oth_amt +
                     patent_time + Location + strata(Sector) + 
                     strata(Year.Founded),
                   data=newhaz2_sub3, cluster=ID)

summary(Cph3s_amt)

# Dollar investment amounts for model 3 failure
Cph3f_amt <- coxph(Surv(tstart,tstop,failure) ~ cvc_amt + oth_amt +
                     patent_time + Location + strata(Sector) + 
                     strata(Year.Founded),
                   data=newhaz2_sub3, cluster=ID)

summary(Cph3f_amt)
###################################
# Model 4                         #
###################################
mod4 <- haz_df1 %>%
  dplyr::filter(CVC_bin == 1) %>%
  unique()
newhaz_sub4 <- newhaz %>%
  dplyr::filter(ID %in% mod4$ID)
newhaz2_sub4 <- newhaz2 %>%
  dplyr::filter(ID %in% mod4$ID)

# Count variable variation for model 4 success
Cph4s <- coxph(Surv(tstart,tstop,success) ~ Oth_inv + Pub_gr +
                 patent_time + Location + strata(Sector) + 
                 strata(Year.Founded), 
               data=newhaz_sub4, cluster=ID)

summary(Cph4s)

# Count variable variation for model 4 failure
Cph4f <- coxph(Surv(tstart,tstop,failure) ~ Oth_inv + Pub_gr +
                 patent_time + Location + strata(Sector) + 
                 strata(Year.Founded), 
               data=newhaz_sub4, cluster=ID)

summary(Cph4f)

# Dollar investment amounts
Cph4s_amt <- coxph(Surv(tstart,tstop,success) ~ oth_amt +
                     pub_amt + patent_time + Location + strata(Sector) +
                     strata(Year.Founded), 
                   data=newhaz2_sub4, cluster=ID)

summary(Cph4s_amt)

# Dollar investment amounts
Cph4f_amt <- coxph(Surv(tstart,tstop,failure) ~ oth_amt +
                     pub_amt + patent_time + Location + strata(Sector) +
                     strata(Year.Founded), 
                   data=newhaz2_sub4, cluster=ID)

summary(Cph4f_amt)

###################################
# Model 5                         #
###################################

newhaz_sub5 <- newhaz %>%
  dplyr::filter(!(ID %in% mod4$ID))
newhaz2_sub5 <- newhaz2 %>%
  dplyr::filter(!(ID %in% mod4$ID))

# Count variable variation for model 5 success
Cph5s <- coxph(Surv(tstart,tstop,success) ~ Oth_inv + Pub_gr +
                 patent_time + Location + strata(Sector) + 
                 strata(Year.Founded), 
               data=newhaz_sub5, cluster=ID)

summary(Cph5s)

# Count variable variation for model 5 failure
Cph5f <- coxph(Surv(tstart,tstop,failure) ~ Oth_inv + Pub_gr +
                 patent_time + Location + strata(Sector) + 
                 strata(Year.Founded), 
               data=newhaz_sub5, cluster=ID)

summary(Cph5f)

# Dollar investment amounts
Cph5s_amt <- coxph(Surv(tstart,tstop,success) ~ oth_amt +
                     pub_amt + patent_time + Location + strata(Sector) +
                     strata(Year.Founded), 
                   data=newhaz2_sub5, cluster=ID)

summary(Cph5s_amt)

# Dollar investment amounts
Cph5f_amt <- coxph(Surv(tstart,tstop,failure) ~ oth_amt +
                     pub_amt + patent_time + Location + strata(Sector) +
                     strata(Year.Founded), 
                   data=newhaz2_sub5, cluster=ID)

summary(Cph5f_amt)
#################################################
#                                               #
# Exits - CPH regression with subnational and   #
# international scale separated                 #
#                                               #
#################################################
Cph1s_sub <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                     + patent_time + Location + nation_bin +
                     subnat_bin + inter_bin + strata(Sector) + 
                     strata(Year.Founded), 
                   data=newhaz1, cluster=ID)

summary(Cph1s_sub)

# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(Cph1s_sub) 
print(temp)    

#################################################
# Failure - CPH regression with subnational     #
#################################################
Cph1f_sub <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                     + patent_time + Location + nation_bin +
                     subnat_bin + inter_bin + strata(Sector) + 
                     strata(Year.Founded), 
                   data=newhaz1, cluster=ID)

summary(Cph1f_sub)

# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(Cph1f_sub) 
print(temp)    

#########################################
# Model 2 variation
#########################################
# Now check for significance of public grant
# scale for only startups that got public grants
newhaz_sub2 <- newhaz1 %>%
  dplyr::filter(ID %in% mod2$ID)

Cph2s_sub <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                   + patent_time + Location + nation_bin +
                   subnat_bin + inter_bin + strata(Sector) + 
                   strata(Year.Founded), 
               data=newhaz_sub2, cluster=ID)

summary(Cph2s_sub)

# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(Cph2s_sub) 
print(temp)    
plot(temp, var=1)
ggcoxzph(temp, font.main = 9, font.submain = 9,font.y=9,font.x=9, font.tickslab=9) 

# If dfbeta values are small compared to regression coefficient,
# there are no influential outliers
ggcoxdiagnostics(Cph1s, type = "dfbeta", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 

tbl_regression(Cph1s,exponentiate = TRUE, label = c(CVC ~ 'Corporate Investment',
                                                    Oth_inv ~ "Other Private Investment",
                                                    Pub_gr ~ "Public Grant",
                                                    Location ~ "Location",
                                                    patent_time ~ "Patent Count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/Table2_exits_investmentcounts.docx'))

#################################################
# Failure - CPH regression with subnational     #
#################################################
Cph2f_sub <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                     + patent_time + Location + nation_bin +
                     subnat_bin + inter_bin + strata(Sector) + 
                     strata(Year.Founded), 
                   data=newhaz_sub2, cluster=ID)

summary(Cph2f_sub)

# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(Cph2f_sub) 
print(temp)    

#########################################
# Model 4 variation
#########################################
# Now check for significance of public grant
# scale for only startups that got a corporate investment
newhaz_sub4 <- newhaz1 %>%
  dplyr::filter(ID %in% mod4$ID)

Cph4s_sub <- coxph(Surv(tstart,tstop,success) ~ Oth_inv +
                     + patent_time + Location + nation_bin +
                     subnat_bin + inter_bin + strata(Sector) + 
                     strata(Year.Founded), 
                   data=newhaz_sub4, cluster=ID)

summary(Cph4s_sub)

# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(Cph4s_sub) 
print(temp)    

#################################################
# Failure - CPH regression with subnational     #
#################################################
Cph4f_sub <- coxph(Surv(tstart,tstop,failure) ~ Oth_inv +
                     + patent_time + Location + nation_bin +
                     subnat_bin + inter_bin + strata(Sector) + 
                     strata(Year.Founded), 
                   data=newhaz_sub4, cluster=ID)

summary(Cph4f_sub)

# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(Cph4f_sub) 
print(temp)    
#########################################
# Model 5 variation
#########################################
# Now check for significance of public grant
# scale for only startups that did not get a corporate investment
newhaz_sub5 <- newhaz1 %>%
  dplyr::filter(!(ID %in% mod4$ID))

Cph5s_sub <- coxph(Surv(tstart,tstop,success) ~ Oth_inv +
                     + patent_time + Location + nation_bin +
                     subnat_bin + inter_bin + strata(Sector) + 
                     strata(Year.Founded), 
                   data=newhaz_sub5, cluster=ID)

summary(Cph5s_sub)

# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(Cph5s_sub) 
print(temp) 

#################################################
# Failure - CPH regression with subnational     #
#################################################
Cph5f_sub <- coxph(Surv(tstart,tstop,failure) ~ Oth_inv +
                     + patent_time + Location + nation_bin +
                     subnat_bin + inter_bin + strata(Sector) + 
                     strata(Year.Founded), 
                   data=newhaz_sub5, cluster=ID)

summary(Cph5f_sub)

# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(Cph5f_sub) 
print(temp)    


######################################
# Interaction terms
#####################################

# Interaction terms for investment variables
# Use primary convention of binary variables, not count
Cph1s <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                 Pub_gr + patent_time + Location + strata(Sector) + 
                 strata(Year.Founded),
               #+ CVC:Pub_gr, 
               #+ Oth_inv:Pub_gr, 
               #+ CVC:Oth_inv, 
               #+ CVC:Pub_gr:Oth_inv, 
               data=newhaz1, cluster=ID)

summary(Cph1s)

# Interaction terms for investment variables
# Use primary convention of binary variables, not count
Cph1f <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                 Pub_gr + patent_time + Location + strata(Sector) + 
                 strata(Year.Founded)
               #+ CVC:Pub_gr,
               #+ Oth_inv:Pub_gr,
               #+ CVC:Oth_inv,
               + CVC:Pub_gr:Oth_inv, 
               data=newhaz1, cluster=ID)

summary(Cph1f)

