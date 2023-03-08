# Code to perform Cox Proportional Hazards Regression and generate figures
# Kathleen Kennedy 2022
########################################################################
library(tidyverse)
library(survival)
library(survminer)
library(coxme)
library(gtsummary)
library(flextable)
library(broom)
library(stringr)


###################################################
#                                                 #
# Create dataframe for hazards analysis           #
#                                                 #
###################################################
# Read in data from i3_data_cleaning.ipynb and get in format for analysis
haz_df1 <- read.csv(paste0(getwd(),'/outputs/hazard_data_recurrent.csv')) %>%
  dplyr::mutate(log_patent = log1p(patent_count)) %>% 
  dplyr::mutate(end_period = 2021 - Year.Founded) %>%
  rename(total_capital = Total.paid.in.Capital....)

id_list <- haz_df1 %>% select(ID,company,Year.Founded) %>% unique()

patent_df <- read.csv(paste0(getwd(),'/patents/output/patent_counts_2.csv')) %>%
  dplyr::right_join(id_list) %>%
  mutate(t_pat = year - Year.Founded) %>%
  filter(t_pat >= 0) %>%
  mutate(pat_log = log1p(pat_count))

end_times <- haz_df1 %>% 
  select(ID,end_period) %>%
  unique()

base <- haz_df1 %>% 
  select(ID, Company, Sector, Status, Year.Founded, age, total_capital, patent_count, log_patent, Location) %>% 
  unique()

invs <- haz_df1 %>%
  select(ID,t_inv)
CVC_inv <- haz_df1 %>% 
  select(ID,t_inv, CVC_bin) %>%
  filter(CVC_bin == 1) %>%
  unique()
Oth_inv <- haz_df1 %>% 
  select(ID,t_inv, Other_inv_bin) %>%
  filter(Other_inv_bin == 1) %>%
  unique()
Pub_gr <- haz_df1 %>%
  select(ID,t_inv, Pub_grant_bin) %>%
  filter(Pub_grant_bin == 1) %>%
  unique()
success <- haz_df1 %>%
  select(ID, time_to_success) %>% 
  drop_na() %>% unique()
failure <- haz_df1 %>%
  select(ID,time_to_failure) %>% 
  drop_na() %>% unique()
ipo <- haz_df1 %>% 
  select(ID,time_to_ipo) %>%
  drop_na() %>% unique()
ma <- haz_df1 %>%
  select(ID,time_to_ma) %>%
  drop_na() %>% unique()
outcome <- haz_df1 %>%
  select(ID,time_to_outcome) %>%
  drop_na() %>% unique()


newhaz1 <- tmerge(data1=base,data2=end_times,id=ID,tstop=end_period)
newhaz1 <- tmerge(newhaz1,patent_df,id=ID, patent_time=tdc(t_pat,pat_count)) 
newhaz1 <- tmerge(newhaz1,CVC_inv,id=ID, CVC=tdc(t_inv,CVC_bin))
newhaz1 <- tmerge(newhaz1,Oth_inv,id=ID, Oth_inv=tdc(t_inv,Other_inv_bin))
newhaz1 <- tmerge(newhaz1,Pub_gr,id=ID, Pub_gr=tdc(t_inv,Pub_grant_bin))

newhaz1 <- tmerge(newhaz1,patent_df,id=ID, patent_time_cum=cumtdc(t_pat,pat_count)) 
newhaz1 <- tmerge(newhaz1,CVC_inv,id=ID, CVC_cum=cumtdc(t_inv,CVC_bin))
newhaz1 <- tmerge(newhaz1,Oth_inv,id=ID, Oth_inv_cum=cumtdc(t_inv,Other_inv_bin))
newhaz1 <- tmerge(newhaz1,Pub_gr,id=ID, Pub_gr_cum=cumtdc(t_inv,Pub_grant_bin))

newhaz1 <- tmerge(newhaz1,success,id=ID, success=event(time_to_success))
newhaz1 <- tmerge(newhaz1,failure,id=ID, failure=event(time_to_failure))
newhaz1 <- tmerge(newhaz1,ipo,id=ID, ipo=event(time_to_ipo))
newhaz1 <- tmerge(newhaz1,ma,id=ID, ma=event(time_to_ma))
newhaz1 <- tmerge(newhaz1,outcome,id=ID, outcome=event(time_to_outcome))

newhaz1[is.na(newhaz1)] <- 0

newhaz1 <- newhaz1 %>%
  mutate(Sector = ifelse(Sector == "", "uncategorized", Sector))

###################################################
#                                                 #
# Model 1: Full population model of all start-ups #
#                                                 #
###################################################
#################################################
# Exits - Cox proportional hazards regression   #
#################################################
myCph1s <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                   Pub_gr + patent_time + Location + strata(Sector) + 
                   strata(Year.Founded), 
                 data=newhaz1, cluster=ID)

summary(myCph1s)
# Check assumptions:
# If no variables are significant, 
# proportional hazards assumption is valid
temp <- cox.zph(myCph1s) 
print(temp)    
plot(temp, var=1)
ggcoxzph(temp, font.main = 9, font.submain = 9,font.y=9,font.x=9, font.tickslab=9) 

# If dfbeta values are small compared to regression coefficient,
# there are no influential outliers
ggcoxdiagnostics(myCph1s, type = "dfbeta", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 

# Check linearity of different transformations of patent data
ggcoxfunctional(Surv(tstart,tstop,success) ~ patent_time + log1p(patent_time) + sqrt(patent_time), data = newhaz1) 

tbl_regression(myCph1s,exponentiate = TRUE, label = c(CVC ~ 'Corporate Investment',
                                                      Oth_inv ~ "Other Private Investment",
                                                      Pub_gr ~ "Public Grant",
                                                      Location ~ "Location",
                                                      patent_time ~ "Patent Count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/Table2_exits.docx'))

# Cumulative hazard plots faceted by investors
myFit <- survfit(Surv(tstart,tstop,success) ~ CVC,
                 data=newhaz1, cluster=ID)

myFitpub <- survfit(Surv(tstart,tstop,success) ~ Pub_gr,
                    data=newhaz1, cluster=ID)

# Cumulative hazard plots faceted by year founded
ggsurvplot_facet(myFit, data = newhaz1, fun = "cumhaz", conf.int = TRUE, facet.by = "Year.Founded",
                 title = 'Corporate Investment Impact on Exits', font.title = c(11),
                 font.x = c(7), font.y = c(7), font.tickslab = c(7),
                 censor = FALSE,
                 legend.title = '', legend = 'bottom',
                 palette = c('#b2182b','#2166ac'),
                 legend.labs = c('No Corporate Investment','Corporate Investment'),
                 short.panel.labs = TRUE, panel.labs.font = list(size = 7),
                 xlab = 'Time since founding (yrs)',
                 ggtheme = theme_minimal()) 
ggsave(paste0(getwd(), "/figs/model1_s_yrfound.png"), width = 6.5, height = 5)

ggsurvplot_facet(myFitpub, data = newhaz1, fun = "cumhaz", conf.int = TRUE, facet.by = "Year.Founded",
                 title = 'Public Grant Impact on Exits', font.title = c(11),
                 font.x = c(7), font.y = c(7), font.tickslab = c(7),
                 censor = FALSE,
                 legend.title = '', legend = 'bottom',
                 palette = c('#b2182b','#2166ac'),
                 legend.labs = c('No Public Grant','Public Grant'),
                 short.panel.labs = TRUE, panel.labs.font = list(size = 7),
                 xlab = 'Time since founding (yrs)',
                 ggtheme = theme_minimal()) 
ggsave(paste0(getwd(), "/figs/model1_s_yrfound_pub.png"), width = 6.5, height = 5)

# Cumulative hazard plots faceted by sectors 
ggsurvplot_facet(myFit, data = newhaz1, fun = "cumhaz", conf.int = TRUE, 
                 facet.by = "Sector", ylim = c(0,1.5),
                 title = 'Corporate Investment Impact on Exits', 
                 font.title = c(9), font.x = c(7), font.y = c(7), 
                 font.tickslab = c(7), censor = FALSE,
                 palette = c('#b2182b','#2166ac'),
                 legend.labs = c('No Corporate Investment','Corporate Investment'),
                 short.panel.labs = TRUE, panel.labs.font = list(size = 7),
                 legend.title = '', legend = c('bottom'),
                 xlab = 'Time since founding (yrs)',
                 ggtheme = theme_minimal()) +
  theme(legend.text=element_text(size=7),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="mm"),
        legend.position="bottom")
ggsave(paste0(getwd(), "/figs/model1_s_sectors.png"), width = 6.5, height = 5)


sect_check <- newhaz1 %>% filter(Sector %in% c('energy efficiency','transportation',
                                               'advanced materials','other cleantech',
                                               'solar','agriculture & food'))
sect_check <- sect_check %>% 
  mutate(Sector = factor(Sector, levels = c('energy efficiency','transportation',
                                            'advanced materials','other cleantech',
                                            'solar','agriculture & food')))

myFit <- survfit(Surv(tstart,tstop,success) ~ CVC,
                 data=sect_check, cluster=ID)

myFitpub <- survfit(Surv(tstart,tstop,success) ~ Pub_gr,
                    data=sect_check, cluster=ID)

#ggsurv <- ggsurvplot(myFitpub, fun = "cumhaz", conf.int = TRUE)
#ggsurv$plot +theme_bw() + facet_grid(~Sector) + theme(legend.position="none")

sect_s_corp <- ggsurvplot_facet(myFit, data = sect_check, fun = "cumhaz", conf.int = TRUE, 
                                facet.by = "Sector", nrow = 1, ylim = c(0,1.5),
                                title = 'b. Corporate Investment Impact on Exits', 
                                font.title = c(9), font.x = c(7), font.y = c(7), 
                                font.tickslab = c(7), censor = FALSE,
                                palette = c('#b2182b','#2166ac'),
                                legend.labs = c('No Corporate\nInvestment','Corporate\nInvestment'),
                                short.panel.labs = TRUE, panel.labs.font = list(size = 7),
                                legend.title = '', legend = c('bottom'),
                                xlab = 'Time since founding (yrs)',
                                ggtheme = theme_minimal()) +
  theme(legend.text=element_text(size=7),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="mm"),
        legend.position="right")
#ggsave(paste0(getwd(), "/figs/model1_s_sectors.png"), width = 3, height = 6)

sect_s_pub <- ggsurvplot_facet(myFitpub, data = sect_check, fun = "cumhaz", conf.int = TRUE, 
                               facet.by = "Sector", nrow = 1, ylim = c(0,1.5), 
                               title = 'a. Public Grant Impact on Exits', 
                               font.title = c(9), font.x = c(7), font.y = c(7), 
                               font.tickslab = c(7), censor = FALSE,
                               palette = c('#b2182b','#2166ac'),
                               legend.labs = c('No Public\nGrant','Public Grant'),
                               short.panel.labs = TRUE, panel.labs.font = list(size = 7),
                               legend.title = '', legend = c('bottom'),
                               xlab = 'Time since founding (yrs)',
                               ggtheme = theme_minimal()) +
  theme(legend.text=element_text(size=7),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="mm"),
        legend.position="right")
#ggsave(paste0(getwd(), "/figs/model1_s_sectors_pub.png"), width = 3, height = 6)



#################################################
# Failure - Cox proportional hazards regression #
#################################################
myCph1f <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                   Pub_gr + patent_time + Location + strata(Sector) +
                   strata(Year.Founded), 
                 data=newhaz1, cluster=ID)

summary(myCph1f)
# Chech assumptions
temp2 <- cox.zph(myCph1f) 
print(temp2)            
plot(temp2, var=4)

tbl_regression(myCph1f,exponentiate = TRUE, label = c(CVC ~ 'Corporate investment',
                                                      Oth_inv ~ "Other equity investment",
                                                      Pub_gr ~ "Public grant",
                                                      Location ~ "Location",
                                                      patent_time ~ "Patent count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/Table2_failure.docx'))

# Cumulative hazard plots faceted by investors
myFit <- survfit(Surv(tstart,tstop,failure) ~ CVC,
                 data=newhaz1, cluster=ID)

myFitpub <- survfit(Surv(tstart,tstop,failure) ~ Pub_gr,
                    data=newhaz1, cluster=ID)

# Cumulative hazard plots faceted by year founded
ggsurvplot_facet(myFit, data = newhaz1, fun = "cumhaz", conf.int = TRUE, facet.by = "Year.Founded",
                 title = 'Corporate Investment Impact on Failure', font.title = c(11),
                 font.x = c(7), font.y = c(7), font.tickslab = c(7),
                 censor = FALSE,
                 legend.title = '', legend = 'bottom',
                 palette = c('#b2182b','#2166ac'),
                 legend.labs = c('No Corporate Investment','Corporate Investment'),
                 short.panel.labs = TRUE, panel.labs.font = list(size = 7),
                 xlab = 'Time since founding (yrs)',
                 ggtheme = theme_minimal()) 
ggsave(paste0(getwd(), "/figs/model1_f_yrfound.png"), width = 6.5, height = 5)

ggsurvplot_facet(myFitpub, data = newhaz1, fun = "cumhaz", conf.int = TRUE, facet.by = "Year.Founded",
                 title = 'Public Grant Impact on Failure', font.title = c(11),
                 font.x = c(7), font.y = c(7), font.tickslab = c(7),
                 censor = FALSE,
                 legend.title = '', legend = 'bottom',
                 palette = c('#b2182b','#2166ac'),
                 legend.labs = c('No Public Grant','Public Grant'),
                 short.panel.labs = TRUE, panel.labs.font = list(size = 7),
                 xlab = 'Time since founding (yrs)',
                 ggtheme = theme_minimal()) 
ggsave(paste0(getwd(), "/figs/model1_f_yrfound_pub.png"), width = 6.5, height = 5)

# Cumulative hazard plots faceted by sectors 
myFit <- survfit(Surv(tstart,tstop,failure) ~ CVC,
                 data=sect_check, cluster=ID)

myFitpub <- survfit(Surv(tstart,tstop,failure) ~ Pub_gr,
                    data=sect_check, cluster=ID)

sect_f_corp <- ggsurvplot_facet(myFit, data = sect_check, fun = "cumhaz", conf.int = TRUE, 
                                facet.by = "Sector", censor = FALSE, ylim = c(0,1.5),
                                title = 'd. Corporate Investment Impact on Failure', 
                                font.title = c(9), font.x = c(7), font.y = c(7), 
                                font.tickslab = c(7), nrow =1,
                                palette = c('#b2182b','#2166ac'),
                                legend.labs = c('No Corporate\nInvestment','Corporate\nInvestment'),
                                short.panel.labs = TRUE, panel.labs.font = list(size = 7),
                                legend.title = '', legend = c('bottom'),
                                xlab = 'Time since founding (yrs)',
                                ggtheme = theme_minimal()) +
  theme(legend.text=element_text(size=7),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="mm"),
        legend.position="right")

sect_f_pub <- ggsurvplot_facet(myFitpub, data = sect_check, fun = "cumhaz", conf.int = TRUE, 
                               facet.by = "Sector", censor = FALSE, ylim = c(0,1.5),
                               title = 'c. Public Grant Impact on Failure',
                               font.title = c(9), font.x = c(7), font.y = c(7), 
                               font.tickslab = c(7), nrow = 1,
                               palette = c('#b2182b','#2166ac'),
                               legend.labs = c('No Public\nGrant','Public Grant'),
                               short.panel.labs = TRUE, panel.labs.font = list(size = 7),
                               legend.title = '', legend = c('bottom'),
                               xlab = 'Time since founding (yrs)',
                               ggtheme = theme_minimal()) +
  theme(legend.text=element_text(size=7),
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="mm"),
        legend.position="right")

##############################################################
#                                                            #
# Model 2: Start-ups that received at least one public grant #
#                                                            #
##############################################################
mod2 <- haz_df1 %>%
  dplyr::filter(Pub_grant_bin == 1) %>%
  unique()
newhaz2 <- newhaz1 %>%
  dplyr::filter(ID %in% mod2$ID)

#################################################
# Exits - Cox proportional hazards regression   #
#################################################
myCph2s <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                   patent_time + Location + strata(Sector) + 
                   strata(Year.Founded), 
                 data=newhaz2, cluster=ID)

summary(myCph2s)
# Check assumptions
temp <- cox.zph(myCph2s) 
print(temp)                  
plot(temp, var=2)
#ggcoxzph(temp, font.main = 9, font.submain = 9,font.y=9,font.x=9, font.tickslab=9) 

tbl_regression(myCph2s,exponentiate = TRUE, label = c(CVC ~ 'Corporate Investment',
                                                      Oth_inv ~ "Other Private Investment",
                                                      Location ~ "Location",
                                                      patent_count ~ "Patent Count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/model2_summary_success.docx'))

myFit <- survfit(Surv(tstart,tstop,success) ~ CVC,
                 data=newhaz2, cluster=ID)
s2 <- ggsurvplot(myFit, data = newhaz2, fun = "cumhaz", conf.int = TRUE, #facet.by = 'Oth_inv',
           title = 'Exits: Start-ups with Public Grants', ylim = c(0,.75), censor = TRUE,
           legend.labs = c('No CVC Investment','CVC Investment'),
           legend.title = "", legend = c("none"),
           short.panel.labs = TRUE,
           ggtheme = theme_minimal())
ggsave(paste0(getwd(), "/figs/model2_s.png"), width = 6, height = 4)


ggsurvplot_facet(myFit, data = newhaz2, fun = "cumhaz", conf.int = TRUE, 
           title = 'Corporate Investment Impact on Success', facet.by = "Sector",
           legend.labs = c('No Corporate Investment','Corporate Investment'),
           ggtheme = theme_minimal())
ggsave(paste0(getwd(), "/figs/model2_s_sectors.png"), width = 6, height = 4)

#################################################
# Failure - Cox proportional hazards regression #
#################################################
myCph2f <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                   patent_time + Location + strata(Sector) +
                   strata(Year.Founded), 
                 data=newhaz2, cluster=ID)

summary(myCph2f)
# Check assumptions
temp2 <- cox.zph(myCph2f) 
print(temp2)
plot(temp2, var=1)

tbl_regression(myCph2f,exponentiate = TRUE, label = c(CVC ~ 'Corporate Investment',
                                                      Oth_inv ~ "Other Private Investment",
                                                      Location ~ "Location",
                                                      patent_count ~ "Patent Count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/model2_summary_failure.docx'))

myFitf <- survfit(Surv(tstart,tstop,failure) ~ CVC,
                  data=newhaz2, cluster=ID)
ggsurvplot(myFitf, data = newhaz2, fun = "cumhaz", conf.int = TRUE, 
           title = 'Corporate Investment Impact on Failure',
           ggtheme = theme_minimal())

##############################################################
#                                                            #
# Model 3: Start-ups that did not receive any public grants  #
#                                                            #
##############################################################
newhaz3 <- newhaz1 %>%
  dplyr::filter(!(ID %in% mod2$ID))

#################################################
# Exits - Cox proportional hazards regression   #
#################################################
myCph3s <- coxph(Surv(tstart,tstop,success) ~ CVC + Oth_inv +
                   patent_time + Location + strata(Sector) + 
                   strata(Year.Founded), 
                 data=newhaz3, cluster=ID)

summary(myCph3s)
# Check assumptions
temp <- cox.zph(myCph3s) 
print(temp)                  # display the results 
plot(temp, var=1)
#ggcoxzph(temp, font.main = 9, font.submain = 9,font.y=9,font.x=9, font.tickslab=9) 

termplot(myCph2s, term=2, se=TRUE, col.term=1, col.se=1)

tbl_regression(myCph3s,exponentiate = TRUE, label = c(CVC ~ 'Corporate Investment',
                                                      Oth_inv ~ "Other Private Investment",
                                                      Location ~ "Location",
                                                      patent_count ~ "Patent Count")) %>% 
  modify_header(label='**Variable**') %>% 
  modify_footnote(update = everything() ~ NA) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),'/tables/model3_summary_success.docx'))

myFit <- survfit(Surv(tstart,tstop,success) ~ CVC,
                 data=newhaz3, cluster=ID)
ggsurvplot(myFit, data = newhaz3, fun = "cumhaz", conf.int = TRUE, 
           title = 'Corporate Investment Impact on Success',
           ggtheme = theme_minimal())
ggsave(paste0(getwd(), "/model3_s_cvc.png"), width = 6, height = 4)

s3 <- ggsurvplot(myFit, data = newhaz3, fun = "cumhaz", conf.int = TRUE, #facet.by = 'Oth_inv',
                 title = 'Exits: Start-ups without Public Grants', ylim = c(0,.75), censor = TRUE,
                 legend.title = "", legend = c("bottom"),
                 legend.labs = c('No CVC Investment','CVC Investment'),
                 short.panel.labs = TRUE,
                 ggtheme = theme_minimal())

#################################################
# Failure - Cox proportional hazards regression #
#################################################
myCph3f <- coxph(Surv(tstart,tstop,failure) ~ CVC + Oth_inv +
                   patent_time + Location + strata(Sector) +
                   strata(Year.Founded), 
                 data=newhaz3, cluster=ID)

summary(myCph3f)
# Check assumptions
temp2 <- cox.zph(myCph3f) 
print(temp2)                  # display the results 
plot(temp2, var=1)

myFitf <- survfit(Surv(tstart,tstop,failure) ~ CVC,
                  data=newhaz3, cluster=ID)
ggsurvplot(myFitf, data = newhaz3, fun = "cumhaz", conf.int = TRUE, 
           title = 'Corporate Investment Impact on Failure',
           ggtheme = theme_minimal())

#######################################################################
#                                                                     #
# Model 4: Start-ups that received at least one corporate investment  #
#                                                                     #
#######################################################################
mod4 <- haz_df1 %>%
  dplyr::filter(CVC_bin == 1) %>%
  unique()
newhaz4 <- newhaz1 %>%
  dplyr::filter(ID %in% mod4$ID)

#################################################
# Exits - Cox proportional hazards regression   #
#################################################
myCph4s <- coxph(Surv(tstart,tstop,success) ~ Oth_inv + Pub_gr +
                   patent_time + Location + strata(Sector) + 
                   strata(Year.Founded), 
                 data=newhaz4, cluster=ID)

summary(myCph4s)
# Check assumptions
temp <- cox.zph(myCph4s) 
print(temp)                  # display the results 
plot(temp, var=1)
#ggcoxzph(temp, font.main = 9, font.submain = 9,font.y=9,font.x=9, font.tickslab=9) 

termplot(myCph2s, term=2, se=TRUE, col.term=1, col.se=1)

myFit <- survfit(Surv(tstart,tstop,success) ~ Pub_gr,
                 data=newhaz4, cluster=ID)
ggsurvplot(myFit, data = newhaz4, fun = "cumhaz", conf.int = TRUE, 
                 title = 'Public Grant Impact on Success',
           ggtheme = theme_minimal())
ggsave(paste0(getwd(), "/model4_s_pub.png"), width = 6, height = 4)

s4 <- ggsurvplot(myFit, data = newhaz4, fun = "cumhaz", conf.int = TRUE, facet.by = 'Oth_inv',
                 title = 'Model 4 (CVC Investment): Success', ylim = c(0,.75), censor = TRUE,
                 legend.title = "", legend = c("bottom"),
                 legend.labs = c('No Public Grant','Public Grant'),
                 panel.labs = list(Oth_inv = c('No Other Investment', 'Other Investment')),
                 short.panel.labs = TRUE,
                 ggtheme = theme_minimal())

#################################################
# Failure - Cox proportional hazards regression #
#################################################
myCph4f <- coxph(Surv(tstart,tstop,failure) ~ Oth_inv + Pub_gr +
                   patent_time + Location + strata(Sector) +
                   strata(Year.Founded), 
                 data=newhaz4, cluster=ID)

summary(myCph4f)
# Check assumptions
temp2 <- cox.zph(myCph4f) 
print(temp2)                  # display the results 
plot(temp2, var=1)

myFitf <- survfit(Surv(tstart,tstop,failure) ~ Pub_gr,
                  data=newhaz4, cluster=ID)
ggsurvplot(myFitf, data = newhaz4, fun = "cumhaz", conf.int = TRUE, 
           title = 'Public Grant Impact on Failure',
           ggtheme = theme_minimal())

#######################################################################
#                                                                     #
# Model 5: Start-ups that did not receive any corporate investments   #
#                                                                     #
#######################################################################
newhaz5 <- newhaz1 %>%
  dplyr::filter(!(ID %in% mod4$ID))

#################################################
# Exits - Cox proportional hazards regression   #
#################################################
myCph5s <- coxph(Surv(tstart,tstop,success) ~ Oth_inv + Pub_gr +
                   patent_time + Location + strata(Sector) + 
                   strata(Year.Founded), 
                 data=newhaz5, cluster=ID)

summary(myCph5s)
# Check assumptions
temp <- cox.zph(myCph5s) 
print(temp)                  # display the results 
plot(temp, var=1)
#ggcoxzph(temp, font.main = 9, font.submain = 9,font.y=9,font.x=9, font.tickslab=9) 

termplot(myCph2s, term=2, se=TRUE, col.term=1, col.se=1)

myFit <- survfit(Surv(tstart,tstop,success) ~ Pub_gr,
                 data=newhaz5, cluster=ID)
ggsurvplot(myFit, data = newhaz5, fun = "cumhaz", conf.int = TRUE, 
           title = 'Public Grant Impact on Success',
           ggtheme = theme_minimal())
ggsave(paste0(getwd(), "/model5_s_pub.png"), width = 6, height = 4)

s5 <- ggsurvplot(myFit, data = newhaz5, fun = "cumhaz", conf.int = TRUE, facet.by = 'Oth_inv',
                 title = 'Model 5 (No CVC Investment): Success', ylim = c(0,.75), censor = TRUE,
                 legend.title = "", legend = c("bottom"),
                 legend.labs = c('No Public Grant','Public Grant'),
                 panel.labs = list(Oth_inv = c('No Other Investment', 'Other Investment')),
                 short.panel.labs = TRUE,
                 ggtheme = theme_minimal())
#################################################
# Failure - Cox proportional hazards regression #
#################################################
myCph5f <- coxph(Surv(tstart,tstop,failure) ~ Oth_inv + Pub_gr +
                   patent_time + Location + strata(Sector) +
                   strata(Year.Founded), 
                 data=newhaz5, cluster=ID)

summary(myCph5f)
# Check assumptions
temp2 <- cox.zph(myCph5f) 
print(temp2)                  # display the results 
plot(temp2, var=1)

myFitf <- survfit(Surv(tstart,tstop,failure) ~ Pub_gr,
                  data=newhaz5, cluster=ID)
ggsurvplot(myFitf, data = newhaz5, fun = "cumhaz", conf.int = TRUE, 
           title = 'Public Grant Impact on Failure',
           ggtheme = theme_minimal())

############################################################################
# Combined plots

ggarrange(sect_s_pub,sect_s_corp,sect_f_pub,sect_f_corp, ncol = 1, nrow = 4, common.legend = FALSE)
ggsave(paste0(getwd(), "/figs/Fig3_sectors.png"), width = 7, height = 7.5)

ggarrange(s2$plot, s3$plot, ncol = 2, nrow = 1, common.legend = TRUE, legend = c('bottom'))
ggsave(paste0(getwd(), "/figs/mod2_mod3_s.png"), width = 7, height = 3.5)

ggarrange(s4, s5, ncol = 1, nrow = 2, common.legend = TRUE, legend = c('bottom'))
ggsave(paste0(getwd(), "/figs/mod4_mod5_s.png"), width = 6, height = 5)

############################################################################