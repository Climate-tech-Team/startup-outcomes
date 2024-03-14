#############################################
# Data visualization for Climate-tech Start-up Outcome Analysis
# Kathleen Kennedy
#############################################
library(tidyverse)
library(ggpubr)
library(dotwhisker)
library(broom)

#===================================
# Read in data
#===================================

# Model 1 - all startups with at least one funding source
haz_df1 <- read.csv(paste0(getwd(),'/outputs/hazard_data.csv')) %>%
  mutate(age = 2021 - Year.Founded)
# Model 2 - All startups with at least one public grant
haz_df2 <- read.csv(paste0(getwd(),'/outputs/hazard_data.csv')) %>%
  filter(Public_grant_bin == 1) %>%
  mutate(age = 2021 - Year.Founded)
# Model 3 - All startups with no public funding
haz_df3 <- read.csv(paste0(getwd(),'/outputs/hazard_data.csv')) %>%
  filter(Public_grant_bin == 0) %>%
  mutate(age = 2021 - Year.Founded)
# Model 4 - All startups with at least one investment from a corporation
haz_df4 <- read.csv(paste0(getwd(),'/outputs/hazard_data.csv')) %>%
  filter(CVC_bin == 1) %>%
  mutate(age = 2021 - Year.Founded)
# Model 5 - All startups with no corporate funding
haz_df5 <- read.csv(paste0(getwd(),'/outputs/hazard_data.csv')) %>%
  filter(CVC_bin == 0) %>%
  mutate(age = 2021 - Year.Founded)

#===================================
# Sector charts for sub-populations
#===================================
mod1 <- ggplot(data=haz_df1, aes(x=Sector)) +
  geom_bar(fill='#1CC447', alpha = 0.7) +
  labs(y = 'Number of Start-ups') +
  #geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Model 1: All Startups") + 
  theme_minimal() +
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

mod2 <- ggplot(data=haz_df2, aes(x=Sector)) +
  geom_bar(fill='#1CC447', alpha = 0.7) +
  labs(y = 'Number of Start-ups') +
  ggtitle("Model 2: Startups with Public Grants") + 
  theme_minimal() +
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

mod3 <- ggplot(data=haz_df3, aes(x=Sector)) +
  geom_bar(fill='#1CC447', alpha = 0.7) +
  labs(y = 'Number of Start-ups') +
  ggtitle("Model 3: Startups without Public Grants") +
  theme_minimal() +
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

mod4 <- ggplot(data=haz_df4, aes(x=Sector)) +
  geom_bar(fill='#1CC447', alpha = 0.7) +
  labs(y = 'Number of Start-ups') +
  ggtitle("Model 4: Startups with Corporate Investment") + 
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  #theme(axis.title.y = element_text('Number of Start-ups')) +
  theme(axis.text.x = element_text(angle = 45,hjust=1)) 


ggarrange(mod1, mod2, mod3, mod4,
          ncol = 2, nrow = 2)
ggsave(paste0(getwd(), "/figs/sector_by_model.png"), width = 9, height = 6)

#================================
# Historgrams of age
#================================
h1 <- ggplot(haz_df1, aes(x=age)) + 
  geom_histogram(binwidth = 1,color = 'black', fill = 'gray') + 
  ggtitle('Model 1') + xlab('') +
  ylab('Number of Start-ups') +
  theme_minimal() +
  theme(plot.title = (element_text(hjust = 0.5)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())
h2 <- ggplot(haz_df2, aes(x=age)) + 
  geom_histogram(binwidth = 1,color = 'black', fill = 'gray') + 
  ggtitle('Model 2') + 
  ylab('') + xlab('') +
  theme_minimal() +
  theme(plot.title = (element_text(hjust = 0.5)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())
h3 <- ggplot(haz_df3, aes(x=age)) + 
  geom_histogram(binwidth = 1,color = 'black', fill = 'gray') + 
  ggtitle('Model 3') + 
  xlab('Start-up Age') +
  ylab('') +
  theme_minimal() +
  theme(plot.title = (element_text(hjust = 0.5)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())
h4 <- ggplot(haz_df4, aes(x=age)) + 
  geom_histogram(binwidth = 1,color = 'black', fill = 'gray') + 
  ggtitle('Model 4') + 
  ylab('Number of Start-ups') +
  xlab('Start-up Age') +
  theme_minimal() +
  theme(plot.title = (element_text(hjust = 0.5)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())
h5 <- ggplot(haz_df5, aes(x=age)) + 
  geom_histogram(binwidth = 1,color = 'black', fill = 'gray') + 
  ggtitle('Model 5') + 
  xlab('Start-up Age') +
  ylab('') +
  theme_minimal() +
  theme(plot.title = (element_text(hjust = 0.5)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

ggarrange(h1, h2, h3, h4, h5, ncol = 3, nrow = 2)
ggsave(paste0(getwd(), "/figs/startup_ages.png"), width = 6.5, height = 3.5, dpi = 200)

#================================
# CPH HR plots
#================================
tidyExit1 <- broom::tidy(myCph1s, exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::mutate(model = 'Model 1')
tidyExit2 <- broom::tidy(myCph2s, exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::mutate(model = 'Model 2')
tidyExit3 <- broom::tidy(myCph3s, exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::mutate(model = 'Model 3')
tidyExit4 <- broom::tidy(myCph4s, exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::mutate(model = 'Model 4')
tidyExit5 <- broom::tidy(myCph5s, exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::mutate(model = 'Model 5')

tidyExits <- rbind(tidyExit1,tidyExit2,tidyExit3,tidyExit4,tidyExit5) %>%
 # select(!(std.error)) %>%
#  select(!(robust.se)) %>%
  group_by(model)

dwplot(tidyExits) %>%
  relabel_predictors(
    c(
      CVC = "Corporate Investment",
      Pub_gr = "Public Grant",
      Oth_inv = "Other Investment",
      Location = "Location",
      patent_time = "Patent Count"
    )) +
  theme_minimal() +
  theme(legend.justification=c(.02, .993), legend.position=c(.7, .97),
        legend.title = element_blank()) + 
        #legend.background = element_rect(color="white"), 
        #text=element_text(size=11)))
        #axis.text.y=element_text(angle=25), 
        # plot.title=element_text(size=10),
        #legend.key.size = unit(0.1, 'cm')) + 
  geom_vline(xintercept = 1, colour = "grey60", linetype = 2) #+
# ggtitle('Model 5: Logistic Regression of Start-up Survival')

ggsave(paste0(getwd(), "/figs/HR_plot_exits.png"), width = 6, height = 4, dpi = 100)
#================================
# Ordered survival time plot
#================================
# https://quantdev.ssri.psu.edu/tutorials/part-2-single-episode-cox-regression-model-time-invariant-predictors
summary(haz_df1$time_to_success)
haz_df1$count <- c(1:length(haz_df1$Company))
haz_df1 <- haz_df1[order(-haz_df1$time_to_success,haz_df1$Success),]
haz_df1$order <- c(1:length(haz_df1$Company))

ggplot(data=haz_df1, aes(x=time_to_success, y=order)) +
  geom_rect(xmin=0,xmax=haz_df1$time_to_success,ymin=haz_df1$order,ymax=haz_df1$order+1, colour="lightgray") +
  geom_rect(xmin=haz_df1$time_to_success-2,xmax=haz_df1$time_to_success,ymin=haz_df1$order,ymax=haz_df1$order+1,
            fill=factor(haz_df1$Success+1)) +
  geom_vline(xintercept= 9,linetype="solid") +
  #scale_x_continuous(breaks=seq(0,480,120)) +
  #geom_text(aes(140, 95, label="Median Survival Time")) +
  xlab("Time to Success (Years)") + ylab("Start-ups (ordered by time to success)") +
  ggtitle("Time to Success for Each Startup") +
  theme_classic() +
  theme(legend.position="none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))
