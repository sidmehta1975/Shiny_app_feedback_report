# Code to be used for the shiny app. Feb 22, 2018

# load relevant libraries
library(dplyr)
library(ggplot2)
library(purrr)
library(readxl)
library(ggthemes)

# set up the data ------------------------------------------------------------------------------

## setwd("~/Mercer.Work/analytics/IMDA_SF/Shiny_app_skills_feedback")
data <- read_excel("Mock_up_data_for_app.xlsx", 
                   range = "consolidated!B3:G63") 
str(data) # need to change the typeof() to factor for the following vars

data2 <- data %>% select(Feedback_seeker, Feedback_giver, Feedback_profile, Competency) %>%
  map_df(as.factor) # use map to change typeof()

head(data2) # done 

#extract the two cols thaat need to be joined to data2
Feedback_score <- data$Feedback_score
Remarks <- data$Remarks

# join the components to create a new final dataset
data_final <- cbind(data2, Feedback_score, Remarks)
summary(data_final) # for some reason Remarks is coerced to factors
str(data_final) # strange!
data_final$Remarks <- as.character(data_final$Remarks)
str(data_final) # fine now
summary(data_final)

# Create individual reports -----------------------------------------------------------

## Indiviudal reports for chosen competency -----------------------------------------

### For sarah, Sid Mehta, Cheryl Er
Name <- "Sarah Tan" #inputID$Name
data_name <- data_final %>% filter(Feedback_seeker== Name)

## select competency
competency_name <- "Data Design" #inputID$Competency
data_name_competency <- data_name %>% filter(Competency== competency_name)

# draw barplots for the selected dataset  data_name_competency
avg <- data_name_competency %>% summarise(mean(Feedback_score)) %>% pull() #to be calculated
data_name_competency_bar <- data_name_competency %>% group_by(Feedback_profile) %>%
  summarise(Average_score= round(mean(Feedback_score, na.rm = TRUE), 2)) #to be calculated

data_name_competency_bar %>%  ggplot(aes(Feedback_profile, Average_score))+
  geom_col(aes(fill=Feedback_profile))+
  scale_y_continuous(name = "Feedback Score",
                     breaks = seq(0,5,1), limits=c(0,5))+
  scale_x_discrete("")+
  geom_hline(yintercept= avg, linetype= "dashed", colour= "black", size=0.8)+
  geom_text(aes(label=paste0("Overall\n avg score= ", round(avg,2)),
                x=3.5, y=avg+0.05), 
          #x values for discrete scale simply 1-3 for 3 levels,
           # use Inf / -Inf for extreme range
            colour="black", vjust="top", hjust="center") + 
  #experiment wth vjus/ hjust to get the position of the text right
  labs(title= paste0(Name,": ", competency_name) ,
       subtitle= paste0("Comparison of average score across raters"))+
  scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
  #guide_legend is sued to customise the legend
  coord_flip()+theme_economist()

## Indiviudal reports for all competencies -----------------------------------------

Name <- "Sarah Tan" #inputID$Name
data_name <- data_final %>% filter(Feedback_seeker== Name)

## select competency & create data table
competency_name <- "All" #inputID$Competency
data_name_competency <- data_name

# create the dataset
avg <- data_name_competency %>% group_by(Competency)%>% 
  summarise(Overall_avg=mean(Feedback_score))#Calculate overall_avg 
data_name_competency_bar <- data_name_competency %>% group_by(Competency, Feedback_profile) %>%
  summarise(Average_score= round(mean(Feedback_score, na.rm = TRUE), 2)) #calculate profile-wise avg
data_name_competency_bar <- full_join(data_name_competency_bar, avg)

# draw wrap_facet barplots for all competencies
data_name_competency_bar %>%  ggplot(aes(Feedback_profile, Average_score))+
  geom_col(aes(fill=Feedback_profile))+
  scale_y_continuous(name = "Feedback Score",
                     breaks = seq(0,5,1), limits=c(0,5))+
  scale_x_discrete("")+labs(title= paste0(Name,": ", competency_name) ,
                            subtitle= paste0("Comparison of average score across raters"))+
  scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
  #guide_legend is sued to customise the legend
  theme_economist()+facet_wrap(~ Competency)+
  geom_hline(aes(yintercept=Overall_avg), colour= "black", size=0.8, linetype= "dashed")+
  geom_text(aes(x=2, y=Overall_avg+0.05, label=paste0("Overall avg score: ", round(Overall_avg,2))),
           #x values for discrete scale simply 1-3 for 3 levels,
           # use Inf / -Inf for extreme range
           colour="black", vjust="bottom", hjust="center") 
  #experiment wth vjus/ hjust to get the position of the text right.
  #for some reason vjust is counterinutitive - top/ bottom are reversed
  

