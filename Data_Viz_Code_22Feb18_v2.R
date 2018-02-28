# Code to be used for the shiny app. Feb 22, 2018, version 2

# load relevant libraries
library(dplyr)
library(ggplot2)
library(purrr)
library(readxl)
library(ggthemes)
library(rlang)
library(debugme)
debugonce(devtools::install)

# set up the data ------------------------------------------------------------------------------

setwd("~/Mercer.Work/analytics/IMDA_SF/Shiny_app_skills_feedback")
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
head(data_final)

# Create individual reports -----------------------------------------------------------

## Indiviudal reports for chosen competency -----------------------------------------

Name <- "Sarah Tan" #inputID$Name
competency_name <- "All" #inputID$Competency

# create dataset based on name and competency combination

  #if(Name =="Team") {filter(data_final, Competency == competency_name)} else
    #{ filter(data_final, Feedback_seeker == Name, Competency == competency_name) } # works. develop this further

data_name_competency <- if(Name =="Team") {if(competency_name != "All")
                              {filter(data_final, Competency == competency_name)} else
                                {data_final}} else 
                                  {if(competency_name != "All") 
                                    {filter(data_final,Feedback_seeker == Name, Competency == competency_name)} else 
                                      {filter(data_final,Feedback_seeker == Name)}}

data_name_competency

# draw barplots for the selected dataset data_name_competency - Plot1 - Name != Team, Competency =! All

Overall_avg <- data_name_competency %>% group_by(Competency) %>%  
  summarise(round(mean(Feedback_score, na.rm=TRUE),2)) %>% pull() #use data()

data_plot <- data_name_competency %>% 
                 group_by(Feedback_profile, Competency) %>% 
                 summarise(Avg_score = round(mean(Feedback_score, na.rm=TRUE),2))

#data_plot

data_plot %>%  ggplot(aes(Feedback_profile, Avg_score))+
  geom_col(aes(fill=Feedback_profile))+
  scale_y_continuous(name = "Feedback Score",
                     breaks = seq(0,5,1), limits=c(0,5))+
  scale_x_discrete("")+
  geom_hline(aes(yintercept= Overall_avg), linetype= "dashed", colour= "black", size=0.8)+
  geom_text(aes(label=paste0("Overall\n avg score= ", round(Overall_avg,2)),
                x=3.5, y=Overall_avg+0.05), colour= "black", vjust="top", hjust="center") + 
                #x values for discrete scale simply 1-3 for 3 levels,
                # use Inf / -Inf for extreme range,experiment wth vjus/ hjust to get the position of the text right
  labs(title= paste0(Name,": ", competency_name) ,
       subtitle= paste0("Comparison of average score across raters"))+
  scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
  #guide_legend is sued to customise the legend
  coord_flip()+theme_economist()

## Indiviudal reports for all competencies - plot3 - Name != "All" and Competency_name = "All" , facet_wrap(Plot1)

Overall_avg <- data_name_competency %>% group_by(Competency) %>% 
  summarise(Overall_avg = round(mean(Feedback_score, na.rm=TRUE),2))  #use data()

Overall_avg

data_plot <- data_name_competency %>% 
  group_by(Feedback_profile, Competency) %>% 
  summarise(Avg_score = round(mean(Feedback_score, na.rm=TRUE),2))

data_plot
data_plot_full <- full_join(data_plot, Overall_avg)
data_plot_full

data_plot_full %>%  ggplot(aes(Feedback_profile, Avg_score))+
  geom_col(aes(fill=Feedback_profile))+
  scale_y_continuous(name = "Feedback Score",
                     breaks = seq(0,5,1), limits=c(0,5))+
  scale_x_discrete("")+
  geom_hline(aes(yintercept= Overall_avg), linetype= "dashed", colour= "black", size=0.8)+
  geom_text(aes(label=paste0("Overall\n avg score= ", round(Overall_avg,2)),
                x=3.5, y= Overall_avg+0.05), colour= "black", vjust="top", hjust="center") + 
  #x values for discrete scale simply 1-3 for 3 levels,
  # use Inf / -Inf for extreme range,experiment wth vjus/ hjust to get the position of the text right
  labs(title= paste0(Name,": ") ,
       subtitle= paste0("Comparison of average score across raters"))+
  scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
  #guide_legend is sued to customise the legend
  coord_flip()+theme_economist()+ facet_wrap(~ Competency, ncol=1)


## draw plot when Name= "Team", Competency != "All" - Plot 2 ---------------------------------------------

#inherit data_name_competency

quartiles <- data_name_competency %>% summarise(P25 = round(quantile(Feedback_score, 0.25),2),
                                   P50 = round(quantile(Feedback_score, 0.5),2),
                                   P75 = round(quantile(Feedback_score, 0.75),2),
                                   Avg = round(mean(Feedback_score, na.rm=TRUE),2)) #caal quartiles

plot_data <- data_name_competency %>% group_by(Feedback_seeker, Competency) %>% 
  summarise(Overall_avg = round(mean(Feedback_score, na.rm=TRUE),2))

plot_data %>% ggplot(aes(Feedback_seeker, Overall_avg))+
  geom_col(aes(fill= Feedback_seeker))+
  scale_y_continuous(name = "Feedback Score",breaks = seq(0,5,1), limits=c(0,5))+
  scale_x_discrete("")+
  labs(title= paste0("Team Report: ", competency_name),
       subtitle= paste0("Comparison of average score across raters"))+
  scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
  #guide_legend is sued to customise the legend
  theme_economist() # did not add the quartiles to the plot yet. Need to do later.
  
## draw plot when Name= "Team", Competency = "All" - Plot 4 ---------------------------------------------

#inherit data_name_competency

quartiles <- data_name_competency %>% group_by(Competency) %>%
  summarise(P25 = round(quantile(Feedback_score, 0.25),2),
            P50 = round(quantile(Feedback_score, 0.5),2),
            P75 = round(quantile(Feedback_score, 0.75),2),
            Avg = round(mean(Feedback_score, na.rm=TRUE),2)) #cal quartiles. only averages interesing. quartiles same across competenccies

plot_data <- data_name_competency %>% group_by(Feedback_seeker, Competency) %>% 
  summarise(Overall_avg = round(mean(Feedback_score, na.rm=TRUE),2))

plot_data %>% ggplot(aes(Feedback_seeker, Overall_avg))+
  geom_col(aes(fill= Feedback_seeker))+
  scale_y_continuous(name = "Feedback Score",breaks = seq(0,5,1), limits=c(0,5))+
  scale_x_discrete("")+
  labs(title= "Team Report",
       subtitle="Comparison of average score across raters")+
  scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
  #guide_legend is sued to customise the legend
  theme_economist()+facet_wrap(~ Competency, ncol=1)+coord_flip()

## Use if else for the plots -----------------------------------------------------------------------------------------
Name <- "Sarah Tan" #inputID$Name
competency_name <- "All" #inputID$Competency



if(Name =="Team") {if(competency_name != "All") 
  {plot_data <- data_name_competency %>% group_by(Feedback_seeker, Competency) %>% 
    summarise(Overall_avg = round(mean(Feedback_score, na.rm=TRUE),2))
  
  plot_data %>% ggplot(aes(Feedback_seeker, Overall_avg))+
    geom_col(aes(fill= Feedback_seeker))+
    scale_y_continuous(name = "Feedback Score",breaks = seq(0,5,1), limits=c(0,5))+
    scale_x_discrete("")+
    labs(title= paste0("Team Report: ", competency_name),
         subtitle= paste0("Comparison of average score across raters"))+
    scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
    #guide_legend is sued to customise the legend
    theme_economist() # did not add the quartiles to the plot yet. Need to do later.
   } else 
     {plot_data <- data_name_competency %>% group_by(Feedback_seeker, Competency) %>% 
       summarise(Overall_avg = round(mean(Feedback_score, na.rm=TRUE),2))
     
     plot_data %>% ggplot(aes(Feedback_seeker, Overall_avg))+
       geom_col(aes(fill= Feedback_seeker))+
      scale_y_continuous(name = "Feedback Score",breaks = seq(0,5,1), limits=c(0,5))+
      scale_x_discrete("")+
      labs(title= "Team Report",
           subtitle="Comparison of average score across raters")+
      scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
      #guide_legend is sued to customise the legend
      theme_economist()+facet_wrap(~ Competency, ncol=1)+coord_flip()
    }} else {if(competency_name != "All") 
      {Overall_avg <- data_name_competency %>% group_by(Competency) %>%  
        summarise(round(mean(Feedback_score, na.rm=TRUE),2)) %>% pull() #use data()
      
      data_plot <- data_name_competency %>% 
        group_by(Feedback_profile, Competency) %>% 
        summarise(Avg_score = round(mean(Feedback_score, na.rm=TRUE),2))
      
      #data_plot
      
      data_plot %>%  ggplot(aes(Feedback_profile, Avg_score))+
        geom_col(aes(fill=Feedback_profile))+
        scale_y_continuous(name = "Feedback Score",
                           breaks = seq(0,5,1), limits=c(0,5))+
        scale_x_discrete("")+
        geom_hline(aes(yintercept= Overall_avg), linetype= "dashed", colour= "black", size=0.8)+
        geom_text(aes(label=paste0("Overall\n avg score= ", round(Overall_avg,2)),
                      x=3.5, y=Overall_avg+0.05), colour= "black", vjust="top", hjust="center") + 
        #x values for discrete scale simply 1-3 for 3 levels,
        # use Inf / -Inf for extreme range,experiment wth vjus/ hjust to get the position of the text right
        labs(title= paste0(Name,": ", competency_name) ,
             subtitle= paste0("Comparison of average score across raters"))+
        scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
        #guide_legend is sued to customise the legend
        coord_flip()+theme_economist()
    } else 
      {Overall_avg <- data_name_competency %>% group_by(Competency) %>% 
        summarise(Overall_avg = round(mean(Feedback_score, na.rm=TRUE),2))  #use data()
      
      data_plot <- data_name_competency %>% 
        group_by(Feedback_profile, Competency) %>% 
        summarise(Avg_score = round(mean(Feedback_score, na.rm=TRUE),2))
      
      data_plot_full <- full_join(data_plot, Overall_avg)
      
      data_plot_full %>%  ggplot(aes(Feedback_profile, Avg_score))+
        geom_col(aes(fill=Feedback_profile))+
        scale_y_continuous(name = "Feedback Score",
                           breaks = seq(0,5,1), limits=c(0,5))+
        scale_x_discrete("")+
        geom_hline(aes(yintercept= Overall_avg), linetype= "dashed", colour= "black", size=0.8)+
        geom_text(aes(label=paste0("Overall\n avg score= ", round(Overall_avg,2)),
                      x=3.5, y= Overall_avg+0.05), colour= "black", vjust="top", hjust="center") + 
        #x values for discrete scale simply 1-3 for 3 levels,
        # use Inf / -Inf for extreme range,experiment wth vjus/ hjust to get the position of the text right
        labs(title= paste0(Name,": ") ,
             subtitle= paste0("Comparison of average score across raters"))+
        scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
        #guide_legend is sued to customise the legend
        coord_flip()+theme_economist()+ facet_wrap(~ Competency, ncol=1)
      }}

