
library("shiny")
library("shinythemes")
library(dplyr)
library(ggplot2)
library(purrr)
library(readxl)
library(ggthemes)

dirname <- "~/Mercer.Work/analytics/IMDA_SF/Shiny_app_skills_feedback/Skills_survey_report_app/Shiny_app_feedback_report/Mgr_Feedback_report_22Feb18/flatly"
if (!dir.exists(dirname))dir.create(dirname,recursive=TRUE)

data <- read_excel(path= "Mock_up_data_for_app.xlsx",
                   range = "consolidated!B3:G63") 

data2 <- data %>% select(Feedback_seeker, Feedback_giver, Feedback_profile, Competency) %>%
  map_df(as.factor) # use map to change typeof()

Feedback_score <- data$Feedback_score #extract the two cols thaat need to be joined to data2
Remarks <- data$Remarks

# join the components to create a new final dataset
data_final <- cbind(data2, Feedback_score, Remarks)
summary(data_final) # for some reason Remarks is coerced to factors
data_final$Remarks <- as.character(data_final$Remarks)

# Define UI for application that draws a histogram
ui <- fluidPage(
   theme = shinytheme("flatly"), #or "sanstone"
   
   # Application title
   titlePanel(h1("Competency Feedback Report: Manager View")),
   br(),
   hr(),
   p(h2(em(strong(("Welcome!"))))),
   
   h3("You have done well as a manager to focus your team on their developmental goals. 
      Now you will be able to use this interactive dashboard to learn about the feedback they have received
      for their skills survey. This information will allow you to work with the team and create a customised
      learning plan to accelerate their development."),
   
   br(),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
         br(),
         
         selectInput(inputId = "Name",
                     label = strong("For who do you want to generate the report :"),
                     choices = c(levels(data_final$Feedback_seeker), "Team"),
                     selected= data_final$Feedback_seeker[1]) , 
         
         h6(em("Please choose 'Team' to get a combined report.")),
         
         br(),
         
         selectInput(inputId = "Competency",
                     label= strong("Please choose the competency you want to focus on: "),
                     choices= c(levels(data_final$Competency), "All"),
                     selected = data_final$Competency[1]),
         
         h6(em("Please select 'All' to get a combined skills report.")) ,
         
         br()
         
         
         #actionButton(inputId = "goButton",
                      #label = "Go!",
                      #icon = NULL
                      #)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", 
                             plotOutput("Plot")),
                    tabPanel("Data", 
                             DT::dataTableOutput("Table")),
                    tabPanel("Comments", 
                             "This panel is intentionally left blank")
                    )
        )
)) 

# Define server logic
server <- function(input, output) {
  
  # trigger the code based on input$goButon, assemble data based on input$Name and input$Competency 
  
  data_name_competency <- reactive({
    
    if(input$Name =="Team") {if(input$Competency != "All")  
      {filter(data_final, Competency == input$Competency)} else
        {data_final}} else {if(input$Competency != "All") 
          {filter(data_final,Feedback_seeker == input$Name, Competency == input$Competency)} else
          {filter(data_final,Feedback_seeker == input$Name)}
          }
    })
  
 
  # Render the plot
   output$Plot <- renderPlot({
     
     if(input$Name =="Team") {if(input$Competency != "All") 
       {plot_data <- data_name_competency() %>% group_by(Feedback_seeker, Competency) %>% 
         summarise(Overall_avg = round(mean(Feedback_score, na.rm=TRUE),2))
       
       plot_data %>% ggplot(aes(Feedback_seeker, Overall_avg))+
         geom_col(aes(fill= Feedback_seeker))+
         scale_y_continuous(name = "Feedback Score",breaks = seq(0,5,1), limits=c(0,5))+
         scale_x_discrete("")+
         labs(title= paste0("Team Report: ", input$Competency),
              subtitle= paste0("Comparison of average score across raters"))+
         scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
         #guide_legend is sued to customise the legend
         theme_economist() # did not add the quartiles to the plot yet. Need to do later.
       } else {plot_data <- data_name_competency() %>% group_by(Feedback_seeker, Competency) %>% 
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
       }} else {if(input$Competency != "All") 
       {Overall_avg <- data_name_competency() %>% group_by(Competency) %>%  
         summarise(round(mean(Feedback_score, na.rm=TRUE),2)) %>% pull() #use data()
       
       data_plot <- data_name_competency() %>% 
         group_by(Feedback_profile, Competency) %>% 
         summarise(Avg_score = round(mean(Feedback_score, na.rm=TRUE),2))
       
       data_plot %>%  ggplot(aes(Feedback_profile, Avg_score))+
         geom_col(aes(fill=Feedback_profile))+
         scale_y_continuous(name = "Feedback Score",
                            breaks = seq(0,5,1), limits=c(0,5))+
         scale_x_discrete("")+
         geom_hline(aes(yintercept= Overall_avg), linetype= "dashed", colour= "black", size=0.8)+
         geom_text(aes(label=paste0("Overall\n avg score= ", round(Overall_avg,2)),
                       x=3.5, y=Overall_avg+0.05), colour= "black", vjust="top", hjust="center")+ 
         #x values for discrete scale simply 1-3 for 3 levels,
         # use Inf / -Inf for extreme range,experiment wth vjus/ hjust to get the position of the text right
         labs(title= paste0(input$Name,": ", input$Competency),
              subtitle= paste0("Comparison of average score across raters"))+
         scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
         #guide_legend is sued to customise the legend
         coord_flip()+theme_economist()
       } else {Overall_avg <- data_name_competency() %>% group_by(Competency) %>% 
           summarise(Overall_avg = round(mean(Feedback_score, na.rm=TRUE),2))
         
         data_plot <- data_name_competency() %>% group_by(Feedback_profile, Competency) %>% 
           summarise(Avg_score = round(mean(Feedback_score, na.rm=TRUE),2))
         
         data_plot_full <- full_join(data_plot, Overall_avg)
         
         data_plot_full %>%  ggplot(aes(Feedback_profile, Avg_score))+
           geom_col(aes(fill=Feedback_profile))+
           scale_y_continuous(name = "Feedback Score",breaks = seq(0,5,1), limits=c(0,5))+
           scale_x_discrete("")+
           geom_hline(aes(yintercept= Overall_avg), linetype= "dashed", colour= "black", size=0.8)+
           geom_text(aes(label=paste0("Overall\n avg score= ", round(Overall_avg,2)),
                         x=3.5, y= Overall_avg+0.05), colour= "black", vjust="top", hjust="center")+ 
           #x values for discrete scale simply 1-3 for 3 levels,
           # use Inf / -Inf for extreme range,experiment wth vjus/ hjust to get the position of the text right
           labs(title= paste0(input$Name,": "),
                subtitle= paste0("Comparison of average score across raters"))+
           scale_fill_discrete(guide= guide_legend(title=" Feedback provider: "))+
           #guide_legend is sued to customise the legend
           coord_flip()+theme_economist()+ facet_wrap(~ Competency, ncol=1)
         }}
     })
   
   
  # Render the table
   output$Table <- DT::renderDataTable(
     DT::datatable(
     data_name_competency(),
     options= list(pageLength= 10)
     ))
   }

# Run the application 
shinyApp(ui = ui, server = server)

