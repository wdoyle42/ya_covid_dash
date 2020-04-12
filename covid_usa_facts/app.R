# Positive COVID-19 Case Counts in WA
# Plots data from: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
# AU: Will Doyle, wdoyle42@gmail.com
# REV: 2020-04-11

library(tidyverse)
library(shiny)
library(scales)
library(ggrepel)

## Default state
select_state<-"NY"

usa_data<-read_rds("usa_data.Rds")
last_update<-read_lines("last_update.txt")

## Key function: Case PLot
## Takes data, state, county, variable and transformation
## Creates a line plot showing results. 

case_plot<-function(data_frame,select_state,select_counties,var_name,transformation){

# type of transformation    
if(transformation=="Log"){
    transformation="log"
} else{
    transformation="identity"
}   
    
data_frame$var<-unlist(data_frame[var_name][[1]])    

data_frame<-data_frame%>%
    filter(State==select_state)%>%
    filter(County%in%select_counties)
    
gg<-ggplot(data_frame,
           aes(x=Date,
               y=var,
               color=County
           ))
gg<-gg+geom_line()
gg<-gg+geom_point()
gg<-gg+scale_y_continuous(breaks=pretty_breaks(n=10),trans=transformation)
gg<-gg+geom_label_repel(data=filter(data_frame,
                              County%in%select_counties,
                              Date==max(data_frame$Date)),
                              aes(label=paste(County,
                                              round(var))),
                              parse=FALSE,
                  size=5.5
#                  nudge_y = 15,
#                  nudge_x=-2
)
gg<-gg+theme(legend.position = "none",text = element_text(size=18))+ylab(var_name)
gg
}

## Only key variables listed
usa_data_sub<-usa_data%>%
    select(-State,-County,-Date)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Confirmed Positive Covid-19 Cases by State and County"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel( 
            
            htmlOutput("state_selector"),
            
            htmlOutput("county_selector"),
            
              varSelectizeInput("variable", 
                             "Choose Measure:", 
                             usa_data_sub,
                             selected = "`Case Count`",
                             multiple=FALSE
                            ),
            selectInput("transformation",
                        "Choose Transformtion",
                        choices = c("None","Log"),
                        selected="None"
                        ),
          
         HTML(paste('This graphic shows the number of positive tests recorded in each 
                  county in each state from data 
                collected and reported by USA facts:   <a href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/" 
                target="_blank" > Coronavirus in the United States </a> 
    
    <br><br>
                  You can choose counties in the search bar above by typing in the county name.
                  Counties can be dropped by selecting and using the delete key. 
    <br><br>          
    
    Written by Will Doyle, wdoyle42@gmail.com
    <br><br>          
    
              Last update :
              ' ,last_update, " GMT"))
        ),

        # Show a plot 
        mainPanel(
           plotOutput("casePlot")
        )
    )
)

# Define server 
server <- function(input, output,session) {
    
    
updateSelectizeInput(session,"county_choice","state_choice")     
    
    
    output$state_selector <-    
        renderUI({
        selectizeInput(
            "state_choice",
            "State Choice",
            choices = unique(usa_data$State),
            multiple = FALSE,
            selected = select_state)
        }) 
        
    
    state_counties <-reactive({
        
        req(input$state_choice)
        
        usa_data%>%
            filter(State==input$state_choice)%>%
            mutate(County=fct_reorder(.f=as_factor(as.character(County)),.x=`Case Count`,.fun = "max",.desc = TRUE))%>%
            arrange(-`Case Count`)%>%
            select(County)%>%
            group_by(County)%>%    
            summarize(count=n()) %>%
            select(County)%>%    
            as.vector()    
    })
      
    output$county_selector<-renderUI({
        ## Get list of counties    
        
        state_counties=state_counties()
        
        selectizeInput("county_choice",
                       "County Choice",
                       choices=state_counties,
                       multiple=TRUE,
                       selected=state_counties$County[1:5])
        })
    
    output$casePlot <- renderPlot({
        
        req(input$state_choice,input$county_choice)
        
     case_plot(usa_data,
               input$state_choice,
               input$county_choice,
               deparse(input$variable),
              input$transformation)
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
