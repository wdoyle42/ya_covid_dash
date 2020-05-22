# Positive COVID-19 Case Counts in US
# Plots data from: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
# AU: Will Doyle, wdoyle42@gmail.com
# REV: 2020-05-23

library(tidyverse)
library(shiny)
library(scales)
library(ggrepel)
library(plotly)

usa_data_county<-read_rds("usa_data_county.Rds")
usa_data_state<-read_rds("usa_data_state.Rds")

last_update<-read_lines("last_update.txt")


## Default state
select_state<-"NY"

## default state list
state_list<-levels(usa_data_state$State)[1:5]


## Key function: Case PLot
## Takes data, state, county, variable and transformation
## Creates a line plot showing results. 

county_case_plot<-function(data_frame,select_state,select_counties,var_name,transformation){

# type of transformation    
if(transformation=="Log"){
    transformation="log"
} else{
    transformation="identity"
}   

data_frame<-data_frame%>%
  filter(State==select_state,
         County%in%select_counties)  
      
data_frame$var<-unlist(data_frame[var_name][[1]])    

gg<-ggplot(data_frame,
           aes(text=paste0(data_frame$County," ",data_frame$Date,":" , prettyNum(data_frame$var,big.mark=",",digits=0,scientific=FALSE)))
)
             
gg<-gg+geom_line(aes(x=Date,
                          y=var,
                          group=County,
                     color=County))

gg<-gg+geom_point(aes(x=Date,
                       y=var,
                       color=County,
                      group=County))

gg<-gg+scale_y_continuous(breaks=pretty_breaks(n=10),trans=transformation)

gg<-gg+theme(legend.position = "none")+ylab(var_name)

out<-ggplotly(gg,tooltip="text")%>%layout(hovermode='compare')

out
}

## State version
state_case_plot<-function(data_frame,state_list,var_name,transformation){

  data_frame<-data_frame%>%
    filter(State%in%state_list)
  
  # type of transformation    
  if(transformation=="Log"){
    transformation="log"
  } else{
    transformation="identity"
  }   
  
  data_frame$var<-unlist(data_frame[var_name][[1]])    
  
  gg<-ggplot(data_frame,
             aes(text=paste0(data_frame$State," ",data_frame$Date,":" , prettyNum(data_frame$var,big.mark=",",digits=0,scientific=FALSE)))
  )
  
  gg<-gg+geom_line(aes(x=Date,
                       y=var,
                       group=State,
                       color=State))
  
  gg<-gg+geom_point(aes(x=Date,
                        y=var,
                        color=State,group=State))
  
  gg<-gg+scale_y_continuous(breaks=pretty_breaks(n=10),trans=transformation)
  
  gg<-gg+theme(legend.position = "none")+ylab(var_name)
  
  out<-ggplotly(gg,tooltip="text")%>%layout(hovermode='compare')
  
  out
}

## Only key variables listed
usa_data_county_sub<-usa_data_county%>%
    select(-State,-County,-Date)

usa_data_state_sub<-usa_data_state%>%
  select(-State,-Date)


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Confirmed Positive Covid-19 Cases by State and County"),
  
  tabsetPanel(
    ## State Panel
    tabPanel(title="State Level",
             
             # Sidebar
             sidebarLayout(
               sidebarPanel(
                 
                 htmlOutput("multi_state_selector"),
                 
                 varSelectizeInput(
                   inputId = "state_variable",
                   label= "Choose Measure:",
                   data=usa_data_state_sub,
                   selected = "`Case Count`",
                   multiple = FALSE
                 ),
                 
                 selectInput(
                   "transformation",
                   "Choose Transformtion",
                   choices = c("None", "Log"),
                   selected = "None"
                 ),
                 
                 HTML(
                   paste(
                     'This graphic shows the number of positive tests recorded in each
                  each state from data
                collected and reported by USA facts:   <a href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/"
                target="_blank" > Coronavirus in the United States </a>

                <br><br>
                  You can choose states in the search bar above by typing in the county name.
                  States can be dropped by selecting and using the delete key.
                <br><br>

                Written by Will Doyle, wdoyle42@gmail.com
                <br><br>

              Last update :
              ' ,
                     last_update,
                     " GMT"
                   ) #Close Paste
                 ) #Close HTML
               ), # Ends sidebar for states
               
               mainPanel(plotlyOutput("casePlotState"))  #close mainpanel
             ) #close sidebarlayout
    ), # End state tab
    ## Close Tab
    tabPanel(
      title="County Level",
      
      # Sidebar
      sidebarLayout(
        sidebarPanel(
          ## Selectize input for one state
          htmlOutput("single_state_selector"),
          
          ## selectivze input for multiple counties
          htmlOutput("county_selector"),
          
          varSelectizeInput(
            "variable",
            "Choose Measure:",
            usa_data_county_sub,
            selected = "`Case Count`",
            multiple = FALSE
          ),
          selectInput(
            "transformation",
            "Choose Transformtion",
            choices = c("None", "Log"),
            selected = "None"
          ),
          
          HTML(
            paste(
              'This graphic shows the number of positive tests recorded in each
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
              ' ,
              last_update,
              " GMT"
            )
          )
        ),
        # Ends sidebar for counties
        
        mainPanel(plotlyOutput("casePlotCounty")) # End county main panel
      )) # End county tab
   

      )
    
    )
  
    
    # Define server
    server <- function(input, output, session) {
      ## Ask for input
      updateSelectizeInput(session, "county_choice", "state_choice")
      
      ## single state selector for county tab, default is select_state
      output$single_state_selector <-
        renderUI({
          selectizeInput(
            "state_choice",
            "State Choice",
            choices = unique(usa_data_county$State),
            multiple = FALSE,
            selected = select_state
          )
        })
  
  ## state counties list: highest 5 on measure    
      state_counties <- reactive({
        
        req(input$state_choice)
      
        usa_data_county %>%
          filter(State == input$state_choice) %>%
          mutate(County = fct_reorder(
            .f = as_factor(as.character(County)),
            .x = `Case Count`,
            .fun = "max",
            .desc = TRUE
          )) %>%
          arrange(-`Case Count`) %>%
          select(County) %>%
          group_by(County) %>%
          summarize(count = n()) %>%
          select(County) %>%
          as.vector()
      })

  ## county selector widget          
      output$county_selector <- renderUI({
        ## Get list of counties
        
        state_counties = state_counties()
        
        selectizeInput(
          "county_choice",
          "County Choice",
          choices = state_counties,
          multiple = TRUE,
          selected = state_counties$County[1:5]
        )
      })
      
      ## Execute function
      output$casePlotCounty <- renderPlotly({
        
        req(input$state_choice, input$county_choice)
        
        county_case_plot(
          usa_data_county,
          input$state_choice,
          input$county_choice,
          deparse(input$variable),
          input$transformation
        )
        
      })
      
      
  # ## State tab: state selector     
      output$multi_state_selector <-
        renderUI({
          selectizeInput(
            "state_list_choice",
            "State Choice",
            choices = unique(usa_data_state$State),
            multiple = TRUE,
            selected = state_list
          )
        })

    ## Execute function
      output$casePlotState <- renderPlotly({
        req(input$state_list_choice)

        state_case_plot(
          data_frame=usa_data_state,
          input$state_list_choice,
          deparse(input$state_variable),
          input$transformation
        )
      })

    }
    
    # Run the application
    shinyApp(ui = ui, server = server)
    