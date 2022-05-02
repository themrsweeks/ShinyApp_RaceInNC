############### IMPORTANT NOTE ###############

### I MANUALLY CHANGED THE COLUMN NAME FOR THE
#   THE STUDENT RACE (IPEDS RACE TO IPEDS_RACE)



############### WHAT'S LEFT TO DO ############
#   3) Comment everything




# libraries
library(treemapify)
library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(DT)
library("scales")

# Read csv files
list_RandE <- read_csv("data/Race and Ethnicity.csv")
list_SRandE <- read_csv("data/Student Race and Ethnicity.csv") %>%
    filter(Year != 2012)
incomebyloc <- read_csv("data/Income_by_Location.csv")
povbyraceethn <- read_csv("data/Poverty_by_Race_and_Ethnicity.csv")

# Define UI for application
ui <- fluidPage(
    
    theme = shinytheme("cerulean"),
    
    # Application title
    h2("Exploring Race as an Attribute of Education and Income in North Carolina (2013-2019)"),
    h4("Are there disparities in education and income by race?"),
    hr(),
    
    fluidRow(
    
        tabsetPanel(
            #race and ethnicity---------------------------------------------------
            tabPanel("Race and Ethnicity Overview",
                     sidebarLayout(
                         sidebarPanel(
                             
                             h4("North Carolina Race and Ethnicity Breakdown"),
                             p("\n The following visualization and table show an overview of the 7 races 
                               represented in North Carolina, both as a total count of and as a 
                               percentage of the total population."),
                             p("\n Across every year and ethnicity, the predominant race by population is White, 
                               though this proportion fluctuates over time."),
                             p("\n That being said, are there disparities in education and income by race? Review the following visualizations, under each tab, to determine if 
                               race has a role in income and education levels."),
                             hr(),
                             # drop-down for Year
                             selectInput(inputId = "RandE_year_input", label = "Year (Select one.)",
                                         choices = sort(unique(list_RandE$Year)),
                                         selected = 2013),
                             selectInput(inputId = "RandE_ethnicity_input", label = "Ethnicity (Select one.)",
                                         choices = sort(unique(list_RandE$Ethnicity))
                                         ),
                             radioButtons("RandE_ytype", "Which would you like to visualize?",
                                          choices = c("Counts", "Percentages"))
                             ),
                         
                         mainPanel(
                             fluidRow(plotOutput("RandE_tab", height = "400px", width = "800px")),

                             dataTableOutput(outputId = "RandE_table")
                             )
                         )
                     ),
            #college student race and ethnicity ---------------------------------
            tabPanel("College Student Race and Ethnicity",
                     sidebarLayout(
                         sidebarPanel(
                             h4("Student Race/Ethnicity Breakdown in North Carolina"),
                             p("\n This visualization displays the race and ethnicity breakdown,
                               by year, for the college students in North Carolina."),
                
                             hr(),
                             # drop-down for Year
                             selectInput(inputId = "SRandE_year_input", label = "Year (Select one.)",
                                         choices = sort(unique(list_SRandE$Year)),
                                         selected = 2013),
                             radioButtons("SRandE_ytype", "Which would you like to visualize?",
                                          choices = c("Counts", "Percentages"))
                         ),
                         
                         mainPanel(
                             fluidRow(plotOutput(outputId = "SRandE_tab", height = "400px", width = "800px"))
                         )
                     )
            ),
            #graduate percentages ------------------------------------------------
            tabPanel("Graduate Race Percentages",
                     sidebarLayout(
                         sidebarPanel(
                             h4("Graduate Percentages of NC Students by Race"),
                             p("\n This visualization displays the percentage of graduates in North Carolina
                               by race. The percentages were calculated by taking the total number of degrees
                               awarded to students for each race and dividing it by the total population of 
                               each race."),
                             hr(),
                             # drop-down for Year
                             selectInput(inputId = "GRE_year_input", label = "Year (Select one.)",
                                         choices = sort(unique(list_RandE$Year)),
                                         selected = 2013)
                         ),
                         
                         mainPanel(
                             fluidRow(plotOutput(outputId = "GRE_tab", height = "400px", width = "800px"))
                             )
                         )
                     ),
            #Income by race and location -----------------------------------------
            tabPanel("Income by Race and County",
                     sidebarLayout(
                       sidebarPanel(
                         h4("Income by Race and County"),
                         p("\n This visualization shows household income by race and county over
                           time."),
                         hr(),
                         
                         
                         em(p("\n NOTE: If a selection does not display, there is no data for the selected race in the selected county. If a line graph does not display for all years, there is no data for the selected race during that year.")),
                         hr(),
                         # drop-down for Year
                         selectInput(inputId = "geog_RL", label = "County (Select one.)",
                                     choices = sort(unique(incomebyloc$Geography)),
                                     selected = 2013),
                         hr(),
                         selectizeInput(inputId = "race_RL", label = "Race (Select one or multiple.)",
                                     choices = unique(incomebyloc$Race),
                                     selected = "Total", multiple = TRUE),
                       ),
                       
                       mainPanel(
                         fluidRow(plotOutput(outputId = "inc_by_race_loc_tab", height = "400px", width = "800px"))
                       )
                     )
            ),
            #Poverty by Race------------------------------------------------------
            tabPanel("Poverty by Race",
                     sidebarLayout(
                       sidebarPanel(
                         h4("Poverty Population by Race"),
                         p("\n This visualization displays the count and percentages of persons in poverty by race."),
                         hr(),
                         em(p("\n NOTE: These percentages were calculated using the total number of persons in poverty for a given year.")),
                         hr(),
                         # drop-down for Year
                         selectInput(inputId = "pov_by_race_yr", label = "Year (Select one.)",
                                     choices = sort(unique(povbyraceethn$ID_Year)),
                                     selected = 2013),
                         hr(),
                         #radial button for counts or percentages
                         radioButtons("pov_types", "Which would you like to visualize?",
                                      choices = c("Counts", "Percentages"))
                       ),
                       
                       mainPanel(
                         fluidRow(plotOutput(outputId = "pov_tab", height = "400px", width = "800px"))
                       )
                     )
            ),
            
            #About---------------------------------------------------------------
            tabPanel("About",
                     br(),
                     column(1),
                     column(8, 
                            h5('This app was developed by Marian Weeks and Srikar Vavilala.'),
                            p("It was the result of Chase Romano's Visual Analytics course at the University of 
                 North Carolina Charlotte through the Data Science and Business Analytics MS program."),
                            br(),
                            p("Data used in the development of this app can be obtained by navigating to the following link."),
                          
                            HTML('<a href="https://datausa.io/profile/geo/north-carolina" style="color: #e36209">Data USA: North Carolina</a>'),
                            br(),
                            br(),
                            br(),
                            HTML('<a href="https://github.com/SrikarVavilala/Race-in-NC-Shiny" style="color: #e36209">View This App\'s Code on GitHub</a>'),
                            br(),
                            br(),
                            HTML('<a href="https://www.linkedin.com/in/marian-weeks-3b57632b/" style="color: #e36209">Marian\'s LinkedIn</a>'),
                            br(),
                            HTML('<a href="https://github.com/themrsweeks" style="color: #e36209">Marian\'s GitHub</a>'),
                            br(),
                            br(),
                            HTML('<a href="https://www.linkedin.com/in/srikarvavilala" style="color: #e36209">Srikar\'s LinkedIn</a>'),
                            br(),
                            HTML('<a href="https://www.github.com/SrikarVavilala" style="color: #e36209">Srikar\'s GitHub</a>')
                     ),
                     column(3)
                     )
            )
        )
    )


# Define server logic
server <- function(input, output) {
    
    # ---------------------------------- Race and Ethnicity Tab --------------------------
    
    # filtered data based on user input
    list_filtered_RandE <- reactive({
        list_RandE[complete.cases(list_RandE), ] %>%
            filter(Year == input$RandE_year_input & Ethnicity == input$RandE_ethnicity_input) %>% 
            mutate(Percents = round((share*100),2)) %>%
            mutate(perc_ET_disp = round(share,4))
    })

    # output bar chart
    output$RandE_tab <- renderPlot({
        
        if(input$RandE_ytype == "Counts"){
            ggplot(list_filtered_RandE(), aes(area = Population, fill = Population, 
                                              label = paste(Race, scales::comma(Population), sep = "\n"))) +
                geom_treemap() +
                geom_treemap_text(colour = "white",
                                  place = "centre",
                                  size = 15)+
                theme(legend.position="none", 
                      plot.title = element_text(size = 18)) +
                labs(title = "North Carolina Race and Ethnicity")
        } else {
            ggplot(list_filtered_RandE(), aes(area = Population, fill = Percents, 
                                              label = paste(Race, scales::percent(perc_ET_disp), sep = "\n"))) +
                geom_treemap() +
                geom_treemap_text(colour = "white",
                                  place = "centre",
                                  size = 15)+
                theme(legend.position="none",
                      plot.title = element_text(size = 18)) +
                labs(title = "North Carolina Race and Ethnicity")
            }
        
    })
    
    output$RandE_table <- DT::renderDataTable({
        if(input$RandE_ytype == "Counts"){
            datatable(list_filtered_RandE()[ , c("Population", "Race")], 
                      options = list(order = list(0,'desc')), rownames = FALSE)
        } else {
            datatable(list_filtered_RandE()[ , c("Percents", "Race")], 
                      options = list(order = list(0,'desc')), rownames = FALSE)
        }
    })

    
    
    # ---------------------------------- Student Race and Ethnicity Tab --------------------------

    # filtered data based on user input
    list_filtered_SRandE <- reactive({
        list_SRandE[complete.cases(list_SRandE), ] %>%
            filter(Year == input$SRandE_year_input) %>% 
            mutate(IPEDS_Race = plyr::mapvalues(IPEDS_Race, 
                                           from=c("Native Hawaiian or Other Pacific Islanders"), 
                                           to=c("Pacific Islander"))) %>%
            mutate(Percents = round((share*100),2)) %>%
            mutate(perc_disp_STE = round(share,4))
    })

    output$SRandE_tab <- renderPlot({
            if(input$SRandE_ytype == "Counts"){
                ggplot(list_filtered_SRandE(), aes(reorder(IPEDS_Race, Completions), Completions)) +
                    geom_bar(stat="identity", fill="steelblue")+
                    geom_text(aes(label=scales::comma(Completions)), vjust=0.005, color="Black", size=4.5)+
                    scale_y_continuous(labels = comma)+
                    theme(plot.margin = margin(1, 1, 0, 4, "cm"),
                          text = element_text(size=15),
                          axis.text.x = element_text(angle=10, hjust=1)) +
                    labs(title = "College Student Race and Ethnicity", x = "Race", y = "Total Degrees Awarded")
            } else {
                ggplot(list_filtered_SRandE(), aes(reorder(IPEDS_Race, share), share)) +
                    geom_bar(stat="identity", fill="steelblue")+
                    geom_text(aes(label= scales::percent(perc_disp_STE)), vjust=0.005, color="Black", size=4.5)+
                    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
                    theme(plot.margin = margin(1, 1, 0, 4, "cm"),
                          text = element_text(size=15),
                          axis.text.x = element_text(angle=10, hjust=1)) +
                    labs(title = "College Student Race and Ethnicity", x = "Race and Ethnicity", y = "Percentage")
            }

     })
    
    
    
    # ---------------------------------- Graduate Race and Ethnicity Tab --------------------------
    
    # store the year value chosen by the user
    val_GRE_year <- reactive({ input$GRE_year_input })
    
    # vector of percents
    vec_GRE_percs <- reactive({
        # create a list to store key:value mappings for races in SRandE vs. RandE
        list_race_map <- list("Asian" = "Asian Alone", "Black or African American" = "Black or African American Alone",
                              "Native Hawaiian or Other Pacific Islanders" = "Native Hawaiian & Other Pacific Islander Alone",
                              "Two or More Races" = "Two or More Races",
                              "American Indian or Alaska Native" = "American Indian & Alaska Native Alone",
                              "White" = "White Alone")
        
        vec_percs <- vector("double", ncol(data.frame(list())))
        
        count <- 1
        for (race in list_race_map) {                            
            vec_percs[[race]] <- round((filter(list_SRandE, Year%in%val_GRE_year() & IPEDS_Race%in%names(list_race_map)[count])$Completions/filter(list_RandE, Year%in%val_GRE_year() & Race%in%race & Ethnicity%in%"Not Hispanic or Latino")$Population)*100,2)
            
            count <- count + 1
        }
        
        vec_percs[["Hisp. or Latino"]] <- round((filter(list_SRandE, Year%in%val_GRE_year() & IPEDS_Race%in%"Hispanic or Latino")$Completions/sum(filter(list_RandE, Year%in%val_GRE_year() & Ethnicity%in%"Hispanic or Latino")$Population))*100,2)
        
        vec_percs
    })
    
    # plot a bar chart
    output$GRE_tab <- renderPlot({
        ggplot(data.frame(vec_GRE_percs()),aes(reorder(names(vec_GRE_percs()), (vec_GRE_percs()/100)),
                                               (vec_GRE_percs()/100)), 
               )+
            geom_bar(stat="identity", fill="steelblue")+
            geom_text(aes(label= scales::percent(vec_GRE_percs()/100)), vjust=0.005, color="Black", size=4.5)+
            scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
            theme(plot.margin = margin(1, 1, 0, 4, "cm"),
                  text = element_text(size=15),
                  axis.text.x = element_text(angle=10, hjust=1)) +
            labs(title = "Percentage of Graduates by Specific Race Population", x = "Races", y = "Percentage")
    })

    # ---------------------------------- Income by Race and Location --------------------------
    # filtered data based on user input
    incomebyloc_sub <- reactive({
      incomebyloc %>%
        filter(Geography == input$geog_RL & Race %in% input$race_RL) 
    })
    
    # plot
    output$inc_by_race_loc_tab <- renderPlot({
      ggplot(incomebyloc_sub(), aes(x = Year, y = Household_Income_by_Race, colour = Race)) +
        geom_line(size = 1) + 
        scale_y_continuous(labels = comma)+
        theme(plot.margin = margin(1, 1, 0, 4, "cm"),
              text = element_text(size=15)) +
        scale_color_manual(values = c("black", "darkorange", "blue1", "cornflowerblue", "azure4", "deepskyblue4", "darkgoldenrod1", "cyan", "chocolate4", "aquamarine"))+
        geom_point()+
        labs(title = "Household Income by County and Race", x = "Year", y = "Household Income")
      
    })
    
    # ---------------------------------- Poverty by Race --------------------------------------
    #filtering data by user input
    povbyraceethn_sub <- reactive({
      povbyraceethn %>%
        filter(Year == input$pov_by_race_yr) %>%
        mutate(perc_povs = round(share*100),2) %>%
        mutate(perc_povs_disp = round(share,4))
    })

  
    output$pov_tab <- renderPlot({
      if (input$pov_types == "Counts"){
        ggplot(povbyraceethn_sub(), aes(reorder(Race, Poverty_Population), Poverty_Population)) +
          geom_bar(stat = 'identity', fill = 'steelblue', position = 'dodge')+
          geom_text(aes(label= scales::comma(Poverty_Population)), vjust=0.005, color = 'Black', size = 4.5)+
          scale_y_continuous(labels = comma)+
          theme(plot.margin = margin(1,1,0,4,'cm'),
                text = element_text(size=15),
                axis.text.x = element_text(angle=10, hjust=1))+
          labs(title = "Poverty Population by Race", x = "Race", y = "Count of Persons in Poverty")
      } else {
        ggplot(povbyraceethn_sub(), aes(reorder(Race, share), share)) +
          geom_bar(stat = 'identity', fill = 'steelblue', position = 'dodge')+
          geom_text(aes(label= scales::percent(perc_povs_disp)), vjust=0.005, color = 'Black', size = 4.5)+
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
          theme(plot.margin = margin(1,1,0,4,'cm'),
                text = element_text(size=15),
                axis.text.x = element_text(angle=10, hjust=1))+
          labs(title = "Poverty Population by Race", x = "Race", y = "Percentage by Total Persons in Poverty")
      } 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
