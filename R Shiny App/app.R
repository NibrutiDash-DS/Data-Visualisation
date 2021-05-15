#Name- Nibruti Dash
#Student Id - 30834406
#Tutor name - Jeffery Liu
#Date Created- 10th June 2020
#Task - Implement interactive data visualisations using R (Shiny) for identifying the causes of employee attrition.
#Last Modified- 21st June 2020

# Required packages
#install.packages("plotly")
#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("shinydashboard")
#install.packages("scales")
#install.packages("viridis")
#install.packages("tree")
#install.packages("treemapify")
#install.packages("treemap")
#install.packages("ggcorrplot")
#install.packages("leaflet")
#install.packages("shinythemes")
#install.packages("shinyalert")

# Calling required libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinydashboard)
library(scales)
library(viridis)
library(tree)
library(treemapify)
library(treemap)
library(ggcorrplot)
library(leaflet)
library(shinythemes)
library(shinyalert)

# Reading files 
emp <-read.csv("EmployeeInfo.csv")
city <-read.csv("CityInfo.csv")

# Filtering for terminated employees
terminated_emp <- emp %>% dplyr::filter(status=="TERMINATED",termination_year!="NA")

# Defining the UI for Employee Attrition Analysis app
ui <- navbarPage("Employee Attrition Analysis",theme= shinytheme("superhero"),
                 # Page 1 design
                 tabPanel("Cause Analysis",useShinyalert(),
                          sidebarLayout(
                              sidebarPanel(h6(strong("User Guide:")),
                                           p("Drag the slide bar to increase or decrease the year range."),
                                           p("The graphs shown in this page can be controlled using the year range slider."),
                                           p("It is recommended to choose atleast two years range for comparison."),
                                  sliderInput(inputId = "year", "Choose the Year Range:",
                                              min = 2006, max = 2015,
                                              value = c(2009,2012))
                              ),
                              mainPanel(
                                  tags$h3("Identifying the major causes of Employee Attrition"),
                                  textOutput("yearText"),
                                  tags$p(""),
                                  tags$p("Violin plot allows to visualize the distribution of a monthly salary over the years grouped by gender for the terminated employees. 
                                        Here, data are organized by termination year and gender to build a grouped violin chart.
                                        The shape represents the density estimate of the variable: the more data points in a specific range, the larger the violin is for that range.
                                        From the graph it can be said that the low paid employees are prone to leave.
                                        The Box plot gives the summary of the monthly salary for each termination year."),
                                  tags$p("Hover over the violin for more information"),
                                  plotlyOutput(outputId = 'violinPlot'),
                                  tags$p(""),
                                  tags$p("The bubble chart shows the relationship between department and average salary of the employees. 
                                         Different color of the bubble signify different level of job satisfaction(Low, Medium, High, Very High) and the size of the bubble
                                         represents if the employee was working overtime(Yes or No)."),
                                  tags$p("Bigger size of the bubble is for employees who worked overtime."),
                                  tags$p("Hover over the bubble for more information."),
                                  plotlyOutput(outputId = 'bubblePlot'),
                                  tags$p(""),
                                  tags$p("Further analyzing attrition department wise.
                                        Every department has multiple job titles forming a hierarchical data.
                                        The Tree map here displays hierarchical data as a set of nested rectangles. 
                                        Each department is represented by a rectangle, which area is proportional to its value.
                                        This rectangle is further divided on the basis of job title."),
                                  plotOutput(outputId = 'treePlot'),
                                  tags$p(""),
                                  tags$p("The Radial Bar chart shows the number of employees leaving for different education background having different performance rating.
                                         From the graph it can be said that excellent performing employees from Medical and Life Science department are leaving."),
                                  plotOutput(outputId = 'radialBarPlot')
                              )
                          )
                 ),
                 # Page 2 design
                 tabPanel("Spatial Analysis",
                          sidebarLayout(
                              sidebarPanel(h6(strong("User Guide:")),
                                        p("Select the cities from 'Select Cities:' drop down to see the comparison of termination reasons in those cities."),
                                        p("By default, bar graph is generated for Vancouver and Victoria cities. 
                                          Selecting and unselecting the cities, will increase and decrease the number of bar graphs in the facet."),
                                        p("Cities can be selected by clicking on the city name seen in 'Select Cities:' drop down."),
                                        p("Cities can be unselected by the use of backspace key in the selected cities tab."),
                                        p("Leaflet shows all the cities independent of the cities selected by the user."),
                                        p("It is recommended to choose atleast two cities and less than 10 cities for better comparison."),
                              selectInput(inputId = 'cities', label = 'Select Cities:', c(
                                  'ABBOTSFORD' = 'ABBOTSFORD', 
                                  'ALDERGROVE' = 'ALDERGROVE', 
                                  'BELLA BELLA' = 'BELLA BELLA', 
                                  'BURNABY' = 'BURNABY', 
                                  'CHILLIWACK' = 'CHILLIWACK', 
                                  'CORTES ISLAND' = 'CORTES ISLAND', 
                                  'CRANBROOK' = 'CRANBROOK', 
                                  'DAWSON CREEK' = 'DAWSON CREEK', 
                                  'FORT NELSON' = 'FORT NELSON', 
                                  'FORT ST JOHN' = 'FORT ST JOHN', 
                                  'GRAND FORKS' = 'GRAND FORKS', 
                                  'HANEY' = 'HANEY', 
                                  'KAMLOOPS' = 'KAMLOOPS', 
                                  'KELOWNA' = 'KELOWNA', 
                                  'LANGLEY' = 'LANGLEY', 
                                  'NANAIMO' = 'NANAIMO', 
                                  'NELSON' = 'NELSON', 
                                  'NEW WESTMINSTER' = 'NEW WESTMINSTER', 
                                  'NORTH VANCOUVER' = 'NORTH VANCOUVER', 
                                  'OCEAN FALLS' = 'OCEAN FALLS', 
                                  'PITT MEADOWS' = 'PITT MEADOWS', 
                                  'PORT COQUITLAM' = 'PORT COQUITLAM', 
                                  'PRINCE GEORGE' = 'PRINCE GEORGE', 
                                  'PRINCETON' = 'PRINCETON', 
                                  'QUESNEL' = 'QUESNEL', 
                                  'RICHMOND' = 'RICHMOND', 
                                  'SQUAMISH' = 'SQUAMISH', 
                                  'SURREY' = 'SURREY', 
                                  'TERRACE' = 'TERRACE', 
                                  'TRAIL' = 'TRAIL', 
                                  'VANCOUVER' = 'VANCOUVER', 
                                  'VERNON' = 'VERNON', 
                                  'VICTORIA' = 'VICTORIA', 
                                  'WEST VANCOUVER' = 'WEST VANCOUVER', 
                                  'WHITE ROCK' = 'WHITE ROCK', 
                                  'WILLIAMS LAKE' = 'WILLIAMS LAKE'
                              ),multiple = TRUE, selected = c('VANCOUVER'='VANCOUVER','VICTORIA' = 'VICTORIA')),
                              ), 
                              mainPanel(
                                  tags$h3("Spatial Analysis of Employee Attrition"),
                                  tags$p("The facet bar graph shows the comparison of termination reasons for the selected cities.
                                         From the graph, Langley has highest layoffs and Vancouver has highest resignations.
                                         Here, comparison among the cities can also be done which will be useful for the companies having branches in different locations.
                                         Hover over the bars to see the count of terminated employees for the category."),
                                  plotlyOutput(outputId = 'cityPlot'),
                                  tags$p(""),
                                  tags$p("The leaflet shows attrition across the cities. 
                                         The circle size is proportional to count of attrition in that city,
                                         bigger the circle, higher is the attrition."),
                                  tags$p("Hover over the dots to see more information like city name and count of terminated employees.")
                                )
                              ),
                          leafletOutput("leafletmap")
                          ),
                 # Page 3 design
                 tabPanel("Correlations",
                          sidebarLayout(
                              sidebarPanel(h6(strong("User Guide:")),
                                           p("X axis and Y axis are drop down input parameters.
                                             For analyzing correlation between any two attributes, 
                                             select each attribute from X and Y axis, 
                                             scatter plot will be generated for the selected attributes."),
                                           p("By default, the scatter plot generated will be plotted considering
                                             the data of terminated employees only."),
                                           p("Uncheck the 'Show for Terminated Employees only' box to see the 
                                             scatter plot for both active and terminated employees."),
                                  selectInput(
                                      inputId = "x",
                                      label = "X-axis:",
                                      choices = c(
                                          "Age" = "age",
                                          "Business Travel" = "business_travel",
                                          "Department" = "department_name",
                                          "Distance From Home" = "distance_from_home",
                                          "Education Field" = "education_field",
                                          "Environment Satisfaction" = "environment_satisfaction_range",
                                          "Gender" = "gender_full",
                                          "Job Involvement" = "job_involvement_range",
                                          "Job Satisfaction" = "job_satisfaction_range",
                                          "Job Title" = "job_title",
                                          "Length of Service" = "length_of_service",
                                          "Marital Status" = "marital_status",
                                          "Monthly Income" = "monthly_income",
                                          "Number of Companies Worked" = "num_companies_worked",
                                          "Overtime" = "over_time",
                                          "Performance Rating" = "performance_rating_range",
                                          "Salary Hike Percent" = "percent_salary_hike",
                                          "Work Life Balance" = "work_life_balance_range",
                                          "Years Since Last Promotion" = "years_since_last_promotion"
                                      ),
                                      selected = "monthly_income"
                                  ),
                                  selectInput(
                                      inputId = "y",
                                      label = "Y-axis:",
                                      choices = c(
                                          "Age" = "age",
                                          "Business Travel" = "business_travel",
                                          "Department" = "department_name",
                                          "Distance From Home" = "distance_from_home",
                                          "Education Field" = "education_field",
                                          "Environment Satisfaction" = "environment_satisfaction_range",
                                          "Gender" = "gender_full",
                                          "Job Involvement" = "job_involvement_range",
                                          "Job Satisfaction" = "job_satisfaction_range",
                                          "Job Title" = "job_title",
                                          "Length of Service" = "length_of_service",
                                          "Marital Status" = "marital_status",
                                          "Monthly Income" = "monthly_income",
                                          "Number of Companies Worked" = "num_companies_worked",
                                          "Overtime" = "over_time",
                                          "Performance Rating" = "performance_rating_range",
                                          "Salary Hike Percent" = "percent_salary_hike",
                                          "Work Life Balance" = "work_life_balance_range",
                                          "Years Since Last Promotion" = "years_since_last_promotion"
                                      ),
                                      selected = "department_name"
                                  ),
                                  checkboxInput("terminated", "Show for Terminated Employees only", TRUE)
                              ),
                              mainPanel(
                                  tags$h3("Correlation Analysis between the Attributes"),
                                  tags$p("The correlogram shows the relationship between each pair of numeric variables in a dataset. 
                                         The value of corr(Correlation Coefficient) is always between +1 and –1.
                                         Corr value 0 says no linear relationship between the attributes, values closer says +1 is a positive linear relationship and values closer to -1 says a negative linear relationship."),
                                  plotOutput(outputId = 'correlogram'),
                                  tags$p(""),
                                  tags$p("The scatter plot displays the relationship between the selected X axis and Y axis attributes.
                                         Considering the default X axis as Monthly Income and Y axis as Department, the graph says attrition
                                         is high in departments Produce, Processed Foods, Meats, Dairy, Customer Services and Bakery.
                                         It also says that attrition is high among low paid employees. This graph suggests that salary is a factor leading
                                         to employee attrition."),
                                  textOutput("axisText"),
                                  plotOutput(outputId = 'scatterPlot')
                                  
                              )
                          )
                 )
)

# Server Logic for Employee Attrition Analysis app
server <- function(input, output) {
    
    shinyalert("Employee Attrition", "This is the process by which employees leave the company in ways like retirement, resignation or termination and this is an issue that impacts all organizations, independent of location, type and size of the organization. Identifying the factors that result in employee attrition and any correlations between the factors would help the company in employee retention and the company’s growth.
               Here is the analysis done for Employee Retention.",confirmButtonText = "Lets Begin!")
    
    output$yearText <- renderText(paste0('The selected the year range between ',input$year[1],' and ',input$year[2]))
    
    output$axisText <- renderText(paste0('The selected X axis and Y axis are ',input$x,' and ',input$y))
    
    # Leaflet for city wise attrition
    output$leafletmap <- renderLeaflet({

        cityData <- left_join(
            emp %>%
                dplyr::filter(status=="TERMINATED") %>% 
                dplyr::group_by(city_name) %>% 
                dplyr::summarise(sum=n()) %>%
                dplyr::select(city_name,sum), 
            city %>% dplyr::select(city_name,Latitude, Longitude), 
            by='city_name')
        
        leaflet(data=cityData) %>%
            addTiles() %>%
            addCircleMarkers(cityData$Longitude, cityData$Latitude,
                             label = (paste(paste('City:', cityData$city_name, sep = ' '), paste('Count of Terminated Employees:', cityData$sum, sep = ' '), sep= ", ")) ,
                             labelOptions = labelOptions(noHide = F, textsize = "10px", style = list(
                                 "color" = "green",
                                 "font-family" = "Arial",
                                 "font-style" = "italic")),
                             radius = (cityData$sum/sum(cityData$sum))*100 )
        
    })
    
    # Violin plot for gender and average salary wise attrition
    output$violinPlot <- renderPlotly({
        filtered_emp <- terminated_emp[terminated_emp$termination_year >= input$year[1] & terminated_emp$termination_year <= input$year[2],]
        filtered_emp %>% 
            ggplot(aes( fill = as.factor(filtered_emp$gender_full), y = as.numeric(round(filtered_emp$monthly_income,1)), x = as.factor(filtered_emp$termination_year))) +
            geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent")+
            geom_boxplot(width=0.1, color="grey", alpha=0.2)+
            scale_fill_viridis(discrete=T, name="") +
            xlab("Termination Year") +
            ylab("Average Salary")
    })
    
    # Bubble plot for departments wise attrition
    output$bubblePlot <- renderPlotly({
        filtered_emp <- terminated_emp[terminated_emp$termination_year >= input$year[1] & terminated_emp$termination_year <= input$year[2],]
         graph <- filtered_emp %>%
            mutate(text = paste("Department Name: ", department_name, "\nMonthly Income: ", monthly_income, "\nJob Satisfaction: ", job_satisfaction_range, "\nOvertime: ", over_time, sep="")) %>%
            ggplot( aes(department_name,monthly_income, color = job_satisfaction_range, size = over_time, text = text)) +
            geom_point() +
            theme_bw() +
            theme(axis.text.x = element_text(angle=60,hjust = 1)) +
            xlab("Department") +
            ylab("Average Salary")
        ggplotly(graph, tooltip = "text")
    })
    
    # Tree Plot for showing job titles wise attrition in each department
    output$treePlot <- renderPlot({
        filtered_emp <- terminated_emp[terminated_emp$termination_year >= input$year[1] & terminated_emp$termination_year <= input$year[2],]
        job <- filtered_emp %>% 
            select(department_name, job_title) %>% 
            group_by(department_name, job_title) %>% 
            summarize(amount = n())
        treemap(job ,
            index=c("department_name","job_title"),
            vSize="amount",
            type="index",
            palette = "Set2",
            bg.labels=c("white"),
            align.labels=list(c("center", "center"), c("right", "bottom"))  
            )
        
    })
    
    # Radial Bar Plot for Education Fields wise attrition
    output$radialBarPlot <- renderPlot({
        filtered_emp <- terminated_emp[terminated_emp$termination_year >= input$year[1] & terminated_emp$termination_year <= input$year[2],]
        filtered_emp %>% 
            group_by(education_field,performance_rating_range) %>% 
            summarise(Count=n()) %>%
            ggplot(aes(x = education_field, y = Count, fill = performance_rating_range), show.legend = TRUE, width = 0.9) + 
            geom_bar(stat="identity", position="dodge") + 
            theme_minimal() + 
            theme(plot.title = element_text(hjust=0.5), aspect.ratio = 1) + 
            labs(title = "Number of Terminated Employees by Education Field",
            x = "Education Field",
            y = "Count of Terminated Employees",
            fill = "Performance Rating") +
            coord_polar()
    })
    
    # Facet plot for Cities wise attrition
    output$cityPlot <- renderPlotly({
        filtered_emp <- emp[emp$city_name %in% input$cities ,]
        filtered_emp %>% 
            dplyr::filter(status=="TERMINATED") %>% 
            dplyr::group_by(city_name,termreason_desc) %>% 
            ggplot(aes(x=termreason_desc, fill = termreason_desc)) + 
            geom_histogram(stat = "count") +
            theme(
                axis.text.x = element_text(size=5,angle=90),
                strip.text.x = element_text(size = 8)) + 
            facet_wrap(~city_name, ncol=4, scales = "free" ) + 
            labs(y='Count of attrition',
                 x='Termination Reason',
                 subtitle='City wise Attrition',
                 fill = 'Termination Reason') +
            guides(color=FALSE)
    })
    
    # Correlogram to get the correlation coefficient and check for relationships
    output$correlogram <- renderPlot({
        options(repr.plot.width=10, repr.plot.height=7) 
        nums <- select_if(emp, is.numeric)
        corr <- round(cor(nums), 1)
        ggcorrplot(corr, 
                   type = "lower", 
                   lab = TRUE, 
                   lab_size = 3, 
                   method="square", 
                   colors = c("orange", "white", "green"), 
                   title="Correlogram Employee Attritions", 
                   ggtheme=theme_minimal())
    })
    
    # Scatter plot for identifying correlations
    output$scatterPlot <- renderPlot({
        if(input$terminated == FALSE){
            ggplot(data = emp, aes_string(x = input$x, y = input$y,color = emp$status))  + 
                geom_point(size = 3) +
                theme(axis.text.x = element_text(angle=60,hjust = 1)) 
        }
        else {
            ggplot(data = terminated_emp, aes_string(x = input$x, y = input$y, color = terminated_emp$status))  + 
                geom_point(size=3) +
                theme(axis.text.x = element_text(angle=60,hjust = 1)) 
        }
    })
    
}

# Running the application 
shinyApp(ui = ui, server = server)
