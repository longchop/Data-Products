
library(shiny);library(dplyr); library(pipeR); library(readr); library(tidyr); library(ggplot2); library(lubridate); library(shinythemes)
source('ProcessingData.R')


# Define UI for application
shinyUI(fluidPage( # Create a tab layout. 4 tabs 1 intro, 2 Chart summary, 3 data table, and 4 calorie intake
    
    theme = shinytheme("united"), # Set the overall theme
    tabsetPanel( #This tab provides a overall visual summary of nutrition intake
        tabPanel(
            'Intro',
            mainPanel(h1('Fast Food Nutritional Values Application'),
                      h5('Using the data provided by www.FastFoodNutrition.org this application takes a look at the nutritional values of the menu items from 15 fast food franchises.'),
                      h5('This application will enable you to view your nutritional intake, and help you understand how much exercise you will need to burn off the calories consumed.'))
        ),
        tabPanel('Nutritional Breakdown',
                 # Application title
                 titlePanel("Fast Food Nutrition"),
                 
                 # Sidebar
                 sidebarLayout(
                     sidebarPanel(
                         'Filter your selection by company and by type via the drop down box and check boxes.',
                         selectInput("company", "Select the fast food restaurant: ", tblFastFood %>>% (Company) %>>% unique %>>% sort %>>% as.character, 'Arbys'),
                         checkboxGroupInput("Itemtype", "Select the item type: ", tblFastFood %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character),
                         checkboxGroupInput("Nutrition", "Select the Nutritional Category: ", 
                                            c('Carbs', 'Cholesterol', 'Fiber', 'Protein', 'Sodium', 'Sugars', 'Total.Fat'),
                                            c('Carbs', 'Cholesterol', 'Fiber', 'Protein', 'Sodium', 'Sugars', 'Total.Fat'))
                     ),
                     #The plot created in server.R is displayed
                     mainPanel(h4('See the nutritional breakdown of hundreds of fast food items by making selections from the menus on left.
                                  You can filter your options on the left.'),
                               h5('All of the values are shown in grams and the recommended allowances is based on a 2,000 calorie diet.'),
                               plotOutput('plot', height = 500, hover = hoverOpts(id = 'plot_hover')),
                               'See how each food items nutritional make up compares to the the other food options as well as the daily recommended values based on a 2,000 calorie diet. The red vertical line indicates the daily recommeded values.',
                               plotOutput('nutritionPlots', height = 500),
                               '* All Daily Values based on a 2000 calorie diet'
                     )
                 )
        ),
        
        tabPanel( # This tab provides a detailed table for quick food lookup
            'Nutrition Table',
            titlePanel('Nutrition by Company'),
            h4('This nutrition table will enable you to quickly look up the nutritional values for the different food types in a table format.'),
            h5('You will be able to limit the number of rows on the table by using the dropdown box on the top left of the table.'),
            h5('Please use the opposing arrows on the top of each column to sort the table in ascending/descending order. Use the textboxes on the bottom of each column or the search box on the top right of the table to filter the table.'),
            mainPanel(dataTableOutput('table'))
        ),
        
        tabPanel( #This tab calculates calorie intake and required exercise to burn off the given calorie intake
            'Calorie Counter',
            titlePanel('Burning off the calories'),
            # Sidebar
            sidebarLayout(
                sidebarPanel(
                    'To find out just how many calories your meal has, please enter your weight in lbs, select the items that you plan to eat. Finally tell us how many of each of those item you plan to eat using the numerical input boxes.',
                    numericInput(inputId = 'numInput_Wgh', label = 'Enter your current weight (lbs)', value = 250, min = 0, step = 5),
                    selectInput("company2", "Select the company: ", tblFastFood %>>% (Company) %>>% unique %>>% sort %>>% as.character, 'Arbys'),
                    selectInput(inputId = "Itemtype2", label = 'Select the item type: ', choices =  tblFastFood %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character),
                    numericInput(inputId = 'Itemtype2_Num', label = 'Quantity', value = 0, min = 0),
                    selectInput(inputId = "Itemtype3", label = 'Select the item type: ', choices =  tblFastFood %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character),
                    numericInput(inputId = 'Itemtype3_Num', label = 'Quantity', value = 0, min = 0),
                    selectInput(inputId = "Itemtype4", label = 'Select the item type: ', choices =  tblFastFood %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character),
                    numericInput(inputId = 'Itemtype4_Num', label = 'Quantity', value = 0, min = 0),
                    selectInput(inputId = "Itemtype5", label = 'Select the item type: ', choices =  tblFastFood %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character),
                    numericInput(inputId = 'Itemtype5_Num', label = 'Quantity', value = 0, min = 0),
                    selectInput(inputId = "Itemtype6", label = 'Select the item type: ', choices =  tblFastFood %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character),
                    numericInput(inputId = 'Itemtype6_Num', label = 'Quantity', value = 0, min = 0)
                ),
                #The plot created in server.R is displayed
                mainPanel(h4('See the associated calories for each food item and how much physical exertion is required to burn off these calories. The table below shows a summary of the selected foods with the number of calories against each food type'),
                          tableOutput('tableCalc'),
                          h5('Based on your selections your total calorie intake was:'),
                          verbatimTextOutput('textTotalCalories'),
                          h5('The chart below provides you with a estimate of how long it would take to burn off the total calories from your selection. Different exercises will burn these calories with differing times.'),
                          plotOutput('plotActivity', height = 400))
            )
            
        )
    )

))
