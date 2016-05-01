#Load required libraries
library(shiny);library(dplyr); library(pipeR); library(readr); library(tidyr); library(ggplot2); library(lubridate); library(shinythemes)


# Shiny Application
shinyServer(function(input, output, session) {
    
    observe({ #Enable the dropdown list to filter (for item type) on the selected Fast food franchise
        company.itemtype <- switch(input$company, 
                                   `Arbys` = tblFastFood %>>% filter(Company == 'Arbys') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Baskin Robbins` = tblFastFood %>>% filter(Company == 'Baskin Robbins') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Blimpies` = tblFastFood %>>% filter(Company == 'Blimpies') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Boston Market` = tblFastFood %>>% filter(Company == 'Boston Market') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Burger King` = tblFastFood %>>% filter(Company == 'Burger King') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Dairy Queen` = tblFastFood %>>% filter(Company == 'Dairy Queen') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Dominos` = tblFastFood %>>% filter(Company == 'Dominos') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Hardees` = tblFastFood %>>% filter(Company == 'Hardees') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `KFC` = tblFastFood %>>% filter(Company == 'KFC') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `McDonalds` = tblFastFood %>>% filter(Company == 'McDonalds') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Papa Johns` = tblFastFood %>>% filter(Company == 'Papa Johns') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Pizza Hut` = tblFastFood %>>% filter(Company == 'Pizza Hut') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Subway` = tblFastFood %>>% filter(Company == 'Subway') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Taco Bell` = tblFastFood %>>% filter(Company == 'Taco Bell') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character,
                                   `Wendys` = tblFastFood %>>% filter(Company == 'Wendys') %>>% (Item.Type) %>>% unique %>>% sort %>>% as.character)
        updateCheckboxGroupInput(session,'Itemtype', choices = company.itemtype)})
    
    observe({ #Enable the dropdown list to filter (for items) on the selected Fast food franchise
        company.itemtype2 <- switch(input$company2, 
                                    `Arbys` = tblFastFood %>>% filter(Company == 'Arbys') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Baskin Robbins` = tblFastFood %>>% filter(Company == 'Baskin Robbins') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Blimpies` = tblFastFood %>>% filter(Company == 'Blimpies') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Boston Market` = tblFastFood %>>% filter(Company == 'Boston Market') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Burger King` = tblFastFood %>>% filter(Company == 'Burger King') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Dairy Queen` = tblFastFood %>>% filter(Company == 'Dairy Queen') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Dominos` = tblFastFood %>>% filter(Company == 'Dominos') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Hardees` = tblFastFood %>>% filter(Company == 'Hardees') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `KFC` = tblFastFood %>>% filter(Company == 'KFC') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `McDonalds` = tblFastFood %>>% filter(Company == 'McDonalds') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Papa Johns` = tblFastFood %>>% filter(Company == 'Papa Johns') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Pizza Hut` = tblFastFood %>>% filter(Company == 'Pizza Hut') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Subway` = tblFastFood %>>% filter(Company == 'Subway') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Taco Bell` = tblFastFood %>>% filter(Company == 'Taco Bell') %>>% (Item) %>>% unique %>>% sort %>>% as.character,
                                    `Wendys` = tblFastFood %>>% filter(Company == 'Wendys') %>>% (Item) %>>% unique %>>% sort %>>% as.character)
        updateSelectInput(session,'Itemtype2', choices = company.itemtype2)
        updateSelectInput(session,'Itemtype3', choices = company.itemtype2)
        updateSelectInput(session,'Itemtype4', choices = company.itemtype2)
        updateSelectInput(session,'Itemtype5', choices = company.itemtype2)
        updateSelectInput(session,'Itemtype6', choices = company.itemtype2)
    })
    
    
    tblFastFoodFiltered <- reactive({ #Reactive code to transform source table for easy ggplot
        tblFastFood %>>% 
            filter(Company == input$company, Item.Type %in% input$Itemtype) %>>% 
            gather(Nutritional.Category, Amount, Calories:Total.Fat) %>>% 
            filter(Nutritional.Category %in% input$Nutrition) %>>%
            mutate(Item = Item %>% as.factor, Nutritional.Category = Nutritional.Category %>% as.factor,
                   Amount = ifelse(Nutritional.Category %in% c('Sodium', 'Cholesterol'), Amount*.001, Amount)) %>>%
            group_by(Item, Nutritional.Category) %>>% summarise(Amount = sum(Amount)) %>>% ungroup %>>%
            left_join((group_by(., Item) %>>% summarise(Rank = sum(Amount))), by = 'Item')
    })
    
    output$plot <- renderPlot({ # Take transformed table and produce a horizontal pareto chart
        tblFastFoodFiltered() %>>%
            filter(!is.na(Amount)) %>>%
            ggplot(aes(reorder(Item, Amount), Amount)) + 
            geom_bar(aes(fill = Nutritional.Category), stat = 'identity', col = 'black') + 
            coord_flip() + ylab('grams') + xlab('Item Type') +
            ggtitle(expression(atop('Nutritional Breakdown - Compare and contrast the nutritional qualities of different foods by company.', 
                                    atop(italic('See the nutritional breakdown of hundreds of fast food items by making selections from the menus on the left. You can single out individual measurements by selecting from the filters below the chart.'), '')))) + 
            theme_minimal() +
            scale_colour_hue("clarity")
    })
    
    tblNutrition <- reactive({ # Reactive code to transform source table for easy trellis charts
        tblFastFood %>>% 
            filter(Company == input$company, Item.Type %in% input$Itemtype) %>>% 
            gather(Nutritional.Category, Amount, Calories:Total.Fat) %>>% 
            filter(Nutritional.Category %in% input$Nutrition) %>>%
            mutate(Item = Item %>% as.factor, Nutritional.Category = Nutritional.Category %>% as.factor,
                   Amount = ifelse(Nutritional.Category %in% c('Sodium', 'Cholesterol'), Amount*.001, Amount)) %>>%
            group_by(Item, Nutritional.Category) %>>% summarise(Amount = sum(Amount)) %>>% ungroup %>>%
            left_join((group_by(., Item) %>>% summarise(Rank = sum(Amount))), by = 'Item')
    })
    
    output$nutritionPlots <- renderPlot({ #Loop through different nutrition types and insert charts into a multiplot
        
        c('Carbs', 'Fiber', 'Protein', 'Sugars', 'Total.Fat', 'Cholesterol') -> vNutrition
        c(300, 25, 50, 40, 65, .3) -> vDailyValue
        
        list(NULL) -> listPlots
        
        for (i in 1:6){
            tblNutrition() %>>% #Loop through vNutrition and create multiple horizontal pareto charts
                filter(Nutritional.Category == vNutrition[i]) %>>%
                arrange(desc(Amount)) %>>%
                top_n(10, Amount) %>>%
                ggplot(aes(reorder(Item, Amount), Amount)) +
                geom_bar(stat = 'identity', colour = 'black', fill = i + 2) +
                geom_text(aes(y = Amount, label = Amount), hjust = 1.5) +
                geom_hline(yintercept = vDailyValue[i], colour = 'red', linetype = 'dashed') +
                ggtitle(paste('Top 10 - ',vNutrition[i], ' (* Daily Value ', vDailyValue[i], 'g)', sep = '')) + 
                labs(x = 'Food', y = paste(vNutrition[i], 'Amount')) +
                coord_flip() +
                theme_minimal() +
                theme(axis.text.x = element_blank()) -> listPlots[[i]]
        }
        Rmisc::multiplot(plotlist = listPlots, cols = 3)
    })
    
    output$table <- renderDataTable(tblFastFood, options = list(pageLength = 10))
    
    tblCaloryIntake <- reactive({ #Create a food intake data frame for display and calculation
        data_frame(
            Item = c(input$Itemtype2, input$Itemtype3, input$Itemtype4, input$Itemtype5, input$Itemtype6),
            Calories = c(tblFastFood %>>% filter(Item == input$Itemtype2) %>>% (Calories),
                         tblFastFood %>>% filter(Item == input$Itemtype3) %>>% (Calories),
                         tblFastFood %>>% filter(Item == input$Itemtype4) %>>% (Calories),
                         tblFastFood %>>% filter(Item == input$Itemtype5) %>>% (Calories),
                         tblFastFood %>>% filter(Item == input$Itemtype6) %>>% (Calories)),
            Qty = c(input$Itemtype2_Num, input$Itemtype3_Num, input$Itemtype4_Num, input$Itemtype5_Num, input$Itemtype6_Num)
        ) %>>% mutate(Total_Calories = Calories * Qty)
    })
    
    
    output$tableCalc <- renderTable({ # Display the data frame showing user data
        tblCaloryIntake()
    })
    
    output$textTotalCalories <- renderText({# Sum the calorie data from the data frame. Shows the total calorie intake based on user selection
        tblCaloryIntake() %>>% (Total_Calories) %>>% sum
    })
    
    output$plotActivity <- renderPlot({ #Create another dataframe on different exercise types and calculate calorie burn rate
        tblCaloryIntake() %>>% (Total_Calories) %>>% sum -> vCalories
        data_frame(
            Activity = factor(1:11, label = c('Stand Still', 'Biking - 6mph', 'Office Work/Housekeeping', 'Walk - 3mph', 'Tennis - Singles', 'Biking - 12 mph',
                                              'Swim - Leisurely Pace', 'Swim 50 yd/min', 'Run - 6 mph', 'Cross-Country Skiing', 'Jumping rope - Moderate Pace')),
            Time = c( # Calorie burn rate differs based on exercise type
                ((vCalories/(input$numInput_Wgh * .532)*60))/1440, #Standing Still
                ((vCalories/(input$numInput_Wgh * 1.6)*60))/1440, #Biking - 6mph
                ((vCalories/(input$numInput_Wgh * 1.6)*60))/1440, #Office Work/Housekeeping
                ((vCalories/(input$numInput_Wgh * 1.98)*60))/1440, #Walk - 3mph
                ((vCalories/(input$numInput_Wgh * 2.66)*60))/1440, #Tennis - Singles
                ((vCalories/(input$numInput_Wgh * 2.73)*60))/1440, #Biking - 12 mph
                ((vCalories/(input$numInput_Wgh * 1.43)*30))/1440, #Swim - Leisurely Pace
                ((vCalories/(input$numInput_Wgh * 1.9)*30))/1440, #Swim 50 yd/min
                ((vCalories/(input$numInput_Wgh * 4.56)*60))/1440, #Run - 6 mph
                ((vCalories/(input$numInput_Wgh * 4.67)*60))/1440, #Cross-Country Skiing
                ((vCalories/(input$numInput_Wgh * 4.76)*60))/1440 #Jumping rope - Moderate Pace
            )
        ) %>>%
            mutate(`D HH:MM:SS` = as.Date(Time, origin = '1899-12-31'),
                   `D HH:MM:SS` = .POSIXct(unclass(`D HH:MM:SS`)*86400, tz = 'GMT'),
                   `D HH:MM:SS` = ifelse(Time < 1,format(`D HH:MM:SS`, '%H:%M:%S'), format(`D HH:MM:SS`, '%d %H:%M:%S'))) %>>%
            arrange(Activity) %>>%
            (~ tblExersion) %>>%
            ggplot(aes(reorder(Activity, Time), Time)) + # Show exercise time required to burn off the total calorie intake
            geom_bar(stat = 'identity', color = 'black', fill = 'red') +
            geom_text(aes(label = `D HH:MM:SS`, y = Time + max(tblExersion$Time) * 0.05), size = 3) +
            ylim(0, max(tblExersion$Time) * 1.4) +
            coord_flip() +
            labs(x = 'Activity', y = 'Time: Day HH:MM:SS', title = 'Time taken to burn off calory intake by activity type') +
            theme_bw() + 
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
        
    })
    
})
