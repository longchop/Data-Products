library(shiny);library(dplyr); library(pipeR); library(readr); library(tidyr); library(ggplot2); library(lubridate); library(shinythemes)
# Load and change the format of the source data
read_csv('https://raw.githubusercontent.com/longchop/Data-Products/master/Data/FastFood.csv') %>>%
    mutate(Amount = ifelse(Amount == '?', NA, Amount) %>% as.numeric, Company = Company %>% as.factor, 
           Item = Item %>% as.factor, `Nutritional Category` = `Nutritional Category`  %>% as.factor,
           `Item Type` = `Item Type` %>% as.factor) %>>%
    spread(`Nutritional Category`, Amount) %>>% {. -> tblFastFood} %>>% {names(.) %>% make.names -> names(tblFastFood)} 