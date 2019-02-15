########## packages ##########

install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("ggplot2")


library(readr)
library(dplyr)
library(tidyr)
library(stringr)
#library(ggplot2)




########## import data ##########

## import data from local file: 
names_last10y <- "/Users/fredrikaaserud/Documents/Navn/navn_ant_2018-2009.csv"
names <- read_csv(names_last10y) 

## removing unnecessary columns
remove <- c(12)
names <- names[,-remove]




########## mutate ##########

## change capitalization
names <- mutate_all(names, funs(tolower))

## create long dataset
names_long <- gather(names, year, number, -NAVN)

## change header names
colnames(names_long) = c("name", "year", "number") 

## change format
names_long$number <- format(names_long$number, digits = 0, big.mark = ",", justify = "right")
names_long$number <- as.numeric(names_long$number)




########## munge ##########

names_sum <- names_long %>%
  #filter(name == "fredrik") %>%
  select(name, number) %>%
  group_by(name) %>%
  summarise(sum = sum(number),
            max = max(number),
            min = min(number),
            med = median(number),
            mea = mean(number)) %>%
  mutate(length = str_length(name)) %>%
  arrange(desc(med))
print(names_sum)


