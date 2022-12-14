require("purrr")
require("dplyr")
require("stringr")
require("data.table")


get_data <- function(patt, df.name){
# Identify all CSV files with selected pattern in the folder
all_path <- list.files(path = "C:/Users/cszet/Desktop/Fall 2022/BIOSTAT 625/Final Proj/raw data/", 
                       pattern = patt, full.names = TRUE) 
# read file
data_join <- all_path %>% lapply(read.csv)  

# get variable year from file name
all_filenames <- all_path %>% basename() %>% str_extract(., "(\\d)+") %>% as.list()

data_join <- mapply(c, data_join, all_filenames, SIMPLIFY = F)

#join all data
all_result <- rbindlist(data_join, fill = T) %>% select(County, State, CountyFIPS, Percentage, V1) %>% as.data.frame()
#rename variable
names(all_result)[5] <- "Year"
names(all_result)[4] <- df.name

return(all_result)
}

## get the data
inactive <- get_data("phys_*", "inactive")
diabetes <- get_data("Diagnosed_*", "diabetes")
obesity <- get_data("Ob_*", "obesity")

## replace "No data" as NA
inactive$inactive <- na_if(inactive$inactive, "No Data")
diabetes$diabetes <- na_if(diabetes$diabetes, "No Data")
obesity$obesity <- na_if(obesity$obesity, "No Data")

##merge the dataframe
df_list <- list(inactive, diabetes, obesity)
df <- Reduce(function(x, y) merge(x, y, by=c("Year", "CountyFIPS","County","State"), all = T), df_list)
names(df)[2] <- "County_FIPS"
df$inactive <- as.numeric(df$inactive)
df$diabetes <- as.numeric(df$diabetes)
df$obesity <- as.numeric(df$obesity)
#write.csv(df,"C:/Users/cszet/Desktop/Fall 2022/BIOSTAT 625/Final Proj/merged_biofactor.csv", row.names = FALSE)
