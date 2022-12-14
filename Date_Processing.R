##library 
library(dplyr)

##load data
svi <- rbind(read.csv("2014_SVI.csv"), read.csv("2016_SVI.csv"), read.csv("2018_SVI.csv"))
food <- rbind(read.csv("2014_food.csv"), read.csv("2016_food.csv"), read.csv("2018_food.csv"))
health <- rbind(read.csv("2014_health_ins.csv"), read.csv("2016_health_ins.csv"), read.csv("2018_health_ins.csv"))


##Look at dimension of each data frame
dim(svi)
dim(food)
dim(health)
##Check for missing data
which(svi$Overall.SVI == "No Data")
which(food$Food.Insecurity == "No Data")
which(health$No.Health.Insurance == "No Data")
health$No.Health.Insurance[which(health$No.Health.Insurance == "No Data")] <- NA ##recode missing data to NA

df <- merge(merge(svi, food, by=c("Year", "County_FIPS","County",
                                  "State","Diagnosed.Diabetes.Percentage"), all = T),
            health, by=c("Year", "County_FIPS","County", "State","Diagnosed.Diabetes.Percentage"), all = T)
dim(df)
#write.csv(df,"C:/Users/cszet/Desktop/Fall 2022/BIOSTAT 625/Final Proj/merge_df.csv", row.names = FALSE)


df[order(df$County_FIPS),] %>% View()
aggregate(. ~ County_FIPS, df[,-c(1,3,4)], function(x) sd = sd(x, na.rm = T)) %>% View()
##comment: food insur and health insurance info is not changed since 2014