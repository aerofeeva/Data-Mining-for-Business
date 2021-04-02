#' Author: Anastasia Erofeeva
#' Date: 09-26-2020
#' Purpose: OKCupid Case EDA
#' 

# Libs
library(okcupiddata)
library(dplyr)
library(DataExplorer)
library(ggplot2)
library(ggthemes)
library(maps)
library(wordcloud)

# Set WD
setwd("/cloud/project/Cases/I Ok Cupid")


# See all files in wd, leave this blank inside the parentheses
dir()


# Get the okcupid data as `profiles`
data('profiles')
latlon <- read.csv('LatLon.csv')
addr <- read.csv('addr.csv')


########## BASIC EDA ##########
dim(profiles) #59946 records, 22 variables
names(profiles)
summary(profiles)
sapply(profiles, class)
head(profiles)

# DataExplorer
create_report(profiles)
plot_histogram(profiles) # Numeric variables: age, height, income


########## MISSING VALUES ##########
colSums(is.na(profiles))
plot_missing(profiles)


# Drop variables with highest % of NA (income 80.81%, offspring 59.32%, diet 40.69%)
profiles <- subset(profiles, select=-c(income, offspring, diet))
head(profiles)


########## AGE ##########
# Age summary statistics
summary(profiles$age) # Mean age is 32.34, min age is 18, max age is 110

which(profiles$age == 110) # Oldest user is 110 year-old female, index 2513
profiles[2513,]

which(profiles$age == 109) # Second-oldest user is 109 year-old male, index 25325
profiles[25325,]

which(profiles$age == 108) # Nobody is 108 years old

# Age distribution by gender
p1 <- ggplot(profiles, aes(x=sex, y=age, fill=sex)) +
  geom_boxplot() +
  labs(title="Age distribution by gender",x="Gender", y = "Age") +
  scale_x_discrete(labels=c("f" = "Female", "m" = "Male")) +
  scale_fill_manual(values=c("#f76060", "#577bff")) +
  theme_minimal() +
  theme(legend.position = "none")
p1

male <- profiles[which(profiles$sex=='m'),]
summary(male$age)

female <- profiles[which(profiles$sex=='f'),]
summary(female$age)


########## GENDER ##########
# Gender distribution
p2 <- ggplot(profiles, aes(x=sex, fill=sex)) + 
  geom_bar() +
  labs(title="Gender distribution", x="Gender", y = "Count") +
  scale_x_discrete(labels=c("f" = "Female", "m" = "Male")) +
  scale_fill_manual(values=c("#f76060", "#577bff")) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme_minimal() +
  theme(legend.position = "none")
p2 # 35829 male, 24117 female


########## HEIGHT ##########
# Height distribution
# Some heights are unreasonably low, such as 1 inch
summary(profiles$height) # Mean height = 68.3 inches, min= 1 inch, max = 95 inches

p3 <- ggplot(profiles, aes(x=sex, y=height)) +
  geom_boxplot() +
  labs(title="Height distribution", x="Gender", y="Height in inches") +
  scale_x_discrete(labels=c("f" = "Female", "m" = "Male")) +
  theme_minimal()
p3

# Replace heights under 21 inches with the mean height
# (according to Google, the shortest person ever recorded was 21.5 inches)
profiles$height[profiles$height < 21] <- mean(profiles$height)
summary(profiles$height) # Now, min height is 26, but mean is still 68.3


########## ETHNICITY ##########
# Ethnicity distribution
nlevels(as.factor(profiles$ethnicity))
par(mar=c(12, 4.1, 4.1, 2.1))
plot(as.factor(profiles$ethnicity), las=2)
names(which.max(table(as.factor(profiles$ethnicity)))) # Most common: white


########## ORIENTATION ##########
# Orientation distribution
nlevels(as.factor(profiles$orientation))
names(which.max(table(as.factor(profiles$orientation)))) # Most common: straight


########## EDUCATION ##########
# Education distribution
nlevels(as.factor(profiles$education))
par(mar=c(13, 4.1, 4.1, 2.1))
plot(as.factor(profiles$education), las=3)
names(which.max(table(as.factor(profiles$education)))) # Most common: graduated from college/university


########## PETS ##########
# Pets distribution
nlevels(as.factor(profiles$pets))
par(mar=c(12, 4.1, 4.1, 2.1))
names(which.max(table(as.factor(profiles$pets)))) # Most common: likes dogs and likes cats


######### LOCATION ##########
# Enrich data with geographic data from LatLon.csv and addr.csv
moreData <- left_join(profiles, latlon, by ='location')
head(moreData)
finalData <- left_join(moreData, addr, by='location')
head(finalData)


# Geographic distribution: World
worldData <- map_data('world')
world <- fortify(worldData, region = 'region')
gg1 <- ggplot() + geom_map(data  =  world, map = world,
                          aes(x = long, y = lat, map_id = region, group = group),
                          fill = 'white', color = 'black', size = 0.25) + 
  coord_equal() +
  theme_map() + geom_point(data=moreData, aes(x=lon, y=lat, group=1), color='red', alpha=0.15) +
  labs(title="Geographic Distribution: World")
gg1 

# Geographic distribution: US
# Latitude and longitude points from: https://gist.github.com/jsundram/1251783
stateData <- map_data('state')
us <- fortify(stateData, region = 'region')
onlyUS <- moreData[which(moreData$lat > 24.7433195 & 
                           moreData$lat < 49.3457868 &
                           moreData$lon > -124.7844079 &
                           moreData$lon < -66.9513812), ]

gg <- ggplot() + geom_map(data  =  us, map = us,
                         aes(x = long, y = lat, map_id = region, group = group),
                         fill = 'white', color = 'black', size = 0.25) + 
  coord_map('mercator') +
  theme_map() + geom_point(data=onlyUS, aes(x=lon, y=lat, group=1), color='red', alpha=0.15)
gg 

# Geographic distribution: California
counties <- map_data("county")
CAcounty <- subset(counties, region == "california")
onlyCA <- subset(finalData, finalData$state == "California")

# Now build layer by layer State and county outlines then cell
ca <-ggplot() + geom_map(data = CAcounty, map = CAcounty,
                         aes(x = long, y = lat, map_id = region, group = group),
                         fill = 'white', color = 'black', size = 0.25) + 
  coord_map('albers', lat0 = 39, lat1 = 45) +
  theme_map() + geom_point(data=onlyCA, aes(x=lon, y=lat, group=1), color='red', alpha=0.15)
ca

# Geographic distribution: Zipcodes
summary(as.factor(finalData$postalCode)) #Most common: 94102, 94612, 94704
names(which.max(table(as.factor(finalData$postalCode))))


########## JOBS ##########
# Get table of frequencies for 21 job categories
job_count <- table(profiles$job)

# Remove "other" and "rather not say" responses
job_count <- job_count[-c(13,15)]

# Word cloud of most frequent jobs
par(mar=c(0,0,0,0))
wordcloud(names(job_count), job_count, min.freq=1, scale = c(1.3, 0.2), 
          max.words=200,colors=brewer.pal(8, "Blues"))



########## AGE AND EDUCATION ##########
# Feature Engineer relationship age & education
profiles$ageEDU <- paste(profiles$age, profiles$education, sep = '_')
barplot(profiles$ageEDU)


names(which.max(table(profiles$ageEDU)))
ageEDU_count <- table(profiles$ageEDU)
wordcloud(names(ageEDU_count), ageEDU_count, min.freq=500, scale = c(0.9, 0.1), 
          max.words=200,colors=brewer.pal(8, "Blues"))

# End
