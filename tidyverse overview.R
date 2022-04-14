#install.packages(tidyverse)
#install.packages(xlsx)
library(tidyverse)
library(readxl)

# Part 1: Exploring your data

## ----load 2020 Census Population dataset--------------------------------------

Census2020 <- read_excel("2020 Census File.xlsx")


## ----investigate with glimpse-------------------------------------------------

glimpse(Census2020)


## ----explore the dimensions---------------------------------------------------

dim(Census2020)


## ----display column and row names---------------------------------------------

colnames(Census2020)

rownames(Census2020)


## ----view top and bottom observations-----------------------------------------

head(Census2020)

tail(Census2020)


## ----explore largest and smallest values in a column--------------------------

min(Census2020$`2020 Census Resident Population`)

max(Census2020$`2020 Census Resident Population`)


## ----display summary stats----------------------------------------------------

summary(Census2020)


## ----open and explore dataset-------------------------------------------------

View(Census2020)


## ----identify a column--------------------------------------------------------

Census2020$`2020 Census Resident Population`

Census2020$Region


## ----display contents of column as a table------------------------------------

table(Census2020$Region)

table(Census2020$Area, Census2020$Region)


## ----identify an exact position-----------------------------------------------

Census2020[,1]

Census2020[1,]

Census2020[1,1]


## ----export to csv------------------------------------------------------------

write.csv(Census2020, "Census2020.csv")


# Part 2: Manipulate and transform with Tidyverse: intro to dplyr commands
# -----using select, rename, filter, arrange, mutate, summarize  

## ----readin 2019 ACS population and poverty data------------------------------

Census2019 <- read_csv("2019Pop.csv")

Poverty2019 <- read_csv("2019Poverty.csv")


## ----use the select function to keep/select the columns: state name, region,---- 
# -----2020 population,numeric change, percent change, and state rank 

Census2020Sub1 <- Census2020 %>% 
  select(`Area`,
         `Region`,
         `2020 Census Resident Population`, 
         `Numeric Change`, 
         `Percent Change`, 
         `State Rank Based on 2020 Census Resident Population`)


## ----view subsetted object

Census2020Sub1


## ----use the rename function to rename columns to easy to work with names-----

Census2020Sub1 <- Census2020Sub1 %>% 
  rename(State = Area, 
         Pop2020 = `2020 Census Resident Population`, 
         NumChange2020 = `Numeric Change`, 
         PercentChange2020 = `Percent Change`, 
         StateRank = `State Rank Based on 2020 Census Resident Population`)


## ----view new column names

str(Census2020Sub1)


## ----use the filter function to subset rows by pop size, using 9999999-------- 
#------as the limit

PopAboveLimit <- Census2020Sub1 %>% 
  filter(Pop2020 > 9999999)

PopBelowLimit <- Census2020Sub1 %>% 
  filter(Pop2020 <= 9999999)


## ----view dimensions of new object

dim(PopAboveLimit)

dim(PopBelowLimit)


## ----use filter to subset rows by two conditions, using pop and state rank----

PopAboveLimitAND <- Census2020Sub1 %>% 
  filter(Pop2020 > 9999999 & StateRank >= 9)

PopAboveLimitOR <- Census2020Sub1 %>% 
  filter(Pop2020 > 9999999 | StateRank >= 50)


## ----view new objects

dim(PopAboveLimitAND)

dim(PopAboveLimitOR)


## ----use the arrange function to sort the two pop objects by state rank-------
str(Census2020Sub1$StateRank)

Census2020Sub1$StateRank <- as.numeric(Census2020Sub1$StateRank, na.rm = TRUE)


## ----states with population above and below the set limit, ordered ascending:

TopPopAsce <- PopAboveLimit %>% 
  arrange(StateRank)   

LowPopAsce <- PopBelowLimit %>% 
  arrange(StateRank)  


## ----view new objects- population above the limit, ascending 

head(TopPopAsce)


## ----view new objects- population below the limit, ascending

head(LowPopAsce)


## ----states with population above and below the set limit, ordered descending:

TopPopDesc <- PopAboveLimit %>% 
  arrange(desc(StateRank))

LowPopDesc <- PopBelowLimit %>% 
  arrange(desc(StateRank))


## ----view new objects- population above the limit, descending 

head(TopPopDesc)


## ----view new objects- population below the limit, descending

head(LowPopDesc)


## ----use the mutate function to add a new column------------------------------
#------calculate the 2010 pop using the 2020 pop and numeric change columns

Census2020Mutate <- Census2020Sub1 %>% 
  mutate(Pop2010 = Pop2020-NumChange2020)


## ----view new object

head(Census2020Mutate)


## ----use the summarise function to determine the total population in the US----
#------for both 2020 and 2010 

Census2020PopSum <- Census2020Mutate %>% 
  summarise(Total2020 = sum(Pop2020))

Census2010PopSum <- Census2020Mutate %>% 
  summarise(Total2010 = sum(Pop2010))


## ----view new object with totals of 2020 and 2010

Census2020PopSum

Census2010PopSum


## ----calculate the average national population for 2020 and 2010--------------

Census2020PopMean <- Census2020Mutate %>% 
  summarize(Total2020 = mean(Pop2020))

Census2010PopMean <- Census2020Mutate %>% 
  summarize(Total2010 = mean(Pop2010))


## ----view new objects

Census2020PopMean

Census2010PopMean


## ----calculate the total and average difference in population----------------- 
#------between 2020 and 2010

Census2020PopSum - Census2010PopSum

Census2020PopMean - Census2010PopMean


## ----calculate the sum of large states----------------------------------------

PopAboveLimitSum <- PopAboveLimit %>% 
  summarize(TotalLarge2020 = sum(Pop2020))


## ----view new object

PopAboveLimitSum


## ----calculate the sum of small states----------------------------------------

PopBelowLimitSum <- PopBelowLimit %>% 
  summarize(TotalSmall2020 = sum(Pop2020))


## ----view new object

PopBelowLimitSum


## ----examples combining verbs-------------------------------------------------

## ----combine select and rename------------------------------------------------

Census2020Bonus <- Census2020 %>% 
  select(`Area`, 
         `2020 Census Resident Population`, 
         `2010 Census Resident Population`,
         `State Rank Based on 2020 Census Resident Population`) %>% 
  rename(State = Area, 
         Pop2020 = `2020 Census Resident Population`, 
         Pop2010 = `2010 Census Resident Population`,
         StateRank = `State Rank Based on 2020 Census Resident Population`)


## ----view top obs of new object

head(Census2020Bonus)


## ----combine filter and arrange----------------------------------------------- 

Census2020Bonus1 <- Census2020Bonus %>% 
  filter(StateRank >= 2 & StateRank <= 50) %>% 
  arrange(desc(Pop2020))


## ----view new object

glimpse(Census2020Bonus1)


## ----combine mutate and summarize: sum population of top largest and smallest 
#-----states using prior dataset

Census2020Bonus2 <- Census2020Bonus1 %>% 
  mutate(size = case_when(Pop2020 > 9999999 ~ 'Big', 
                          Pop2020 <= 9999999 ~ 'Small')) %>% 
  group_by(size) %>% 
  summarize(Total2020 = sum(Pop2020))


## ----view new object

glimpse(Census2020Bonus2)
  

## ----put it all together------------------------------------------------------

Census2020Workflow <- Census2020 %>% 
  select(`Area`, 
         `2020 Census Resident Population`, 
         `2010 Census Resident Population`,
         `State Rank Based on 2020 Census Resident Population`) %>% 
  rename(State = Area, 
         Pop2020 = `2020 Census Resident Population`, 
         Pop2010 = `2010 Census Resident Population`,
         StateRank = `State Rank Based on 2020 Census Resident Population`) %>%
  filter(StateRank >= 2 & StateRank <= 50) %>% 
  arrange(desc(Pop2020)) %>%
  mutate(size = case_when(Pop2020 > 9999999 ~ 'Big', 
                          Pop2020 <= 9999999 ~ 'Small')) %>% 
  group_by(size) %>% 
  summarize(Total2020 = sum(Pop2020))

## ----view new object

Census2020Workflow


## ----join 2020 Census with 2019 ACS Pop and 2019 ACS Poverty, by state--------

CensusData1 <- left_join(Census2020Sub1, Census2019, by = "State")


## ----view new object

head(CensusData1)


## ----use rename function to change generic "estimate" column------------------
# ----to something specific 

CensusData1 <- CensusData1 %>% 
  rename(PopEstimate2019 = Estimate)

CensusData2 <- left_join(CensusData1, Poverty2019, by = "State")


## ----view new object

head(CensusData2)


## ----use filter and mutate to add a ranking variable for states based--------- 
# -----on below poverty variable

CensusDataRanked <- CensusData2 %>% 
  mutate(PovertyRank = dense_rank(desc(BelowPoverty))) %>% 
  filter(PovertyRank <= 10)


## ----view new object

glimpse(CensusDataRanked)

glimpse(CensusDataRanked$PovertyRank)


## ----visualize using ggplot---------------------------------------------------

ggplot(CensusDataRanked) +
  geom_bar(mapping = aes(x = reorder(State, -BelowPoverty), 
                         y = BelowPoverty, 
                         fill = PercentChange2020), 
           stat = 'identity') +
  labs(title = "Top 10 Most Populated States in 2020",
       x = "State",
       y = "Population Below Poverty") +
  coord_flip()


#Part 3: Explore with TidyCensus and API

## ----api-key------------------------------------------------------------------
library(tidycensus)

census_api_key("1628931ba3a0a3e59ffe62387248b2f340e3c1e2", install = TRUE)

## ----search-variables---------------------------------------------------------

vars <- load_variables(2020, "pl")

View(vars)


## ----decennial----------------------------------------------------------------

pop20 <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  year = 2020)


## ----view-decennial-----------------------------------------------------------

glimpse(pop20)

##----view DMV population from Census provided data
#District of Columbia
pop20 %>% filter(GEOID == 11)

#Maryland
pop20 %>% filter(GEOID == 24)

#Virginia
pop20 %>% filter(GEOID == 51)


##----view DMV population from outside source provided data
#District of Columbia
Census2020 %>% filter(Area == "District of Columbia")

#Maryland
Census2020 %>% filter(Area == "Maryland")

#Virginia
Census2020 %>% filter(Area == "Virginia")

##----Compare the two sources of data, create new objects for each--------------

#District of Columbia
API_DC <- pop20 %>% 
  filter(GEOID == 11) %>% 
  select(value)

ACS_DC <- Census2020 %>% 
  filter(Area == "District of Columbia") %>% 
  select(`2020 Census Resident Population`)


#Maryland
API_MD <- pop20 %>% filter(GEOID == 24) %>% 
  select(value)

ACS_MD <- Census2020 %>% 
  filter(Area == "Maryland") %>% 
  select(`2020 Census Resident Population`)


#Virginia
API_VA <- pop20 %>% filter(GEOID == 51) %>% 
  select(value)

ACS_VA <- Census2020 %>% 
  filter(Area == "Virginia") %>% 
  (`2020 Census Resident Population`)


## ---- do the two sources of data match?---------------------------------------

#District of Columbia
all(API_DC == ACS_DC)

#Maryland
all(API_MD == ACS_MD)

#Virginia
all(API_VA == ACS_VA)


## ----group quarters data------------------------------------------------------

group_quarters <- get_decennial(
  geography = "state", 
  table = "P5", 
  year = 2020,
  output = "wide")


## ----show group quarters data

head(group_quarters)


##----group quarters DMV data---------------------------------------------------

va_group_quarters <- get_decennial(
  geography = "state", 
  table = "P5", 
  state = "VA",
  year = 2020,
  output = "wide")

md_group_quarters <- get_decennial(
  geography = "state", 
  table = "P5", 
  state = "MD",
  year = 2020,
  output = "wide")

dc_group_quarters <- get_decennial(
  geography = "state", 
  table = "P5", 
  state = "DC",
  year = 2020,
  output = "wide")


##-----concatenate rows with rbind

dmv_group_quarters <- rbind(va_group_quarters,
                            md_group_quarters,
                            dc_group_quarters)


##----view DMV group quarters object

dmv_group_quarters


## ----show hispanic DMV data---------------------------------------------------
va_hispanic <- get_decennial(
  geography = "county", 
  variables = "P2_002N", 
  state = "VA", 
  year = 2020)

md_hispanic <- get_decennial(
  geography = "county", 
  variables = "P2_002N", 
  state = "MD", 
  year = 2020)

dc_hispanic <- get_decennial(
  geography = "county", 
  variables = "P2_002N", 
  state = "DC", 
  year = 2020)

##----show DMV Hispanic data----------------------------------------------------
va_hispanic
md_hispanic
dc_hispanic

