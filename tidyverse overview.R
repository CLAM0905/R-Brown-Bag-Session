#install.packages("tidyverse", repos = "http://cran.us.r project.org")
#install.packages("readxl", repos = "http://cran.us.r project.org")
library(tidyverse)
library(readxl)


# ----Part 1: Exploring your data-----------------------------------------------

## Load 2020 Census Population dataset 

Census2020 <-  read_excel("2020 Census File.xlsx")


## Investigate with glimpse    

glimpse(Census2020)


## Explore the dimensions         

dim(Census2020)


## Display column and row names       

colnames(Census2020)

rownames(Census2020)


## View top and bottom observations    

head(Census2020)

tail(Census2020)


## Explore largest and smallest values in a column      

max(Census2020$`2020 Census Resident Population`)

min(Census2020$`2020 Census Resident Population`)


## Display summary stats               

summary(Census2020)


## Open and explore the dataset in a new pane- with filtering options 

View(Census2020)


## Identify a column                     

Census2020$`2020 Census Resident Population`

Census2020$Region


## Display contents of column as a table   

table(Census2020$Region)

table(Census2020$Area, Census2020$Region)


## Identify an exact position, [rows, columns]    

Census2020[,1]

Census2020[1,]

Census2020[1,1]


## Export to csv                      

write.csv(Census2020, "Census2020.csv")


# Part 2: Manipulate and transform with Tidyverse: intro to dplyr commands using select, rename, filter, arrange, mutate, summarize ----- 

## Read-in two ACS files: 2019 population and 2019 poverty rate

Census2019 <-  read_csv("2019Pop.csv")

Poverty2019 <-  read_csv("2019Poverty.csv")

 
## Use the select function to keep/select the columns: state name, region, 2020 population,numeric change, percent change, and state rank

Census2020Sub1 <-  Census2020 %>% 
  select(`Area`,
         `Region`,
         `2020 Census Resident Population`, 
         `Numeric Change`, 
         `Percent Change`, 
         `State Rank Based on 2020 Census Resident Population`)


## View the subsetted object

Census2020Sub1


## Use the rename function to rename columns to easy to work with names

Census2020Sub1 <-  Census2020Sub1 %>% 
  rename(State = Area, 
         Pop2020 = `2020 Census Resident Population`, 
         NumChange2020 = `Numeric Change`, 
         PercentChange2020 = `Percent Change`, 
         StateRank = `State Rank Based on 2020 Census Resident Population`)


## View new column names

str(Census2020Sub1)


## Use the filter function to subset rows by pop size, using 9999999 as the limit

PopAboveLimit <-  Census2020Sub1 %>% 
  filter(Pop2020 > 9999999)

PopBelowLimit <-  Census2020Sub1 %>% 
  filter(Pop2020 <= 9999999)


## View dimenstions of the new objects

dim(PopAboveLimit)

dim(PopBelowLimit)


## Use filter to subset rows by two conditions, using population and state rank

#Use a population limit of 9999999 and state rank limits to narrow down data

PopAboveLimitAND <-  Census2020Sub1 %>% 
  filter(Pop2020 > 9999999 & StateRank >= 9)

PopAboveLimitOR <-  Census2020Sub1 %>% 
  filter(Pop2020 > 9999999 | StateRank >= 9)

 
## View the contents of the new object

glimpse(PopAboveLimitAND)

glimpse(PopAboveLimitOR)


## Convert state rank from integer to numeric

str(Census2020Sub1$StateRank)

Census2020Sub1$StateRank <-  as.numeric(Census2020Sub1$StateRank, na.rm = TRUE)

 
## Use the arrange function to sort the two population objects by state rank

# Order the filtered objects by ascending

TopPopAsce <-  PopAboveLimit %>% 
  arrange(StateRank)   

LowPopAsce <-  PopBelowLimit %>% 
  arrange(StateRank)  


## View new object containing large states arranged by state rank- ascending 

head(TopPopAsce)

 
## View new object containing small states arranged by state rank- ascending 

head(LowPopAsce)


## Use the arrange function to sort the two population objects by state rank

# Order the filtered objects by descending

TopPopDesc <-  PopAboveLimit %>% 
  arrange(desc(StateRank))

LowPopDesc <-  PopBelowLimit %>% 
  arrange(desc(StateRank))

 
## View new object with large states arranged by state rank- descending 

head(TopPopDesc)


## View new object with small states arranged by state rank- descending 

head(LowPopDesc)


## Use the mutate function to add a new column

# Calculate the 2010 pop using the 2020 pop and numeric change columns

Census2020Mutate <-  Census2020Sub1 %>% 
  mutate(Pop2010 = Pop2020 - NumChange2020)


## View top observations of new object

head(Census2020Mutate)


## Use the summarise function to determine the total population in the US across all states, for 2020 and 2010

# 2020

Census2020PopSum <-  Census2020Mutate %>% 
  summarise(Total2020 = sum(Pop2020))


# 2010

Census2010PopSum <-  Census2020Mutate %>% 
  summarise(Total2010 = sum(Pop2010))


## View new objects with totals of 2020 and 2010 population size

# 2020

Census2020PopSum


# 2010

Census2010PopSum


## Use the summarise function to determine the total population in the US across all states, for 2020 and 2010. Include group_by region

# 2020

Census2020PopbyRegion <-  Census2020Mutate %>% 
  group_by(Region) %>%
  summarise(Total2020 = sum(Pop2020))


# 2010

 Census2010PopbyRegion <-  Census2020Mutate %>% 
  group_by(Region) %>%
  summarise(Total2010 = sum(Pop2010))


## View new objects with totals of 2020 and 2010 population size, grouped by region

# 2020 

Census2020PopbyRegion


# 2010

Census2010PopbyRegion


## Calculate the average national population for 2020 and 2010, include group_by region 

# 2020

Census2020PopbyRegion <-  Census2020Mutate %>% 
  group_by(Region) %>%
  summarize(Total2020 = mean(Pop2020))

# 2010

Census2010PopbyRegion <-  Census2020Mutate %>% 
  group_by(Region) %>%
  summarize(Total2010 = mean(Pop2010))


## View new objects with averages of 2020 and 2010 population size, grouped by region

# 2020

Census2020PopbyRegion


# 2010

Census2010PopbyRegion

 
## Calculate the sum of large states, include group_by region     

PopAboveLimitbyRegion <-  PopAboveLimit %>% 
  group_by(Region) %>%
  summarize(TotalLarge2020 = sum(Pop2020))


## View new object with total population of large states, grouped by region

PopAboveLimitbyRegion

 
## Calculate the sum of small states, include group_by region 

# Use the object PopBelowLimit

PopBelowLimitbyRegion <- PopBelowLimit %>% 
  group_by(Region) %>%
  summarize(TotalSmall2020 = sum(Pop2020))

 
## View new object with total population of small states, grouped by region

PopBelowLimitbyRegion

 
## Examples of combining multiple dplyr verbs in one workflow                         - You can use all of the verbs chained together in logical order to achieve complex results                      

## Utilize select and rename functions in one workflow

Census2020Bonus <-  Census2020 %>% 
  select(`Area`, 
         `2020 Census Resident Population`, 
         `2010 Census Resident Population`,
         `State Rank Based on 2020 Census Resident Population`) %>% 
  rename(State = Area, 
         Pop2020 = `2020 Census Resident Population`, 
         Pop2010 = `2010 Census Resident Population`,
         StateRank = `State Rank Based on 2020 Census Resident Population`)


## View top observations of new object

head(Census2020Bonus)


## Utilize filter and arrange in one workflow    

Census2020Bonus1 <-  Census2020Bonus %>% 
  filter(StateRank >= 2 & StateRank <= 50) %>% 
  arrange(desc(Pop2020))


## View glimpse of new object

 

glimpse(Census2020Bonus1)

## Combine the mutate and summarize functions in one workflow

# Sum the population of top largest and smallest states using prior object

Census2020Bonus2 <-  Census2020Bonus1 %>% 
  mutate(size = case_when(Pop2020 > 9999999 ~ 'Big', 
                          Pop2020 <= 9999999 ~ 'Small')) %>% 
  group_by(size) %>% 
  summarize(Total2020 = sum(Pop2020))

 
## View glimpse of new object

glimpse(Census2020Bonus2)


## Put it all together     

Census2020Workflow <-  Census2020 %>% 
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


## View outcome, it is the same as the workflow seen prior

Census2020Workflow


## Join 2020 Census with 2019 ACS Population, by state   

CensusData1 <-  left_join(Census2020Sub1, Census2019, by = "State")


## View new joined object

head(CensusData1)


## Join 2020 and 2019 population object with 2019 ACS Poverty, by state

# Use rename function to change generic "estimate" column to something specific before join

CensusData1 <-  CensusData1 %>% 
  rename(PopEstimate2019 = Estimate)

CensusData2 <-  left_join(CensusData1, Poverty2019, by = "State")


## View top observations of the new object

head(CensusData2)


## Use filter and mutate functions to add a ranking variable for states based on below poverty variable

CensusDataRanked <-  CensusData2 %>% 
  mutate(PovertyRank = dense_rank(desc(BelowPoverty))) %>% 
  filter(PovertyRank <= 10)


## View a glimpse of new object

glimpse(CensusDataRanked)

glimpse(CensusDataRanked$PovertyRank)


## Visualize using ggplot  

ggplot(CensusDataRanked) +
  geom_bar(mapping = aes(x = reorder(State,  BelowPoverty), 
                         y = BelowPoverty, 
                         fill = PercentChange2020), 
           stat = 'identity') +
  labs(title = "Top 10 Most Populated States in 2020",
       x = "State",
       y = "Population Below Poverty") +
  coord_flip()


## ----Part 3: Explore with Tidycensus and API----------------------------------

## API Key and load Tidycensus package                                                              

#install.packages("tidycensus") 
library(tidycensus)

census_api_key("INSERT API HERE")


## Search for Variables         

vars <-  load_variables(2020, "pl")

print(tbl_df(vars), n=301)


## Look at Decennial Population Numbers    

pop20 <-  get_decennial(
  geography = "state",
  variables = "P1_001N",
  year = 2020)


## View table of decennial counts

print(tbl_df(pop20), n=52)


## View DMV population from Census provided data

# District of Columbia

pop20 %>% filter(GEOID == 11)


# Maryland
 
pop20 %>% filter(GEOID == 24)


# Virginia
 
pop20 %>% filter(GEOID == 51)


## View DMV population from outside source provided data

# District of Columbia
 
Census2020 %>% filter(Area == "District of Columbia")


# Maryland
 
Census2020 %>% filter(Area == "Maryland")


# Virginia
 
Census2020 %>% filter(Area == "Virginia")

 
## Compare the two sources of data, create new objects for each        

# District of Columbia
 
API_DC <-  pop20 %>% 
  filter(GEOID == 11) %>% 
  select(value)

ACS_DC <-  Census2020 %>% 
  filter(Area == "District of Columbia") %>% 
  select(`2020 Census Resident Population`)


# Maryland

API_MD <- pop20 %>% filter(GEOID == 24) %>% 
  select(value)

ACS_MD <-  Census2020 %>% 
  filter(Area == "Maryland") %>% 
  select(`2020 Census Resident Population`)
 

# Virginia
 
API_VA <-  pop20 %>% filter(GEOID == 51) %>% 
  select(value)

ACS_VA <-  Census2020 %>% 
  filter(Area == "Virginia") %>% 
  select(`2020 Census Resident Population`)


## Do the two sources of population data match?

# District of Columbia

all(API_DC == ACS_DC)


# Maryland
 
all(API_MD == ACS_MD)


# Virginia
 
all(API_VA == ACS_VA)


## Group quarters data                                                      
 
group_quarters <-  get_decennial(
  geography = "state", 
  table = "P5", 
  year = 2020,
  output = "wide")

 
## Show top observations of group quarters data
 
head(group_quarters)

 
## Group quarters DMV data                                                   

# District of Columbia
 
dc_group_quarters <-  get_decennial(
  geography = "state", 
  table = "P5", 
  state = "DC",
  year = 2020,
  output = "wide")

# Maryland
 
md_group_quarters <-  get_decennial(
  geography = "state", 
  table = "P5", 
  state = "MD",
  year = 2020,
  output = "wide")


# Virginia
 
va_group_quarters <-  get_decennial(
  geography = "state", 
  table = "P5", 
  state = "VA",
  year = 2020,
  output = "wide")


## Use rbind to concatenate rows

dmv_group_quarters <-  rbind(va_group_quarters,
                             md_group_quarters,
                             dc_group_quarters)


## View DMV group quarters object
 
dmv_group_quarters


## Show hispanic DMV data                                                   

# District of Columbia
 
dc_hispanic <-  get_decennial(
  geography = "county", 
  variables = "P2_002N", 
  state = "DC", 
  year = 2020)


# Maryland
 
md_hispanic <-  get_decennial(
  geography = "county", 
  variables = "P2_002N", 
  state = "MD", 
  year = 2020)


# Virginia
 
va_hispanic <-  get_decennial(
  geography = "county", 
  variables = "P2_002N", 
  state = "VA", 
  year = 2020)


## Show DMV Hispanic data  

#District of Columbia
 
dc_hispanic
 
# Maryland

md_hispanic

# Virginia

va_hispanic

 

