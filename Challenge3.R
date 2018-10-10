
########################################################################
#                                                                      #
#           Solved by Suri Yaddanapudi on June 20th 2018               #  
########################################################################
## Load Required Packages
library("jsonlite")        # Parsing jSon Files
library("splitstackshape") # String Parsing
library("dplyr")           # Group by Functions 
library("lubridate")       # Time Series
library("reshape2")        # For converting DataFrames to Matrices
library("qlcMatrix")       # For Similarities
library("anytime")         # For cinverting Unix Tine Stamp
library("data.table")      # For Reading Falt Files
library("ggplot2")         # For visualizations
library("stringr")         # For NLP
### Data Cleaning and Parsing

## Read Json File Data to DatFrame and Clean the Data
Challenge3 = fromJSON("challenge_3.json", flatten=TRUE)

## Convert the nested list in Jason File to individual Cloumns
UserDF <- do.call(rbind, lapply(Challenge3$user, as.data.frame.list, stringsAsFactors = FALSE))
Challenge3$user = NULL
Challenge3 = cbind(Challenge3, UserDF)
unix_timestamp = Challenge3$unix_timestamp
unix_timestamp = unlist(unix_timestamp)
unix_timestamp = anytime(unix_timestamp)
Challenge3$unix_timestamp = unix_timestamp
## Change the missing Country to Unknown Country 
Challenge3[Challenge3$country == "", "country"] = "Unknown Country"

## Get the Hour when the user is Online
Challenge3$HourOnline = hour(Challenge3$unix_timestamp)


########### Business Question 1:

### Group the DataFrame by Country and then Hour to get the number of different users online at different hours
Grouped_Challeng3 <- Challenge3 %>% group_by(country,HourOnline)
DFGrouped_Challeng3 = Grouped_Challeng3 %>% summarise(
  Users_Online = n_distinct(session_id)
)
DFGrouped_Challeng3 = data.frame(DFGrouped_Challeng3,stringsAsFactors = FALSE)

#### Generate the Hourly Plot
p = ggplot(DFGrouped_Challeng3,aes(HourOnline,Users_Online,fill=as.factor(country)))+
  geom_bar(position="dodge",stat="identity",show.legend = FALSE)+
  facet_wrap(~country,nrow=length(unique(DFGrouped_Challeng3$country))) + scale_x_continuous(breaks = seq(min(DFGrouped_Challeng3$HourOnline), max(DFGrouped_Challeng3$HourOnline), by = 1)) + theme_bw()

#ggsave(filename="Challenge3.jpeg", plot= p)

### Solution to First Question:
### It's clearly evident from the plot that users in "Unknow Country" tend to sleep when it's working time in US.
## Given the above information, I would assume the country to be Australia




########### Business Question 2:
### Insert the multiple searches in the same session as new rows
Challenge3_Q2 = cSplit(Challenge3,"cities",",","long")
Challenge3_Q2$cities = as.character(Challenge3_Q2$cities)
## To create similarities between Cities, User X City Matrix is generated
UserXCityMat = acast(Challenge3_Q2, cities ~ user_id,fill = 0)

### CityXCity Similarity Matrix. Here, I am using simple Correlation to find the similarities between Cities based on the number of times
## a city is being searched by every used
CityXCity_SimMatrix = corSparse(t(UserXCityMat))
row.names(CityXCity_SimMatrix) = row.names(UserXCityMat)
colnames(CityXCity_SimMatrix) = row.names(UserXCityMat)



## Function to get Similar City for any given City
GetTopSimilarCity <- function(CityName){
  TopSimilarCity = CityXCity_SimMatrix[CityName,]
  TopSimilarCity = sort(TopSimilarCity,decreasing = TRUE)
  TopSimilarCity = head(TopSimilarCity)[2] 
  return(names(TopSimilarCity))
}

## Get Most Similar City
UniqueCities = row.names(CityXCity_SimMatrix)
SimilarCity = unlist(lapply(UniqueCities, GetTopSimilarCity))
AllSimilarCities = data.frame(City_Name = UniqueCities,Similar_City= SimilarCity, stringsAsFactors = FALSE)
write.table(AllSimilarCities,"AllSimilarCities.txt", sep = "\t", quote = FALSE, row.names = FALSE)


########### Business Question 3:

## Using the Similarities obtained between Cities obtained in the question2, 
## we can use this information to assign numerical values to cities searched
## in the same seission

## Calculate the Number of Cities searched by counting the "," in a given session
Challenge3_Q3 = Challenge3
Challenge3_Q3$Count_Cities_Searched = str_count(Challenge3_Q3$cities,",") + 1

## When there are more than one city search in a single session,
## Use the simple mean between their similarities to obtain a single distance

### Function to assign a numerical values to City Search based on CitySimilarities
GetCityDistances <- function(CityName){
  ## Check if there is only one City in the search
  ## If more than take the mean
  TotalDistance = 0
  UniqueCities = trimws(unlist(str_split(CityName,",")))
  if(length(UniqueCities) == 1){
    return(0)
  }else{
    for(FirstLoop in 1:(length(UniqueCities) - 1)){ ## First For loop Begins
      #Distance = 0
      for(SecondLoop in ((FirstLoop+1):length(UniqueCities))){ ## Second For Loop Begins
        Distance =  CityXCity_SimMatrix[UniqueCities[FirstLoop],UniqueCities[SecondLoop]]
        TotalDistance = TotalDistance + Distance
      } ## Second For Loop Ends
      
    } ## First For Loop Ends
    TotalDistance = TotalDistance/length(UniqueCities)
    return(round(TotalDistance,2))
   
  }
  
}


CityDistances = lapply(Challenge3_Q3$cities, GetCityDistances)
CityDistances = unlist(CityDistances)
Challenge3_Q3$CityDistances = CityDistances

### Generate Histogram based on the City Distances
p = ggplot(data=Challenge3_Q3, aes(CityDistances)) + geom_histogram() +
  theme_bw() + xlab("City Distinces") + ylab("Frequency") +
  ggtitle("Histogram For City Distances")
ggsave(filename="Challenge3_Q3.jpeg", plot= p)  

