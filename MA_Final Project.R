rm(list = ls()) 
library(doBy)
library(dplyr)
library(foreign)
library(ggplot2)
library(knitr)
library(lmtest)
library(readstata13)
library(sandwich)
library(stargazer)
library(AER)
library(gdata)
library(wooldridge)
library(openintro)
library(tidyr)
library(readxl)
library(tidyverse)
library(gridExtra)
library(factoextra)

################### Meals Dataset Exploratory and clustering Analysis #######################
### 1. import dataset
#import meals dataset
mydata=Meals3

#general checks 
#check for NAs
sum(is.na(mydata)) #0 NA 
class(mydata)
summary(mydata)

### 2. Exploratory Data Analysis
## 2.1 Some visualizations
# - plot.1 Aggregate by Santa Clara County by Year
SCCyear<-filter(mydata, County=="Santa Clara")
SCCyearMeals<-aggregate(SCCyear$Meals, by=list(Year=SCCyear$Year), FUN=sum)
ggplot(SCCyearMeals, aes(x=Year, y=x/1000)) + geom_col(fill="dark red") +   
  labs(x = "Year", y = "Count (in thousands)", title = "Meals served in Santa Clara County")  + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = .5)) 
# - plot.2 Aggregate by Monterey County by Year
MCyear<-filter(mydata, County=="Monterey")
MCyearMeals<-aggregate(MCyear$Meals, by=list(Year=MCyear$Year), FUN=sum)
ggplot(MCyearMeals, aes(x=Year, y=x/1000)) + geom_col(fill="dark red") +   
  labs(x = "Year", y = "Count (in thousands)", title = "Meals served in Monterey County") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = .5))
# - plot.3 Aggregate by Merced County by Year
MerCyear<-filter(mydata, County=="Merced")
MerCyearMeals<-aggregate(MerCyear$Meals, by=list(Year=MerCyear$Year), FUN=sum)
ggplot(MerCyearMeals, aes(x=Year, y=x/1000)) + geom_col(fill="dark red") +   
  labs(x = "Year", y = "Count (in thousands)", title = "Meals served in Merced County") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = .5))
# - plot.4 Aggregate by San Benito County by Year
SBCyear<-filter(mydata, County=="San Benito")
SBCyearMeals<-aggregate(SBCyear$Meals, by=list(Year=SBCyear$Year), FUN=sum)
ggplot(SBCyearMeals, aes(x=Year, y=x/1000)) + geom_col(fill="dark red") +   
  labs(x = "Year", y = "Count (in thousands)", title = "Meals served in San Benito County") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = .5))
# - plot.5 Aggregate by Santa Cruz County by Year
SCyear<-filter(mydata, County=="Santa Cruz")
SCyearMeals<-aggregate(SCyear$Meals, by=list(Year=SCyear$Year), FUN=sum)
ggplot(SCyearMeals, aes(x=Year, y=x/1000)) + geom_col(fill="dark red") +   
  labs(x = "Year", y = "Count (in thousands)", title = "Meals served in Santa Cruz County") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = .5))
# - plot.6Aggregate by Unknown County by Year
UNKyear<-filter(mydata, County=="Unknown")
UNKyearMeals<-aggregate(UNKyear$Meals, by=list(Year=UNKyear$Year), FUN=sum)
ggplot(UNKyearMeals, aes(x=Year, y=x/1000)) + geom_col(fill="dark red") +   
  labs(x = "Year", y = "Count (in thousands)", title = "Meals served in Unknown County") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = .5))

## 2.2 Analyze MK current state of operations
mydataETA<-mydata
#add column "lower case counties" to dataframe
mydataETA[,"county_lower"] <- tolower(mydataETA$County)
#Filter dataset to 2021 to  current state  
mydataETA21<-filter(mydataETA,Year==2021 )

#load in CA counties and minimize to only Martha's kitchen counties
CA_counties<-map_data("county",region="California")
MK_counties<-CA_counties
MK_counties<-filter(CA_counties, subregion=="alameda"| subregion=="contra costa"| subregion=="monterey"| subregion=="san benito"| subregion=="san francisco"|subregion=="san mateo"|subregion=="santa clara"|subregion=="santa cruz"|subregion=="merced")

# create new df to show Poverty by County
povertydf<-mydataETA21%>% group_by(county_lower) %>% 
  summarise(poverty=mean(`Poverty`))
povertydf<-na.omit(povertydf)
povertydf<-data.frame(povertydf)

#join the poverty df and county df
MK_counties<-left_join(MK_counties,povertydf, by=c("subregion"="county_lower"))

#plot the poverty 
MK_counties %>% 
  ggplot(aes(x=long,y=lat,group=group, fill=poverty)) +
  geom_polygon(color = "gray90", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55)+
  #scale_fill_gradient(low = "#132B43",high = "#56B1F7",space = "Lab",na.value = "grey50", guide = "colourbar", aesthetics = "fill")+
  scale_fill_continuous(type = "gradient")+
  labs(title="Poverty by County")+
  #scale_fill_brewer("Reds")+
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

#create new df to show Meals served by County
mealsdf<-mydataETA21%>% group_by(county_lower) %>% 
  summarise(meals=sum(`Meals`))
#join the meals df and county df
MK_counties<-left_join(MK_counties,mealsdf, by=c("subregion"="county_lower"))

#plot the meals served  
MK_counties %>% 
  ggplot(aes(x=long,y=lat,group=group, fill=meals)) +
  geom_polygon(color = "gray90", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55)+
  #scale_fill_gradient(low = "#132B43",high = "#56B1F7",space = "Lab",na.value = "grey50", guide = "colourbar", aesthetics = "fill")+
  scale_fill_continuous(type = "gradient")+
  labs(title = "Number of Meals by County")+
  #scale_fill_brewer("Reds")+
  theme(legend.position="bottom",
        legend.spacing.x = unit(1,"cm"),
        legend.direction = "vertical",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

# create new df to show Partners by County
partnerdf<-mydataETA21%>% group_by(Partner,county_lower) %>% 
  summarise(count=unique(n()))
partnerdf$count[partnerdf$count>1]<-1
partnerdf<-partnerdf%>%group_by(county_lower)%>%
  summarise(partnercount=sum(count))
#join the partner df and county df
MK_counties<-left_join(MK_counties,partnerdf, by=c("subregion"="county_lower"))

#plot the partners  
MK_counties %>% 
  ggplot(aes(x=long,y=lat,group=group, fill=partnercount)) +
  geom_polygon(color = "gray90", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55)+
  #scale_fill_gradient(low = "#132B43",high = "#56B1F7",space = "Lab",na.value = "grey50", guide = "colourbar", aesthetics = "fill")+
  scale_fill_continuous(type = "gradient")+
  labs(title = "Number of Partners by County")+
  #scale_fill_brewer("Reds")+
  theme(legend.position="bottom",
        legend.spacing.x = unit(1,"cm"),
        legend.direction = "vertical",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

#Let's analyze total meals and total active partners through the years
#scale meals served by 10000
mydataETA[,"meals_scaled"] <- mydataETA$Meals/10000
#group by year and partner
newdf<-mydataETA%>% group_by(Year,Partner) %>% 
  summarise(count=unique(n()), mealsum=sum(meals_scaled))
newdf<-filter(newdf,mealsum>1)
newdf$count[newdf$count>1]<-1
newdf<-newdf%>%group_by(Year) %>%
  summarise(partnersum=sum(count), meals=(sum(mealsum)))

#Plotting Partners versus Years
library(ggplot2)
ggp <- ggplot(newdf)  + 
  geom_bar(aes(x=Year, y=meals),stat="identity", fill="navy",colour="#006000")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = .5))+
  geom_line(aes(x=Year, y=partnersum),stat="identity",color="darkred",size=2)+
  labs(title= "Number Partners vs Number of Years",
       x="Year",y="Number of Partners")+
  scale_y_continuous(sec.axis=sec_axis(~.*.0001,name="Number of Meals"))
ggp

mydata$Date <- ymd(mydata$Date)
today <-Sys.Date()
mydata$Day <- as.numeric(difftime(today, mydata$Date),units="days")

#Create variable for recency, frequency, and monetary and scale the data 
mydata_group <- mydata %>% group_by(`Partner`) %>% filter(Year >= 2019 & Meals >0) %>%
  distinct(`Year`,`Month`, .keep_all = TRUE) %>%summarise(Frequency=n(), Recency = min(Day),Total_meals=sum(Meals)) %>% 
  mutate_at(c(2,3,4), funs(c(scale(.))))
summary(mydata_group)

mydata_subset <- subset(mydata_group, select = -c(`Partner`))
cor(mydata_subset)

#K-mean 
clustermydata<-kmeans(mydata_subset,centers=4,nstart=20)
clustermydata

ggplot(mydata_subset, aes(`Recency`, `Total_meals`, color = clustermydata$cluster))+geom_point()
ggplot(mydata_subset, aes(`Frequency`, `Total_meals`, color = clustermydata$cluster))+geom_point()

fviz_cluster(clustermydata,mydata_subset) +  theme(panel.border = element_rect(color = "black", fill = NA, size = .5))
fviz_nbclust(mydata_subset,kmeans,method='wss') +  theme(panel.border = element_rect(color = "black", fill = NA, size = .5))

#Hierarchical Clustering
#Calculate the distance matrix - default is euclidean
distance=dist(mydata_subset)
#Hierarchical cluster creation
mydata_subset.hcluster <- hclust(distance)

#Characterizing clusters
# Use cutree to separate the tree
member = cutree(mydata_subset.hcluster,4)
table(member)
#Look at characteristics of each group by means on different variables
#using for customer segments based on the Monetary
aggregate(mydata_subset,list(member),mean)

#Find the best cluster 
# Using Silhouette
fviz_nbclust(mydata_subset,FUN = hcut,method='silhouette')+ theme(panel.border = element_rect(color = "black", fill = NA, size = .5))

#Make partner list for each segments
cluster_group <- table(member,mydata_group$Partner)
cluster_group <- data.frame(t(rbind(cluster_group)))

new_partner <- rownames(cluster_group)[cluster_group$X1 == 1]
length(new_partner)
new_partner

potential_partner <- rownames(cluster_group)[cluster_group$X2 == 1]
length(potential_partner)
potential_partner 

low_active <- rownames(cluster_group)[cluster_group$X3 == 1]
length(low_active)
low_active

best_partner <- rownames(cluster_group)[cluster_group$X4 == 1]
length(best_partner)
best_partner

#no active partner list
df_no_serve <- mydata %>% group_by(`Partner`) %>% summarise(Total_meals=sum(Meals)) %>% filter(Total_meals == 0)
df_no_serve_list <- df_no_serve$Partner
df_no_serve_list


########### Website Dataset Exploratory and Regression Analysis + Cross Validation###############
### 1. import dataset
#import the website data set and create a new data frame 'mydata'
df <- traffic_over_time_table_2021_01_11_2022_01_10

##general checks 
#check for NAs
sum(is.na(df)) #0 NA 
class(df)
summary(df)

### 2. dummy coding and visualizations for some categorical columns
#check the unique values of some categorical values
length(unique(df$`Traffic category`))       #5 unique categories 
length(unique(df$`Traffic source`)) #81 unique categories
length(unique(df$`Entry page`))     #15 unique categories
length(unique(df$Region))           #100 unique categories 
length(unique(df$City))             #419 unique categories
#convert sit bounce rate into a numeric value - for calculating the average bounce rate purpose
df$`Site bounce rate`<-as.numeric(sub("%","",df$`Site bounce rate`))/100

## 2.1 column - Traffic category
#group by treaffic category
df_groupby_category = df %>% group_by(`Traffic category`) %>% summarise(count = n(),avg_bounce_rate= mean(`Site bounce rate`))
table(df$`Traffic category`)
# - plot.1 traffic by traffic category
ggplot(df_groupby_category,aes(x = reorder(`Traffic category`,-count),y=count,fill=`Traffic category`)) +geom_col()+ ggtitle('Traffic by Traffic Category') + xlab('Traffic Category')+ylab('Traffic') +geom_text(aes(label=round(count,0),hjust = 0.65,vjust = 0))+theme(panel.border = element_rect(color = "black", fill = NA, size = .5))
# - plot.2 average bounce rate by traffic category
ggplot(df_groupby_category,aes(x =reorder(`Traffic category`, -avg_bounce_rate),y=avg_bounce_rate,fill=`Traffic category`)) +geom_col()+ ggtitle('Avg.Bounce Rate by Traffic Category') + xlab('Traffic Category') +ylab('Avg.Bounce Rate') + geom_text(aes(label=round(avg_bounce_rate,2), hjust = 0.65,  vjust = 0))+theme(panel.border = element_rect(color = "black", fill = NA, size = .5))

## 2.11 Dummy coding for informational traffic categories
#create a new column to represent the traffic category 'Refferal'
df$`Traffic category_referral`=ifelse(df$`Traffic category` == 'Referral', 1, 0)
#create a new column to represent the traffic category 'Social'
df$`Traffic category_social`=ifelse(df$`Traffic category` == 'Social', 1, 0)
#create a new column to represent the traffic category 'Organic search'
df$`Traffic category_organic`=ifelse(df$`Traffic category` == 'Organic search', 1, 0)
#create a new column to represent the traffic category 'Organic search'
df$`Traffic category_direct`=ifelse(df$`Traffic category` == 'Direct', 1, 0)

## 2.2 column - Traffic Source
df$`Traffic source` = str_replace(df$`Traffic source`, "google.com", "Google")
df_groupby_source = df %>% group_by(`Traffic source`) %>% summarise(count = n(),avg_bounce_rate= mean(`Site bounce rate`))
# filter our some outliers
df_groupby_source = filter(df_groupby_source,count > 20 )
# - plot.3 Top traffic source
ggplot(df_groupby_source,aes(x =reorder(`Traffic source`,count),y=count)) +geom_col(fill='dark blue') + ggtitle('Top Traffic Source') + xlab('Traffic') +ylab('Traffic Source') + coord_flip() +theme(panel.border = element_rect(color = "black", fill = NA, size = .5))
#show the groupby dataframe in a descending order
df_groupby_source[order(-df_groupby_source$count),]
#create a new informational column to represent the traffic source - google
df$`Traffic source_Google`=ifelse(df$`Traffic source` == 'Google', 1, 0)

## 2.3 column - Entry page
df$`Entry page` <- replace(df$`Entry page`,df$`Entry page`=='/','Homepage')
#group by entry page
df_groupby_page= df %>% group_by(`Entry page`)%>% summarise(count = n(),avg_bounce_rate= mean(`Site bounce rate`))
#filter out some outliers
df_groupby_page = filter(df_groupby_page,count > 20 )
# - plot.4 traffic by entry page
ggplot(df_groupby_page,aes(x = reorder(`Entry page`, count),y=count,fill=`Entry page`)) +geom_col()+ ggtitle('Traffic by Entry Page') + xlab('Traffic') +ylab('Entry Page') +geom_text(aes(label=round(count,0), hjust = 0.65,  vjust = 0))+ coord_flip()+ theme(panel.border = element_rect(color = "black", fill = NA, size = .5))
# - plot.5 average bounce rate by page
ggplot(df_groupby_page,aes(x = reorder(`Entry page`, avg_bounce_rate),y=avg_bounce_rate,fill=`Entry page`)) +geom_col() + ggtitle('Avg.Bounce Rate by Entry Page') + xlab('Avg.Bounce Rate') +ylab('Entry Page')+geom_text(aes(label=round(avg_bounce_rate,2), hjust = 0.65,  vjust = 0))+ coord_flip()+ theme(panel.border = element_rect(color = "black", fill = NA, size = .5))
#dummy coding for homepage and volunteer
df$`Entry page_homepage`=ifelse(df$`Entry page` == 'Homepage', 1, 0)
df$`Entry page_volunteer`=ifelse(df$`Entry page` == '/volunteer', 1, 0)

## 2.4 column - Region
df$Region=ifelse(df$Region == 'CA', 1, 0)
ggplot(df, aes(x = `Region`)) +geom_bar(fill='dark blue') 

### 2.5 column - City
ggplot(df, aes(y = `City`)) +geom_bar()
df_city = df %>% group_by(`City`) %>% summarise(Number = n()) 
df_city = filter(df_city, Number > 100 ) 
df_city
# dummy coding - 1 represents city San Jose
df$City=ifelse(df$City == 'San Jose', 1, 0)

### 2.4 column - county
df_groupby_county = df %>% group_by(df$County)%>% summarise(count = n(),averagebounce = mean(`Site bounce rate`))
df_groupby_county= filter(df_groupby_county, count > 80 ) 
df_groupby_county
#dummy coding for county santa clara
df$County_Santa_Clara = ifelse(df$County == 'Santa Clara', 1, 0)

## 3. Logistc Regression Analysis
#create the target column
df$`Site bounce rate`=ifelse(df$`Site bounce rate` >= mean(df$`Site bounce rate`), 1, 0)
# logistic regression model
model1 = glm(`Site bounce rate`~ `Traffic category_social` +`Traffic category_direct` + 
               `Traffic category_referral` + `Entry page_homepage` + `Entry page_volunteer`
             + `Traffic source_Google`, data=df)
summary(model1)

## 3.1 Nfold validation technique
library(caret)
set.seed(2022)
trainfold = trainControl(method='cv',number=10,savePredictions = TRUE)
#describe model for training -
modelfold = train(`Site bounce rate`~ `Traffic category_social` +`Traffic category_direct` + 
       `Traffic category_referral` + `Entry page_homepage` + `Entry page_volunteer`
        + `Traffic source_Google`, data=df,method='glm', trControl=trainfold)
#print model summary and prediction
summary(modelfold)
#view final model
names(modelfold)
modelfold$finalModel
modelfold$pred #predictions of all 200 variables
#view predictions for each fold
modelfold$resample

