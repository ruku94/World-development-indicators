#install.packages("dplyr")
#install.packages("mice")
library(mice)
library(dplyr)
data<-read.csv("C:/Users/Arthi/Desktop/Course Material/Bus R/Project/world-development-indicators/indicators.csv")
head(data)
countryCode<- data%>%select(CountryCode)%>%distinct
data_USA<- filter(data,CountryCode=="USA")
max(data_USA$Year)
min(data_USA$Y)
getwd()
setwd("C:/Users/Arthi/Desktop/Course Material/Bus R/Project")
?write.csv
data_USA<-read.csv("dataUSA.csv")
write.csv(data_USA,"dataUSA.csv")
data_USA$IndicatorName<-as.character(data_USA$IndicatorName)
list<-select(data,IndicatorName,IndicatorCode)%>%distinct
list<-data_USA%>%select(Year,IndicatorName)%>%group_by(Year)%>%summarize(count=n())
data_1<-filter(data,Year%in% (1986:1995))
data_ind<-select(data,IndicatorName)%>%distinct
write.csv(data_ind,"data_ind.csv")
#take pop total 
data_1<-filter(data,data$Year%in% (1986:1995))
data_2<-filter(data,IndicatorName %in% c(
                                           "Air transport, passengers carried",
                                           "Arable land (% of land area)",
                                           "CO2 emissions (kt)",
                                           "Combustible renewables and waste (% of total energy)",
                                           "Electric power consumption (kWh per capita)",
                                           "Electricity production from coal sources (% of total)",
                                           "Electricity production from hydroelectric sources (% of total)",
                                           "Electricity production from natural gas sources (% of total)",
                                           "Electricity production from nuclear sources (% of total)",
                                           "Electricity production from oil sources (% of total)",
                                           "Employment in industry (% of total employment)",
                                           "Forest area (% of land area)",
                                           "GDP per capita (current US$)",
                                           "Renewable electricity output (% of total electricity output)",
                                          "Fossil fuel energy consumption (% of total)", 
                                         "Land area (sq. km)",
                                         "Population, total",
                                         "Total reserves (includes gold, current US$)",
                                         "Electricity production from renewable sources, excluding hydroelectric (% of total)"))                                
library(reshape)
write.csv()                                        
tdata<-cast(data_2,CountryName+Year~ IndicatorName, mean)  
tdata<-cast(data_2,CountryName~ IndicatorName, mean) 
colSums(is.na(tdata))
count<-read.csv("countries_null.csv")
tdata<-tdata%>%anti_join(count)
decade3$CountryName<-NULL
decade3<-tdata
decade3<-na.omit(tdata)
#plm
install.packages("plm")
library(plm)
mice_output$CountryName<-as.factor(mice_output)
data_plm<-read.csv("data.csv")
fixed <- plm(data_plm$CO2.emissions..kt.~ data_plm$Electricity.production.from.coal.sources....of.total.+data_plm$Electricity.production.from.hydroelectric.sources....of.total.+data_plm$Combustible.renewables.and.waste....of.total.energy.+data_plm$Electricity.production.from.nuclear.sources....of.total.+data_plm$Electricity.production.from.oil.sources....of.total., data=data_plm, index=c("CountryName", "Year"), model="within")
summary(fixed)


const<-fixef(fixed)
fit3<-lm(decade3$CO2.emissions..kt.~., decade3)
summary(fit3)
#mice imputataion
# Set a random seed
set.seed(129)


# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(decade3, method='cart') 
mice_output <- complete(mice_mod)

boxplot(mice_output)
attach(mice_output)
fit3mice<-lm(`CO2 emissions (kt)`~., mice_output)
plot(fit3mice)
write.csv(mice_output,"data")
summary(fit3mice)

null<-lm(`CO2 emissions (kt)`~1,data=mice_output)
full<-lm(`CO2 emissions (kt)`~.,data=mice_output)
selectedmodel <- step(null, scope=list(lower=null, upper=full), direction="both")
summary(selectedmodel)
plot(selectedmodel)
hist(mice_output$`CO2 emissions (kt)`)
library(moments)
skewness(`CO2 emissions (kt)`)


findMaxLambda <- function(lm.mod, search.range=seq(-1,2,by=0.001)){
  #################################################################
  ##  Find the "best" Box-Cox transformaiton parameter           ##
  ##  lm.mod is a regression model                               ##
  #################################################################
  require(MASS)
  loglike <- boxcox(lm.mod, search.range)               # Plot the log-likelihood function
  lambda.max <- loglike$x[ which.max(loglike$y) ]       # Find lambda at max log-likelihood
  title(bquote("Estimated "*lambda[max]==.(round(lambda.max,4))))
  return(lambda.max)
} 

M<-cor(mice_output)
M
M<-data.frame(M)
install.packages("corrplot")

library(corrplot)
corrplot(M,method="circle")
mice_output$CountryName<-NULL

#CO2 contribution:

data_emissions5<-filter(data_5,IndicatorName %in% c("CO2 emissions from gaseous fuel consumption (% of total)",
                                           "CO2 emissions from liquid fuel consumption (% of total)",
                                           "CO2 emissions from solid fuel consumption (% of total)",
                                           "CO2 emissions from electricity and heat production, total (% of total fuel combustion)",
                                           "CO2 emissions from manufacturing industries and construction (% of total fuel combustion)",
                                           "CO2 emissions from other sectors, excluding residential buildings and commercial and public services (% of total fuel combustion)",
                                           "CO2 emissions from residential buildings and commercial and public services (% of total fuel combustion)",
                                           "CO2 emissions from transport (% of total fuel combustion)"))
data_1<-filter(data,Year%in% (1966:1975))                                           
data_2<-filter(data,Year%in% (1976:1985))                                   
data_3<-filter(data,Year%in% (1986:1995))
data_4<-filter(data,Year%in% (1996:2005))
data_5<-filter(data,Year%in% (2005:2015))

write.csv(data_emissions,"emission.csv")
write.csv(data_emissions1,"emission1.csv")
write.csv(data_emissions2,"emission2.csv")
write.csv(data_emissions3,"emission3.csv")
write.csv(data_emissions4,"emission4.csv")
write.csv(data_emissions5,"emission5.csv")

--------developing vs developing

decade5 <- read.csv("D:/R files/Decade5_Data.csv")
count <-read.csv("D:/R files/Countries.csv")
decade5new <- decade5%>%anti_join(count)
write.csv(decade5new, "decade5new.csv")
getwd()
mice_mod_dec5 <- mice(decade5new, method='cart') 
mice_dec5 <- complete(mice_mod_dec5)
write.csv(mice_dec5, "decade5mice.csv")
getwd()

developed <- read.csv("D:/R files/Developed.csv")
developing <- read.csv("D:/R files/Developing.csv")
lastdecade <- read.csv("D:/R files/06-15.csv")

output6 <- merge(developed, lastdecade, by="TableName")
output7 <- merge(developing, lastdecade, by="TableName")

output6$TableName <- NULL
output7$TableName <- NULL

null6 <- lm(output6$CO2.emissions..kt. ~ 1, data= output6)
full6 <- lm(output6$CO2.emissions..kt. ~ ., data= output6)
step_regress6 <- step(null6, scope=list(lower=null6, upper=full6), direction="forward")
summary(step_regress6)

null7 <- lm(output7$CO2.emissions..kt. ~ 1, data= output7)
full7 <- lm(output7$CO2.emissions..kt. ~ ., data= output7)
step_regress7 <- step(null7, scope=list(lower=null7, upper=full7), direction="forward")
summary(step_regress7)
