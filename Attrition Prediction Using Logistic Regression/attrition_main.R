# Change path below to set working directory to where input files are present
#setwd("C:\\Users\\lenovo t420s\\Downloads\\PA-I_Case_Study_HR_Analytics")

# Importing data into r

library("lubridate")
library("dplyr")
library("tidyr")
library("ggplot2")
library("car")
library("MASS")
library("caret")
library(ROCR)


# Importing data into r

general_data         <- read.csv("general_data.csv",header=T)
in_time              <- read.csv("in_time.CSV",header=T,colClasses=c("ts",NA), stringsAsFactors = F)
manager_survey_data  <- read.csv("manager_survey_data.CSV",header=T, stringsAsFactors = F)
employee_survey_data <- read.csv("employee_survey_data.CSV",header=T,  stringsAsFactors = F)
out_time             <- read.csv("out_time.CSV",header=T ,colClasses=c("ts",NA), stringsAsFactors = F)

#Data Cleaning
#Employee survey data
# duplicate Ids
sum(duplicated(employee_survey_data$EmployeeID)) # No duplicate EIDs

unique(employee_survey_data$EnvironmentSatisfaction)# has NA
unique(employee_survey_data$JobSatisfaction)# has NA
unique(employee_survey_data$WorkLifeBalance)# has NA

#checking for NAs
length(which(is.na(employee_survey_data))) # No NAs here

#Employee survey has 83 rows with NA values. Removing them as the number is very low.
employee_survey_data <- na.omit(employee_survey_data)

summary(employee_survey_data$EnvironmentSatisfaction)
summary(employee_survey_data$JobSatisfaction)
summary(employee_survey_data$WorkLifeBalance)

#Manager Survey Data
sum(duplicated(manager_survey_data$EmployeeID)) #No duplicated entries

#JobInvolvement
unique(manager_survey_data$JobInvolvement)
summary(manager_survey_data$JobInvolvement)

#PerformanceRating
unique(manager_survey_data$PerformanceRating)
summary(manager_survey_data$PerformanceRating)

# there are no NA values



#InTime & outime
# there are a lot of na values but we need them because those values 
#signifies that the employee was on leave

#Converting intime and out time from wide to long data

#In_time
in.time.long <- gather(data = in_time,
                       key ="Date",
                       value = "In_Time",
                       na.rm = F,
                       X2015.01.01,X2015.01.02,X2015.01.05,X2015.01.06,X2015.01.07,X2015.01.08
                       ,X2015.01.09,X2015.01.12,X2015.01.13,X2015.01.14,X2015.01.15,X2015.01.16
                       ,X2015.01.19,X2015.01.20,X2015.01.21,X2015.01.22,X2015.01.23,X2015.01.26
                       ,X2015.01.27,X2015.01.28,X2015.01.29,X2015.01.30,X2015.02.02,X2015.02.03
                       ,X2015.02.04,X2015.02.05,X2015.02.06,X2015.02.09,X2015.02.10,X2015.02.11
                       ,X2015.02.12,X2015.02.13,X2015.02.16,X2015.02.17,X2015.02.18,X2015.02.19
                       ,X2015.02.20,X2015.02.23,X2015.02.24,X2015.02.25,X2015.02.26,X2015.02.27
                       ,X2015.03.02,X2015.03.03,X2015.03.04,X2015.03.05,X2015.03.06,X2015.03.09
                       ,X2015.03.10,X2015.03.11,X2015.03.12,X2015.03.13,X2015.03.16,X2015.03.17
                       ,X2015.03.18,X2015.03.19,X2015.03.20,X2015.03.23,X2015.03.24,X2015.03.25
                       ,X2015.03.26,X2015.03.27,X2015.03.30,X2015.03.31,X2015.04.01,X2015.04.02
                       ,X2015.04.03,X2015.04.06,X2015.04.07,X2015.04.08,X2015.04.09,X2015.04.10
                       ,X2015.04.13,X2015.04.14,X2015.04.15,X2015.04.16,X2015.04.17,X2015.04.20
                       ,X2015.04.21,X2015.04.22,X2015.04.23,X2015.04.24,X2015.04.27,X2015.04.28
                       ,X2015.04.29,X2015.04.30,X2015.05.01,X2015.05.04,X2015.05.05,X2015.05.06
                       ,X2015.05.07,X2015.05.08,X2015.05.11,X2015.05.12,X2015.05.13,X2015.05.14
                       ,X2015.05.15,X2015.05.18,X2015.05.19,X2015.05.20,X2015.05.21,X2015.05.22
                       ,X2015.05.25,X2015.05.26,X2015.05.27,X2015.05.28,X2015.05.29,X2015.06.01
                       ,X2015.06.02,X2015.06.03,X2015.06.04,X2015.06.05,X2015.06.08,X2015.06.09
                       ,X2015.06.10,X2015.06.11,X2015.06.12,X2015.06.15,X2015.06.16,X2015.06.17
                       ,X2015.06.18,X2015.06.19,X2015.06.22,X2015.06.23,X2015.06.24,X2015.06.25
                       ,X2015.06.26,X2015.06.29,X2015.06.30,X2015.07.01,X2015.07.02,X2015.07.03
                       ,X2015.07.06,X2015.07.07,X2015.07.08,X2015.07.09,X2015.07.10,X2015.07.13
                       ,X2015.07.14,X2015.07.15,X2015.07.16,X2015.07.17,X2015.07.20,X2015.07.21
                       ,X2015.07.22,X2015.07.23,X2015.07.24,X2015.07.27,X2015.07.28,X2015.07.29
                       ,X2015.07.30,X2015.07.31,X2015.08.03,X2015.08.04,X2015.08.05,X2015.08.06
                       ,X2015.08.07,X2015.08.10,X2015.08.11,X2015.08.12,X2015.08.13,X2015.08.14
                       ,X2015.08.17,X2015.08.18,X2015.08.19,X2015.08.20,X2015.08.21,X2015.08.24
                       ,X2015.08.25,X2015.08.26,X2015.08.27,X2015.08.28,X2015.08.31,X2015.09.01
                       ,X2015.09.02,X2015.09.03,X2015.09.04,X2015.09.07,X2015.09.08,X2015.09.09
                       ,X2015.09.10,X2015.09.11,X2015.09.14,X2015.09.15,X2015.09.16,X2015.09.17
                       ,X2015.09.18,X2015.09.21,X2015.09.22,X2015.09.23,X2015.09.24,X2015.09.25
                       ,X2015.09.28,X2015.09.29,X2015.09.30,X2015.10.01,X2015.10.02,X2015.10.05
                       ,X2015.10.06,X2015.10.07,X2015.10.08,X2015.10.09,X2015.10.12,X2015.10.13
                       ,X2015.10.14,X2015.10.15,X2015.10.16,X2015.10.19,X2015.10.20,X2015.10.21
                       ,X2015.10.22,X2015.10.23,X2015.10.26,X2015.10.27,X2015.10.28,X2015.10.29
                       ,X2015.10.30,X2015.11.02,X2015.11.03,X2015.11.04,X2015.11.05,X2015.11.06
                       ,X2015.11.09,X2015.11.10,X2015.11.11,X2015.11.12,X2015.11.13,X2015.11.16
                       ,X2015.11.17,X2015.11.18,X2015.11.19,X2015.11.20,X2015.11.23,X2015.11.24
                       ,X2015.11.25,X2015.11.26,X2015.11.27,X2015.11.30,X2015.12.01,X2015.12.02
                       ,X2015.12.03,X2015.12.04,X2015.12.07,X2015.12.08,X2015.12.09,X2015.12.10
                       ,X2015.12.11,X2015.12.14,X2015.12.15,X2015.12.16,X2015.12.17,X2015.12.18
                       ,X2015.12.21,X2015.12.22,X2015.12.23,X2015.12.24,X2015.12.25,X2015.12.28
                       ,X2015.12.29,X2015.12.30,X2015.12.31)

#Outtime
out_time.long <- gather(data = out_time,
                        key ="Date",
                        value = "In_Time",
                        na.rm = F,
                        X2015.01.01,X2015.01.02,X2015.01.05,X2015.01.06,X2015.01.07,X2015.01.08
                        ,X2015.01.09,X2015.01.12,X2015.01.13,X2015.01.14,X2015.01.15,X2015.01.16
                        ,X2015.01.19,X2015.01.20,X2015.01.21,X2015.01.22,X2015.01.23,X2015.01.26
                        ,X2015.01.27,X2015.01.28,X2015.01.29,X2015.01.30,X2015.02.02,X2015.02.03
                        ,X2015.02.04,X2015.02.05,X2015.02.06,X2015.02.09,X2015.02.10,X2015.02.11
                        ,X2015.02.12,X2015.02.13,X2015.02.16,X2015.02.17,X2015.02.18,X2015.02.19
                        ,X2015.02.20,X2015.02.23,X2015.02.24,X2015.02.25,X2015.02.26,X2015.02.27
                        ,X2015.03.02,X2015.03.03,X2015.03.04,X2015.03.05,X2015.03.06,X2015.03.09
                        ,X2015.03.10,X2015.03.11,X2015.03.12,X2015.03.13,X2015.03.16,X2015.03.17
                        ,X2015.03.18,X2015.03.19,X2015.03.20,X2015.03.23,X2015.03.24,X2015.03.25
                        ,X2015.03.26,X2015.03.27,X2015.03.30,X2015.03.31,X2015.04.01,X2015.04.02
                        ,X2015.04.03,X2015.04.06,X2015.04.07,X2015.04.08,X2015.04.09,X2015.04.10
                        ,X2015.04.13,X2015.04.14,X2015.04.15,X2015.04.16,X2015.04.17,X2015.04.20
                        ,X2015.04.21,X2015.04.22,X2015.04.23,X2015.04.24,X2015.04.27,X2015.04.28
                        ,X2015.04.29,X2015.04.30,X2015.05.01,X2015.05.04,X2015.05.05,X2015.05.06
                        ,X2015.05.07,X2015.05.08,X2015.05.11,X2015.05.12,X2015.05.13,X2015.05.14
                        ,X2015.05.15,X2015.05.18,X2015.05.19,X2015.05.20,X2015.05.21,X2015.05.22
                        ,X2015.05.25,X2015.05.26,X2015.05.27,X2015.05.28,X2015.05.29,X2015.06.01
                        ,X2015.06.02,X2015.06.03,X2015.06.04,X2015.06.05,X2015.06.08,X2015.06.09
                        ,X2015.06.10,X2015.06.11,X2015.06.12,X2015.06.15,X2015.06.16,X2015.06.17
                        ,X2015.06.18,X2015.06.19,X2015.06.22,X2015.06.23,X2015.06.24,X2015.06.25
                        ,X2015.06.26,X2015.06.29,X2015.06.30,X2015.07.01,X2015.07.02,X2015.07.03
                        ,X2015.07.06,X2015.07.07,X2015.07.08,X2015.07.09,X2015.07.10,X2015.07.13
                        ,X2015.07.14,X2015.07.15,X2015.07.16,X2015.07.17,X2015.07.20,X2015.07.21
                        ,X2015.07.22,X2015.07.23,X2015.07.24,X2015.07.27,X2015.07.28,X2015.07.29
                        ,X2015.07.30,X2015.07.31,X2015.08.03,X2015.08.04,X2015.08.05,X2015.08.06
                        ,X2015.08.07,X2015.08.10,X2015.08.11,X2015.08.12,X2015.08.13,X2015.08.14
                        ,X2015.08.17,X2015.08.18,X2015.08.19,X2015.08.20,X2015.08.21,X2015.08.24
                        ,X2015.08.25,X2015.08.26,X2015.08.27,X2015.08.28,X2015.08.31,X2015.09.01
                        ,X2015.09.02,X2015.09.03,X2015.09.04,X2015.09.07,X2015.09.08,X2015.09.09
                        ,X2015.09.10,X2015.09.11,X2015.09.14,X2015.09.15,X2015.09.16,X2015.09.17
                        ,X2015.09.18,X2015.09.21,X2015.09.22,X2015.09.23,X2015.09.24,X2015.09.25
                        ,X2015.09.28,X2015.09.29,X2015.09.30,X2015.10.01,X2015.10.02,X2015.10.05
                        ,X2015.10.06,X2015.10.07,X2015.10.08,X2015.10.09,X2015.10.12,X2015.10.13
                        ,X2015.10.14,X2015.10.15,X2015.10.16,X2015.10.19,X2015.10.20,X2015.10.21
                        ,X2015.10.22,X2015.10.23,X2015.10.26,X2015.10.27,X2015.10.28,X2015.10.29
                        ,X2015.10.30,X2015.11.02,X2015.11.03,X2015.11.04,X2015.11.05,X2015.11.06
                        ,X2015.11.09,X2015.11.10,X2015.11.11,X2015.11.12,X2015.11.13,X2015.11.16
                        ,X2015.11.17,X2015.11.18,X2015.11.19,X2015.11.20,X2015.11.23,X2015.11.24
                        ,X2015.11.25,X2015.11.26,X2015.11.27,X2015.11.30,X2015.12.01,X2015.12.02
                        ,X2015.12.03,X2015.12.04,X2015.12.07,X2015.12.08,X2015.12.09,X2015.12.10
                        ,X2015.12.11,X2015.12.14,X2015.12.15,X2015.12.16,X2015.12.17,X2015.12.18
                        ,X2015.12.21,X2015.12.22,X2015.12.23,X2015.12.24,X2015.12.25,X2015.12.28
                        ,X2015.12.29,X2015.12.30,X2015.12.31)

#Bind the two data set, no need to merge as both the data have the same order
dailyInOutTime <- cbind(in.time.long, Out_Time = out_time.long[,3])

#convertTimings to posixct
x <- as.POSIXct(dailyInOutTime$In_Time, "%Y-%m-%d %H:%M:%S", tz = "" )
dailyInOutTime$In_Time <- x

x <- as.POSIXct(dailyInOutTime$Out_Time, "%Y-%m-%d %H:%M:%S", tz = "" )
dailyInOutTime$Out_Time <- x

#Convert date to proper format
x <- gsub(pattern = "X", replacement = "", x = dailyInOutTime$Date) %>%
     as.Date(format = "%Y.%m.%d")
dailyInOutTime$Date <- x

#Convert Id into numeric
dailyInOutTime$X <- as.numeric(dailyInOutTime$X)

#find total hours spent in office for each date
xx <- as.numeric(dailyInOutTime$Out_Time - dailyInOutTime$In_Time)
#assign it to the df
dailyInOutTime$HoursInOffice <- xx
dailyInOutTime$HoursInOffice[which(is.na(dailyInOutTime$HoursInOffice))] <- 0

#Total hours spent in office
TotalHoursSpent <- dailyInOutTime %>% 
  group_by(X) %>% 
  summarise(TotalHoursWorked = sum(HoursInOffice))



#Public holidays: We need to differentiate between public holiday and personal leaves
#Each date where none of the employees has clocked any hours can be considered as vacation date
#total employees = 4410
all.dates <- unique(dailyInOutTime$Date)
isPublicHoliday <- numeric(length(all.dates))
for(i in 1:length(all.dates)){
  if(length(which(dailyInOutTime$Date == all.dates[i] & dailyInOutTime$HoursInOffice == 0)) == 4410){
    isPublicHoliday[i] <- 1
  }else{
    isPublicHoliday[i] <- 0
  }
}
#add a new column publicholiday to demarcate if holiday or not
PublicHoliday <- 0
dailyInOutTime1 <- cbind(dailyInOutTime, PublicHoliday)
#assign the isPublicHoliday value to each date
for(i in 1:length(all.dates)){
  dailyInOutTime1$PublicHoliday[dailyInOutTime1$Date == all.dates[i]] <- isPublicHoliday[i]
}



#number of vacation taken total(removing public hol)
vacations.Total <- dailyInOutTime1[dailyInOutTime1$HoursInOffice==0 & dailyInOutTime1$PublicHoliday==0,] %>%
  group_by(X) %>%
  summarise(Vacations = length(HoursInOffice))

#Average working hours(Excluding all vacations)
avg.Working.Hours <- dailyInOutTime1[dailyInOutTime1$HoursInOffice !=0,] %>%
  group_by(X) %>%
  summarise(AvgWorkingHours = mean(HoursInOffice))

###General data data frame - Exploratory data analysis

#age
summary(general_data$Age)

age.hist <-  ggplot(general_data, aes(x = Age)) + 
  geom_histogram(binwidth = 2,  col = "black")
age.hist  #Most employees are aged between 30 and 40

#Attrition
summary(general_data$Attrition) #711 out of 4410 employees attrited

#business travel
summary(general_data$BusinessTravel) #Most employees (3129/4410 = 70.9%) travel rarely

#Distance from home
summary(general_data$DistanceFromHome)

dist.hist <- ggplot(general_data, aes(x = DistanceFromHome)) + 
  geom_histogram(binwidth = 1, col = "black")
dist.hist #A lot of employees stay close to the office, however there are employees who stay quite far away

#Department
summary(general_data$Department) #Most of the employees work in Research & Development (2883/4410 = 65.3%)

#Education
summary(factor(general_data$Education))

edu.hist <- ggplot(general_data, aes(x = Education)) +
  geom_histogram(binwidth = 1, col = "black")
edu.hist

#Gender
summary(general_data$Gender) #Majority of employees are males (2646/4410 = 60%)

#Gender and attrition
gender.hist.at <- general_data %>% 
  group_by(Gender, Attrition) %>% 
  summarise(Count = length(EmployeeID))%>% 
  ggplot( aes(x = Gender, y= Count)) + 
  geom_col(col = "black", aes(fill = Attrition), position = "fill")
gender.hist.at #Almost similar levels of attrition across male and female populations


#Education field
summary(general_data$Education)

#JobLevel
summary(factor(general_data$JobLevel))

#marital status
summary(general_data$MaritalStatus) #Most of the employees are married (2019/4410 = 45.78%) versus single or divorced

#monthly income
summary(general_data$MonthlyIncome)

boxplot(general_data$MonthlyIncome)#seems to have outlier values

monthlyInc.hist <- ggplot(general_data, aes(x = MonthlyIncome)) +
  geom_histogram(binwidth = 10000, col = "black")
monthlyInc.hist 
# there are very high salary employees but their numbers are quite high; outlier treatment is not required

#company worked for
summary(factor(general_data$NumCompaniesWorked))
#19 NAs present, removing them
general_data <- general_data[which(!is.na(general_data$NumCompaniesWorked)),]

#Over 18
summary(general_data$Over18) #all are above 18

#Salary hike %
summary(general_data$PercentSalaryHike)

percentSalHike.hist <- ggplot(general_data, aes(x = PercentSalaryHike)) + 
  geom_histogram(binwidth = 1, col = "black") 

#Stock Option Level
summary(factor(general_data$StockOptionLevel))

#total working years
summary(general_data$TotalWorkingYears)
#9 NAs present removing them
general_data <- general_data[which(!is.na(general_data$TotalWorkingYears)),]

totwrkyear.hist <- ggplot(general_data, aes(x = TotalWorkingYears)) + 
  geom_histogram(binwidth = 2, col = "black")

#Training times last year
summary(general_data$TrainingTimesLastYear)

#histogram
train.hist <- ggplot(general_data, aes(x = TrainingTimesLastYear)) + 
  geom_histogram(binwidth = 1, col = "black")
train.hist #Most employees received 2 to 3 times in the year

#Years in the company
summary(general_data$YearsAtCompany)
#histogram
atCompany.hist <- ggplot(general_data, aes(x = YearsAtCompany)) + 
  geom_histogram(binwidth = 2, col = "black")
atCompany.hist

#Years since last promotion
summary(general_data$YearsSinceLastPromotion)
#histogram
yearsSinceLastPromotion.hist <- ggplot(general_data, aes(x = YearsSinceLastPromotion)) + 
  geom_histogram(binwidth = 1, col = "black")
yearsSinceLastPromotion.hist #There are a few employees who have not been promoted for 4+ years

#YearsWithCurrManager
summary(general_data$YearsWithCurrManager)
#histogram
yearsWithCurrManager.hist <- ggplot(general_data, aes(x = YearsWithCurrManager)) + 
  geom_histogram(binwidth = 1, col = "black")

yearsWithCurrManager.hist

#Bivariateanalysis with Attrition

age.hist.at <- ggplot(general_data, aes(x = Age)) + 
  geom_histogram(binwidth = 2, aes(fill = Attrition), col = "black" , position = "fill")
age.hist.at #  more attrition among younger age

#distance and attrition analysis
dist.hist.at <- ggplot(general_data, aes(x = DistanceFromHome)) + 
  geom_histogram(binwidth = 1, col = "black", aes(fill = Attrition), position = "fill") # No visible trend
dist.hist.at

#Department and attrition
dept.hist.at <- general_data %>% 
  group_by(Department, Attrition) %>% 
  summarise(Count = length(EmployeeID))%>% 
  ggplot( aes(x = Department, y= Count)) + 
  geom_col(col = "black", aes(fill = Attrition), position = "fill")

dept.hist.at # Higher rate from HR dept.

#Education and attrition
edu.hist.at <- ggplot(general_data, aes(x = Education)) +
  geom_histogram(binwidth = 1, col = "black", aes(fill = Attrition), position = "fill")
edu.hist.at


#EducationField and Attrition

summary(general_data$EducationField)
eduFld.hist.at <- general_data %>% 
  group_by(EducationField, Attrition) %>% 
  summarise(Count = length(EmployeeID))%>% 
  ggplot( aes(x = EducationField, y= Count)) + 
  geom_col(col = "black", aes(fill = Attrition), position = "fill")

eduFld.hist.at # Again human resources seeing higher attrition rate, though their number is quite small

#Joblevel and attrition
jobLvl.hist.at <- ggplot(general_data, aes(x = JobLevel))+
  geom_histogram(binwidth = 1, col = "black", aes(fill = Attrition), position = "fill")
jobLvl.hist.at


#marital status and attrition
marSt.hist.at <- general_data %>% 
  group_by(MaritalStatus, Attrition) %>% 
  summarise(Count = length(EmployeeID))%>% 
  ggplot( aes(x = MaritalStatus, y= Count)) + 
  geom_col(col = "black", aes(fill = Attrition), position = "fill")
marSt.hist.at


#monthly income and attrition
monhlyInc.hist.attr <- ggplot(general_data, aes(x = MonthlyIncome)) +
  geom_histogram(binwidth = 10000, col = "black", position = "fill", aes(fill = Attrition))
monhlyInc.hist.attr


#NumCompaniesWorked and attrition
numCompWorkedfor.hist.at <- general_data %>% 
  group_by(NumCompaniesWorked, Attrition) %>% 
  summarise(Count = length(EmployeeID))%>% 
  ggplot( aes(x = NumCompaniesWorked, y= Count)) + 
  geom_col(col = "black", aes(fill = Attrition), position = "fill")
numCompWorkedfor.hist.at


  #Salary Hike % vs attrition
percentSalHike.hist.at <- ggplot(general_data[general_data$Attrition=="Yes",], aes(x = PercentSalaryHike, y = MonthlyIncome)) + 
  geom_point( aes(col = factor(Attrition)), position = position_jitter(width = 0.2,height = 0.2))
percentSalHike.hist.at

#Stock Opt. and attrition
stkopt.hist.at <- general_data %>% 
  group_by(StockOptionLevel, Attrition) %>% 
  summarise(Count = length(EmployeeID))%>% 
  ggplot( aes(x = StockOptionLevel, y= Count)) + 
  geom_col(col = "black", aes(fill = Attrition), position = "fill")

stkopt.hist.at # Attrition similar across stock option levels

#Total year worked and attrition
totwrkyear.hist.att <- ggplot(general_data, aes(x = TotalWorkingYears)) + 
  geom_histogram(binwidth = 2, col = "black", position = "fill", aes(fill = Attrition)) # Decreasing attrition rate with experience
totwrkyear.hist.att #Highest attrition in 40 year bracket - could be due to retirement.

#Training times last year and attrition
train.hist.att <- ggplot(general_data, aes(x = TrainingTimesLastYear)) + 
  geom_histogram(binwidth = 1, col = "black", position = "fill", aes(fill = Attrition)) 
train.hist.att

#Years at company and attrition
atCompany.hist.att <- ggplot(general_data, aes(x = YearsAtCompany)) + 
  geom_histogram(binwidth = 2, col = "black", position = "fill", aes(fill = Attrition)) # Decreasing attrition rate more years at company
atCompany.hist.att #Highest attrition in 40 year bracket - could be due to retirement.

#Year since last promotion and attrition
yearsSinceLastPromotion.hist.att <- ggplot(general_data, aes(x = YearsSinceLastPromotion)) + 
  geom_histogram(binwidth = 1, col = "black", position = "fill", aes(fill = Attrition))
yearsSinceLastPromotion.hist.att

#YearsWithCurrManager and attrition
yearsWithCurrManager.hist.att <- ggplot(general_data, aes(x = YearsWithCurrManager)) + 
  geom_histogram(binwidth = 1, col = "black", position = "fill", aes(fill = Attrition))
yearsWithCurrManager.hist.att #People tend to leave their managers in the first year if they don't along
                              #After the first year, there is a drop in attrition.

travellFrequency.hist.att <- general_data %>% group_by(BusinessTravel, Attrition) %>% summarise(ProportionOfEmployees = length(EmployeeID)) %>%
  ggplot(aes(x = BusinessTravel, y = ProportionOfEmployees)) + 
  geom_col(aes(fill = Attrition), position = "fill", col = "black")
travellFrequency.hist.att #People with either very high or rare travel tend to attrite more.


## Dummy variable conversion
#######################################################################
levels(general_data$Attrition) <- c(0,1) #711 attrited out of 4410
general_data$Attrition         <- as.numeric(levels(general_data$Attrition))[general_data$Attrition]
levels(general_data$Gender)    <- c(0,1) #1764 female and 2646 male
general_data$Gender            <- as.numeric(levels(general_data$Gender))[general_data$Gender]

dummy_BT   <- data.frame(model.matrix(~BusinessTravel, data = general_data))
dummy_Dept <- data.frame(model.matrix(~Department,     data = general_data))
dummy_Edu  <- data.frame(model.matrix(~EducationField, data = general_data))
dummy_JR   <- data.frame(model.matrix(~JobRole,        data = general_data))
dummy_MS   <- data.frame(model.matrix(~MaritalStatus,  data = general_data))

# Combine the dummy variables with the numeric columns of general_data dataset.
general_data_1   <- cbind(general_data[,c(1:2,5:6,8:11,14:15,17:24)], dummy_BT[,-1],dummy_Dept[,-1],dummy_Edu[,-1],dummy_JR[,-1],dummy_MS[,-1])

# Merge files
glmInput_1       <- merge(general_data_1,TotalHoursSpent,  by.x="EmployeeID", by.y="X")
glmInput_2       <- merge(glmInput_1,avg.Working.Hours,    by.x="EmployeeID", by.y="X")
glmInput_3       <- merge(glmInput_2,vacations.Total,      by.x="EmployeeID", by.y="X")
glmInput_4       <- merge(glmInput_3,manager_survey_data,  by.x="EmployeeID",  by.y="EmployeeID")
glmInput_5       <- merge(glmInput_4,employee_survey_data, by.x="EmployeeID",  by.y="EmployeeID")

# Normalising continuous features 
glmInput_5$MonthlyIncome           <- scale(glmInput_5$MonthlyIncome)
glmInput_5$Age                     <- scale(glmInput_5$Age)
glmInput_5$DistanceFromHome        <- scale(glmInput_5$DistanceFromHome)
glmInput_5$NumCompaniesWorked      <- scale(glmInput_5$NumCompaniesWorked)
glmInput_5$PercentSalaryHike       <- scale(glmInput_5$PercentSalaryHike)
glmInput_5$TotalWorkingYears       <- scale(glmInput_5$TotalWorkingYears)
glmInput_5$TrainingTimesLastYear   <- scale(glmInput_5$TrainingTimesLastYear)
glmInput_5$YearsSinceLastPromotion <- scale(glmInput_5$YearsSinceLastPromotion)
glmInput_5$YearsWithCurrManager    <- scale(glmInput_5$YearsWithCurrManager)
glmInput_5$YearsAtCompany          <- scale(glmInput_5$YearsAtCompany)
glmInput_5$TotalHoursWorked        <- scale(glmInput_5$TotalHoursWorked)
glmInput_5$AvgWorkingHours         <- scale(glmInput_5$AvgWorkingHours)
glmInput_5$Vacations               <- scale(glmInput_5$Vacations)
glmInput_5$JobInvolvement          <- scale(glmInput_5$JobInvolvement)
glmInput_5$PerformanceRating       <- scale(glmInput_5$PerformanceRating)
glmInput_5$EnvironmentSatisfaction <- scale(glmInput_5$EnvironmentSatisfaction)
glmInput_5$JobSatisfaction         <- scale(glmInput_5$JobSatisfaction)
glmInput_5$WorkLifeBalance         <- scale(glmInput_5$WorkLifeBalance)

colnames(glmInput_5) <- c("EmployeeID", "Age","Attrition", "DistanceFromHome", "Education","EmployeeCount", "Gender", "JobLevel", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike","StandardHours", "StockOptionLevel", "TotalWorkingYears","TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion","YearsWithCurrManager","BusinessTravel.TravelFrequently","BusinessTravel.TravelRarely","Department.ResearchAndDevelopment","Department.Sales","EducationField.LifeSciences","EducationField.Marketing", "EducationField.Medical","EducationField.Other","EducationField.TechnicalDegree","JobRole.HumanResources", "JobRole.LaboratoryTechnician", "JobRole.Manager","JobRoleManufacturing.Director","JobRoleResearch.Director", "JobRoleResearch.Scientist", "JobRoleSales.Executive", "JobRoleSales.Representative","MaritalStatus.Married","MaritalStatus.Single","TotalHoursWorked", "AvgWorkingHours", "Vacations","Mgr.Feedback.JobInvolvement", "Mgr.Feedback.PerformanceRating", "Emp.Feedback.EnvironmentSatisfaction","Emp.Feedback.JobSatisfaction","Emp.Feedback.WorkLifeBalance")



# create the training and test datasets
set.seed(100)
indices <- sample(1:nrow(glmInput_5), 0.7*nrow(glmInput_5))

train  <-  glmInput_5[indices,]
test   <-  glmInput_5[-indices,]

# create the first model 
model1 <- glm(Attrition ~ .,data=train[,-1], family = "binomial")
summary(model1)      #AIC is 2130 
#Highly significant variables are: Age, Number of companies worked, distance from home, numbe of companies worked, total working years, training times last year,
#years since last promotion, years with current manager, frequent business travel, marital status single, job satisfaction
#satisfaction with environment and worklife balance

# apply the stepwise selection to shortlist significant drivers of attrition
set.seed(100)
model_2           <- stepAIC(model1, direction="both") #Build model 2 based on variables selected through AIC
summary(model_2)     #AIC is 2103.23
#Highly significant variables are: Age, Number of companies worked, total working years, training times last year,
#years since last promotion, years with current manager, frequent business travel, marital status single, job satisfaction
#satisfaction with environment, department sales, average working hours, worklife balance, department research & development,
#department sales

# create model 2 based in AIC 
model_2 <- glm(formula = Attrition ~ Age + Education + JobLevel + NumCompaniesWorked + 
                 PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.TravelFrequently + 
                 BusinessTravel.TravelRarely + Department.ResearchAndDevelopment + 
                 Department.Sales + EducationField.Other + EducationField.TechnicalDegree + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 JobRoleSales.Executive + JobRoleSales.Representative + MaritalStatus.Single + 
                 AvgWorkingHours + Emp.Feedback.EnvironmentSatisfaction + 
                 Emp.Feedback.JobSatisfaction + Emp.Feedback.WorkLifeBalance, 
               family = "binomial", data = train)

summary(model_2) #AIC = 2103.2

#checking for multicollinearity through VIF check, no variable has VIF > 5 which is ok
sort(vif(model_2),decreasing=TRUE)

# removing variables with low significance (p > 0.05). These are education, education field technical degree, 
# job role sales executive, job role sales representative

model_3 <- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                 PercentSalaryHike + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.TravelFrequently + 
                 BusinessTravel.TravelRarely + Department.ResearchAndDevelopment + 
                 Department.Sales + EducationField.Other + 
                 JobRoleManufacturing.Director + JobRoleResearch.Director + 
                 MaritalStatus.Single + 
                 AvgWorkingHours + Emp.Feedback.EnvironmentSatisfaction + 
                 Emp.Feedback.JobSatisfaction + Emp.Feedback.WorkLifeBalance, 
               family = "binomial", data = train)

summary(model_3) # AIC is 2106
# Job level is now not significant (p > 0.05)
sort(vif(model_3),decreasing=TRUE)     # All VIF values are less than 5

# removing Job Level due to low significance  

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear +  YearsSinceLastPromotion + 
                 YearsWithCurrManager + BusinessTravel.TravelFrequently + 
                 BusinessTravel.TravelRarely + Department.ResearchAndDevelopment + 
                 Department.Sales + 
                 JobRoleManufacturing.Director + 
                 MaritalStatus.Single + AvgWorkingHours + Emp.Feedback.EnvironmentSatisfaction + 
                 Emp.Feedback.JobSatisfaction + Emp.Feedback.WorkLifeBalance, 
               family = "binomial", data = train)

summary(model_4) # AIC is 2110.6
# Percent salary hike is the only variable with P > 0.05 and will be removed.

sort(vif(model_4), decreasing=TRUE) #VIF values are less than 5 so it's OK

# removing percent salary hike as it has low significance
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear +  YearsSinceLastPromotion + 
                 YearsWithCurrManager + BusinessTravel.TravelFrequently + 
                 BusinessTravel.TravelRarely + Department.ResearchAndDevelopment + 
                 Department.Sales + 
                 JobRoleManufacturing.Director + 
                 MaritalStatus.Single + AvgWorkingHours + Emp.Feedback.EnvironmentSatisfaction + 
                 Emp.Feedback.JobSatisfaction + Emp.Feedback.WorkLifeBalance, 
               family = "binomial", data = train)

summary(model_5) #AIC is 2112.4, all variables with high significance (p < 0.05)

sort(vif(model_5), decreasing=TRUE) #VIF values are less than 5 so it's OK

#Let's choose between models 5 and 4

test_pred_5 = predict(model_5, type = "response", 
                      newdata = test[,-1])
test$prob_5 <- test_pred_5

test_pred_4 = predict(model_4, type = "response", 
                      newdata = test[,-1])
test$prob_4 <- test_pred_4

head(test)

# Let's use the probability cutoff of 20% for model 5
test_pred_churn_5   <- factor(ifelse(test_pred_5 >= 0.20, "Yes", "No"))
test_actual_churn_5 <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_churn_5,test_pred_churn_5) #Accuracy = 78.37%, Sensitivity = 70.45%, Specificity = 80.00%

# Let's use the probability cutoff of 20% for model 4
test_pred_churn_4   <- factor(ifelse(test_pred_4 >= 0.20, "Yes", "No"))
test_actual_churn_4 <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_churn_4,test_pred_churn_4) #Accuracy = 77.91%, Sensitivity = 68.64%, Specificity = 79.81%

# Clearly model 5 performs better than model 4 on sensitvity and overall accuracy.

# Confusion matrix for model 5
test_conf <- confusionMatrix(test_pred_churn_5, test_actual_churn_5, positive = "Yes")
test_conf

# Confusion matrix for model 4
test_conf <- confusionMatrix(test_pred_churn_4, test_actual_churn_4, positive = "Yes")
test_conf

#########################################################################################
# Let's find out the optimal probability cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_churn, positive = "Yes")
  acc  <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out  <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probabilities for models 5 and 4 respectively
# Comment the test_pred and actual churn variable assignments based on which model you are running the test for

#test_pred          <-  test_pred_5           # Model 5
#test_actual_churn  <-  test_actual_churn_5   # Model 5

test_pred          <-  test_pred_4           # Model 4
test_actual_churn  <-  test_actual_churn_4   # Model 4

summary(test_pred)

s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


cutoff <- s[which.min(abs(OUT[,1]-OUT[,2]))]
cutoff # value is 0.1775758

# Let's plot the cutoff

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


# Let's choose the cutoff value of 0.1775758
test_cutoff_churn <- factor(ifelse(test_pred >=0.1775758, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_churn, test_actual_churn, positive = "Yes")

acc  <- conf_final$overall[1]  # Accuracy    is 75.19% for model 5 and 75.74% for model 4
sens <- conf_final$byClass[1]  # Sensitivity is 73.64% for model 5 and 75.45% for model 4
spec <- conf_final$byClass[2]  # Specificity is 75.51% for model 5 and 75.79% for model 4

head(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_churn <- ifelse(test_cutoff_churn=="Yes",1,0)
test_actual_churn <- ifelse(test_actual_churn=="Yes",1,0)

#install package ("ROCR") if not done so already
#on testing  data
pred_object_test<- prediction(test_cutoff_churn, test_actual_churn)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  # Value is 0.4915038 for model 5 and 0.5124894 for model 4

####################################################################
# Lift & Gain Chart 

# plotting the lift chart


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_churn, test_pred, groups = 10)
Churn_decile # Upto 81.36% of variables by the 4th decile for model 5 and 80% of variables by 4th decile for model 4

## Conclusion #################################################################################################
# Both models 4 and 5 are very close in terms of performance. As XYZ is most interested in finding out
# the probability of attrition and preventing it, we choose model 4 as it has a marginally better sensitivity 
# of 75.45% versus model 5 which has sensitivity of 73.64%.
