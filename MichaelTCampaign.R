###Briana Lynch
##Mikey's Campaign

library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
library(reshape2)#melt
library(tibble)
library(tidyr) #spread
library(devtools)
library(dplyr)# gather
###Other Libraries used in the past
library(forcats)
library(rlang)
###Make Dataframes ###########
setwd("C:/Users/lynchba2/desktop/Mikey")

###Sheet ####################################################
IL<- read.csv("Illinois_District_01.csv")
IL$Numbers <- as.numeric(IL$District.01.Estimate) 

summary(IL)

## People ##############################################################
SexAge <- IL %>% filter(Subject=="Sex and Age")

### Population by Age
Age <- SexAge[-c(1:3,17),]
###Percentage
Age <- Age %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	711039.0*100)
Age$Percentage <- round(Age$Percentage)

##Mean Median Mode
MeanAge <- mean(Age$District.01.Estimate)
Age$Mean <- rep(40,15)
MedianAge <- median(Age$District.01.Estimate)
Age$MedianAge <- rep(38.8,15)

ggplot(Age, aes(District.01.Estimate))+
  geom_histogram(bins = 5, color="black", fill="#56B4E9") +
  scale_x_continuous(trans = "log10")

## Population by Sex
Sex <- SexAge[c(2:3),]

###Percentage
Sex <- Sex %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	711039.0*100)
Sex$Percentage <- round(Sex$Percentage,digits=2)

#### RACE
Race <- IL %>% filter(Subject=="Race")
### Population by Race
Race2 <- Race[-c(1),]
###Percentage
Race2 <- Race2 %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	711039.0*100)
Race2$Percentage <- round(Race2$Percentage, digits=2)

### Hispanics 
HispanicLatinoRace <- IL %>% filter(Subject=="Hispanic or Latino and Race")
### Population by Hispanics
Hispanics <- HispanicLatinoRace[-c(1),]
###Percentage
Hispanics<- Hispanics %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	711039.0*100)
Hispanics$Percentage <- round(Hispanics$Percentage, digits=2)

##Place of Birth
PlaceofBirth <- IL %>% filter(Subject=="Place of Birth")
### Population by Birth
PlaceofBirth  <- PlaceofBirth [-c(1),]
###Percentage
PlaceofBirth <- PlaceofBirth  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	711039.0*100)
PlaceofBirth $Percentage <- round(PlaceofBirth$Percentage, digits=2)

##Ancestry
Ancestry <- IL %>% filter(Subject=="Ancestry")
### Population by Ancestry
Ancestry   <- Ancestry[-c(1),]
###Percentage
Ancestry  <- Ancestry   %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	711039.0*100)
Ancestry$Percentage <- round(Ancestry$Percentage, digits=2)

##Vet
Vet <- IL %>% filter(Subject=="Veteran Status")
###Percentage
Vet  <- Vet   %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	711039.0*100)
Vet$Percentage <- round(Vet$Percentage, digits=2)

##Disability
Disability <- IL %>% filter(Subject=="Disability Status of the Civilian Noninstitutionalized Population")
Disability  <- Disability   %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	706225.0*100)
Disability$Percentage <- round(Disability$Percentage, digits=2)



### Workers #####################

##Commuting
Commuting <- IL %>% filter(Subject=="Commuting to Work")
Commuting2  <- Commuting[-c(1,8),]
###Percentage
Commuting2 <- Commuting2 %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	324739.0*100)
Commuting2$Percentage <- round(Commuting2$Percentage,digits = 2)
###
Commuting2$Mean_TravelTime <- rep(35.9,6)

##Ocupation
Occupation <- IL %>% filter(Subject=="Occupation")
Occupation2 <- Occupation[-c(1),]
Occupation <- Occupation %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/		330798*100)
Occupation2$Percentage <- round(Occupation2$Percentage,digits = 2)

##Industry
Industry <- IL %>% filter(Subject=="Industry")
Industry2 <- Industry[-c(1),]
Industry2  <- Industry2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/		330798*100)
Industry2$Percentage <- round(Industry2$Percentage,digits = 2)
sum(Industry2$Percentage)

Class <- IL %>% filter(Subject=="Class of Worker")
Class2 <- Class[-c(1),]
Class2  <- Class2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/		330798*100)
Class2$Percentage <- round(Class2$Percentage,digits = 2)
sum(Class2$Percentage)



## Housing #########################
#Ho
HousingOccupancy <- IL %>% filter(Subject=="Housing Occupancy")
HousingOccupancy2 <- HousingOccupancy[-c(1),]
HousingOccupancy2  <- HousingOccupancy2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/		307269*100)
HousingOccupancy2$Percentage <- round(HousingOccupancy2$Percentage,digits = 2)

##HT
HousingTenure <- IL %>% filter(Subject=="Housing Tenure")
HousingTenure2 <- HousingTenure [-c(1),]
HousingTenure2  <- HousingTenure2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/			271729*100)
HousingTenure2$Percentage <- round(HousingTenure2$Percentage,digits = 2)

##HHY
Householdyear<- IL %>% filter(Subject=="Year Householder Moved into Unit")
Householdyear2 <- Householdyear[-c(1),]
Householdyear2  <- Householdyear2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/			271729*100)
Householdyear2$Percentage <- round(Householdyear2$Percentage,digits = 2)

###MS
MortgageStatus <- IL %>% filter(Subject=="Mortgage Status")
MortgageStatus2 <- MortgageStatus[-c(1),]
MortgageStatus2  <- MortgageStatus2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/				164240*100)
MortgageStatus2$Percentage <- round(MortgageStatus2$Percentage,digits = 2)

#MOC
MonthlyOwnerCosts <- IL %>% filter(Subject=="Selected Monthly Owner Costs(SMOC)")

MonthlyOwnerCosts2 <- MonthlyOwnerCosts[-c(1,9:17),]
MonthlyOwnerCosts2  <- MonthlyOwnerCosts2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/		106566*100)
MonthlyOwnerCosts2$Percentage <- round(MonthlyOwnerCosts2$Percentage,digits = 2)
MonthlyOwnerCosts2$Median <- rep(1673,7)

MonthlyOwnerCosts3 <- MonthlyOwnerCosts[c(11:16),]
MonthlyOwnerCosts3  <- MonthlyOwnerCosts3  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/		57674*100)
MonthlyOwnerCosts3$Percentage <- round(MonthlyOwnerCosts3$Percentage,digits = 2)
MonthlyOwnerCosts3$Median <- rep(684,6)

M4 <- rbind(MonthlyOwnerCosts2,MonthlyOwnerCosts3)

## Gross Rent
GrossRent <- IL %>% filter(Subject=="Gross Rent")
GrossRent2 <- GrossRent [-c(1,9),]
GrossRent2  <- GrossRent2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	102835*100)
GrossRent2$Percentage <- round(GrossRent2$Percentage,digits = 2)
GrossRent2$Median <- rep(970,8)


###Socioeconomic ######################
IncomeBenefits<- IL %>% filter(Subject=="Income and Benefits (In 2018 inflation-adjusted dollars)")
IncomeBenefits2 <- IncomeBenefits  [-c(1,12,13),]
IncomeBenefits2  <- IncomeBenefits2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/		271729.0*100)
IncomeBenefits2$Percentage <- round(IncomeBenefits2$Percentage,digits = 2)


##Health Insurance
HealthInsurance<- IL %>% filter(Subject=="Health Insurance Coverage")
HealthInsurance2 <- HealthInsurance[-c(1,6:7),]
HealthInsurance2  <- HealthInsurance2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/		706225.0*100)
HealthInsurance2$Percentage <- round(HealthInsurance2$Percentage,digits = 2)

HealthI2 <- HealthInsurance[c(7),]
HealthI2  <- HealthI2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/			164681.0*100)
HealthI2$Percentage <- round(HealthI2$Percentage,digits = 2)

HealthInsurance2 <- rbind(HealthInsurance2,HealthI2)

### Poverty Data
PovertyLevel <- IL %>% filter(Subject=="Percentage of Families and People Whose Income in the Past 12 Months is Below the Poverty Level")
PovertyLevel2  <- PovertyLevel  %>% select(Subject,Title,District.01.Estimate)
colnames(PovertyLevel2)[3] <- "Percentage"


##Education ##########################
SchoolEnrollment<- IL %>% filter(Subject=="School Enrollment")
SchoolEnrollment2 <- SchoolEnrollment[-c(1),]
SchoolEnrollment2  <- SchoolEnrollment2  %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/			175127	*100)
SchoolEnrollment2$Percentage <- round(SchoolEnrollment2$Percentage,digits = 2)

#Education
EducationalAttainment<- IL %>% filter(Subject=="Educational Attainment")
EducationalAttainment2 <- EducationalAttainment[-c(1),]
EducationalAttainment2  <- EducationalAttainment2 %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/				487651.0	*100)
EducationalAttainment2$Percentage <- round(EducationalAttainment2$Percentage,digits = 2)

##Business ##################################
PaidEmployees<- IL %>% filter(Subject=="Paid employees for pay period including March 12")
PaidEmployees2 <- PaidEmployees[-c(1),]
PaidEmployees2  <- PaidEmployees2 %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/	190634	*100)
PaidEmployees2$Percentage <- round(PaidEmployees2$Percentage,digits = 2)
sum(PaidEmployees2$Percentage)

####
Annualpayroll <- IL %>% filter(Subject=="Annual payroll ($1,000)")

Annualpayroll2<- Annualpayroll[-c(1),]
Annualpayroll2  <- Annualpayroll2 %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/		8461772	*100)
Annualpayroll2$Percentage <- round(Annualpayroll2$Percentage,digits = 2)
sum(Annualpayroll2$Percentage)

##Total Establishment
TotalEstablishments<- IL %>% filter(Subject=="Total Establishments")
TotalEstablishments2<- TotalEstablishments[-c(1),]
TotalEstablishments2  <- TotalEstablishments2 %>% select(Subject,Title,District.01.Estimate) %>% 
  mutate(Percentage=District.01.Estimate/		11941	*100)
TotalEstablishments2$Percentage <- round(TotalEstablishments2$Percentage,digits = 2)
sum(TotalEstablishments2$Percentage)



library(writexl) 

People <- write_xlsx(list(Age=Age,Sex=Sex,Race=Race2,Hispancs=Hispanics,PlaceofBirth=PlaceofBirth,
                          Ancestry=Ancestry, Disability=Disability),
                           "C:/Users/lynchba2/desktop/People.xlsx")

Workers <- write_xlsx(list(Commuting=Commuting2,Ocupation=Occupation2,Industry=Industry2,
                           Class=Class2),
                      "C:/Users/lynchba2/desktop/Workers.xlsx") 

Housing <- write_xlsx(list(HousingOccupancy=HousingOccupancy2,HousingTenure=HousingTenure2,
                           Householdyear=Householdyear2,MortgageStatus=MortgageStatus2,
                           MonthlyOwnerCosts=M4,GrossRent=GrossRent2),
                      "C:/Users/lynchba2/desktop/Housing.xlsx") 

SocioEconomic <- write_xlsx(list(IncomeBenefits=IncomeBenefits2,HealthInsurance=HealthInsurance2,
                                 PovertyLevel=PovertyLevel2,SchoolEnrollment=SchoolEnrollment2,
                                 EducationalAttainment=EducationalAttainment2,PaidEmployees=PaidEmployees2,
                                 Annualpayroll=Annualpayroll2,TotalEstablishments=TotalEstablishments2),
                      "C:/Users/lynchba2/desktop/SocioEconomic.xlsx") 





#Excel
#import xlxs with multiple worksheets
library(readxl)
library(tidyverse)

setwd("C:/Users/lynchba2/Desktop/Mikey")
ward_list<- excel_sheets("dataexport2.xlsx")

x <- as.list(ward_list)

data <- lapply(ward_list, read_excel, path = "dataexport2.xlsx")
names(data) <- c(x)


Ward_4 <- data[[2]]
summary(Ward_4)
Ward_4_Above <- Ward_4 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_4_Below <- Ward_4 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))

Ward_5 <- data[[3]]
summary(Ward_5)
Ward_5_Above <- Ward_5 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_5_Below<- Ward_5 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))

Ward_13 <- data[[4]]
summary(Ward_13)
Ward_13_Above <- Ward_13 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_13_Below <- Ward_13 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))

Ward_19 <- data[[5]]
summary(Ward_19)
Ward_19_Above <- Ward_19 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_19_Below <- Ward_19 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))

Ward_34 <- data[[6]]
summary(Ward_34)
Ward_34_Above <- Ward_34 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_34_Below <- Ward_34 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))

Ward_21 <- data[[7]]
summary(Ward_21)
Ward_21_Above <- Ward_21 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_21_Below <- Ward_21 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))

Ward_20 <- data[[8]]
summary(Ward_20)
Ward_20_Above <- Ward_20 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_20_Below <- Ward_20 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))


Ward_18 <- data[[9]]
summary(Ward_18)
Ward_18_Above <- Ward_18 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_18_Below <- Ward_18 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))

Ward_17 <- data[[10]]
summary(Ward_17)
Ward_17_Above <- Ward_17 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_17_Below <- Ward_17 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))


Ward_16 <- data[[11]]
summary(Ward_16)
Ward_16_Above <- Ward_16 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_16_Below <- Ward_16 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))


Ward_3 <- data[[12]]
summary(Ward_3)
Ward_3_Above <- Ward_3 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_3_Below <- Ward_3 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))


Ward_6 <- data[[13]]
summary(Ward_6)
Ward_6_Above <- Ward_6 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_6_Below <- Ward_6 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))

Ward_7 <- data[[14]]
summary(Ward_7)
Ward_7_Above <- Ward_7 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_7_Below <- Ward_7 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))

Ward_8 <- data[[15]]
summary(Ward_8)
Ward_8_Above <- Ward_8 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_8_Below <- Ward_8 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))

Ward_9 <- data[[16]]
summary(Ward_9)
Ward_9_Above <- Ward_9 %>% filter(Percentage_Rush>=mean(Percentage_Rush, na.rm = TRUE))
Ward_9_Below <- Ward_9 %>% filter(Percentage_Rush<=mean(Percentage_Rush, na.rm = TRUE))




Above <- rbind(Ward_4_Above,Ward_5_Above, Ward_13_Above,Ward_19_Above,Ward_34_Above,Ward_21_Above,
                     Ward_20_Above,Ward_18_Above,Ward_17_Above,Ward_16_Above,Ward_3_Above,Ward_6_Above, 
                        Ward_7_Above,Ward_8_Above,Ward_9_Above) 

Below <- rbind(Ward_4_Below,Ward_5_Below, Ward_13_Below,Ward_19_Below,Ward_34_Below,Ward_21_Below,
               Ward_20_Below,Ward_18_Below,Ward_17_Below,Ward_16_Below,Ward_3_Below,Ward_6_Below, 
               Ward_7_Below,Ward_8_Below,Ward_9_Below)


Ward_Data <- rbind(Ward_4,Ward_5, Ward_13,Ward_19,Ward_34,Ward_21,
                   Ward_20,Ward_18,Ward_17,Ward_16,Ward_3,Ward_6, 
                   Ward_7,Ward_8,Ward_9)


library(writexl) 

AverageData <- write_xlsx(list(Below=Below,Above=Above),"C:/Users/lynchba2/desktop/Mikey/MeanData.xlsx")
Ward_Data_ALL <- write_xlsx(list(Ward_Data=Ward_Data),"C:/Users/lynchba2/desktop/Mikey/Ward_Data.xlsx")

Ward_Data<- read_excel("Ward_Data.xlsx")

WholeDF <- Ward_Data[,-c(1:2,4,6,8,10)]

WardData2 <- WholeDF %>% melt(key = Name,value = Number) 

WardData2 %>% 
  ggplot(aes(Ward,value, fill=variable))+
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_brewer(palette = "Dark2")+
  scale_y_continuous(limits = c(0,400),breaks = c(0,50,100,150,200,250,300,350,400)) +
  theme_ipsum(grid="xY",axis_title_family = "Arial Narrow",
              axis_title_size = 16, axis_title_face = "bold",
              plot_title_size = 20, plot_title_face = "bold" ) +
  labs(x=NULL, y="Total Number of Votes",
       title="Ward Data",
       subtitle="Created by Briana Lynch 3.17.21")+
  theme(axis.text.x = element_text(face="bold",size=14, color="Black",
                                   margin = unit(c(5, 5, 5,5), "mm")),
        axis.text.y = element_text(face="bold", size=14,color="Black",
                                   margin = unit(c(5, 5, 5,5), "mm")),
        legend.text = element_text(face = "bold"),legend.title = element_blank(),
        axis.title.y = element_text(hjust = 0.5))

## Percentage
Ward_Data<- read_excel("Ward_Data.xlsx")
Ward_Data <- Ward_Data[,-c(1,4,6,8,10)]

Ward_4 <- Ward_Data %>% filter(Ward=="Ward 4") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_4 <- Ward_4 %>% distinct(Ward,.keep_all = TRUE)
Ward_4 <- Ward_4[,-c(1:5)]

Ward_5 <- Ward_Data %>% filter(Ward=="Ward 5") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_5 <- Ward_5 %>% distinct(Ward,.keep_all = TRUE)
Ward_5 <- Ward_5[,-c(1:5)]

Ward_13 <- Ward_Data %>% filter(Ward=="Ward 13") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_13 <- Ward_13 %>% distinct(Ward,.keep_all = TRUE)
Ward_13 <- Ward_13[,-c(1:5)]

Ward_19 <- Ward_Data %>% filter(Ward=="Ward 19") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_19 <- Ward_19 %>% distinct(Ward,.keep_all = TRUE)
Ward_19 <- Ward_19[,-c(1:5)]

Ward_34 <- Ward_Data %>% filter(Ward=="Ward 34") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_34 <- Ward_34 %>% distinct(Ward,.keep_all = TRUE)
Ward_34 <- Ward_34[,-c(1:5)]

Ward_21 <- Ward_Data %>% filter(Ward=="Ward 21") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_21 <- Ward_21 %>% distinct(Ward,.keep_all = TRUE)
Ward_21 <- Ward_21[,-c(1:5)]

Ward_20 <- Ward_Data %>% filter(Ward=="Ward 20") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_20 <- Ward_20 %>% distinct(Ward,.keep_all = TRUE)
Ward_20 <- Ward_20[,-c(1:5)]

Ward_18 <- Ward_Data %>% filter(Ward=="Ward 18") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_18 <- Ward_18 %>% distinct(Ward,.keep_all = TRUE)
Ward_18 <- Ward_18[,-c(1:5)]

Ward_17 <- Ward_Data %>% filter(Ward=="Ward 17") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_17 <- Ward_17 %>% distinct(Ward,.keep_all = TRUE)
Ward_17 <- Ward_17[,-c(1:5)]

Ward_16 <- Ward_Data %>% filter(Ward=="Ward 16") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_16 <- Ward_16 %>% distinct(Ward,.keep_all = TRUE)
Ward_16 <- Ward_16[,-c(1:5)]

Ward_3 <- Ward_Data %>% filter(Ward=="Ward 3") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_3 <- Ward_3 %>% distinct(Ward,.keep_all = TRUE)
Ward_3 <- Ward_3[,-c(1:5)]

Ward_6 <- Ward_Data %>% filter(Ward=="Ward 6") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_6 <- Ward_6 %>% distinct(Ward,.keep_all = TRUE)
Ward_6 <- Ward_6[,-c(1:5)]

Ward_7 <- Ward_Data %>% filter(Ward=="Ward 7") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_7 <- Ward_7 %>% distinct(Ward,.keep_all = TRUE)
Ward_7 <- Ward_7[,-c(1:5)]

Ward_8 <- Ward_Data %>% filter(Ward=="Ward 8") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_8 <- Ward_8 %>% distinct(Ward,.keep_all = TRUE)
Ward_8 <- Ward_8[,-c(1:5)]

Ward_9 <- Ward_Data %>% filter(Ward=="Ward 9") %>% 
  mutate(Total=sum(Votes)) %>% mutate(Rush_Percentage=sum(Rush/Total)*100) %>% 
  mutate(Matthews_Percentage=sum(Matthews/Total)*100) %>% mutate(Gad_Percentage=sum(Gad/Total)*100) %>% 
  mutate(Emmons_Percentage=sum(Emmons/Total)*100)
Ward_9 <- Ward_9 %>% distinct(Ward,.keep_all = TRUE)
Ward_9 <- Ward_9[,-c(1:5)]

P_Ward_Data <- rbind(Ward_4,Ward_5, Ward_13,Ward_19,Ward_34,Ward_21,
                   Ward_20,Ward_18,Ward_17,Ward_16,Ward_3,Ward_6, 
                   Ward_7,Ward_8,Ward_9)


library(writexl) 

write_xlsx(P_Ward_Data,"C:/Users/lynchba2/desktop/Mikey/Total.xlsx")

P_Ward_Data <- read_excel("C:/Users/lynchba2/desktop/Mikey/Total.xlsx")
P_Ward_Data <- P_Ward_Data[,-2]
WardData3 <- P_Ward_Data %>% melt(key = Name,value = Number) 

WardData3  %>% 
  ggplot(aes(Ward,value, fill=variable))+
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_brewer(palette = "Dark2")+
  scale_y_continuous(limits = c(0,90), breaks = c(0,15,30,45,60,75,90))+
  theme_ipsum(grid="xY",axis_title_family = "Arial Narrow",
              axis_title_size = 16, axis_title_face = "bold",
              plot_title_size = 20, plot_title_face = "bold" ) +
  labs(x=NULL, y="Percentage",
       title="Ward Data",
       subtitle="Created by Briana Lynch 3.17.21")+
  theme(axis.text.x = element_text(face="bold",size=14, color="Black",
                                   margin = unit(c(5, 5, 5,5), "mm")),
        axis.text.y = element_text(face="bold", size=14,color="Black",
                                   margin = unit(c(5, 5, 5,5), "mm")),
        legend.text = element_text(face = "bold"),legend.title = element_blank(),
        axis.title.y = element_text(hjust = 0.5))


