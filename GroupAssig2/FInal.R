Fertiliser <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/fertiliser data.csv")
IMR <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/IMR.csv")
Landless <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/Landless.csv")
moev <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Margin of election victory.csv")
pop_density <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/Population_Density.csv")
Turnout <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Turn_Out.csv")
rainfall <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/rainfall.csv")
sexy <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Mer.csv")
ndap <- read.csv("/Users/sarvajeethuk/Downloads/13_DataAssignment_1/NDAP_REPORT_7065.csv")
Telecom <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/Telecom.csv")
gini <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Gini_New.csv")


View(Fertiliser)
View(IMR)
View(moev)
View(Turnout)
View(Telecom)
View(sexy)
View(rainfall)
View(gini)


head(sexy7)

Telecom$State <- sub("Uttar Pradesh-(E&W)", "UTTAR PRADESH",Telecom$State)
library(tidyr)
colnames(Telecom)[1:18] <- c("State", "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",'2015',"2016")
Telecom <- gather(Telecom, Year, Value, -State)
Telecom$State <- toupper(Telecom$State)


Fertiliser$State <- toupper(Fertiliser$State)
Fertiliser$District <- toupper(Fertiliser$District)
Fertiliser$State <- toupper(Fertiliser$State)
Fertiliser$District <- toupper(Fertiliser$District)
Fertiliser <- Fertiliser[, -c(6:18)]
Fertiliser <- Fertiliser[, -7]
Fertiliser <- Fertiliser[, -1]
Fertiliser <- Fertiliser[, -2]


IMR <- IMR[, -1]
IMR <- IMR[, -c(2:30)]
colnames(IMR)[1:19] <- c("State", "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2015","2016",'2017',"2018","2019")
IMR <- gather(IMR, Year, Value, -State)
IMR$State <- toupper(IMR$State)
names(IMR)[3] <- "IMR"




moev<- moev[, -1]
names(moev)[1] <- "District"
moev$State <- gsub("Uttar Pradesh \\[2000 Onwards\\]", "UTTAR PRADESH",moev$State)
moev$State <- gsub("Andhra Pradesh \\[2014 Onwards\\]", "ANDHRA PRADESH",moev$State)
moev$State <- gsub("Madhya Pradesh \\[2000 Onwards\\]", "MADHYA PRADESH",moev$State)
moev$State <- gsub("Bihar \\[2000 Onwards\\]", "BIHAR",moev$State)
moev$State <- gsub("Delhi \\[1977 Onwards\\]", "DELHI",moev$State)
moev$State <- toupper(moev$State)
moev$District <- toupper(moev$District)
moev<- moev[, -c(3:5)]
names(moev)[3] <- "2018"
library(dplyr)
moev <- moev %>%mutate(across(starts_with("20"), ~ gsub("%", "", .)))
names(moev)[3] <- "Margin_final"



Turnout<- Turnout[, -1]
colnames(Turnout)[1:5] <- c("District","State","2019","2014","2009")
Turnout<- Turnout %>%mutate(across(starts_with("20"), ~ gsub("%", "", .)))
Turnout$State <- gsub("Uttar Pradesh \\[2000 Onwards\\]", "UTTAR PRADESH",Turnout$State)
Turnout$State <- gsub("Andhra Pradesh \\[2014 Onwards\\]", "ANDHRA PRADESH",Turnout$State)
Turnout$State <- gsub("Madhya Pradesh \\[2000 Onwards\\]", "MADHYA PRADESH",Turnout$State)
Turnout$State <- gsub("Bihar \\[2000 Onwards\\]", "BIHAR",Turnout$State)
Turnout$State <- gsub("Delhi \\[1977 Onwards\\]", "DELHI",Turnout$State)
Turnout$State <- toupper(Turnout$State)
Turnout$District <- toupper(Turnout$District)
Turnout <- Turnout[, -c(3:5)]
names(Turnout)[3] <- "Turnout_Average"





gini$State <- toupper(gini$State)
gini$State <- sub("\\(INCLUDING TELANGANA\\)", "", gini$State)
colnames(gini)[1:5] <- c("State", "2005","2014","2019","Average")
gini <- gini[, -c(2:4)]
names(gini)[2] <- "Gini.Index_new"

rainfall <- rainfall[, -c(6:17)]
rainfall<- rainfall[, -1]
rainfall<- rainfall[, -2]
colnames(rainfall)[2:3] <- c("State","District")
rainfall$State <- toupper(rainfall$State)
rainfall$District <- toupper(rainfall$District)

sexy$District<- toupper(sexy$District)
names(sexy)[3] <- "Year"
names(sexy)[36] <- "Yearinfo"


sexy <- merge(sexy, Fertiliser, by = c("District", "State","Year"))
sexy2 <- merge(sexy, IMR, by = c("State","Year"))
sexy3 <- merge(sexy2, rainfall, by = c("State","Year","District"))
sexy4 <- merge(sexy3,Turnout,by = c("State","District"))
sexy5 <- merge(sexy4,moev,by = c("State","District"))
sexy6 <- merge(sexy5,gini,by = c("State"))
sexy7 <- merge(sexy6,Telecom,by = c("State","Year"))
names(sexy7)[names(sexy7) == "Value"] <- "Telephones per sq Km"
View(sexy7)

#sexy7$SDP_sq <- sexy7$SDP^2
#sexy7$SDP_cu <- sexy7$SDP^3

model <- lm(formula = sexy7$Amount.of.Electrical.Conductivity ~ sexy7$SDP + sexy7$SDP_sq + sexy7$SDP_cu + sexy7$Gini.Index_new + sexy7$Turnout_Average + sexy7$IMR + sexy7$Margin_final+ sexy7$ANNUAL.RAINFALL..Millimeters.+sexy7$TOTAL.PER.HA.OF.NCA..Kg.per.ha.+sexy7$`Telephones per sq Km`,data=sexy7)

summary(model)
stargazer(model)

error <- summary(model)$coefficients[,"std. error"]
print(error)
