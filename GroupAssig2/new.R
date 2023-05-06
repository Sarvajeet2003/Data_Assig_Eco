Fertiliser <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/fertiliser data.csv")
IMR <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/IMR.csv")
Landless <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/Landless.csv")
mo <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Margin of election victory.csv")
pop_density <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/Population_Density.csv")
Turnout <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Turn_Out.csv")

sexy <- read.csv("/Users/sarvajeethuk/Downloads/13_DataAssignment_1/13_MergedData.csv")
ndap <- read.csv("/Users/sarvajeethuk/Downloads/13_DataAssignment_1/NDAP_REPORT_7065.csv")

View(Fertiliser)
View(IMR)
View(Landless)
View(mo)
View(pop_density)
View(Turnout)
View(momo)
View(sexy)

Fertiliser$State <- toupper(Fertiliser$State)
Fertiliser$District <- toupper(Fertiliser$District)
Fertiliser$State <- toupper(Fertiliser$State)
Fertiliser$District <- toupper(Fertiliser$District)

IMR <- IMR[, -1]
IMR <- IMR[, -c(2:30)]
colnames(IMR)[1:19] <- c("State", "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2015","2016",'2017',"2018","2019")
IMR <- gather(IMR, Year, Value, -State)
IMR$State <- toupper(IMR$State)

Landless <- Landless[, -c(2:3)]




pop_density <- pop_density[, -1]
pop_density <- pop_density[, -5]
names(pop_density)[1] <- "State"
pop_density$State <- toupper(pop_density$State)


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





gini <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Gini_New.csv")
gini$State <- toupper(gini$State)
gini$State <- sub("\\(INCLUDING TELANGANA\\)", "", gini$State)
colnames(gini)[1:5] <- c("State", "2005","2014","2019","Average")
View(gini)


View(sexy)
View(pop_density)
pop_new <- merge(sexy,pop_density,by = c("State","State"))
View(pop_new)




momo <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/Telecom.csv")
library(tidyr)
colnames(momo)[1:18] <- c("State", "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014",'2015',"2016")
momo <- gather(momo, Year, Value, -State)
momo$State <- toupper(momo$State)
momo$State <- gsub("NORTH EAST-II", "NAGALAND", momo$State)
momo$State <- gsub("NORTH EAST-I", "MIZORAM", momo$State)
momo$State <- gsub("NORTH EAST-I", "MIZORAM", momo$State)
momo$state <- gsub("(E&W)", "UTTAR PRADESH", momo$State)
momo$state <- gsub("UTTAR PRADESH-(E&W)", "UTTAR PRADESH", momo$State)
View(momo)
momo_new <- merge(sexy, momo, by = "State")
View(momo_new)




mo <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Margin of election victory.csv")
mo<- mo[, -1]
names(mo)[1] <- "District"
mo$State <- gsub("Uttar Pradesh \\[2000 Onwards\\]", "UTTAR PRADESH",mo$State)
mo$State <- gsub("Andhra Pradesh \\[2014 Onwards\\]", "ANDHRA PRADESH",mo$State)
mo$State <- gsub("Madhya Pradesh \\[2000 Onwards\\]", "MADHYA PRADESH",mo$State)
mo$State <- gsub("Bihar \\[2000 Onwards\\]", "BIHAR",mo$State)
mo$State <- gsub("Delhi \\[1977 Onwards\\]", "DELHI",mo$State)
mo$State <- toupper(mo$State)
mo$District <- toupper(mo$District)
View(mo)
mo$Margin.Final <- gsub("%", "", mo$Margin.Final)
mo_new <- merge(sexy,mo,by ="State")
mo_new <- subset(mo_new, select = -Margin.....2019)
mo_new <- subset(mo_new, select = -Margin.....2014)
mo_new <- subset(mo_new, select = -Margin.....2009)
View(mo_new)



