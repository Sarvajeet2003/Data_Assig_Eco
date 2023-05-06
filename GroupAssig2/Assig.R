# Gini Index
gini <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Gini_New.csv")
View(gini)
gini <- gini[,-c(2:4)]
names(gini)[1] <- "State"
gini$State <- toupper(gini$State)
gini$State <- gsub("ANDHRA PRADESH\\(INCLUDING TELANGANA\\)", "ANDHRA PRADESH",gini$State)
View(gini)

# Infant Mortality
im <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Infant_Mortality.csv")
View(im)
names(im)[2] <- "State"
im$State <- toupper(im$State)
im$State <- gsub("ANDHRA PRADESH\\(INCLUDING TELANGANA\\)", "ANDHRA PRADESH",im$State)
View(im)

# Internet_P
net <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Intenet_P.csv")
View(net)
names(net)[1] <- "State"
net$State <- toupper(net$State)
View(net)

# RainFall
rain <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/rainfall.csv")
View(rain)
names(rain)[4] <- "State"
rain$State <- toupper(rain$State)
View(rain)
names(rain)[5] <- "District"
rain$District <- toupper(rain$District)
View(rain)

# VOTER TurnOut
voter <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/Turn_Out.csv")
names(voter)[2] <- "District"
voter$State <- gsub("Uttar Pradesh \\[2000 Onwards\\]", "UTTAR PRADESH",voter$State)
voter$State <- gsub("Andhra Pradesh \\[2014 Onwards\\]", "ANDHRA PRADESH",voter$State)
voter$State <- gsub("Madhya Pradesh \\[2000 Onwards\\]", "MADHYA PRADESH",voter$State)
voter$State <- gsub("Bihar \\[2000 Onwards\\]", "BIHAR",voter$State)
voter$State <- gsub("Delhi \\[1977 Onwards\\]", "DELHI",voter$State)
voter$State <- toupper(voter$State)
View(voter)




# Water_Source
water <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/Water_Source.csv")
View(water)
names(water)[4] <- "State"
water$State <- toupper(water$State)
View(water)
names(water)[5] <- "District"
water$District <- toupper(water$District)
View(water)


# Fertilizer
fertilizer <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/fertiliser data.csv")
View(fertilizer)
names(fertilizer)[4] <- "State"
fertilizer$State <- toupper(fertilizer$State)
View(fertilizer)
names(fertilizer)[5] <- "District"
fertilizer$District <- toupper(fertilizer$District)
View(fertilizer)

# Irrigation
irrigation <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/irrigation.csv")
View(irrigation)
names(irrigation)[4] <- "State"
irrigation$State <- toupper(irrigation$State)
View(irrigation)
names(irrigation)[5] <- "District"
irrigation$District <- toupper(irrigation$District)
View(irrigation)

# Wage
wage <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig2/New data/waged.csv")
View(wage)
names(wage)[4] <- "State"
wage$State <- toupper(wage$State)
View(wage)
names(wage)[5] <- "District"
wage$District <- toupper(wage$District)
View(wage)



# State Wise Tele Comm
df <- read.csv("/Users/sarvajeethuk/Downloads/Table_31.7-State_wise-Post_and_Tele_communication_1.csv")
df[is.na(df)] <- 0
View(df)