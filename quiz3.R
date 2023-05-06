data <- read.csv("/Users/sarvajeethuk/Downloads/Econometrics/A1/quiz3.csv")

#1
data$yearcode <- as.numeric(data$yearcode)
data$districtcode <- as.numeric(data$districtcode)
data$road_sum <-as.numeric(data$road_sum)
df <- data
library(dplyr)
df4 <- df %>%mutate(road_sum = districtcode * 10000 + yearcode)
df4 <- df %>%mutate(t = yearcode-2010)
library(ggplot2)

p <- ggplot(df4, aes(x = t, y = road_sum)) + geom_smooth(method="lm")
p + geom_point(position = position_jitter(width = .2), alpha = .3) + ggtitle("RL ID = β0 + β1 tID + uID") + labs(y = "ID",x = "t")

model <- lm(road_sum ~ t, data = df4)
names(model$coefficients) <- c("Beta0", "Beta1")
summary(model)$coefficients


#2
DSouth <- ifelse(data$state %in% c("Andhra Pradesh", "Tamil Nadu", "Kerala", "Telangana"), 1, 0)
DEast <- ifelse(data$state %in% c("Assam", "Manipur", "Tripura", "Sikkim", "West Bengal", "Odisha"), 1, 0)
DWest <- ifelse(data$state %in% c("Gujarat", "Maharashtra", "Karnataka"), 1, 0)
DNorth <- ifelse(data$state %in% c("Jammu and Kashmir", "Himachal Pradesh", "Delhi", "Punjab", "Haryana", "Rajasthan", "Uttarakhand"), 1, 0)
DCentre <- ifelse(data$state %in% c("Madhya Pradesh", "Uttar Pradesh", "Chhattisgarh", "Jharkhand"), 1, 0)



#3
p <- ggplot(df4, aes(x = DSouth, y = ID)) + geom_smooth(method="lm")
p + geom_point(position = position_jitter(width = .2), alpha = .3) + ggtitle("RLID = γ0 + γ1DSouth + εID") + labs(y = "ID",x = "DSouth")

model2 <- lm(road_sum ~ DSouth, data=df4)
names(model2$coefficients) <- c("gamma0", "gamma1")
summary(model2)$coefficients

#4

df5 <- df4%>% mutate(DSouth = case_when(state == 'Andhra Pradesh'~1,state == 'Tamil Nadu'~1,state == 'Kerala'~1,state == 'Telangana'~1,TRUE ~ 0))
df5 <- df4%>% mutate(DEast = case_when(state == 'Assam'~1,state == 'Manipur'~1,state == 'Tripura'~1,state == 'Sikkim'~1,state=='West Bengal'~1,state=='Odisha'~1,TRUE ~ 0))
df5 <- df4%>% mutate(DWest = case_when(state == 'Gujarat'~1,state == 'Maharashtra'~1,state == 'Karnataka'~1,TRUE ~ 0))
df5 <- df4%>% mutate(DNorth = case_when(state == 'Jammu and Kashmir'~1,state == 'Himachal Pradesh'~1,state == 'Delhi'~1,state == 'Punjab'~1,state=='Haryana'~1,state=='Rajasthan'~1,state=='Uttarakhand'~1,TRUE ~ 0))
df5 <- df4%>% mutate(DCentre = case_when(state == 'Madhya Pradesh'~1,state == 'Uttar Pradesh'~1,state == 'Chhattisgarh'~1,state == 'Jharkhand'~1 ,TRUE ~ 0))
model4 <- lm(road_sum ~ DSouth + DNorth + DEast + DWest, data = df)
names(model4$coefficients) <- c("eta0", "eta1", "eta2", "eta3", "eta4")

# Print the coefficients
summary(model4)$coefficients

#5
data$t <- data$year - 2010
model5 <- lm(road_sum ~ t + DSouth + DSouth * t, data = df4)
names(model5$coefficients) <- c("alph0", "alpha1","alpha2", "alpha3")
summary(model5)$coefficients
