# QUESTION-1

# We picked Ground Water Quality as our Environmental Quality measure & we are loading the NDAP data into 'df' dataframe # nolint # nolint: line_length_linter.
df <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig/EcoG13/NDAP_REPORT_7065.csv")

View(df)

# QUESTION-2

# Fetching data from different columns of df dataset
df$District.LGD.Code
df$YearCode

# Transforming this original dataset into  district-year level data-set that includes district-year ID for each row in the sample.
df$District_Year_ID<-with(df,(df$District.LGD.Code)*10000+df$YearCode)

#Removing all rows with NA values with respect to the variables in the data-set
remove_me_na <- c(11:32)
df <- df[rowSums(is.na(df[, remove_me_na])) != length(remove_me_na), ]

#Serializing for uniqueness again
library(dplyr)
df <- df %>% mutate(KeyValue = row_number())
df <- df %>% select(KeyValue, everything())

# Upper case values to State
df$State <- toupper(df$State)
colnames(df)[34] <- "YEAR"


# QUESTION-3
#Loading the SDP Table in pf and modifying it in accordance with 'df' data-set
pf <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig/EcoG13/HSB.csv")
#pf <- pf[-c(18,25,31,39),]
names(pf) <- gsub("\\.\\.\\.", " AND ", names(pf)) 
names(pf) <- gsub("\\.", " ", names(pf))

View(pf)
#Cleaning the data-set and modifying it --> YEAR format,NA values inclusion
pf <- pf[-c(1:4),]
pf$YEAR <- substr(pf$YEAR, 1, 4)
pf[pf == ""] <- NA
pf[pf == "-"] <- NA

View(pf)



#Changing the format of 'pf' to Year->State->SDP
library(tidyr)
pf_f <- gather(pf, key = "State", value = "value", -YEAR)
View(pf_f)

#Merging SDP with NDAP report on the basis of year and state.
merged_data <- merge(df,pf_f, by = c("YEAR","State"))
View(merged_data)

# QUESTION 4😍🥵🫡

# Loading the GINI Index merged csv file from the pdf File with a lot of efforts.
cf <- read.csv("/Users/sarvajeethuk/Downloads/Sem 4/Econometrics/GroupAssig/EcoG13/Gini.csv")
View(cf)

# Renaming the columns 
colnames(cf)[1] <- "District"
colnames(cf)[2] <- "Gini.Index"

# Merging the Gini Index by districts in the already merged data by NDAP & SDP 
merged_data_final <- merge(cf, merged_data, by = c("District"))
View(merged_data_final)
# Renaming the columns
colnames(merged_data_final)[37] <- "SDP"

# Writing the csv final file to a location  
write.csv(merged_data_final, file = "NN.csv", row.names = FALSE)


#QUESTION 9

#Estimating the given regression
nf$SDP_sq <- nf$SDP^2
nf$SDP_cu <- nf$SDP^3
mr <- lm(formula = Amount.of.Electrical.Conductivity ~ SDP + SDP_sq + SDP_cu + Gini.Index, data = nf)
summary(mr)
stargazer(mr) 
stargazer(mr, type = "text", out = "mr.txt")