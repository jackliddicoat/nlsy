
# Set working directory
# setwd()


new_data <- read.table('cox_sibling_analysis.dat', sep=' ')
names(new_data) <- c('E8033000',
'E8043000',
'R0000100',
'R1193000',
'R9829600')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -4] = NA  # Valid missing
new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
data$E8033000[198701.0 <= data$E8033000 & data$E8033000 <= 198912.0] <- 198701.0
data$E8033000[199001.0 <= data$E8033000 & data$E8033000 <= 199412.0] <- 199001.0
data$E8033000[199501.0 <= data$E8033000 & data$E8033000 <= 199912.0] <- 199501.0
data$E8033000[200001.0 <= data$E8033000 & data$E8033000 <= 200412.0] <- 200001.0
data$E8033000[200501.0 <= data$E8033000 & data$E8033000 <= 200912.0] <- 200501.0
data$E8033000[201001.0 <= data$E8033000 & data$E8033000 <= 201412.0] <- 201001.0
data$E8033000[201501.0 <= data$E8033000 & data$E8033000 <= 201912.0] <- 201501.0
data$E8033000[202001.0 <= data$E8033000 & data$E8033000 <= 202512.0] <- 202001.0
data$E8033000 <- factor(data$E8033000,
levels=c(198701.0,199001.0,199501.0,200001.0,200501.0,201001.0,201501.0,202001.0),
labels=c("198701 TO 198912: Jan 1987-Dec 1989",
"199001 TO 199412: Jan 1990-Dec 1994",
"199501 TO 199912: Jan 1995-Dec 1999",
"200001 TO 200412: Jan 2000-Dec 2004",
"200501 TO 200912: Jan 2005-Dec 2009",
"201001 TO 201412: Jan 2010-Dec 2014",
"201501 TO 201912: Jan 2015-Dec 2019",
"202001 TO 202512: Jan 2020-Dec2025"))
data$E8043000[198701.0 <= data$E8043000 & data$E8043000 <= 198912.0] <- 198701.0
data$E8043000[199001.0 <= data$E8043000 & data$E8043000 <= 199412.0] <- 199001.0
data$E8043000[199501.0 <= data$E8043000 & data$E8043000 <= 199912.0] <- 199501.0
data$E8043000[200001.0 <= data$E8043000 & data$E8043000 <= 200412.0] <- 200001.0
data$E8043000[200501.0 <= data$E8043000 & data$E8043000 <= 200912.0] <- 200501.0
data$E8043000[201001.0 <= data$E8043000 & data$E8043000 <= 201412.0] <- 201001.0
data$E8043000[201501.0 <= data$E8043000 & data$E8043000 <= 201912.0] <- 201501.0
data$E8043000[202001.0 <= data$E8043000 & data$E8043000 <= 202512.0] <- 202001.0
data$E8043000 <- factor(data$E8043000,
levels=c(198701.0,199001.0,199501.0,200001.0,200501.0,201001.0,201501.0,202001.0),
labels=c("198701 TO 198912: Jan 1987-Dec 1989",
"199001 TO 199412: Jan 1990-Dec 1994",
"199501 TO 199912: Jan 1995-Dec 1999",
"200001 TO 200412: Jan 2000-Dec 2004",
"200501 TO 200912: Jan 2005-Dec 2009",
"201001 TO 201412: Jan 2010-Dec 2014",
"201501 TO 201912: Jan 2015-Dec 2019",
"202001 TO 202512: Jan 2020-Dec2025"))
data$R0000100[1.0 <= data$R0000100 & data$R0000100 <= 999.0] <- 1.0
data$R0000100[1000.0 <= data$R0000100 & data$R0000100 <= 1999.0] <- 1000.0
data$R0000100[2000.0 <= data$R0000100 & data$R0000100 <= 2999.0] <- 2000.0
data$R0000100[3000.0 <= data$R0000100 & data$R0000100 <= 3999.0] <- 3000.0
data$R0000100[4000.0 <= data$R0000100 & data$R0000100 <= 4999.0] <- 4000.0
data$R0000100[5000.0 <= data$R0000100 & data$R0000100 <= 5999.0] <- 5000.0
data$R0000100[6000.0 <= data$R0000100 & data$R0000100 <= 6999.0] <- 6000.0
data$R0000100[7000.0 <= data$R0000100 & data$R0000100 <= 7999.0] <- 7000.0
data$R0000100[8000.0 <= data$R0000100 & data$R0000100 <= 8999.0] <- 8000.0
data$R0000100[9000.0 <= data$R0000100 & data$R0000100 <= 9999.0] <- 9000.0
data$R0000100 <- factor(data$R0000100,
levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0),
labels=c("0",
"1 TO 999",
"1000 TO 1999",
"2000 TO 2999",
"3000 TO 3999",
"4000 TO 4999",
"5000 TO 5999",
"6000 TO 6999",
"7000 TO 7999",
"8000 TO 8999",
"9000 TO 9999"))
data$R1193000[1.0 <= data$R1193000 & data$R1193000 <= 999.0] <- 1.0
data$R1193000[1000.0 <= data$R1193000 & data$R1193000 <= 1999.0] <- 1000.0
data$R1193000[2000.0 <= data$R1193000 & data$R1193000 <= 2999.0] <- 2000.0
data$R1193000[3000.0 <= data$R1193000 & data$R1193000 <= 3999.0] <- 3000.0
data$R1193000[4000.0 <= data$R1193000 & data$R1193000 <= 4999.0] <- 4000.0
data$R1193000[5000.0 <= data$R1193000 & data$R1193000 <= 5999.0] <- 5000.0
data$R1193000[6000.0 <= data$R1193000 & data$R1193000 <= 6999.0] <- 6000.0
data$R1193000[7000.0 <= data$R1193000 & data$R1193000 <= 7999.0] <- 7000.0
data$R1193000[8000.0 <= data$R1193000 & data$R1193000 <= 8999.0] <- 8000.0
data$R1193000[9000.0 <= data$R1193000 & data$R1193000 <= 9999.0] <- 9000.0
data$R1193000 <- factor(data$R1193000,
levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0),
labels=c("0",
"1 TO 999",
"1000 TO 1999",
"2000 TO 2999",
"3000 TO 3999",
"4000 TO 4999",
"5000 TO 5999",
"6000 TO 6999",
"7000 TO 7999",
"8000 TO 8999",
"9000 TO 9999"))
data$R9829600[1.0 <= data$R9829600 & data$R9829600 <= 999.0] <- 1.0
data$R9829600[1000.0 <= data$R9829600 & data$R9829600 <= 19999.0] <- 1000.0
data$R9829600[20000.0 <= data$R9829600 & data$R9829600 <= 39999.0] <- 20000.0
data$R9829600[40000.0 <= data$R9829600 & data$R9829600 <= 59999.0] <- 40000.0
data$R9829600[60000.0 <= data$R9829600 & data$R9829600 <= 79999.0] <- 60000.0
data$R9829600[80000.0 <= data$R9829600 & data$R9829600 <= 100000.0] <- 80000.0
data$R9829600 <- factor(data$R9829600,
levels=c(0.0,1.0,1000.0,20000.0,40000.0,60000.0,80000.0),
labels=c("0",
"1 TO 999: .001-.999",
"1000 TO 19999: 1.000-19.999",
"20000 TO 39999: 20.000-39.999",
"40000 TO 59999: 40.000-59.999",
"60000 TO 79999: 60.000-79.999",
"80000 TO 100000: 80.000-100.000"))
return(data)
}

varlabels <- c("ARREST_FIRST",
"INCARC_FIRST",
"PUBID - YTH ID CODE 1997",
"SIDCODE 1997",
"ASVAB_MATH_VERBAL_SCORE_PCT 1999"
)


# Use qnames rather than rnums

qnames = function(data) {
names(data) <- c("ARREST_FIRST_XRND",
"INCARC_FIRST_XRND",
"PUBID_1997",
"SIDCODE_1997",
"ASVAB_MATH_VERBAL_SCORE_PCT_1999")
return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels.
categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
#new_data <- qnames(new_data)
categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
# summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#categories <- vallabels_continuous(new_data)
#summary(categories)

#************************************************************************************************************
glimpse(categories)
