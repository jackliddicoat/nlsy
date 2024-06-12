
# Set working directory
# setwd()


new_data <- read.table('sibling_analysis.dat', sep=' ')
names(new_data) <- c('E8043100',
'R0000100',
'R0536401',
'R0536402',
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
  data$R0536401 <- factor(data$R0536401,
levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),
labels=c("1: January",
"2: February",
"3: March",
"4: April",
"5: May",
"6: June",
"7: July",
"8: August",
"9: September",
"10: October",
"11: November",
"12: December"))
return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
data$E8043100[1.0 <= data$E8043100 & data$E8043100 <= 2.0] <- 1.0
data$E8043100[3.0 <= data$E8043100 & data$E8043100 <= 4.0] <- 3.0
data$E8043100[5.0 <= data$E8043100 & data$E8043100 <= 6.0] <- 5.0
data$E8043100[7.0 <= data$E8043100 & data$E8043100 <= 8.0] <- 7.0
data$E8043100[9.0 <= data$E8043100 & data$E8043100 <= 10.0] <- 9.0
data$E8043100[11.0 <= data$E8043100 & data$E8043100 <= 12.0] <- 11.0
data$E8043100[13.0 <= data$E8043100 & data$E8043100 <= 14.0] <- 13.0
data$E8043100[15.0 <= data$E8043100 & data$E8043100 <= 16.0] <- 15.0
data$E8043100[17.0 <= data$E8043100 & data$E8043100 <= 18.0] <- 17.0
data$E8043100[19.0 <= data$E8043100 & data$E8043100 <= 20.0] <- 19.0
data$E8043100 <- factor(data$E8043100,
levels=c(0.0,1.0,3.0,5.0,7.0,9.0,11.0,13.0,15.0,17.0,19.0),
labels=c("0: No incarcerations",
"1 TO 2: incarcerations",
"3 TO 4: incarcerations",
"5 TO 6: incarcerations",
"7 TO 8: incarcerations",
"9 TO 10: incarcerations",
"11 TO 12: incarcerations",
"13 TO 14: incarcerations",
"15 TO 16: incarcerations",
"17 TO 18: incarcerations",
"19 TO 20: incarcerations"))
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

varlabels <- c("INCARC_TOTNUM",
"PUBID - YTH ID CODE 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"SIDCODE 1997",
"ASVAB_MATH_VERBAL_SCORE_PCT 1999"
)


# Use qnames rather than rnums

qnames = function(data) {
names(data) <- c("INCARC_TOTNUM_XRND",
"PUBID_1997",
"KEY_BDATE_M_1997",
"KEY_BDATE_Y_1997",
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
summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#categories <- vallabels_continuous(new_data)
#summary(categories)

#************************************************************************************************************
glimpse(categories)


