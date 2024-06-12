
# Set working directory
# setwd()


new_data <- read.table('politics.dat', sep=' ')
names(new_data) <- c('R0000100',
'R0536402',
'S7514300',
'S8646900',
'S8647300',
'S8647500',
'S8647800')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -4] = NA  # Valid missing
new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$S7514300 <- factor(data$S7514300,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0),
labels=c("None",
"GED",
"High school diploma (Regular 12 year program)",
"Associate/Junior college (AA)",
"Bachelor's degree (BA, BS)",
"Master's degree (MA, MS)",
"PhD",
"Professional degree (DDS, JD, MD)"))
  data$S8646900 <- factor(data$S8646900,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Definitely should be",
"Probably should be",
"Probably should not be",
"Definitely should not be"))
  data$S8647300 <- factor(data$S8647300,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Definitely should be",
"Probably should be",
"Probably should not be",
"Definitely should not be"))
  data$S8647500 <- factor(data$S8647500,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Definitely should be",
"Probably should be",
"Probably should not be",
"Definitely should not be"))
  data$S8647800 <- factor(data$S8647800,
levels=c(1.0,2.0,3.0,4.0),
labels=c("Definitely should be",
"Probably should be",
"Probably should not be",
"Definitely should not be"))
return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
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
return(data)
}

varlabels <- c("PUBID - YTH ID CODE 1997",
"KEY!BDATE M/Y (SYMBOL) 1997",
"CV_HIGHEST_DEGREE_0607 2006",
"GOVT RESPONSIBILITY - PROVIDE JOBS 2006",
"GOVT RESPNSBLTY -PROV IND HELP 2006",
"GOVT RESPNSBLTY -REDUCE INC DIFF 2006",
"GOVT RESPNSBLTY -PROTECT ENVIRONMENT 2006"
)


# Use qnames rather than rnums

qnames = function(data) {
names(data) <- c("PUBID_1997",
"KEY_BDATE_Y_1997",
"CV_HIGHEST_DEGREE_0607_2006",
"YTEL-11A_2006",
"YTEL-11E_2006",
"YTEL-11G_2006",
"YTEL-11J_2006")
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
names(categories) <- varlabels
