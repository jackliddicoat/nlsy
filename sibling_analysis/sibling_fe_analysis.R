library(NlsyLinks)
library(lme4)
library(jtools)

# run this code before running cox_sibling_analysis.R

View(Links97PairExpanded)
View(categories)

Links97PairExpanded %>% 
  filter(!is.na(R)) %>% 
  count(R)

df1 <- Links97PairExpanded %>% 
  distinct(ExtendedID, .keep_all = T)
df2 <- categories %>% 
  rename(ExtendedID = SIDCODE_1997)

df3 <- merge(df1, df2)

dat <- df3 %>% 
  mutate(ever_inc = ifelse(INCARC_TOTNUM_XRND > 0, 1, 0),
         asvab = ASVAB_MATH_VERBAL_SCORE_PCT_1999/100000)

dat_og_analysis <- dat %>% 
  select(ExtendedID, KEY_BDATE_Y_1997, ever_inc, PUBID_1997)


model1 <- lmer(ever_inc ~ asvab + (1 | ExtendedID), data = dat, REML = F)
summary(model1)

model2 <- glmer(ever_inc ~ asvab + (1 | ExtendedID), data = dat,
               family = "binomial", nAGQ = 0)
summary(model2)
# Lower IQ scores predict higher probability of incarceration within families






