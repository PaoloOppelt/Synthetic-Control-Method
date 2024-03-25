
# getting Y02T for pricing set --------------------------------------------

# reads transport.csv from PATSTAT
# exports y02t_by_country_regression

# install.packages("hrbrthemes")
library(hrbrthemes)
library(readxl)
library(data.table)
library(plm)
library(urca)
library(feasts)
library(forecast)
library(smooth)
library(rstatix)
library(dplyr)
library(stargazer)
library(stringr)
library(tidyr)
library(visdat)
library(caret)
library(dslabs)
library(fossil)
library(GGally)
library(ggbeeswarm)
library(ggrepel)
library(mclust)
library(mlbench)
library(pheatmap)
library(scales)
library(plotROC)
library(ggplot2)
library(magrittr)
library(rpart)
library(ggplot2) 
library(data.table)
library(magrittr) 
library(lubridate)
library(patchwork)
library(tidymodels)
library(glmnet)
library(probably)
library(caret)
library(smotefamily)
library(themis)
library(broom)
library(ggpubr)
# install.packages("gplots")
library(gplots)
library(car)
library(writexl)
library(viridis)
library("ggsci")


setwd("~/Desktop/thesis_code")
dir <- "~/Desktop/thesis_code"
dirRslt  <- paste0(dir, "/")

# Cleaning ----------------------------------------------------------------

# this part before the for loop is needed to create the dt_y02t dataset for Sweden
# this one is then appended for all the other countries in the for loop

transport <- read.csv("~/Desktop/thesis_code/transport.csv")
dt <- as.data.table(transport)
dt[, person_id := NULL]
dt[, earliest_filing_date := NULL]

dt$appln_id <- as.factor(dt$appln_id)
dt$person_ctry_code <- as.factor(dt$person_ctry_code)
dt$inpadoc_family_id <- as.factor(dt$inpadoc_family_id)
dt$cpc_class_symbol <- as.factor(dt$cpc_class_symbol)

country <- "SE"

# count number of inventors per family (sometimes there is one inventor that has to different patents (different application ids))
# this still counts as two inventors 
dt[ , inventor_count := .N, by = .(inpadoc_family_id)]
# problem now is that the fraction is only for the Country rows but they should be in all rows for one family
dt[ person_ctry_code == country, inventor_country := .N, by = .(inpadoc_family_id)]
# problem fixed: now every row shows the same percentage
dt[is.na(dt)] <- 0
dt[, inventor_country := max(inventor_country), by = .(inpadoc_family_id)]

# number of Swedish investors per family
dt$inventor_count <- as.numeric(dt$inventor_count)
dt$inventor_country <- as.numeric(dt$inventor_country)
dt[ , fraction_se := inventor_country/inventor_count, by = .(inpadoc_family_id)]


# what I need is the number of different family patents per year for Country
dt[person_ctry_code == country , family_count := .(count = length(unique(inpadoc_family_id))) , by = .(earliest_filing_year)]
dt[is.na(dt)] <- 0
dt[, family_count := max(family_count), by = .(inpadoc_family_id)]



# appendix 3 table
dt_appendix_3 <- dt[earliest_filing_year <= 1999]

dt_appendix_3 <- dt_appendix_3[, mean(fraction_se), by = .(cpc_class_symbol, inpadoc_family_id)]

dt_appendix_3 <- dt_appendix_3[, sum(V1), by = .(cpc_class_symbol)]

dt_appendix_3 <- dt_appendix_3[order(-V1)]
names(dt_appendix_3)[names(dt_appendix_3) == 'V1'] <- 'Swedish Patent Families'

stargazer(dt_appendix_3, summary=FALSE,
          no.space = TRUE, align=TRUE,
          type="text")

stargazer(dt_appendix_3, summary=FALSE,
          no.space = TRUE, align=TRUE,
          type="html", out=paste0(dirRslt, "dt_appendix_3.doc"))

dt_y02t <- dt[, max(family_count), by = .(earliest_filing_year)]
dt_y02t <- dt_y02t[order(earliest_filing_year)]
# for checks family id: 145613
dt_copy <- copy(dt)
dt_copy$earliest_filing_year <- as.factor(dt_copy$earliest_filing_year) 
dt_grouped_by_family <- dt_copy[, .(fraction_se = mean(fraction_se)), by = .(inpadoc_family_id, earliest_filing_year)]
dt_grouped_by_family <- dt_grouped_by_family[, .(fraction_se = sum(fraction_se)), by = earliest_filing_year]
dt_grouped_by_family$earliest_filing_year <- as.character(dt_grouped_by_family$earliest_filing_year) 
dt_grouped_by_family$earliest_filing_year <- as.numeric(dt_grouped_by_family$earliest_filing_year) 
dt_y02t <- copy(dt_grouped_by_family)



# don't include Sweden. It is already a column
countries = list("DK", "EE", "FI", "FR", "IS", "IE",
                 "LV", "LI", "LU", "NL", "PL", "PT", 
                 "SI", "ES", "NO")
length(countries)

for (i in countries) {
  dt <- as.data.table(transport)
  dt[, person_id := NULL]
  dt[, earliest_filing_date := NULL]
  
  dt$appln_id <- as.factor(dt$appln_id)
  dt$person_ctry_code <- as.factor(dt$person_ctry_code)
  dt$inpadoc_family_id <- as.factor(dt$inpadoc_family_id)
  dt$cpc_class_symbol <- as.factor(dt$cpc_class_symbol)
  
  country <- i
  # count number of inventors per family (sometimes there is one inventor that has to different patents (different application ids))
  # this still counts as two inventors 
  dt[ , inventor_count := .N, by = .(inpadoc_family_id)]
  # problem now is that the fraction is only for the Country rows but they should be in all rows for one family
  # THIS IS WHAT I CHANGE i FOR EACH OT THE COUNTRES
  dt[ person_ctry_code == i, inventor_country := .N, by = .(inpadoc_family_id)]
  # problem fixed: now every row shows the same percentage
  dt[is.na(dt)] <- 0
  dt[, inventor_country := max(inventor_country), by = .(inpadoc_family_id)]
  
  # number of Swedish investors per family
  dt$inventor_count <- as.numeric(dt$inventor_count)
  dt$inventor_country <- as.numeric(dt$inventor_country)
  dt[ , fraction_se := inventor_country/inventor_count, by = .(inpadoc_family_id)]
  
  dt_copy <- copy(dt)
  dt_copy$earliest_filing_year <- as.factor(dt_copy$earliest_filing_year) 
  dt_grouped_by_family <- dt_copy[, .(fraction_se = mean(fraction_se)), by = .(inpadoc_family_id, earliest_filing_year)]
  dt_grouped_by_family <- dt_grouped_by_family[, .(fraction_se = sum(fraction_se)), by = earliest_filing_year]
  dt_grouped_by_family$earliest_filing_year <- as.character(dt_grouped_by_family$earliest_filing_year) 
  dt_grouped_by_family$earliest_filing_year <- as.numeric(dt_grouped_by_family$earliest_filing_year) 
  new <- dt_grouped_by_family$fraction_se
  dt_y02t$new <- new
  colnames(dt_y02t)[ncol(dt_y02t)] <- paste0(i)
}
dt_y02t <- dt_y02t[earliest_filing_year < 2019]
dt_y02t <- dt_y02t[order(earliest_filing_year)]


# renaming the first column so that it is not called fraction_se
names(dt_y02t)[names(dt_y02t) == 'fraction_se'] <- "SE"
# reordering columns so that Sweden is at the end 
# so the frame fits with the other predictor variables
dt_y02t_new <- dt_y02t[,c(1,3:17, 2)]
# dt_y02t <- setNames(t(dt_y02t))
# dt_y02t <-  t(dt_y02t)
write_xlsx(dt_y02t_new,"~/Desktop/thesis_code/y02t_regression.xlsx")

# now manually merge the two on excel
# then load dataset2 from excel
# Regression --------------------------------------------------------------

dt_reg <- read_excel("Data_paolo.xlsx", sheet = "dataset_3")
dt_reg <- as.data.table(dt_reg)

dt_reg_m <- melt(dt_reg,
                    id.vars = c("country", "var"),
                    measure.vars = 4:length(dt_reg),
                    variable.name = "year",
                    value.name = "values")

dt_reg_cas <- dcast(dt_reg_m, ... ~ var,  value.var = "values")


dt_reg <- as.data.table(dt_reg_cas)
dt_reg <- dt_reg[country == "Sweden"]
# dt_reg$year <- as.character(dt_reg$year)

model <- lm(y02t ~ usd_t_carbon, data = dt_reg)
cor(dt_reg$usd_t_carbon, dt_reg$y02t)
summary(model)


stargazer(model, title="Results", align=TRUE)
stargazer(model, summary=TRUE,
          no.space = TRUE, align=TRUE,
          type="html", out=paste0(dirRslt, "appendix_10.doc"))


require(gridExtra)
plt1 <- dt_reg %>% 
  ggplot(aes(usd_t_carbon, y02t)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_ipsum() +
  xlab("Carbon Tax in USD") +
  ylab("Y02T patents") +
  labs(subtitle = "Detecting linearity")

# Non-linearity is typically revealed by noticing that the average of the residual 
# depends on the predicted values. A smooth fit (geom_smooth default) on the residual 
# plot can help spotting systematic non-linear dependencies (See Figure 10.3).

plt2 <- dt_reg %>% 
  ggplot(aes(usd_t_carbon, y02t)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_ipsum() +
  xlab("Carbon Tax in USD") +
  ylab("Y02T patents") + 
  labs(subtitle = "Geometric Smooth on the residual")

grid.arrange(plt1, plt2, ncol=2)


m <- lm(dt_reg$usd_t_carbon ~ dt_reg$y02t)
qqnorm(residuals(m))
qqline(residuals(m))


ggplot(data=dt_reg, aes(predict(m), resid(m))) + 
  geom_point() + geom_abline(intercept=0, slope=0) +
  theme_ipsum() +
  labs(x=expression(hat(dt_reg$y02t)), y="residual") +
  xlab("y hat") +
  ylab("residuals") 



