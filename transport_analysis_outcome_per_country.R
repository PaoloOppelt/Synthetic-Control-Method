# reads transport.csv from PATSTAT
# exports y02t_by_country

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
# transport.csv is generated from the SQL querries from PATSTAT

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



# appendix_4
dt_appendix_4 <- dt[earliest_filing_year <= 1999]

dt_appendix_4 <- dt_appendix_4[, mean(fraction_se), by = .(cpc_class_symbol, inpadoc_family_id)]

dt_appendix_4 <- dt_appendix_4[, sum(V1), by = .(cpc_class_symbol)]

dt_appendix_4 <- dt_appendix_4[order(-V1)]
names(dt_appendix_4)[names(dt_appendix_4) == 'V1'] <- 'Swedish Patent Families'

stargazer(dt_appendix_4, summary=FALSE,
          no.space = TRUE, align=TRUE,
          type="text")

stargazer(dt_appendix_4, summary=FALSE,
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


# For-loop to get y02T per country for donor pool -------------------------

countries = list("AT", "BE", "BG", "HR", "CY", "CZ", "FR", "GR", "HU",
                 "LT", "LU", "MT", "PL", "PT", "RO", "SK", "ES")
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
dt_y02t <- dt_y02t[,c(1,3:19, 2)]
# dt_y02t <- setNames(t(dt_y02t))
# dt_y02t <-  t(dt_y02t)
view(dt_y02t)
write_xlsx(dt_y02t,"~/Desktop/thesis_code/y02t_by_country.xlsx")


# Data Analysis -----------------------------------------------------------
summary(dt)
summary(dt$person_ctry_code)

dt_y02t_m <- melt(dt_y02t,
                  id.vars = "earliest_filing_year",
                  measure.vars = c(2:19),
                  variable.name = "country",
                  value.name = "patents")

dt_y02t_m$country <- as.factor(dt_y02t_m$country)
summary(dt_y02t_m)
ggplot(dt_y02t_m, color=country) + geom_point(aes(earliest_filing_year, patents))


dt_test <- dt[earliest_filing_year < 2000 & person_ctry_code == country]

ggplot(dt_grouped_by_family, aes(x = earliest_filing_year, y = fraction_se)) + 
  geom_line() + 
  ggtitle('Annual family patent count weighted by Swedish inventors')

ggplot(dt_y02t_m[country == "SE" & earliest_filing_year < 2014], aes(x = earliest_filing_year, y = patents)) + 
  geom_line() + 
  ggtitle('Annual family patent count weighted by SE inventors') +
  ylab("Y02T patent families")


dt_y02t_m <- dt_y02t_m[country != "FR"]
ggplot(dt_y02t_m[earliest_filing_year < 2014],) + 
  geom_line( aes(x = earliest_filing_year, y = patents, color = country)) + 
  ggtitle('Annual family patent count weighted by SE inventors') +
  ylab("Y02T patent families") +
  scale_color_viridis(discrete = TRUE, option = "D")

# excluding france
dt_y02t_heat <- dt_y02t[, "FR":= NULL]
dt_y02t_heat <- dt_y02t[earliest_filing_year < 2015, ]
# first column to rownames
dt_y02t_heat <- dt_y02t_heat %>% remove_rownames %>% column_to_rownames(var="earliest_filing_year")

pheatmap(dt_y02t_heat[, 1:18], cluster_rows=FALSE, 
         cluster_cols=FALSE)
install.packages("ggnewscale")

library(ggnewscale)

dt_y02t_m <- dt_y02t_m[country != "FR"]
dt_y02t_m <- dt_y02t_m[country != "PT"]
dt_y02t_m <- dt_y02t_m[country != "ES"]

# write_xlsx(dt_y02t_m,"~/Library/Mobile Documents/com~apple~CloudDocs/TUM_iCloud/Master_Thesis/driving_innovation/own_code/family_count.xlsx")

country_spelled_out <- read_excel("family_count.xlsx", sheet = "Sheet1")
country_spelled_out <- as.data.table(country_spelled_out)
ggplot(country_spelled_out[earliest_filing_year <= 2018],) +
  scale_color_viridis(discrete = FALSE, option = "H") + 
  #new_scale_color() +
  geom_line( aes(x = earliest_filing_year, y = patents, color = patents)) + 
  ggtitle('Annual family patent count weighted by inventors country') +
  ylab("Annual Y02T patent family count") +
  xlab("Year") +
  # scale_color_viridis(discrete = TRUE, option = "D") + 
  facet_wrap(~country) +
  #+
  # scale_colour_continuous(guide = guide_colourbar(reverse = TRUE)) 
  #
  # theme(text = element_text(size = 10),
  #       axis.text=element_text(size=10))

 theme_ipsum(axis_text_size = 10,
             axis_title_size = 12,
             strip_text_size = 12,
             plot_title_size = 15)


# new analysis ------------------------------------------------------------

dt_carbon_price <- read_excel("Data_paolo.xlsx", sheet = "carbon_price")
dt_carbon_price <- as.data.table(dt_carbon_price)

colnames(dt_carbon_price)

names(dt_carbon_price)[names(dt_carbon_price) == '2021'] <- 'last_col'

# dt_carbon_price_high <- dt_carbon_price[last_col > 10]
# dt_carbon_price_low <- dt_carbon_price[last_col <= 10]

names(dt_carbon_price)[names(dt_carbon_price) == 'last_col'] <- '2021'
# dt_carbon_price

# exclude Luxembourg and the Netherlands because they do not have an explicit
# carbon tax
# exclude Poland because the tax is below 1 euro
dt_carbon_price <- dt_carbon_price[country != "Luxembourg"]
dt_carbon_price <- dt_carbon_price[country != "Netherlands"]
dt_carbon_price <- dt_carbon_price[country != "Poland"]
dt_carbon_price <- dt_carbon_price[country != "Portugal"]
dt_carbon_price <- dt_carbon_price[country != "Latvia"]
dt_carbon_price <- dt_carbon_price[country != "Estonia"]

dt_carbon_price_m <- melt(dt_carbon_price,
                  id.vars = "country",
                  # limits the time frame until 2018
                  measure.vars = c(3:30),
                  variable.name = "year",
                  value.name = "carbon_price")


p <- ggplot(dt_carbon_price_m,aes(x = year, y = carbon_price))
p + geom_line(aes(group = country, linetype = country, color = country), size = .8) +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    text = element_text(size=14)
  ) + 
  scale_fill_discrete(name = "Country")+
  xlab("Year") +
  ylab("Carbon Price in USD")

