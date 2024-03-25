
# libraries ---------------------------------------------------------------
# get export_for_python.xlsx from transformation_for_python_matteo
library(Synth) # load Synth package
library(SCtools)
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
# install.packages("hrbrthemes")
library(hrbrthemes)

dir <- "~/Desktop/thesis_code"
dirRslt  <- paste0(dir, "/")

# excluding columns 3-9 so the years 1978 until 1984 because there is so little
# data available for these dates.
# 
dt_python <- read_excel("Data_paolo.xlsx", sheet = "dataset")
dt_python <- as.data.table(dt_python)
dt_python <- dt_python[, c(1:2, 10:43)]
dt_python_m <- melt(dt_python,
                    id.vars = c("country", "variable"),
                    measure.vars = 3:36,
                    variable.name = "year",
                    value.name = "values")

dt_python_cast <- dcast(dt_python_m, ... ~ variable,  value.var = "values")


dt_python <- as.data.table(dt_python_cast)
dt_python$year <- as.character(dt_python$year)


dt_python <- dt_python[country != "Portugal"]


start <- 1985
end <- 2018

dt_python<- dt_python[ year <= end]
dt_python<- dt_python[ year >= start]


dt_python <- copy(dt_python[, country_no:= as.numeric(as.factor(country))])
# CHANGE THIS IF CHANGING THE NUMBER OF COUNTRIES
dt <- dt_python[,c(11,1:10)]
dt$year <- as.numeric(dt$year)
summary(dt)
dt$climate_change_mitigation <- as.numeric(dt$climate_change_mitigation)
dt$climate_transport_epo <- as.numeric(dt$climate_transport_epo)
dt$transport_wipo  <- as.numeric(dt$transport_wipo)
dt$triadic_patent_families <- as.numeric(dt$triadic_patent_families)
dt$y02t  <- as.numeric(dt$y02t)
dt$enrollment_tertiary <- as.numeric(dt$enrollment_tertiary)
dt$gdp <- as.numeric(dt$gdp)
dt$b60 <- as.numeric(dt$b60)


my_predictors <- c( "transport_wipo","triadic_patent_families", "y02t",
                    "climate_change_mitigation", "climate_transport_epo",
                    "enrollment_tertiary", "gdp", "b60")
# usd_t_carbon cannot be used as a predictor because it does not affect
# the control units

dataprep.out <- dataprep(
  foo = dt,
  predictors = my_predictors,
  predictors.op = "mean",
  # Note: the time.predictors.prior must be set to until 1990
  # otherwise the treatment event is set to 1989 not 1990!
  time.predictors.prior = start:1990,
  dependent = "y02t",
  unit.variable = "country_no",
  unit.names.variable = "country",
  time.variable = "year",
  # 20 is the country number of sweden
  # treatment.identifier = 12,
  treatment.identifier = max(dt$country_no),
  # controls.identifier = c(1:11),
  controls.identifier = c(1:max(dt$country_no-1)),
  # A numeric vector identifying the periods of the dependent variable over which
  # the loss function should be minimized (i.e. the periods over which mean squared
  # prediction error (MSPE) , that is the sum of squared residuals between treated
  # and the synthetic control unit, are minimized
  time.optimize.ssr = start:1990,
  # time period over which the results should be plotted
  time.plot = start:end)

# dataprep() returns a list object dataprep.out that contains several elements including X0,X1,Z0,Z1 

print(dataprep.out$X0) # check 
print(dataprep.out$Z1) # Sweden y02t for the pre-intervention period



# The synth() command searches for the W∗ vector of weights that identifies the synthetic control 
# for the Sweden region by solving the nested optimization problem 

synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")
# view(synth.out$solution.v)
# view(synth.out$solution.w)

# calculate y02t difference in trend between the Sweden region and its synthetic control
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
synthetic_sweden_mean <- mean((dataprep.out$Y0plot %*% synth.out$solution.w)[6:34])
sweden_mean <- mean(dataprep.out$Y1plot[6:34])
percentage_difference <- sweden_mean/synthetic_sweden_mean
print(gaps[1:7, 1]) # check
gaps_mean <- mean(gaps[6:34])
value_1990 <- gaps[13]
value_2018 <- gaps[length(gaps)]
perc_incr <- ((value_2018-value_1990)/value_1990)*100

synth.tables <- synth.tab(dataprep.res = dataprep.out,synth.res = synth.out)
# country weights table
synth.tables$tab.w


# table_2 -----------------------------------------------------------------

pred_weight <- as.data.table(synth.out$solution.v)
pred_weight <- t(pred_weight)

stargazer(pred_weight, summary=FALSE,
          no.space = TRUE, align=TRUE,
          type="text")
stargazer(pred_weight, summary=FALSE,
          no.space = TRUE, align=TRUE,
          type="html", out=paste0(dirRslt, "table_2.doc"))

# table_3 -----------------------------------------------------------------


# table_3
# comparing pre-treatment predictor values for the treated unit, the synthetic control unit, and all the units in the sample

# synth.tables$tab.pred 
# check balance across treated and control for pre-period predictors

stargazer(synth.tables$tab.pred, summary=FALSE,
          no.space = TRUE, align=TRUE,
          type="text")
stargazer(synth.tables$tab.pred, summary=FALSE,
          no.space = TRUE, align=TRUE,
          type="html", out=paste0(dirRslt, "table_3.doc"))

# appendix_6
stargazer(synth.tables$tab.w, summary=FALSE,
          no.space = TRUE, align=TRUE,
          type="text")
stargazer(synth.tables$tab.w, summary=FALSE,
          no.space = TRUE, align=TRUE,
          type="html", out=paste0(dirRslt, "appendix_6.doc"))
# appendix_5 ----------------------------------------------------------------
# delete the country_no, all variables except outcome
dt1 <- copy(dt[, -c(1, 4:7)])
dt1 <- dt1[country == "Sweden", country := "Sweden"]
dt1 <- dt1[country != "Sweden", country := "Donor Pool"]
dt1 <- dt1[, .(mean_y02t = mean(y02t)), by = .(country, year)]

require(gridExtra)
dt_plt1 <- dt1[year >= 1985]
dt_plt1 $country <- as.character(dt1$country)
plt1 <- ggplot(dt_plt1 , aes(x = year, y = mean_y02t, group = country, linetype = country))+
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  # ggtitle("Title") +
  theme_ipsum() +
  xlab("Year") +
  ylab("Y02T patents") +
  geom_vline(xintercept = 1990) +
  geom_text(aes(x= 1990, label="\nCarbon Tax", y=120), colour="black", angle=90) +
  theme(legend.position="none") +
  labs(subtitle = "Panel A")
# labs(tag = "(a) Panel A")
# guides(color = FALSE, size = FALSE)

dt_plt2 <- dt1[year <= 1995]
plt2 <- ggplot(dt_plt2, aes(x = year, y = mean_y02t, group = country, linetype = country))+
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  # ggtitle("Title") +
  theme_ipsum() +
  xlab("Year") +
  ylab("Y02T patents") +
  geom_vline(xintercept = 1990) +
  geom_text(aes(x= 1990, label="\nCarbon Tax", y=14), colour="black", angle=90) +
  labs(subtitle = "Panel B")

grid.arrange(plt1, plt2, ncol=2)



# appendix_5 ----------------------------------------------------------------

dataprep.out <- dataprep(
  foo = dt,
  predictors = my_predictors,
  predictors.op = "mean",
  time.predictors.prior = start:1990,
  dependent = "y02t",
  unit.variable = "country_no",
  unit.names.variable = "country",
  time.variable = "year",
  # 20 is the country number of sweden
  treatment.identifier = max(dt$country_no),
  controls.identifier = c(1:max(dt$country_no-1)),
  time.optimize.ssr = start:1990,
  # time period over which the results should be plotted
  time.plot = start:1995)

# plot treatment vs control outcomes for pre and post periods

# appendix_7
plot_single <- path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
                         Ylab = "Y02T patents", Xlab = "Year"
                         ,Legend = c("Sweden","synthetic Sweden"),
                         tr.intake = 1990)


dataprep.out <- dataprep(
  foo = dt,
  predictors = my_predictors,
  predictors.op = "mean",
  time.predictors.prior = start:1990,
  dependent = "y02t",
  unit.variable = "country_no",
  unit.names.variable = "country",
  time.variable = "year",
  # 20 is the country number of sweden
  treatment.identifier = max(dt$country_no),
  controls.identifier = c(1:max(dt$country_no-1)),
  time.optimize.ssr = start:1990,
  # time period over which the results should be plotted
  time.plot = start:end)


# figure_6 ----------------------------------------------------------------

outcome_treated <- as.data.table(dataprep.out[["Y1plot"]])
outcome_treated <- outcome_treated[, new:= ""]

data <- as.data.table(plot1[["data"]])

# figure_4 ----------------------------------------------------------------


# view control unit weights
synth.tables$tab.w[1:max(dt$country_no-1), ]
par(mfrow=c(1,2))

# plot treatment vs control outcomes for pre and post periods
plot1 <- path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
                   Ylab = "Y02T patents", Xlab = "Year",
                   Legend = c("Sweden","synthetic Sweden"),
                   tr.intake = 1990)

# gap plot
plot2 <- gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out,
                   Ylab = "Gap in Y02T patents", Xlab = "Year",
                   tr.intake = 1990)

par(mfrow=c(1,1))


# SCtools package ---------------------------------------------------------


# now continue with the SCtools package which allows for an extension of the synthetic
# control method to generate and plot placebos, significance tests and plots, and
# calculating average treatment effects for multiple treated units

# depends on the lfirst/ or last? scm function
tdf <- generate.placebos(
  dataprep.out,
  synth.out,
  Sigf.ipop = 2,
  strategy = "sequential"
)

# plots the trajectories of the other countries
p <- plot_placebos(
  tdf = tdf,
  discard.extreme = TRUE,
  # here it makes a difference where we cutt off
  mspe.limit = 20,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  alpha.placebos = 1,
)
# figure_5
# Creates plots with the difference between observed units and synthetic controls 
# for the treated and control units
p

# not much changes when we change the mspe.limit. 
# figure_5.1

p2 <- plot_placebos(
  tdf = tdf,
  discard.extreme = TRUE,
  mspe.limit = 5,
  # Numerical. Used if discard.extreme is TRUE. It indicates how many times the
  # pre-treatment MSPE of a placebo should be higher than that of the treated unit
  # to be considered extreme and discarded. Default is 20.
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  alpha.placebos = 1,
)
p2



# Bootstrapping -----------------------------------------------------------


set.seed(42)
# careful, this is not the same as the other function before
# maybe change the time back to 1989 here
multi <- multiple.synth(
  foo = dt,
  predictors = my_predictors,
  predictors.op = "mean",
  time.predictors.prior = start:1990,
  treatment.time = 1990,
  special.predictors = list(list("y02t", 1991, "mean")),
  dependent = "y02t",
  unit.variable = "country_no",
  unit.names.variable = "country",
  time.variable = "year",
  # 20 is the country number of sweden
  treated.units = max(dt$country_no),
  control.units = c(1:max(dt$country_no-1)),
  time.optimize.ssr = start:1990,
  # time period over which the results should be plotted
  time.plot = start:end,
  gen.placebos = TRUE,
  # it makes no difference if a value larger than 2 is chosen 
  # but it takes considerably longer to compute
  Sigf.ipop = 2)

## Plot with the average path of the treated units and the average of their
## respective synthetic controls:
multi$p

# Bootstrap the placebo units to get a distribution of placebo average
# treatment effects, and plot the distribution with a vertical line
# indicating the actual ATT:

# appendix_8
att.test <- plac.dist(multi)
att.test$p

# plac.dist(multi, nboots = 500)


# NOT USE: Biased Result!: ----------------------------------------------------------------
# mean squared prediction errors for the pretreatment period for each placebo
mspe <- tdf$mspe.placs
mspe_average <- as.data.table(mspe)
# THIS VALUE IS REPORTED 
mspe_average <- mean(mspe[, 1])
mspe_treatment_country <- tdf$loss.v
donor_synthetics <- tdf$df

ratio <- mspe.test(tdf)
ratio$p.val

mspe.plot(
  tdf,
  discard.extreme = TRUE,
  mspe.limit = 2,
  plot.hist = FALSE,
  title = "Dotplot of the post/pre-treatment mean squared prediction error (MSPE) for Sweden and placebos",
  xlab = "Post/Pre MSPE ratio",
  ylab = NULL
)

