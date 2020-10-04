###################################
### AIDS/HIV ARIMA ###
###
### author: Bin Xu, B.S. and Mengqiao Wang, Ph.D.
### updated: October 4, 2020


library(forecast)


dat <- read.csv("./data/aidsANDhiv_04to17.csv")


## six ARIMA models 
# 2 levels of incidence and death x 3 levels of AIDS, HIV, and combined


## the follwoing code applies to ARIMA for AIDS incidence
## for the other five models, just modify the filtering parameters

## fit ARIMA model with 2004-2015 data
## select an optimal model

aids_inc <- auto.arima(ts(dat[dat$group == "AIDS" & dat$month != "annual", 
                              "incidence"][1:((2015 - 2004 + 1)*12)],
                          start = c(2004, 1), frequency = 12),
                       trace = TRUE)
aids_inc

# Best model: ARIMA(0,1,3)(2,0,0)[12] 


## forecast of data in 2016 and 2017
fcast <- forecast(Arima(aids_inc, order = c(0,1,3), seasonal = c(2,0,0)), h = (2017 - 2016 + 1) * 12, level = c(95))
fcast


## output of actual and fitted data (with 95% prediction interval)
result <- data.frame(actual = dat[dat$group == "AIDS" & dat$month != "annual", 
                                  "incidence"][((2015 - 2004 + 1) * 12 + 1):((2017 - 2004 + 1) * 12)],
                     fitted = fcast$mean,
                     upper = fcast$upper,
                     lower = fcast$lower)
result


### END
