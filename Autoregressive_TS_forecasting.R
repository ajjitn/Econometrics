library(tidyverse)
library(forecast)
library(lubridate)
library(broom)
library(strucchange)
library(lmtest)
library(knitr)

### Reading in data
data = read.csv("ECOMNSA.csv")
data = data %>%
  mutate(date = as.Date(DATE),
         logecomnsa = log(ECOMNSA),
         ecomnsa = ECOMNSA,
         year = year(DATE),
         month = month(DATE),
         day = day(DATE),
         Q2 = ifelse(month == 4, 1, 0),
         Q3 = ifelse(month == 7, 1, 0),
         Q4 = ifelse(month == 10,1, 0),
         trend = 1:nrow(data),
         trend2 = trend^2) %>%
  select(month, year, logecomnsa, ecomnsa, trend,trend2, Q2, Q3, Q4)


# Descriptive stats plots
ggplot(data, aes(x = trend)) +
  geom_line(aes(y = ecomnsa))
ggplot(data, aes(x = trend)) +
  geom_line(aes(y = logecomnsa))

#plot(data[, "logecomnsa"], ylab = "log ecomnsa)")
season_trendfit = lm(logecomnsa ~ trend+Q2+Q3+Q4, data = data)
season_trend2fit = lm(logecomnsa ~ trend+trend2+Q2+Q3+Q4, data = data)


summ_basemodels = rbind(glance(season_trendfit),(glance(season_trend2fit)))
summ_basemodels = cbind(Model = c("Season + Trend", "Season + Trend + Trend^2"),summ_basemodels)


data$seasontrendfitted = season_trendfit$fitted.values
data$seasontrendresid = season_trendfit$residuals
data$seasontrend2fitted = season_trend2fit$fitted.values
data$seasontrend2resid = season_trend2fit$residuals


ggplot(data, aes(x = trend)) +
  geom_line(aes(y = logecomnsa)) +
  geom_line(aes(y = seasontrendfitted), col = "blue") +
  geom_line(aes(y = seasontrend2fitted), col = "red") +
  ggtitle("Season + Trend + Trend^2 Model")

ggplot(data, aes(x = trend)) +
  geom_line(aes(y = seasontrend2resid)) +
  geom_hline(yintercept = 0, col = "red")+
  ggtitle("Season + Trend + Trend^2 Model Residuals")+
  ylab("Residuals")

kable(tidy(dwtest(season_trend2fit)))


f_Stats = Fstats(logecomnsa ~ trend+trend2+Q2+Q3+Q4, data = data)
plot(f_Stats)

print(paste0("point of biggest F-stat is t=",f_Stats$breakpoint))
maxchowtest = sctest(f_Stats, type = c("supF"))
maxchowtest




rec_resid = data.frame(rec_residu = recresid(season_trend2fit))

ggplot(data = rec_resid) +
  geom_line(aes(x = 1:nrow(rec_resid), y =rec_residu))+ 
  xlab("Index") +
  ylab("Recursive Residuals") +
  ggtitle("Recursive Residual Plot")


data_pre = data %>%
  filter(trend %in% 1:33)
data_recession = data %>%
  filter(trend %in% 34:40)
data_post = data %>%
  filter(trend %in% 41:nrow(data))

data_post = data_post %>%
  mutate(trend = 1:nrow(data_post),
         trend2 = trend^2)
data_recession = data_recession %>%
  mutate(trend = 1:nrow(data_recession),
         trend2 = trend^2)


ggplot(aes(x=trend), data = data_pre)+
  geom_line(aes(y = logecomnsa))

ggplot(aes(x=trend), data = data_recession)+
  geom_line(aes(y = logecomnsa))

ggplot(aes(x=trend), data = data_post)+
  geom_line(aes(y = logecomnsa))


f_stats_pre = Fstats(logecomnsa ~ trend+trend2+Q2+Q3+Q4, data = data_pre)
plot(f_stats_pre)

f_stats_post = Fstats(logecomnsa ~ trend+Q2+Q3+Q4, data = data_post)
plot(f_stats_post)

print(paste0("point of biggest F-stat is t=",f_stats_post$breakpoint))
maxchowtest = sctest(f_stats_post, type = c("supF"))
maxchowtest



pre_trend = lm(logecomnsa ~ trend+Q2+Q3+Q4, data = data_pre)
pre_trend2= lm(logecomnsa ~ trend+trend2+Q2+Q3+Q4, data = data_pre)
data_pre$trendfitted = pre_trend$fitted.values
data_pre$trendresid  = pre_trend$residuals
data_pre$trend2fitted= pre_trend2$fitted.values
data_pre$trend2resid = pre_trend2$residuals

post_trend = lm(logecomnsa ~ trend+Q2+Q3+Q4, data = data_post)
post_trend2= lm(logecomnsa ~ trend+trend2+Q2+Q3+Q4, data = data_post)
data_post$trendfitted = post_trend$fitted.values
data_post$trendresid  = post_trend$residuals
data_post$trend2fitted= post_trend2$fitted.values
data_post$trend2resid = post_trend2$residuals


recession_trend = lm(logecomnsa ~ trend+Q2+Q3+Q4, data = data_recession)
recession_trend2= lm(logecomnsa ~ trend+trend2+Q2+Q3+Q4, data = data_recession)
data_recession$trendfitted = recession_trend$fitted.values
data_recession$trendresid  = recession_trend$residuals
data_recession$trend2fitted= recession_trend2$fitted.values
data_recession$trend2resid = recession_trend2$residuals

ggplot(aes(x=trend), data = data_pre)+
  geom_line(aes(y = logecomnsa)) +
  geom_line(aes(y = trendfitted), col = "red") +
  geom_line(aes(y = trend2fitted), col = "steelblue")


ggplot(aes(x=trend), data = data_recession)+
  geom_line(aes(y = logecomnsa))+
  geom_line(aes(y = trendfitted), col = "red") +
  geom_line(aes(y = trend2fitted), col = "steelblue")


ggplot(aes(x=trend), data = data_post)+
  geom_line(aes(y = logecomnsa))+
  geom_line(aes(y = trendfitted), col = "red") +
  geom_line(aes(y = trend2fitted), col = "steelblue")


summ_models_pre = rbind(glance(pre_trend),(glance(pre_trend2)))
summ_models_pre = cbind(Model = c("Season + Trend", "Season + Trend + Trend^2"),summ_models_pre)

summ_models_rec = rbind(glance(recession_trend),(glance(recession_trend2)))
summ_models_rec = cbind(Model = c("Season + Trend", "Season + Trend + Trend^2"),summ_models_rec)

summ_models_post = rbind(glance(post_trend),(glance(post_trend2)))
summ_models_post = cbind(Model = c("Season + Trend", "Season + Trend + Trend^2"),summ_models_post)

kable(summ_models_pre)
kable(summ_models_rec)
kable(summ_models_post)


ggplot(data = data_pre, aes(x = trend)) +
  geom_line(aes(y = trendresid)) +
  geom_hline(yintercept = 0, col = "red")+
  ggtitle("Season + Trend Model Residuals Pre-Recession")+
  ylab("Residuals")

ggplot(data = data_recession, aes(x = trend)) +
  geom_line(aes(y = trendresid)) +
  geom_hline(yintercept = 0, col = "red")+
  ggtitle("Season + Trend Model Residuals During Recession")+
  ylab("Residuals")


ggplot(data = data_post, aes(x = trend)) +
  geom_line(aes(y = trendresid)) +
  geom_hline(yintercept = 0, col = "red")+
  ggtitle("Season + Trend Model Residuals Post-Recession")+
  ylab("Residuals")


kable(tidy(dwtest(pre_trend)))
kable(tidy(dwtest(post_trend)))
kable(tidy(dwtest(recession_trend)))


Acf(data_pre$trendresid, main = "Pre-Recession Model with Trend + Seasonality")
Pacf(data_pre$trendresid,main = "Pre-Recession Model with Trend + Seasonality")

Acf(data_recession$trendresid, main = "Recession Model with Trend + Seasonality")
Pacf(data_recession$trendresid,main = "Recession Model with Trend + Seasonality")

Acf(data_post$trendresid, main = "Post-Recession Model with Trend + Seasonality")
Pacf(data_post$trendresid,main = "Post-Recession Model with Trend + Seasonality")



ts_data_pre = data_pre[,"logecomnsa"]
ts_data_post = data_post[,"logecomnsa"]

ar1pre = arima(ts_data_pre, order = c(1,0,0), xreg = data_pre %>% select(trend, Q2, Q3, Q4))
ar1post = arima(ts_data_post, order = c(1,0,0), xreg = data_post %>% select(trend, Q2, Q3, Q4))

ar1pre2 = arima(ts_data_pre, order = c(1,0,0), xreg = data_pre %>% select(trend, trend2, Q2, Q3, Q4))
ar1post2 = arima(ts_data_post, order = c(1,0,0), xreg = data_post %>% select(trend, trend2, Q2, Q3, Q4))

data_pre$ar1fittedtrend  = fitted(ar1pre)
data_pre$ar1residtrend   = ar1pre$residuals

data_post$ar1fittedtrend = fitted(ar1post)
data_post$ar1residtrend   = ar1post$residuals

data_pre$ar1fittedtrend2 = fitted(ar1pre2)
data_pre$ar1residtrend2   = ar1pre2$residuals

data_post$ar1fittedtrend2= fitted(ar1post2)
data_post$ar1residtrend2   = ar1post2$residuals



kable(tidy(ar1pre))
kable(tidy(ar1pre2))

summary(ar1pre)
summary(ar1pre2)

kable(data.frame(Pre_model = c("AR(1) + Trend + Seasonal", "AR(1) + Trend + Trend^2+ Seasonal"),
                 BIC = c(AIC(ar1pre, k = log(length(ts_data_post))),AIC(ar1pre2, k = log(length(ts_data_pre)))),
                 sigma = c(ar1pre$sigma2, ar1pre2$sigma2),
                 loglik = c(ar1pre$loglik,ar1pre2$loglik)))

kable(tidy(ar1post))
kable(tidy(ar1post2))

summary(ar1post)
summary(ar1post2)

kable(data.frame(Post_model = c("AR(1) + Trend + Seasonal", "AR(1) + Trend + Trend^2+ Seasonal"),
                 BIC = c(AIC(ar1post, k = log(length(ts_data_post))),AIC(ar1post2, k = log(length(ts_data_post)))),
                 sigma = c(ar1post$sigma2, ar1post2$sigma2),
                 loglik = c(ar1post$loglik,ar1post2$loglik)))




ggplot(data = data_pre, aes(x=trend))+
  geom_line(aes(y=logecomnsa))+
  geom_line(aes(y= ar1fittedtrend, col = "red"))+
  geom_line(aes(y= ar1fittedtrend2, col = "steelblue"))+
  ggtitle("AR(1) Model for Pre Recession Data")+
  scale_color_hue(labels = c("AR 1 with Trend","AR 1 with Trend^2" )) +
  labs(col = "Models")

ggplot(data = data_post, aes(x=trend))+
  geom_line(aes(y=logecomnsa))+
  geom_line(aes(y= ar1fittedtrend, col = "red"))+
  geom_line(aes(y= ar1fittedtrend2, col = "steelblue"))+
  ggtitle("AR(1) Model for Post Recession Data")+  
  scale_color_hue(labels = c("AR 1 with Trend","AR 1 with Trend^2" )) +
  labs(col = "Models")                             


ggplot(data_pre, aes(x = trend)) +
  geom_line(aes(y = ar1residtrend), col = "red") +
  geom_line(aes(y = ar1residtrend2), col = "steelblue") +
  geom_hline(yintercept = 0, col = "black")+
  ggtitle("Pre-recession Data Model Residuals")

ggplot(data_post, aes(x = trend)) +
  geom_line(aes(y = ar1residtrend), col = "red") +
  geom_line(aes(y = ar1residtrend2), col = "steelblue") +
  geom_hline(yintercept = 0, col = "black")+
  ggtitle("Post-recession Data Model Residuals")



Pacf(data_pre$ar1residtrend, main = "Pre Recession Trend Model")
Pacf(data_pre$ar1residtrend2, main = "Pre Recession Trend^2 Model")

Pacf(data_post$ar1residtrend, main = "Post Recession Trend Model")
Pacf(data_post$ar1residtrend2, main = "Post Recession Trend^2 Model")


ggplot(data = data_pre)+
  geom_histogram(aes(ar1residtrend2), binwidth = .005)+
  ggtitle("Pre-recession Data Residuals")
ggplot(data = data_recession)+
  geom_histogram(aes(trendresid), binwidth = .005)+
  ggtitle("Recession Data Residuals")
ggplot(data = data_post)+
  geom_histogram(aes(ar1residtrend), binwidth = .005)+
  ggtitle("Post-recession Data Residuals")



x1 = data.frame(trend = 32:35, trend2 = (32:35)^2, Q2 = c(0,0,0,1), Q3 = c(1,0,0,0), Q4 = c(0,1,0,0))

predic = predict(ar1post2, n.ahead = 4, newxreg = x1,
                 se.fit = TRUE)


final_predictions = data.frame(pred = (append(data_post$ar1fittedtrend2, predic$pred)), actual = append(data_post$logecomnsa, c(NA, NA, NA, NA)))
final_predictions = data.frame(pred = (append(data_post$ar1fittedtrend2, predic$pred)), actual = append(data_post$logecomnsa, c(NA, NA, NA, NA)), trend = 1:nrow(final_predictions))

ggplot(data= final_predictions, aes(x=trend))+
  geom_line(aes(y=pred), col = "steelblue")+
  geom_line(aes(y=actual)) +
  ggtitle("Future Predictions")

ggplot(data= final_predictions, aes(x=trend))+
  geom_line(aes(y=exp(pred)), col = "steelblue")+
  geom_line(aes(y=exp(actual))) +
  ggtitle("Future Predictions") +
  ylab("ECOMNSA")
