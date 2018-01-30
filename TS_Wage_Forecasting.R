library(psych)
library(knitr)
library(extrafont)
loadfonts(quiet=T)
library(gridExtra)
library(ggplot2)
library(scales)
library(Blossom)
library(quantreg)
library(lmtest)
library(corrplot)
library(reshape2)
library(PerformanceAnalytics)
library(broom)
library(tidyr)
library(data.table)
library(DAAG)
library(MASS)
library(plotly)
library(tseries)
library(dplyr)


##Reading in data
wage_95 =tbl_df(read.csv("output95_update.csv",colClasses=c(rep("factor",3),rep("numeric",5))))
wage_04 =tbl_df( read.csv("output04_update.csv",colClasses=c(rep("factor",3),rep("numeric",5))))
wage_12 = tbl_df(read.csv("output12_update.csv",colClasses=c(rep("factor",3),rep("numeric",5))))




#filtering Outliers
data_95 = wage_95 %>% 
  filter(EXPER>= 0,
         WAGE >= 4.25,
         WAGE <= 40)# %>%



# Setting thems and plotting descriptive stats
darkgray_theme=theme(text=element_text(size=8,  family="LM Roman 10", color = "gray45"), axis.text= element_text(size=9,  family="LM Roman 10", color = "gray45"), plot.title = element_text(hjust = 0.5), axis.ticks = element_line(color = "gray45"))

wage_hist = ggplot(data = data_95, aes(WAGE)) +
  geom_histogram(col = "white",
                 fill = "darkgreen",
                 binwidth = 3,
                 alpha=.5) +
  labs(title="Wage") +
  labs(x="Wage (Dollars)", y="")+
  darkgray_theme
lnwage_hist = ggplot(data = data_95, aes(LNWAGE)) +
  geom_histogram(col = "white",
                 fill = "darkgreen",
                 binwidth = .2,
                 alpha=.5) +
  labs(title="Log Wage") +
  labs(x="Log Wage (Dollars)", y="")+
  darkgray_theme
# grid.arrange(wage_hist, lnwage_hist, ncol =2)


# layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
age_hist = ggplot(data = data_95, aes(AGE)) +
  geom_histogram(col = "white",
                 fill = "darkgreen",
                 binwidth = 3,
                 alpha=.5) + 
  labs(title="Age") +
  labs(x="Years", y="Count")+
  darkgray_theme

educ_hist = ggplot(data = data_95, aes(EDUC)) +
  geom_histogram(col = "white",
                 fill = "darkgreen",
                 binwidth = 3,
                 alpha=.5) + 
  labs(title="Education") +
  labs(x="Years", y=" ")+
  darkgray_theme

exper_hist = ggplot(data = data_95, aes(EXPER)) +
  geom_histogram(col = "white",
                 fill = "darkgreen",
                 binwidth = 4,
                 alpha=.5) + 
  labs(title="Experience") +
  labs(x="Years", y=" ")+
  darkgray_theme

female_hist = ggplot(data = data_95, aes(data_95$FEMALE)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), 
           col = "white",
           fill = "darkgreen",
           alpha=.5) +
  labs(title="Female") +
  labs(x="", y="Percentage")+
  scale_y_continuous(labels=percent) +
  darkgray_theme

nonwhite_hist = ggplot(data = data_95, aes(as.character(data_95$NONWHITE))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),
           col = "white",
           fill = "darkgreen",
           alpha=.5) +
  labs(title="Non-White") +
  labs(x="", y="")+
  scale_y_continuous(labels=percent) +
  darkgray_theme

union_hist = ggplot(data = data_95, aes(as.character(data_95$UNION))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),
           col = "white",
           fill = "darkgreen",
           alpha=.5) +
  labs(title="Union") +
  labs(x="", y="")+
  scale_y_continuous(labels=percent) +
  darkgray_theme

grid.arrange(wage_hist, lnwage_hist, age_hist, educ_hist, exper_hist, female_hist, nonwhite_hist, union_hist, ncol = 3)

data_95 = data_95 %>%
  dplyr::select(FEMALE, NONWHITE, UNION, AGE, EXPER, EDUC, LNWAGE)

data_95[] <- lapply(data_95, function(x) as.numeric(as.character(x)))

corrplot(cor(data_95), method ="number", type = "lower")


# ggplot(data = data_95, aes(x = EXPER, y = LNWAGE)) +
#     geom_point(aes(color = as.factor(data_95$NONWHITE)) )

exper_scatter = ggplot(data = data_95, aes(x = EXPER, y =LNWAGE)) +
  geom_point(color = "darkgreen", alpha =0.4) +
  geom_smooth(method = "loess", se = F, color = "steelblue1")+
  geom_smooth(method = "lm", se = F, color = "firebrick3")+
  darkgray_theme

educ_scatter = ggplot(data = data_95, aes(x = EDUC, y =LNWAGE)) +
  geom_jitter(color = "darkgreen", alpha =0.4) +
  geom_smooth(method = "loess", se = F, color = "steelblue1")+
  geom_smooth(method = "lm", se = F, color = "firebrick3")+
  darkgray_theme

age_scatter = ggplot(data = data_95, aes(x = AGE, y =LNWAGE)) +
  geom_point(color = "darkgreen", alpha =0.4) +
  geom_smooth(method = "loess", se = F, color = "steelblue1")+
  geom_smooth(method = "lm", se = F, color = "firebrick3")+
  darkgray_theme

# ggplot()+
#   geom_point(data = data_95, aes(x = FEMALE, y = LNWAGE))

grid.arrange(exper_scatter, educ_scatter, ncol = 2)

### Dropped the negative experience person
data_95 = data_95 %>%
  filter(EXPER >= 0)%>%
  mutate(EXPER2 = EXPER^2,
         EDUC2  = EDUC^2,
         AGE2   = AGE^2  )


attach(data_95, warn.conflicts = F)
age_ols = lm(LNWAGE ~ FEMALE + NONWHITE + UNION + AGE + EDUC, data = data_95)
age_ols = lm(LNWAGE ~ FEMALE + NONWHITE + UNION + AGE + EDUC, data = data_95)
exper_ols = lm(LNWAGE ~ FEMALE + NONWHITE + UNION + EXPER + EDUC, data = data_95)
exper2_ols = lm(LNWAGE ~FEMALE + NONWHITE + UNION + EXPER + EDUC + EXPER2+
                  EXPER*EDUC, data = data_95)
exper_all2_ols = lm(LNWAGE ~FEMALE + NONWHITE+UNION+EXPER+EDUC+EXPER2+EDUC2 
                    +EXPER*EDUC+NONWHITE*UNION+FEMALE*UNION+FEMALE*NONWHITE, data = data_95)
exper_int_binary = lm(LNWAGE ~FEMALE + NONWHITE+UNION+EXPER+EDUC+EXPER*EDUC+
                        NONWHITE*UNION+FEMALE*UNION+FEMALE*NONWHITE, data = data_95)
exper2_educ_ols = lm(LNWAGE ~EXPER+EDUC+EXPER*EDUC+EXPER2, data = data_95)
exper2_educ2_ols = lm(LNWAGE ~EXPER+EDUC+EXPER*EDUC+EXPER2+EDUC2, data = data_95)


# summary(exper_ols)
# summary(exper2_ols)
# summary(exper_all2_ols)

# cv_age_ols = CVlm(data =data_95, form.lm = age_ols, m = 5, printit = F, plotit = F )
# tidy_age   =tidy(summary(age_ols))
# glance_age = glance(summary(age_ols))%>%
#   mutate(SIC = BIC(age_ols),
#          RMSE = sqrt(attr(cv_age_ols, "ms")), 
#          predictors = " Female, Nonwhite, Union, Age, Educ")%>%
#   setnames("statistic","f.statistic")

cv_exper_ols = CVlm(data =data_95, form.lm = exper_ols, m = 5, 
                    printit = F, plotit = F )
tidy_exper   =tidy(summary(exper_ols))
glance_exper = glance(summary(exper_ols))%>%
  mutate(SIC = BIC(exper_ols),
         RMSE = sqrt(attr(cv_exper_ols, "ms")), 
         predictors = " Female, Nonwhite, Union, Exper, Educ")%>%
  setnames("statistic","f.statistic")

cv_exper2_ols = CVlm(data =data_95, form.lm = exper2_ols, m = 5, 
                     printit = F, plotit = F )
tidy_exper2  =  tidy(summary(exper2_ols))
glance_exper2 = glance(summary(exper2_ols))%>%
  mutate(SIC = BIC(exper2_ols),
         RMSE = sqrt(attr(cv_exper2_ols, "ms")), 
         predictors = " Female, Nonwhite, Union, Exper, Educ, Exper^2, Exper*Educ")%>%
  setnames("statistic","f.statistic")

cv_exper_all2_ols = CVlm(data =data_95, form.lm = exper_all2_ols, m = 5, 
                         printit = F, plotit = F )
tidy_exper_all2 = tidy(summary(exper_all2_ols))
glance_exper_all2 = glance(summary(exper_all2_ols))%>%
  mutate(SIC = BIC(exper_all2_ols),
         RMSE = sqrt(attr(cv_exper_all2_ols, "ms")), 
         predictors = c("Female, Nonwhite, Union, Exper, Educ, Exper^2, Educ^2 ExperxEduc, NonwhitexUnion, FemalexUnion, FemalexNonwhite")) %>%
  setnames("statistic","f.statistic")

cv_exper_int_binary = CVlm(data =data_95, form.lm = exper_int_binary, m = 5, 
                           printit = F, plotit = F )
tidy_exper_int_binary = tidy(summary(exper_int_binary))
glance_exper_int_binary = glance(summary(exper_int_binary))%>%
  mutate(SIC = BIC(exper_int_binary),
         RMSE = sqrt(attr(cv_exper_int_binary, "ms")), 
         predictors = c("Female, Nonwhite, Union, Exper, Educ, ExperxEduc, NonwhitexUnion, FemalexUnion, FemalexNonwhite")) %>%
  setnames("statistic","f.statistic")

cv_exper2_educ_ols = CVlm(data =data_95, form.lm = exper2_educ_ols, m = 5, 
                          printit = F, plotit = F )
tidy_exper2_educ_ols = tidy(summary(exper2_educ_ols))
glance_exper2_educ_ols = glance(summary(exper2_educ_ols))%>%
  mutate(SIC = BIC(exper2_educ_ols),
         RMSE = sqrt(attr(cv_exper2_educ_ols, "ms")), 
         predictors = c(" Exper, Educ, ExperxEduc, Exper^2")) %>%
  setnames("statistic","f.statistic")

cv_exper2_educ2_ols = CVlm(data =data_95, form.lm = exper2_educ2_ols, m = 5, 
                           printit = F, plotit = F )
tidy_exper2_educ2_ols = tidy(summary(exper2_educ2_ols))
glance_exper2_educ2_ols = glance(summary(exper2_educ2_ols))%>%
  mutate(SIC = BIC(exper2_educ2_ols),
         RMSE = sqrt(attr(cv_exper2_educ2_ols, "ms")), 
         predictors = c(" Exper, Educ, ExperxEduc, Exper^2, Educ^2")) %>%
  setnames("statistic","f.statistic")


kable(list(glance_exper2_educ_ols, glance_exper2_educ2_ols, glance_exper,
           glance_exper2, glance_exper_all2, glance_exper_int_binary),
      caption = "Regression Models")





female_hist_lnwage = ggplot(data_95, aes(x = LNWAGE, fill = as.factor(FEMALE)))+
  geom_histogram(position= "identity",
                 bins = 20,
                 alpha = 0.4,
                 col = "white")+
  labs(title = "Log Wage broken down by Sex",
       x = "Ln Wage",
       y = "Count") +
  scale_fill_discrete(name="Sex",
                      breaks=c("0", "1"),
                      labels=c("Male", "Female"))+
  darkgray_theme


female_hist_exper = ggplot(data_95, aes(x = EXPER, fill = as.factor(FEMALE)))+
  geom_histogram(position= "identity",
                 bins = 20,
                 alpha = 0.4,
                 col = "white")+
  labs(title = "Experience broken down by Sex",
       x = "Experience",
       y = "Count") +
  scale_fill_discrete(name="Sex",
                      breaks=c("0", "1"),
                      labels=c("Male", "Female"))+
  darkgray_theme


data_95_female = data_95 %>%
  filter(FEMALE == 1, EXPER>=0)

data_95_male = data_95 %>%
  filter(FEMALE == 0, EXPER>=0)

grid.arrange(female_hist_lnwage, female_hist_exper, ncol =2)

# kable(summary(data_95_female), caption = "Females")
# kable(summary(data_95_male), caption = "Males")


female_95_lm = lm(LNWAGE ~FEMALE + NONWHITE + UNION + EXPER + EDUC + EXPER2+
                    EDUC2 + EXPER*EDUC, data = data_95_female )

male_95_lm = lm(LNWAGE ~ FEMALE + NONWHITE + UNION + EXPER + EDUC + EXPER2+
                  EDUC2 + EXPER*EDUC, data = data_95_male )


female_95_lm = lm(LNWAGE ~FEMALE + NONWHITE + UNION + EXPER + EDUC + EXPER2 
                  + EXPER*EDUC, data = data_95_female)


cv_female_95 = CVlm(data =data_95_female, form.lm = female_95_lm, m = 3, 
                    printit = F, plotit = F )
kable(tidy(summary(female_95_lm))%>%
        mutate(SIC = c(round(BIC(female_95_lm),2),"","","","","",""),
               adj.R2 = c(round(summary(female_95_lm)$adj.r.squared, 4), "",
                          "","","","",""),
               RMSE = c(round(sqrt(attr(cv_female_95, "ms")),3),"","","","",
                        "","" )),
      caption = "Female")




male_95_lm = lm(LNWAGE ~FEMALE + NONWHITE + UNION + EXPER + EDUC + EXPER2+
                  + EXPER*EDUC, data = data_95_male)
cv_male_95 = CVlm(data =data_95_male, form.lm = male_95_lm, m = 3, 
                  printit = F, plotit = F )
kable(tidy(summary(male_95_lm))%>%
        mutate(SIC = c(round(BIC(male_95_lm),2),"","","","","",""),
               adj.R2 = c(round(summary(male_95_lm)$adj.r.squared, 4), "",
                          "","","","",""),
               RMSE = c(round(sqrt(attr(cv_male_95, "ms")),3),"","","","",
                        "","" )),
      caption = "Male")



kable(tidy(summary(exper2_ols)) %>% setnames("statistic","t.statistic")%>%
        mutate(SIC = c(round(BIC(exper2_ols),2),"","","","","","",""),
               adj.R2 = c(round(summary(exper2_ols)$adj.r.squared, 4), "","",
                          "","","","",""),
               RMSE = c(round(sqrt(attr(cv_exper2_ols, "ms")),3),"","","","","",
                        "","" )))

#plots to test heteroscedasticity
data_95$resid = exper2_ols$residuals

lnwage_vsresid = ggplot(data = data_95, aes(LNWAGE, y =resid^2)) +
  geom_point(col = "steelblue",
             alpha=.5) +
  labs(title="Log Wages vs Residuals") +
  labs(x="Log Wage", y="Unstandardized Residuals")+
  darkgray_theme
exper_vsresid = ggplot(data = data_95, aes(EXPER, y =resid^2)) +
  geom_point(col = "steelblue",
             alpha=.5) +
  labs(title="Experience vs Residuals") +
  labs(x="Experience", y="Unstandardized Residuals")+
  darkgray_theme

educ_vsresid = ggplot(data = data_95, aes(EDUC, y =resid^2)) +
  geom_point(col = "steelblue",
             alpha=.5) +
  labs(title="Education vs Residuals") +
  labs(x="Education", y="Unstandardized Residuals")+
  darkgray_theme

grid.arrange(exper_vsresid, educ_vsresid, lnwage_vsresid, ncol = 3)


#formal tests for heteroscedasticity

bgp_ols = lm(exper2_ols$residuals^2 ~ EDUC+EXPER+EXPER2+NONWHITE+FEMALE+UNION+EXPER*EDUC, 
             data = data_95)
bgp_stat = nrow(data_95)*(summary(bgp_ols)$r.squared)
#### bgpstat = 36, p-value is 0, we reject the null of homoscedasticity

# equivalently could use this one liner from lmtest pkg
kable(tidy(bptest(exper2_ols))%>% setnames("statistic","bgp_statistic")%>% 
        select(bgp_statistic, p.value, parameter))

# kable(tidy(summary(bgp_ols)))

whitelm<-lm(exper2_ols$residuals^2 ~ EDUC+EXPER+EXPER2+EDUC^2+NONWHITE+FEMALE+UNION+
              EXPER*EDUC+NONWHITE*FEMALE+NONWHITE*UNION+UNION*FEMALE, data = data_95)

sd_resid = sqrt(predict(bgp_ols, data_95))
std_resid = exper2_ols$residuals / sd_resid
bgp_stat = nrow(data_95)*(summary(bgp_ols)$r.squared)

data_95$std_resid = std_resid

std_res_hist = ggplot()+
  geom_histogram(aes(data_95$std_resid),
                 col = "white",
                 fill = "darkgreen",
                 binwidth = .4,
                 alpha=.5) +
  labs(title="Std Res. Histogram") +
  labs(x="Std Residuals", y="")+
  darkgray_theme


#Code for creating qqline
y     <- quantile(std_resid, c(0.25, 0.75)) # Find the 1st and 3rd quartiles
x     <- qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
slope <- diff(y) / diff(x)             # Compute the line slope
int   <- y[1] - slope * x[1]           # Compute the line intercept

std_res_qqplot = ggplot()+
  geom_qq(aes(sample = std_resid), 
          alpha = 0.4) +
  geom_abline(intercept=int, 
              slope=slope,
              col = "red")+ 
  ylab("actual")+
  ggtitle("QQ Plot Std Res.")+
  darkgray_theme

### stat = 36, p-value is 0, we reject the null of homoscedasticity

grid.arrange(std_res_hist, std_res_qqplot, ncol = 2)

# Print out table of JB test
kable(tbl_df(t(unlist(jarque.bera.test(data_95$std_resid))))%>%
        setnames("statistic.X-squared","JB.test.stat") %>%
        setnames("p.value.X-squared", "p.value") %>%
        setnames("parameter.df", "df") %>%
        mutate(JB.test.stat = (round(as.numeric(JB.test.stat), 3)),
               p.value      = (round(as.numeric(p.value),3))) %>%
        select(JB.test.stat, p.value, df, method))


# Dont need plot of std residuals vs xs and ys bc its the same as above
# lnwage_vs_stdresid = ggplot(data = data_95, aes(LNWAGE, y =std_resid^2)) +
#   geom_point(col = "steelblue",
#             alpha=.5) +
#             labs(title="Log Wages vs Residuals") +
#             labs(x="Log Wage", y="Unstandardized Residuals")+
#             darkgray_theme
# exper_vs_stdresid = ggplot(data = data_95, aes(EXPER, y =std_resid^2)) +
#   geom_point(col = "steelblue",
#             alpha=.5) +
#             labs(title="Experience vs Residuals") +
#             labs(x="Experience", y="Unstandardized Residuals")+
#             darkgray_theme
# 
# educ_vs_stdresid = ggplot(data = data_95, aes(EDUC, y =std_resid^2)) +
#   geom_point(col = "steelblue",
#             alpha=.5) +
#             labs(title="Education vs Residuals") +
#             labs(x="Education", y="Unstandardized Residuals")+
#             darkgray_theme
# grid.arrange(exper_vs_stdresid, educ_vs_stdresid, lnwage_vs_stdresid, ncol = 3)


# Simulation algorithm for density prediction

# draw_std_res =  sample(data_95$std_resid, 10000, replace = TRUE)
# draw_fitted_val=sample(exper2_ols$fitted.values, 10000, replace = TRUE)
# full_draw = draw_std_res +draw_fitted_val
new_arrival = data.frame(UNION =1, FEMALE = 1, NONWHITE = 0, EDUC = 12, 
                         EXPER = 3, EXPER2 = 9, EDUC2 = 12^2, AGE = 0, 
                         AGE2 = 0, xxx = 10, yyy = 10)

sample_std_res = sample(data_95$std_resid, 10000, replace = TRUE)
new_arrival_sd = sqrt(predict(bgp_ols,new_arrival))

# new_arrival_sd=sqrt(predict(whitelm,new_arrival))
new_arrival_mean = predict(exper2_ols, new_arrival)
new_arrival_dist = sample_std_res*new_arrival_sd + new_arrival_mean


new_arrival_sd = sqrt(predict(bgp_ols,new_arrival))
dist_new_arrival = sample_std_res*new_arrival_sd + new_arrival_mean


new_arrival_distribution_hist = ggplot()+
  geom_histogram(aes(dist_new_arrival),
                 col = "white",
                 fill = "darkgreen",
                 binwidth = .09,
                 alpha=.5) +
  labs(title="Density Prediction ") +
  labs(x="Log Wage", y="Count")+
  darkgray_theme

new_person_predictions = cbind(Point_estimate = new_arrival_mean, 
                               data.frame(as.list(quantile(new_arrival_dist, c(0.025,0.975)))))


new_arrival_distribution_hist  
kable(new_person_predictions)


#Wage_04 has 1941 rows, 2 of which have negative experoence, 15 of which have WAGE > 60, and 48 rows less than 5.15 (min wage)
data_04 = wage_04 %>% 
  filter(EXPER>= 0,
         WAGE >= 5.15,
         WAGE <= 60)# %>%
# mutate(DIFF = (AGE-EXPER)) %>%
# filter(DIFF>=14)
#kable(summary(data_04))  

#wage_12 has 1229 rows, 2 of which have negative experience, 16 of whom have wages higher than 63, and 24 rows less thatn 7.35

data_12 = wage_12 %>% 
  filter(EXPER>= 0,
         WAGE >= 7.25,
         WAGE <= 63)

### Dropped the negative experience person
data_04 = data_04 %>%
  filter(EXPER >= 0)%>%
  mutate(EXPER2 = EXPER^2,
         EDUC2  = EDUC^2,
         AGE2   = AGE^2  )


# attach(data_95, warn.conflicts = F)
age_ols_04 = lm(LNWAGE ~ FEMALE + NONWHITE + UNION + AGE + EDUC, data = data_04)
exper_ols_04 = lm(LNWAGE ~ FEMALE + NONWHITE + UNION + EXPER + EDUC, data = data_04)
exper2_ols_04 = lm(LNWAGE ~FEMALE + NONWHITE + UNION + EXPER+EXPER2+EDUC+
                     EXPER*EDUC, data = data_04)
exper_all2_ols_04 = lm(LNWAGE ~FEMALE + NONWHITE+UNION+EXPER+EDUC+EXPER2+EDUC2 
                       +EXPER*EDUC+NONWHITE*UNION+FEMALE*UNION+FEMALE*NONWHITE, data = data_04)
exper_int_binary_04 = lm(LNWAGE ~FEMALE + NONWHITE+UNION+EXPER+EDUC+EXPER*EDUC+
                           NONWHITE*UNION+FEMALE*UNION+FEMALE*NONWHITE, data = data_04)
exper2_educ_ols_04 = lm(LNWAGE ~EXPER+EDUC+EXPER*EDUC+EXPER2, data = data_04)
exper2_educ2_ols_04 = lm(LNWAGE ~EXPER+EDUC+EXPER*EDUC+EXPER2+EDUC2+FEMALE+
                           NONWHITE+UNION, data = data_04)


# summary(exper_ols)
# summary(exper2_ols)
# summary(exper_all2_ols)

# cv_age_ols = CVlm(data =data_95, form.lm = age_ols, m = 5, printit = F, plotit = F )
# tidy_age   =tidy(summary(age_ols))
# glance_age = glance(summary(age_ols))%>%
#   mutate(SIC = BIC(age_ols),
#          RMSE = sqrt(attr(cv_age_ols, "ms")), 
#          predictors = " Female, Nonwhite, Union, Age, Educ")%>%
#   setnames("statistic","f.statistic")

cv_exper_ols_04 = CVlm(data =data_04, form.lm = exper_ols_04, m = 5, 
                       printit = F, plotit = F )
tidy_exper_04   =tidy(summary(exper_ols_04))
glance_exper_04 = glance(summary(exper_ols_04))%>%
  mutate(SIC = BIC(exper_ols_04),
         RMSE = sqrt(attr(cv_exper_ols_04, "ms")), 
         predictors = " Female, Nonwhite, Union, Exper, Educ")%>%
  setnames("statistic","f.statistic")

cv_exper2_ols_04 = CVlm(data =data_04, form.lm = exper2_ols_04, m = 5, 
                        printit = F, plotit = F )
tidy_exper2_04  =  tidy(summary(exper2_ols_04))
glance_exper2_04 = glance(summary(exper2_ols_04))%>%
  mutate(SIC = BIC(exper2_ols_04),
         RMSE = sqrt(attr(cv_exper2_ols_04, "ms")), 
         predictors = " Female, Nonwhite, Union, Exper, Educ, Exper^2, Exper*Educ")%>%
  setnames("statistic","f.statistic")

cv_exper_all2_ols_04 = CVlm(data =data_04, form.lm = exper_all2_ols_04, m = 5, 
                            printit = F, plotit = F )
tidy_exper_all2_04 = tidy(summary(exper_all2_ols_04))
glance_exper_all2_04 = glance(summary(exper_all2_ols_04))%>%
  mutate(SIC = BIC(exper_all2_ols_04),
         RMSE = sqrt(attr(cv_exper_all2_ols_04, "ms")), 
         predictors = c("Female, Nonwhite, Union, Exper, Educ, Exper^2, Educ^2 ExperxEduc, 
                        NonwhitexUnion, FemalexUnion, FemalexNonwhite")) %>%
  setnames("statistic","f.statistic")

cv_exper_int_binary_04 = CVlm(data =data_04, form.lm = exper_int_binary_04, m = 5, 
                              printit = F, plotit = F )
tidy_exper_int_binary_04 = tidy(summary(exper_int_binary_04))
glance_exper_int_binary_04 = glance(summary(exper_int_binary_04))%>%
  mutate(SIC = BIC(exper_int_binary_04),
         RMSE = sqrt(attr(cv_exper_int_binary_04, "ms")), 
         predictors = c("Female, Nonwhite, Union, Exper, Educ, ExperxEduc, NonwhitexUnion, 
                        FemalexUnion, FemalexNonwhite")) %>%
  setnames("statistic","f.statistic")

cv_exper2_educ_ols_04 = CVlm(data =data_04, form.lm = exper2_educ_ols_04, m = 5, 
                             printit = F, plotit = F )
tidy_exper2_educ_ols_04 = tidy(summary(exper2_educ_ols_04))
glance_exper2_educ_ols_04 = glance(summary(exper2_educ_ols_04))%>%
  mutate(SIC = BIC(exper2_educ_ols_04),
         RMSE = sqrt(attr(cv_exper2_educ_ols_04, "ms")), 
         predictors = c(" Exper, Educ, ExperxEduc, Exper^2")) %>%
  setnames("statistic","f.statistic")

cv_exper2_educ2_ols_04 = CVlm(data =data_04, form.lm = exper2_educ2_ols_04, m = 5, 
                              printit = F, plotit = F )
tidy_exper2_educ2_ols_04 = tidy(summary(exper2_educ2_ols_04))
glance_exper2_educ2_ols_04 = glance(summary(exper2_educ2_ols_04))%>%
  mutate(SIC = BIC(exper2_educ2_ols_04),
         RMSE = sqrt(attr(cv_exper2_educ2_ols_04, "ms")), 
         predictors = c(" Exper, Educ, ExperxEduc, Exper^2, Educ^2, Female, NonWhite, Union")) %>%
  setnames("statistic","f.statistic")


kable(list(glance_exper2_educ_ols_04, glance_exper2_educ2_ols_04, glance_exper_04,
           glance_exper2_04, glance_exper_all2_04, 
           glance_exper_int_binary_04),
      caption = "Regression Models")


### Dropped the negative experience person
data_12 = data_12 %>%
  filter(EXPER >= 0)%>%
  mutate(EXPER2 = EXPER^2,
         EDUC2  = EDUC^2,
         AGE2   = AGE^2  )


# attach(data_95, warn.conflicts = F)
age_ols_12 = lm(LNWAGE ~ FEMALE + NONWHITE + UNION + AGE + EDUC, data = data_12)
exper_ols_12 = lm(LNWAGE ~ FEMALE + NONWHITE + UNION + EXPER + EDUC, data = data_12)
exper2_ols_12 = lm(LNWAGE ~FEMALE + NONWHITE + UNION + EXPER+EXPER2+EDUC+
                     EXPER*EDUC, data = data_12)
exper_all2_ols_12 = lm(LNWAGE ~FEMALE + NONWHITE+UNION+EXPER+EDUC+EXPER2+EDUC2 
                       +EXPER*EDUC+NONWHITE*UNION+FEMALE*UNION+FEMALE*NONWHITE, data = data_12)
exper_int_binary_12 = lm(LNWAGE ~FEMALE + NONWHITE+UNION+EXPER+EDUC+EXPER*EDUC+
                           NONWHITE*UNION+FEMALE*UNION+FEMALE*NONWHITE, data = data_12)
exper2_educ_ols_12 = lm(LNWAGE ~EXPER+EDUC+EXPER*EDUC+EXPER2, data = data_12)
exper2_educ2_ols_12 = lm(LNWAGE ~EXPER+EDUC+EXPER*EDUC+EXPER2+EDUC2+FEMALE+
                           NONWHITE+UNION, data = data_12)
ols_final_12 = lm(LNWAGE ~ EXPER+ EXPER2+ EDUC2+ FEMALE+ UNION+ EDUC*EXPER, data = data_12)

# summary(exper_ols)
# summary(exper2_ols)
# summary(exper_all2_ols)

# cv_age_ols = CVlm(data =data_95, form.lm = age_ols, m = 5, printit = F, plotit = F )
# tidy_age   =tidy(summary(age_ols))
# glance_age = glance(summary(age_ols))%>%
#   mutate(SIC = BIC(age_ols),
#          RMSE = sqrt(attr(cv_age_ols, "ms")), 
#          predictors = " Female, Nonwhite, Union, Age, Educ")%>%
#   setnames("statistic","f.statistic")

cv_exper_ols_12 = CVlm(data =data_12, form.lm = exper_ols_12, m = 5, 
                       printit = F, plotit = F )
tidy_exper_12   =tidy(summary(exper_ols_12))
glance_exper_12 = glance(summary(exper_ols_12))%>%
  mutate(SIC = BIC(exper_ols_12),
         RMSE = sqrt(attr(cv_exper_ols_12, "ms")), 
         predictors = " Female, Nonwhite, Union, Exper, Educ")%>%
  setnames("statistic","f.statistic")

cv_exper2_ols_12 = CVlm(data =data_12, form.lm = exper2_ols_12, m = 5, 
                        printit = F, plotit = F )
tidy_exper2_12  =  tidy(summary(exper2_ols_12))
glance_exper2_12 = glance(summary(exper2_ols_12))%>%
  mutate(SIC = BIC(exper2_ols_12),
         RMSE = sqrt(attr(cv_exper2_ols_12, "ms")), 
         predictors = " Female, Nonwhite, Union, Exper, Educ, Exper^2, Exper*Educ")%>%
  setnames("statistic","f.statistic")

cv_exper_all2_ols_12 = CVlm(data =data_12, form.lm = exper_all2_ols_12, m = 5, 
                            printit = F, plotit = F )
tidy_exper_all2_12 = tidy(summary(exper_all2_ols_12))
glance_exper_all2_12 = glance(summary(exper_all2_ols_12))%>%
  mutate(SIC = BIC(exper_all2_ols_12),
         RMSE = sqrt(attr(cv_exper_all2_ols_12, "ms")), 
         predictors = c("Female, Nonwhite, Union, Exper, Educ, Exper^2, Educ^2 ExperxEduc, 
                        NonwhitexUnion, FemalexUnion, FemalexNonwhite")) %>%
  setnames("statistic","f.statistic")

cv_exper_int_binary_12 = CVlm(data =data_12, form.lm = exper_int_binary_12, m = 5, 
                              printit = F, plotit = F )
tidy_exper_int_binary_12 = tidy(summary(exper_int_binary_12))
glance_exper_int_binary_12 = glance(summary(exper_int_binary_12))%>%
  mutate(SIC = BIC(exper_int_binary_12),
         RMSE = sqrt(attr(cv_exper_int_binary_12, "ms")), 
         predictors = c("Female, Nonwhite, Union, Exper, Educ, ExperxEduc, NonwhitexUnion, 
                        FemalexUnion, FemalexNonwhite")) %>%
  setnames("statistic","f.statistic")

cv_exper2_educ_ols_12 = CVlm(data =data_12, form.lm = exper2_educ_ols_12, m = 5, 
                             printit = F, plotit = F )
tidy_exper2_educ_ols_12 = tidy(summary(exper2_educ_ols_12))
glance_exper2_educ_ols_12 = glance(summary(exper2_educ_ols_12))%>%
  mutate(SIC = BIC(exper2_educ_ols_12),
         RMSE = sqrt(attr(cv_exper2_educ_ols_12, "ms")), 
         predictors = c(" Exper, Educ, ExperxEduc, Exper^2")) %>%
  setnames("statistic","f.statistic")

cv_exper2_educ2_ols_12 = CVlm(data =data_12, form.lm = exper2_educ2_ols_12, m = 5, 
                              printit = F, plotit = F )
tidy_exper2_educ2_ols_12 = tidy(summary(exper2_educ2_ols_12))
glance_exper2_educ2_ols_12 = glance(summary(exper2_educ2_ols_12))%>%
  mutate(SIC = BIC(exper2_educ2_ols_12),
         RMSE = sqrt(attr(cv_exper2_educ2_ols_12, "ms")), 
         predictors = c(" Exper, Educ, ExperxEduc, Exper^2, Educ^2, Female, NonWhite, Union")) %>%
  setnames("statistic","f.statistic")


kable(list(glance_exper2_educ_ols_12, glance_exper2_educ2_ols_12, glance_exper_12,
           glance_exper2_12, glance_exper_all2_12, 
           glance_exper_int_binary_12),
      caption = " 2012 Regression Models")



### FINAL MODEL FOR 2004 = exper2_educ2_ols_04
kable(tidy(summary(exper2_educ2_ols_04)) %>% setnames("statistic","t.statistic")%>%
        mutate(SIC = c(round(BIC(exper2_educ2_ols_04),2),"","","","","","","",""),
               adj.R2 = c(round(summary(exper2_educ2_ols_04)$adj.r.squared, 4), "","",
                          "","","","","",""),
               RMSE = c(round(sqrt(attr(cv_exper2_educ2_ols_04, "ms")),3),"","","","","",
                        "","","" )))
### FINAL MODEL FOR 2012 = exper2_ols_12
kable(tidy(summary(exper2_ols_12)) %>% setnames("statistic","t.statistic")%>%
        mutate(SIC = c(round(BIC(exper2_ols_12),2),"","","","","","",""),
               adj.R2 = c(round(summary(exper2_ols_12)$adj.r.squared, 4), "","",
                          "","","","",""),
               RMSE = c(round(sqrt(attr(cv_exper2_ols_12, "ms")),3),"","","","","",
                        "","" )))
#plots to test heteroscedasticity
data_04$resid = exper2_educ2_ols_04$residuals
data_04$fitted = exper2_educ2_ols_04$fitted.values

lnwage_vsresid_04 = ggplot(data = data_04, aes(LNWAGE, y =resid^2)) +
  geom_point(col = "steelblue",
             alpha=.5) +
  labs(title="Log Wages vs Residuals^2") +
  labs(x="Log Wage", y="Raw Residuals^2")+
  darkgray_theme
exper_vsresid_04 = ggplot(data = data_04, aes(EXPER, y =resid^2)) +
  geom_point(col = "steelblue",
             alpha=.5) +
  labs(title="Experience vs Residuals^2") +
  labs(x="Experience", y="Raw Residuals^2")+
  darkgray_theme

educ_vsresid_04 = ggplot(data = data_04, aes(EDUC, y =resid^2)) +
  geom_point(col = "steelblue",
             alpha=.5) +
  labs(title="Education vs Residuals^2") +
  labs(x="Education", y="Raw Residuals^2")+
  darkgray_theme

grid.arrange(exper_vsresid_04, educ_vsresid_04, lnwage_vsresid_04, ncol = 3)


#formal tests for heteroscedasticity

bgp_ols_04 = lm(exper2_educ2_ols_04$residuals^2 ~ EDUC+EXPER+EXPER2+EDUC2+NONWHITE+FEMALE+UNION+EXPER*EDUC, 
                data = data_04)
bgp_stat_04 = nrow(data_04)*(summary(bgp_ols_04)$r.squared)
#### bgpstat = 36, p-value is 0, we reject the null of homoscedasticity

# equivalently could use this one liner from lmtest pkg
kable(tidy(bptest(exper2_educ2_ols_04))%>% setnames("statistic","bgp_statistic")%>% 
        select(bgp_statistic, p.value, parameter))

# kable(tidy(summary(bgp_ols)))


data_04$sq_resid = exper2_educ2_ols_04$residuals^2 

whitelm_04<-lm(exper2_educ2_ols_04$residuals^2  ~ EDUC+EXPER+EXPER2+EDUC2+NONWHITE+FEMALE+UNION+
                 EXPER*EDUC+NONWHITE*FEMALE+NONWHITE*UNION+UNION*FEMALE, data = data_04)
#plots to test heteroscedasticity
data_12$resid = exper2_educ2_ols_12$residuals
data_12$fitted = exper2_educ2_ols_12$fitted.values

lnwage_vsresid_12 = ggplot(data = data_12, aes(LNWAGE, y =resid^2)) +
  geom_point(col = "steelblue",
             alpha=.5) +
  labs(title="Log Wages vs Residuals^2") +
  labs(x="Log Wage", y="Raw Residuals^2")+
  darkgray_theme
exper_vsresid_12 = ggplot(data = data_12, aes(EXPER, y =resid^2)) +
  geom_point(col = "steelblue",
             alpha=.5) +
  labs(title="Experience vs Residuals^2") +
  labs(x="Experience", y="Raw Residuals^2")+
  darkgray_theme

educ_vsresid_12 = ggplot(data = data_12, aes(EDUC, y =resid^2)) +
  geom_point(col = "steelblue",
             alpha=.5) +
  labs(title="Education vs Residuals^2") +
  labs(x="Education", y="Raw Residuals^2")+
  darkgray_theme

grid.arrange(exper_vsresid_12, educ_vsresid_12, lnwage_vsresid_12, ncol = 3)


#formal tests for heteroscedasticity

bgp_ols_12 = lm(exper2_educ2_ols_12$residuals^2 ~ EDUC+EXPER+EXPER2+EDUC2+NONWHITE+FEMALE+UNION+EXPER*EDUC, 
                data = data_12)
bgp_stat_12 = nrow(data_12)*(summary(bgp_ols_12)$r.squared)
#### bgpstat = 36, p-value is 0, we reject the null of homoscedasticity

# equivalently could use this one liner from lmtest pkg
kable(tidy(bptest(exper2_educ2_ols_12))%>% setnames("statistic","bgp_statistic")%>% 
        select(bgp_statistic, p.value, parameter))

# kable(tidy(summary(bgp_ols)))


data_12$sq_resid = exper2_educ2_ols_12$residuals^2 

whitelm_12<-lm(exper2_educ2_ols_12$residuals^2  ~ EDUC+EXPER+EXPER2+EDUC2+NONWHITE+FEMALE+UNION+
                 EXPER*EDUC+NONWHITE*FEMALE+NONWHITE*UNION+UNION*FEMALE, data = data_12)

sd_resid_04 = sqrt(predict(whitelm_04, data_04))
std_resid_04= exper2_educ2_ols_04$residuals / sd_resid_04
bgp_stat_04 = nrow(data_04)*(summary(bgp_ols_04)$r.squared)

data_04$sd_resid_04 = sd_resid_04
data_04$std_resid_04 = std_resid_04


std_res_hist_04 = ggplot()+
  geom_histogram(aes(data_04$std_resid_04),
                 col = "white",
                 fill = "darkgreen",
                 binwidth = .4,
                 alpha=.5) +
  labs(title="Std Res. Histogram") +
  labs(x="Std Residuals", y="Count")+
  darkgray_theme


#Code for creating qqline
y1     <- quantile(std_resid_04, c(0.25, 0.75), na.rm=TRUE) # Find the 1st and 3rd quartiles
x1    <- qnorm(c(0.25, 0.75))             # Find the matching normal values on the x-axis
slope1 <- diff(y1) / diff(x1)             # Compute the line slope
int1   <- y1[1] - slope * x1[1]           # Compute the line intercept

std_res_qqplot_04 = ggplot()+
  geom_qq(aes(sample = std_resid_04), 
          alpha = 0.4) +
  geom_abline(intercept=int1, 
              slope=slope1,
              col = "red")+ 
  ylab("actual")+
  ggtitle("QQ Plot Std Res.")+
  darkgray_theme

### stat = 36, p-value is 0, we reject the null of homoscedasticity

grid.arrange(std_res_hist_04, std_res_qqplot_04, ncol = 2)

# Print out table of JB test
kable(tbl_df(t(unlist(jarque.bera.test(na.omit(data_04$std_resid_04)))))%>%
        setnames("statistic.X-squared","JB.test.stat") %>%
        setnames("p.value.X-squared", "p.value") %>%
        setnames("parameter.df", "df") %>%
        mutate(JB.test.stat = (round(as.numeric(JB.test.stat), 3)),
               p.value      = (round(as.numeric(p.value),3))) %>%
        select(JB.test.stat, p.value, df, method))


sd_resid_12 = sqrt(predict(whitelm_12, data_12))
std_resid_12= exper2_educ2_ols_12$residuals / sd_resid_12
bgp_stat_12 = nrow(data_12)*(summary(bgp_ols_12)$r.squared)

data_12$sd_resid_12 = sd_resid_12
data_12$std_resid_12 = std_resid_12


std_res_hist_12 = ggplot()+
  geom_histogram(aes(data_12$std_resid_12),
                 col = "white",
                 fill = "darkgreen",
                 binwidth = .4,
                 alpha=.5) +
  labs(title="Std Res. Histogram") +
  labs(x="Std Residuals", y="Count")+
  darkgray_theme


#Code for creating qqline
y1     <- quantile(std_resid_12, c(0.25, 0.75), na.rm=TRUE) # Find the 1st and 3rd quartiles
x1    <- qnorm(c(0.25, 0.75))             # Find the matching normal values on the x-axis
slope1 <- diff(y1) / diff(x1)             # Compute the line slope
int1   <- y1[1] - slope * x1[1]           # Compute the line intercept

std_res_qqplot_12 = ggplot()+
  geom_qq(aes(sample = std_resid_12), 
          alpha = 0.4) +
  geom_abline(intercept=int1, 
              slope=slope1,
              col = "red")+ 
  ylab("actual")+
  ggtitle("QQ Plot Std Res.")+
  darkgray_theme

### stat = 36, p-value is 0, we reject the null of homoscedasticity

grid.arrange(std_res_hist_12, std_res_qqplot_12, ncol = 2)

# Print out table of JB test
kable(tbl_df(t(unlist(jarque.bera.test(na.omit(data_12$std_resid_12)))))%>%
        setnames("statistic.X-squared","JB.test.stat") %>%
        setnames("p.value.X-squared", "p.value") %>%
        setnames("parameter.df", "df") %>%
        mutate(JB.test.stat = (round(as.numeric(JB.test.stat), 3)),
               p.value      = (round(as.numeric(p.value),3))) %>%
        select(JB.test.stat, p.value, df, method))

# Simulation algorithm for density prediction

new_arrival_04 = data.frame(UNION =as.factor(1), FEMALE = as.factor(1), NONWHITE = as.factor(0), 
                            EDUC = 12, EXPER = 3, EXPER2 = 9, EDUC2 = 12^2, AGE = 0, 
                            AGE2 = 0, xxx = 10, yyy = 10)

sample_std_res_04 = sample(na.omit(data_04$std_resid_04), 10000, replace = TRUE)
new_arrival_sd_04 = sqrt(predict(whitelm_04,new_arrival_04))

# new_arrival_sd=sqrt(predict(whitelm,new_arrival))
new_arrival_mean_04 = predict(exper2_educ2_ols_04, new_arrival_04)
new_arrival_dist_04 = sample_std_res_04*new_arrival_sd_04 + new_arrival_mean_04




new_arrival_distribution_hist_04 = ggplot()+
  geom_histogram(aes(new_arrival_dist_04),
                 col = "white",
                 fill = "darkgreen",
                 binwidth = .09,
                 alpha=.5) +
  labs(title="Density Prediction ") +
  labs(x="Log Wage", y="Count")+
  darkgray_theme

new_person_predictions_04 = cbind(Point_estimate = new_arrival_mean_04, 
                                  data.frame(as.list(quantile(new_arrival_dist_04, c(0.025,0.975)))))

new_arrival_distribution_hist_04  
kable(new_person_predictions_04)

# Simulation algorithm for density prediction

new_arrival_12 = data.frame(UNION =as.factor(1), FEMALE = as.factor(1), NONWHITE = as.factor(0), 
                            EDUC = 12, EXPER = 3, EXPER2 = 9, EDUC2 = 12^2, AGE = 0, 
                            AGE2 = 0, xxx = 10, yyy = 10)

sample_std_res_12 = sample(na.omit(data_12$std_resid_12), 10000, replace = TRUE)
new_arrival_sd_12 = sqrt(predict(whitelm_12,new_arrival_12))

# new_arrival_sd=sqrt(predict(whitelm,new_arrival))
new_arrival_mean_12 = predict(exper2_ols_12, new_arrival_12)
new_arrival_dist_12 = sample_std_res_12*new_arrival_sd_12 + new_arrival_mean_12




new_arrival_distribution_hist_12 = ggplot()+
  geom_histogram(aes(new_arrival_dist_12),
                 col = "white",
                 fill = "darkgreen",
                 binwidth = .09,
                 alpha=.5) +
  labs(title="Density Prediction ") +
  labs(x="Log Wage", y="Count")+
  darkgray_theme

new_person_predictions_12 = cbind(Point_estimate = new_arrival_mean_12, 
                                  data.frame(as.list(quantile(new_arrival_dist_12, c(0.025,0.975)))))

new_arrival_distribution_hist_12
kable(new_person_predictions_12)
