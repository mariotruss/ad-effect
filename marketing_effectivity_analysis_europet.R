
summary(data)

fit<-lm(Sales ~ TV+Radio, data=data)
summary(fit)

#Radio
plot(Sales ~ Radio, data=data)
fit_radio <- lm(Sales ~ Radio, data=data)
summary(fit_radio)
abline(fit_radio, col = "blue")
plot(fit_radio$fitted.values, fit_radio$residuals, main = "Scatterplot",ylab="Residuals_Radio",xlab="Predicted Values")

#TV
plot(Sales ~ TV, data=data)
fit_tv <- lm(Sales ~ TV, data=data)
summary(fit_tv)
abline(fit_tv, col = "red")
plot(fit_tv$fitted.values, fit_tv$residuals, main = "Scatterplot",ylab="Residuals_TV",xlab="Predicted Values")

#TV & Radio
fit_tvandradio <- lm(Sales ~ TV+Radio, data=data)
summary(fit_tvandradio)

#confidence
new_dat <- data.frame(TV=40, Radio=80)
predict(fit_tvandradio, newdata = new_dat, interval = 'predict',level = 0.95)

#add temperature
fit_plustemp <- lm(Sales ~ TV+Radio+Temp, data=data)
summary(fit_plustemp)

#add temperature
fit_all <- lm(Sales ~ TV+Temp+Fuelvolume+Fuelprice+Prec+Holiday+Visits12, data=data)
summary(fit_all)

confint(fit_all,level=0.95)*5
names(data)