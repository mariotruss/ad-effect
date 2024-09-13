# install.packages("datarium") 
library(datarium) # for marketing data set
data(marketing)

# number of observations.
nrow(marketing)
head(marketing)
# Marketing dataset contains 200 observations

# median of sales and the mean of newspaper?
summary(marketing)
str(marketing)
# Your answer: Median Sales = 15.48 (in 1000 Sold Units) & Mean Newspaper = 36.66


# correlation between the sales and youtube, 
# Correlation can range from -1 to 1 and indicate strenghts of linear relationship

cor(marketing$sales,marketing$youtube)
# Correlation between the sales and youtube is: 0.7822244
# YouTube ads (measured by a budget of 1000 USD per Unit) is most positively correlated with sales

cor(marketing$sales,marketing$facebook)
# Correlation between the sales and facebook is: 0.5762226
# Facebook ads (measured by a budget of 1000 USD per Unit) is second most positively correlated with sales

cor(marketing$sales,marketing$newspaper)
# Correlation between the sales and newspaper is: 0.228299
# Newspaper ads (measured by a budget of 1000 USD per Unit) is least positively correlated with sales

# This means that there is linear relationship (correlation) between more ads sales, but it varies between the media/ads types
# Correlation does not imply causation and would need to be further evaluated, e.g. via an A/B test with Adobe Target or business experiment


# Plot a histogram and a boxplot of the sales variable. Include correct descriptions of 
hist(marketing$sales,xlab="Sales",ylab="Frequency",main="Distribution of Sales")
# Looks normally distributed
boxplot(marketing$sales,ylab="Sales",main="Sales's Boxplot") # 25%, 50% % 75% percentiles

# Generate a linear regression model that explains the sales depending on ads in 
# youtube and facebook. What is the value of the estimate of the intercept?
fit <- lm(data = marketing, sales ~ youtube + facebook) # sales is predicted from youtube & facebook ads
summary(fit)

# Estimate of intercept = 3.50532

# Interpretation of the Estimate for youtube ads in your previous model.
# On average, and all else equal, we can expect:
# The estimate 0.04575 for YouTube ads (1000 USD budget) seems to have statistically significant impacts on sales, as measured by t-value 32.90 (more than ~2) and Pr <2e-16, as well as the signifance code *** 
# This means that we can reject the h0 (youtube ads have not impact on sales) and therefore can believe in h1 (youtube ads impact sales)
# In clear teams, this means that we can expected sales (1000 Unit sold) to rise by 0.04575 with every 1000 USD YouTube ads budget invested
# This also means that the adspend on YouTube should not be higher than an the estimated increase in sales -> negative ROI

# Optional: We should potentially also investigate the interaction between youtube & facebook (combined effect of) in the future

# 99% Confidence Interval for the true value of the 
confint(fit,level = 0.99)
# The 99% CI for Facebook ranges from 0.04213848 to 0.04937115
# That means that the estimated impact of ads, as described in f, might be slightly higher or lower (see CI)


# Expectation of which range (at 90% confidence level) would you expect the sales of 
# 30k $ ads in facebook and 200k $ ads youtube, to be? (5  points)
test <- data.frame("youtube"=c(200), "facebook"=c(30))
test <- data.frame(youtube=200, facebook=30) #alternative
test

predict(fit, newdata = test, interval="prediction", level = 0.9)
# We can expect these ad spent combinations to have a sales of 
# between ~ 14.953 thousand USD and ~ 21.639 thousand USD. These numbers emerge from using a 90% Prediction Interval.