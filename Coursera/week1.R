library("psych")
library("dplyr")
library("GGally")



d <- mtcars # cars data
glimpse(d)
describe(d) # mean, median, etc..
str(d) 

# linear regression 
model <- lm(data=d, mpg~cyl+hp+wt+am)
model

coef(model) # beta coefficients
residuals(model) 
y_hat <- fitted(model) 
y <- d$mpg 

RSS <- deviance(model) # residual sum 
TSS <- sum((y-mean(y))^2) 
TSS
R2 <- 1-RSS/TSS
R2


