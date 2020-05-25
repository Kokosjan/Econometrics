library("memisc") 
library("dplyr") 
library("psych") 
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2") 
library("foreign") 
library("car")
library("hexbin") 




# generate random variables
z <- rnorm(100, mean=5, sd=3)

qplot(z) # histogram

# density function
x <- seq(-10,15,by=0.5) 
y <- dnorm(x, mean=5, sd=3) 
qplot(x,y, geom="line")

# find P(Z<3) => P(Z<3)=F(3)
pnorm(3, mean=5, sd=3)

# find P(Z \in [4;9]) => P(Z<9)-P(Z<4)
pnorm(9, mean=5, sd=3) - pnorm(4, mean=5, sd=3)

# find quantile а, at which P(Z<a)=0.7.
qnorm(0.7, mean=5, sd=3)

# same for another distributions chisq, t, f
# rchisq, dchisq, pchisq, qchisq
# rt, dt, pt, qt
# rf, df, pf, qf


# multiple regression. hipothesis check
h <- swiss 
glimpse(h) 
help(swiss) 
model <- lm(data=h, Fertility~Catholic+Agriculture+Examination)
summary(model)
coeftest(model)

confint(model) # confidence intervals
sjp.lm(model) # plot confidence intervals

# hypothesis b_Cath=b_Agri check
model_aux <- lm(data=h, Fertility~Catholic+I(Catholic+Agriculture)+Examination)
summary(model_aux)

# hypothesis b_Cath=b_Agri check (build-in function)
linearHypothesis(model, "Catholic-Agriculture=0")

# re-scale every variable 
h_st <- mutate_each(h, "scale")
glimpse(h_st) 
# linear reg to new variables
model_st <- lm(data=h_st, Fertility~Catholic+Agriculture+Examination)
summary(model_st)
# graphical representation
sjp.lm(model, type = "std") 

# compare several models
model2 <- lm(data=h, Fertility~Catholic+Agriculture)
compar_12 <- mtable(model, model2)
compar_12

# save models to file
stuff <- list(data=h, model=model2)
saveRDS(file = "mydata.RDS",stuff) 

mylist <- readRDS("mydata.RDS")
summary(mylist$model)