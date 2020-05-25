library(sf)
library(spdep)  
library(spatialreg) 
library(lattice)
library(RANN)
library(RColorBrewer)
library(readxl)
library(tidyverse)
library(tmap)
library(GWmodel)
library(raster)
library(dplyr)
library(bstats)
library(psych)
library(GGally)
options(scipen = 999)


#Working with database, count the number of people 17-27 in each region 
####

library(openxlsx)

sheets_lst = excel_sheets("C:\\Downloads\\DB_2015.xlsx")
pattern_str = "Таб\\.2\\.\\d\\.\\d"
popul_res <- list()
i <- 1


for (elem in sheets_lst) {
  if (stringr::str_detect(elem, pattern_str)){
    lkj = read.xlsx("C:\\Downloads\\DB_2015.xlsx", sheet = elem)
    sm <- (as.numeric(lkj[33,2]) + 
             as.numeric(lkj[36,2]) + as.numeric(lkj[37,2]) + 
             as.numeric(lkj[44,2]) 
           + as.numeric(lkj[45,2]) + 
             as.numeric(lkj[46,2]) + as.numeric(lkj[47,2]))
    popul_res[[i]] <- c(lkj[2,1], sm)
    # popul_res[[i]] <- sm
    i <- i + 1
  }
}

lapply(popul_res, write, "C:\\Downloads\\popul_st_2015.txt", append=TRUE, ncolumns=1000)
#####

#Working with the map, finding W
###
geo_data <- getData('GADM', country = 'RUS', level = 1)
set.seed(23)

geo_data_trsf <- spTransform(geo_data, CRS("+proj=longlat +lon_wrap=180"))


st_as_sf(geo_data_trsf) -> buf1
reg = st_geometry(buf1)

par(mar = c(1, 1, 1, 1))
#plot(reg, border = "gray50")

coords = reg %>% st_centroid() %>% st_coordinates()

nb_knn = knearneigh(coords, k = 6) %>% knn2nb()
  
plot(reg, border = "grey50")
plot(nb_knn, coords, pch = 19, cex = 0.5, add = TRUE)
title(main = paste("Ближайшие соседи (k = 6)", sep = ''))

Wbin = nb2listw(nb_knn, style = "B")
Wbin


M = listw2mat(Wbin)
levelplot(M, main = "Матрица весов (бинарная)")
###

#Linear model
####
mun.sf <-buf1
mun <- as(mun.sf, 'Spatial')


year <- "2015" #choose year here

classes <- c("character", "character","numeric","character","character",rep("numeric", 6))

library(xlsx)
tab <- read.xlsx2("C:\\Downloads\\russiaStats.xlsx",sheetName = year,
                  colClasses = classes, stringsAsFactors = FALSE)

tab$educ_level <- (tab$students_count/tab$young_pop)

mun_src = mun.sf
mun = mun_src %>% 
  left_join(tab, by = c("GID_1" = "R_code"))
mun <- as(mun, 'Spatial')

max_data_val <- max(mun$risk_pov_rate)
spplot(mun, zcol = "risk_pov_rate",
   col.regions = colorRampPalette(c("white", "orange", "red"))(max_data_val), 
       col = "black")

df <- data.frame("income" = mun$income, "unemp_rate" = mun$unemp_rate,
      "pop_density" = mun$pop_density, "risk_pov_rate" = mun$risk_pov_rate,
      "educ_level" = mun$educ_level) 

#df_16 <- data.frame(
 # "income" = mun$income, "unemp_rate" = mun$unemp_rate,
  #"pop_density" = mun$pop_density, "risk_pov_rate" = mun$risk_pov_rate,
  #"educ_level" = mun$educ_level)

#df_17 <- data.frame(
 # "income" = mun$income, "unemp_rate" = mun$unemp_rate,
#  "pop_density" = mun$pop_density, "risk_pov_rate" = mun$risk_pov_rate,
 # "educ_level" = mun$educ_level)


df$income <- as.numeric(as.character(df$income))

describe(df)

ggpairs(df)

# For qplots with several values on single plot we create dataframe for each
# year and then use these values

# qplot(df$risk_pov_rate, data=df, geom="density",
#       main="Distribution of AROP", xlab="risk_pov_rate",
#       ylab="Density")
# 
# ####
# qplot(year, income, data = data.frame("year" = c(rep("2015",83), rep("2016",83), rep("2017",83)), 
#       "income" = c(df_15$income,df_16$income,df_17$income) ), 
#       geom=c("boxplot"), fill = year)
# qplot(year, unemp_rate, data = data.frame("year" = c(rep("2015",83), rep("2016",83), rep("2017",83)), 
#                                       "unemp_rate" = c(df_15$unemp_rate,df_16$unemp_rate,df_17$unemp_rate) ), 
#       geom=c("boxplot"), fill = year)
# qplot(year, risk_pov_rate, data = data.frame("year" = c(rep("2015",83), rep("2016",83), rep("2017",83)), 
#                                           "risk_pov_rate" = c(df_15$risk_pov_rate,df_16$risk_pov_rate,df_17$risk_pov_rate) ), 
#       geom=c("boxplot"), fill = year)
# qplot(year, educ_level, data = data.frame("year" = c(rep("2015",83), rep("2016",83), rep("2017",83)), 
#                                              "educ_level" = c(df_15$educ_level,df_16$educ_level,df_17$educ_level) ), 
#       geom=c("boxplot"), fill = year)
# 


df <- log(df)

lreg <- lm(risk_pov_rate ~ income+unemp_rate+
             pop_density+educ_level, data = df)
eps_hat <- residuals(lreg)
summary(lreg)

check_lreg <- lm(eps_hat ~ income+unemp_rate+
                   pop_density+educ_level, data= df)
summary(check_lreg)

shapiro.test(eps_hat)

car::vif(lreg)

lmtest::bptest(lreg)

#library(plm)
#phtest(risk_pov_rate ~ income+unemp_rate+
 #             pop_density+educ_level, data= df)


# Durbin model
###
mun$income <- as.numeric(as.character(mun$income))


moran.test(mun$income, Wbin)
moran.test(mun$unemp_rate, Wbin)
moran.test(mun$pop_density, Wbin)
moran.test(mun$educ_level, Wbin)

mun$income <- log(mun$income)
mun$unemp_rate <- log(mun$unemp_rate)
mun$pop_density <- log(mun$pop_density)
mun$educ_level <- log(mun$educ_level)
mun$risk_pov_rate <- log(mun$risk_pov_rate)

 modelreg <- lagsarlm( risk_pov_rate ~ income + unemp_rate + 
                        pop_density + educ_level, data = mun, listw = Wbin)
 
 modelreg

 print(df)
 mun.SLX <- lmSLX(risk_pov_rate ~ income + unemp_rate + pop_density 
                  + educ_level, data = mun, listw=Wbin)#here mun should not be 
 # Spatial, check line 96
 
 
 summary(mun.SLX)
 summary(impacts(mun.SLX))

######
