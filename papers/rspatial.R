# load libraries ----------------------------------------------------------
library(stars)
library(starsExtra)
library(fasterize)
library(tidyverse)
library(sf)
library(mapview)
library(ggplot2)
options("rgdal_show_exportToProj4_warnings"="none")
library(terra)
library(rgdal)
library(raster)
library(dplyr)
library(tidyr)
library(nngeo)
library(vdemdata)
library(dplyr)
library(plyr)
library(tidygeocoder)
library(distanceto)
library(spacetime)
library(fastDummies)
library(measurements)

path = "C:/Users/ACER/Documents/Terror/data"
rasters = "C:/Users/ACER/Documents/Terror/data/raster sets/"
shp = "C:/Users/ACER/Documents/Terror/data/shp files/wa shp/"
code = "C:/Users/ACER/Documents/Terror/Images"

setwd(code)


# study area --------------------------------
shp <- "C:/Users/ACER/Documents/Terror/data/shp files/"
setwd(shp)
wa <- read_sf("wca_admbnda_adm0_ocha_29062021.shp")

map1<- wa[is.element(wa$admin0Name ,c("Burkina Faso", "Cameroon",
                                       "Chad", "Mali", "Niger", "Nigeria")), ]

map2<- wa[is.element(wa$admin0Name ,c("Benin","Burkina Faso", "Cameroon",
                                      "Chad", "Mali", "Niger", "Nigeria", "Togo")), ]

map1 <- map1 |> 
  dplyr::select(c("admin0Name", "geometry"))
map2 <- map2 |> 
  dplyr::select(c("admin0Name", "geometry"))

colnames(map1) <- c("Country", "geometry")


mp <- st_join(fnet, map1, left =T, largest=T)

mp <- mp |> 
  dplyr::select(c("Country", "id"))




st_write(map1, "study area.shp")




# make grid function --------------------------------

make_fnet <- function(geometry) {
  grid <- st_make_grid(map, cellsize = 0.5)
}

grid <- st_make_grid(map1, cellsize = 0.5)


plot(fnet1$x)

#plot(st_geometry(terror), add = T, col="blue")

index <- which(lengths(st_intersects(grid, map1)) > 0)

plot(grid[index])
#plot(st_geometry(terror), add = T, col = "green")


fnet <- grid[index] |> 
  st_as_sf() |>
  dplyr::mutate(id = row_number())


st_write(fnet, "fnet.shp")

# terror data--------------------------------
setwd(path)
main <- read.csv("main_new.csv")

terror <- main |> 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
)
# remove Benin and togo from teror data 
terror1 <- subset(terror, country!="Benin" & country!="Togo")

# join grid to terror data
trr <- st_join(terror1, fnet)

trr1 <- trr[,names(trr) %in% 
               c("id", "year", "actor1", "fatalities")]


trr11 <- aggregate(fatalities ~ id + year + actor1, data = trr1, FUN = sum, na.rm = TRUE)



trr2 = trr1 |> group_by(id, year, actor1) |> 
  reframe(count = n())

terr <- merge(trr2, trr11, by =c("id", "year", "actor1"))

terr.s <- merge(terr, fnet, by ="id")


# final terror data with grid geometries
terrd <- terr.s |> 
  st_set_geometry(terr.s$x)|>
  st_as_sf(dims = c("id", "year")) 


# Distance to nearest national border -------------------------------------

fnett <- sf::as_Spatial(fnet)
grid_center <- coordinates(fnett) #Determine centroids of grid cells 
centers<- SpatialPoints(grid_center, proj4string = CRS(proj4string(e)))

center <- st_as_sf(centers)
map3 <- st_as_sf(map1)
sf::st_crs(center) = sf::st_crs(map3)

dist_for_edge <- st_geometry(obj = map3) %>%
  st_cast(to = 'MULTILINESTRING') %>%
  st_distance(y=center)

dist <- data.frame(t(dist_for_edge))

dist$distance <- with(dist,pmin(dist$X1, dist$X2, dist$X3,
                                dist$X4, dist$X5, dist$X6))

distance <- cbind(fnet, dist$distance)

# road density +geographic --------------------------------

setwd(rasters)
r <- read_stars("GRIP4_density_total/grip4_total_dens_m_km2.asc")

fnet1 <- fnet 
st_crs(fnet1) <- st_crs(r)
road_den <- st_extract(r, at = fnet1, FUN=mean)


road_dens <- cbind(fnet, road_den$grip4_total_dens_m_km2.asc)

plot(road_dens[,2],
     breaks = c(0,50,100,150,200,300))


# GDP and Electricity Consumption +economic -----------------------------------------
rasters = "C:/Users/ACER/Documents/Terror/data/raster sets/Real GDP/updated real GDP"

setwd(rasters)

filesName <- list.files("C:/Users/ACER/Documents/Terror/data/raster sets/Real GDP/updated real GDP" , pattern = "*.tif$")
s <- raster::stack(paste0("C:/Users/ACER/Documents/Terror/data/raster sets/Real GDP/updated real GDP/", filesName), quick=T)


gdp <- extract(s, fnet, FUN=mean, na.rm=TRUE)
gdp_c <- sapply(gdp, function(x) apply(x, 2, mean, na.rm=T))
gdp_t <- data.frame(t(gdp_c))
colnames(gdp_t) <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)



filesName <- list.files("C:/Users/ACER/Documents/Terror/data/raster sets/Electricity consumption/updated electricity consumption" , pattern = "*.tif$")
e <- raster::stack(paste0("C:/Users/ACER/Documents/Terror/data/raster sets/Electricity consumption/updated electricity consumption/", filesName), quick=T)


ec <- extract(e, fnet, FUN=mean, na.rm=TRUE)
ec_c <- sapply(ec, function(x) apply(x, 2, mean, na.rm=T))
ec_t <- data.frame(t(ec_c))
colnames(ec_t) <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

write.csv(ec_t, "C:/Users/ACER/Documents/Terror/data/ec.csv")
write.csv(gdp_t, "C:/Users/ACER/Documents/Terror/data/gdp.csv")


# gdp_main <- cbind(fnet, gdp_t)
# ec_main <- cbind(fnet, ec_t)
# 
# gdp.df = st_set_geometry(gdp_main, NULL)
# ec.df = st_set_geometry(ec_main, NULL)
# 
# mat1 = array(gdp.df[2:18])
# 
# 
# mat1 = as.matrix(gdp.df[2:18])
# mat2 = as.matrix(ec.df[2:18])
# 
# mat = rbind(mat1, mat2)
# 
# dim(mat) = c(cid = 1997, var = 2, year = 17) # make it a 3-dimensional array
# 
# 
# 
# # set dimension values to the array:
# dimnames(mat) = list(cid = gdp_main$id, var = c("GDP", "EC"), year = seq(2003, 2019))
# 
# # convert array into a stars object
# (main.st = st_as_stars(pop = mat))
# 
# 
# (main.geom <- st_set_dimensions(main.st, 1, st_geometry(gdp_main)))
# 
# plot(st_apply(main.geom, c(1,2), mean), key.pos=4,
#      breaks=c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5),
#      pal = rev(RColorBrewer::brewer.pal(9, "RdBu")))





# infant mortality + socio-economic --------------------------------
setwd(rasters)
i <- read_stars("subnational_infant_mortality_rates.tif")

inf_mor <- st_extract(i, at = fnet, FUN=mean)

inf_m <- cbind(fnet, inf_mor$subnational_infant_mortality_rates.tif)

plot(inf_m[,2],
     breaks = c(0,15,25,50,75,100,125))

# population density + socio-economic --------------------------------
filesName <- list.files("C:/Users/ACER/Documents/Terror/data/raster sets/" , pattern = "*.tif$")
p <- raster::stack(paste0("C:/Users/ACER/Documents/Terror/data/raster sets/", filesName), quick=T)


pop <- extract(p, fnet, FUN=mean, na.rm=TRUE)
pop_c <- sapply(pop, function(x) apply(x, 2, mean, na.rm=T))
pop_t <- data.frame(t(pop_c))


#export
write.csv(pop_t, "C:/Users/ACER/Documents/Terror/data/pop.csv")

setwd(path)
popp <- read.csv("pop_tes.csv")
popp$X <- popp$X + 1
colnames(popp) <- c("id", "year", "pop_dens")



# pop_main <- cbind(fnet, pop_t)
# 
# pop.df = st_set_geometry(pop_main, NULL)
# 
# pop_mat = as.matrix(pop.df[2:6])
# 
# dim(pop_mat) = c(cid = 1997, var = 1, year = 5) # make it a 3-dimensional array
# 
# # set dimension values to the array:
# dimnames(pop_mat) = list(cid = pop_main$id, var = c("pop_dens"), year = c(2000, 2005, 2010, 2015, 2020))
# 
# # convert array into a stars object
# (pop.st = st_as_stars(pop = pop_mat))
# 
# 
# (pop.geom <- st_set_dimensions(pop.st, 1, st_geometry(pop_main)))
# 
# plot(st_apply(pop.geom, c(1,2), mean), key.pos=4,
#      breaks=c(0, 25, 50, 100, 125, 150, 200, 250, 300, 350),
#      pal = rev(RColorBrewer::brewer.pal(9, "RdBu")))


# relative wealth index + economic  --------------------------------

path = "C:/Users/ACER/Documents/Terror/data/csv/relative-wealth-index-april-2021/"

setwd(path)

bfa <- read.csv("BFA.csv")
cmr <- read.csv("CMR.csv")
mli <- read.csv("MLI.csv")
ner <- read.csv("NER.csv")
nga <- read.csv("NGA.csv")
tcd <- read.csv("TCD.csv")


rwi <- rbind(bfa, cmr, mli, ner, nga, tcd)



rwi_geo <- rwi |> 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
    
  )


rwi_main <- aggregate(
  rwi_geo,
  fnet,
  FUN=mean,
  do_union = TRUE,
  simplify = TRUE,
  join = st_intersects
)


rwi_main1 <- cbind(fnet$id, rwi_main)

colnames(rwi_main1) <- c("id", "rwi", "error", "geometry")



plot(rwi_main[,1],
     breaks = c(-1, -0.5, 0, 0.5, 1))


# V Democracy + political --------------------------------

vars_z <- vdemdata::vdem  |>
  dplyr::select(c("year", "country_name", "v2x_polyarchy", "v2x_libdem", "v2x_delibdem", "v2x_egaldem",
                    "v2xcl_rol", "v2xel_locelec"))


vars_s <- vars_z[is.element(vars_z$country_name ,c("Burkina Faso", "Cameroon", "Chad",
                   "Mali", "Niger", "Nigeria")), ]
  
vdem_new <- vars_s[is.element(vars_s$year ,c(2003:2022)), ]

#get latitude and longitude for countries 
vdem_l <- geocode(vdem_new, country = country_name, method="osm")

vdem_main <- vdem_l |> 
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  )


vdem_agg <- st_join(wa_sa, vdem_main)

vdem_agg1 <- st_join(fnet, vdem_agg)


vdem_full <- vdem_agg1[,names(vdem_agg1) %in% 
     c("id", "year", "v2x_polyarchy", "v2x_libdem", "v2x_delibdem", "v2x_egaldem",
       "v2xcl_rol", "v2xel_locelec")]


plot(vdem_full["v2x_polyarchy"])



# religious variable  + religious --------------------------------

eth_b <- "C:/Users/ACER/Documents/Terror/data/csv/"
setwd(eth_b)

eth <- read.csv("religious1.csv")


# Elevation  ------------------------------------------------------


setwd(rasters)
elev <- read_stars("elevation_main.tiff")

st_crs(fnet1) <- st_crs(elev)
elevation <- st_extract(elev, at = fnet1, FUN=mean)

elev.e <- cbind(fnet, elevation$elevation_main.tiff)


plot(elev.e['elevation.elevation_main.tiff'],
     breaks = c(100,200,300,400,500,600))

# make panel data for variables -------------------------------------------

#make panel data electricity consumption
setwd(path)
elect <- read.csv("ec_tes.csv")
elect$X <- elect$X + 1
colnames(elect) <- c("id", "year", "elecons")

#make panel data gdp
setwd(path)
gdppa <- read.csv("gdp_tes.csv")
gdppa$X <- gdppa$X + 1
colnames(gdppa) <- c("id", "year", "gdp")



gdp_int <- gdppa %>% 
  complete(id, year = 2003:2022) %>% 
  mutate(gdp = zoo::na.approx(gdp, na.rm = FALSE))

ec_int <- elect %>% 
  complete(id, year = 2003:2022) %>% 
  mutate(elecons = zoo::na.approx(elecons, na.rm = FALSE))


# join interpolated electricity consumption and gdp
p.dt <- merge(gdp_int, ec_int, by=c("id", "year"))

# merge with road dens
p.dt1 <- merge(p.dt, road_dens, by="id")

# merge with infant mortality
p.dt2 <- merge(p.dt1,inf_m, by ="id")

# merge with population density
p.dt3 <- merge(p.dt2,popp, by = c("id", "year"))

# merge with relative wealth index
p.dt4 <- merge(p.dt3,rwi_main1, by ="id")

#merge with varieties of democracy
p.dt5 <- left_join(p.dt4, vdem_full, by = c("id", "year"),keep = F, multiple = "first", unmatched = "drop", relationship = "many-to-one")


#merge with elevation
p.dt6 <- merge(p.dt5, elev.e, by=c("id"))


# merge with distance
p.dt7 <- merge(p.dt6, distance, by=c("id"))


# merge with distance
p.dt8 <- merge(p.dt7, eth, by=c("id"))


# merge with map details
p.dtm <- merge(p.dt8, mp, by=c("id"))

# select variables
var.a <- p.dtm |> 
  dplyr::select(c("id", "year", "admin0Name", "gdp", "elecons", "road_den.grip4_total_dens_m_km2.asc",
                         "inf_mor.subnational_infant_mortality_rates.tif", "pop_dens", "rwi", "geometry",
                         "v2x_polyarchy", "v2x_libdem", "v2x_delibdem", "v2x_egaldem", "v2xcl_rol",
                         "v2xel_locelec", "elevation.elevation_main.tiff", "dist.distance",
                         "christiansmax", "muslimmax"))


colnames(var.a) <- c("id", "year", "country", "gdp", "elecons", "road_den", "inf_mor", "pop_den",
                     "rwi", "geometry", "v2x_polyarchy", "v2x_libdem", "v2x_delibdem", "v2x_egaldem",
                     "v2xcl_rol", "v2xel_locelec", "elevation", "distance", "christiansmax", "muslimmax")

# merge both datasets together 

dta <- full_join(terr, var.a, by=c("id", "year"), )


# convert to sf object

df <- dta |> 
  st_set_geometry(dta$geometry)|>
  st_as_sf(dims = c("id", "year")) 

df$distance <- conv_unit(df$distance, "m", "km")

df$inf_mor[df$inf_mor < 0] <- 0



df <- df[order(df$id),]


df <- dummy_cols(df, select_columns = 'actor1')

# create democracy index from democracy related variables

df$demid <- (df$v2x_polyarchy + df$v2x_libdem + df$v2x_delibdem + df$v2x_egaldem) / 4

  
# Descriptive statictics --------------------------------------------------

require(reporttools)
library(xtable)
sum.test <- summary(df[,7:22])

print(xtable(as.table(t(sum.test)), type = "latex"), file = "test.tex")


# export final data 
write.csv(df, "C:/Users/ACER/Documents/Terror/data/data.csv")



# subset 2022

year22 <- subset(df, year=="2022")

year22is <- subset(year22, actor1=="Islamic State (West Africa) - Greater Sahara Faction")


plot(year22["gdp"], key.pos=4,
        breaks=c(0, 0.15, 0.30, 0.45, 0.60, 0.75, 0.90, 1, 1.15, 4.5),
         pal = rev(RColorBrewer::brewer.pal(9, "RdBu")))

plot(df["fatalities"], 
     breaks=c(5, 25, 50, 75, 100, 125))



# Model specs -------------------------------------------------------------

require(nnet)
require(mlogit)
library(plm)
library(car)
library(ResourceSelection)
library(caTools)
library(DescTools)
library(tidymodels)
library(stargazer)
library(ROCR)
library(rms)
 

# multicollinearity check 

dt$a1 <- as.numeric(as.factor(dt$actor1))


lmm <- lm(a1 ~ gdp + road_den + inf_mor + pop_den + rwi + 
             v2x_polyarchy + v2x_delibdem + v2xcl_rol +
             v2xel_locelec + elevation + distance + christiansmax +  muslimmax, data=dt)
vif(lmm)

lmm <- lm(a1 ~ gdp  + road_den + inf_mor + pop_den + rwi + 
            demid + v2xcl_rol + v2xel_locelec + elevation + distance + christiansmax + muslimmax, data=df)

vif(lmm)

# notes: gdp and electricity highly correlated; opt 1 use only gdp or only electricity
# consumption; opt 2 use the mean of electricity consumption and gdp

# estimate all variables 
m <- multinom(actor2 ~ gdp + road_den + inf_mor + pop_den + rwi + demid +
                v2xcl_rol + v2xel_locelec + elevation + distance, data = df, panel=T)

# Compare OIM and m
anova(OIM,m)

# notes: Model with all variables performs better than model with only intercepts

# estimate all variables with time fixed effects
me <- multinom(actor2 ~ gdp + road_den + inf_mor + pop_den + rwi + demid +
                 v2xcl_rol + v2xel_locelec + elevation + distance + factor(year), data = dff, panel=T, na.action = na.exclude)

# Compare m and me
anova(m, me)

# notes: model with time fixed effects performs better than model with no fixed effects



# estimate all variables with religious variables
mr <- multinom(actor2 ~ gdp + road_den + inf_mor + pop_den + rwi + demid +
                v2xcl_rol + v2xel_locelec + elevation + distance + christiansmax + muslimmax, data = df, panel=T)

# Compare OIM and mr
anova(OIM, mr)

# Notes: model with all vars and religious variable performs better than base 
# model with only intercept

# estimate all variables and religious vars with time fixed effects
mre <- multinom(actor2 ~ gdp + road_den + inf_mor + pop_den + rwi + demid +
                 v2xcl_rol + v2xel_locelec + elevation + distance + christiansmax + muslimmax + factor(year), data = df, panel=T)

# Compare mr and mre
anova(mr, mre)

# Notes: Model with time fixed effects performs better than model with no time fixed effects

# estimate model with key variables
mb <- multinom(actor2 ~ gdp + pop_den + demid +
                 elevation + distance, data = df, panel=T)

# estimate model with key variables and year dummies for time fixed effects

mbe <- multinom(actor2 ~ gdp + pop_den + demid + distance + elevation +
                  factor(year), data = dff, panel=T, na.action = na.exclude)


# Compare mb and mbe
anova(mb, mbe)

# Notes: base model with key variables and fixed time effects performs better 
# than base model with no fixed time effects


# Compare mb and mbe
anova(mb, mbe)

# Notes: base model with key variables and fixed time effects performs better 
# than the model including all vars and fixed time effects



# estimate model with key variables and year dummies for time fixed effects with religious variables

mber <- multinom(actor2 ~ gdp + pop_den + demid + distance + elevation + christiansmax + muslimmax + 
                  factor(year), data = dff, panel=T, na.action = na.exclude)


# Compare mbe and mber
anova(mbe, mber)

# Notes: base model with key variables, religious vars and fixed time effects performs better 
# than the base model with key variables and fixed time effects



# estimate model with key variables, adding some other 2 variables - road_den + rwi
m.ade <- multinom(actor2 ~ gdp + pop_den + demid + distance + elevation + 
                   road_den + rwi + factor(year), data = dff, panel=T, na.action = na.exclude)


# estimate model with key variables and religious vars, adding some other 2 variables
m.ader <- multinom(actor2 ~ gdp + pop_den + demid + distance + elevation + 
                     christiansmax + muslimmax + road_den + rwi + factor(year), data = dff, panel=T, na.action = na.exclude)

# Compare mber and m.ader
anova(mber, m.ader)

# Notes: base model with key variables, religious vars and fixed time effects performs better 
# than the base model with key variables, religious vars, fixed time effects and two added vars


# estimate model with key variables, adding some other 3 variables - inf_mor + v2xcl_rol + v2xel_locelec
m.ade <- multinom(actor2 ~ gdp + pop_den + demid + distance + elevation + 
                    inf_mor + v2xcl_rol + v2xel_locelec + factor(year), data = dff, panel=T, na.action = na.exclude)


# estimate model with key variables and religious vars, adding some other 3 variables
m.aderm <- multinom(actor2 ~ gdp + pop_den + demid + distance + elevation + 
                     christiansmax + muslimmax + inf_mor + v2xcl_rol + v2xel_locelec +
                     factor(year), data = dff, panel=T, na.action = na.exclude)


# Compare mber and m.aderm
anova(mber, m.aderm)

# Notes: base model with key variables, religious vars and fixed time effects performs better 
# than the base model with key variables, religious vars, fixed time effects and three added vars


# Test the goodness of fit
chisq.test(dff$actor2, predict(mber))


# Calculate the R Square
PseudoR2(mber, which = c("CoxSnell","Nagelkerke","McFadden"))

model <- tidy(model_fit, exponentiate = TRUE, conf.int = TRUE) |> 
  mutate_if(is.numeric, round, 4) |> 
  select(-std.error, -statistic)

glance(model_fit)


terror_preds <- model_fit |> 
  augment(new_data = dff)

conf.mat <- conf_mat(terror_preds, truth = actor2, estimate = .pred_class)

accuracy(terror_preds, truth = actor2, estimate = .pred_class)



### final models

#all groups --------

df <- na.omit(df)

df <- df %>% mutate(act1 = +!is.na(actor1))

ag <- glm(act1 ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor + road_den + rwi +
             factor(year) + factor(country), data = df, family = "binomial", type = "response")

#lr test


#model checks
pred_re <- predict(ag, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
table(df$act1, pred_reg)
missing_classerr <- mean(pred_reg != df$act1)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, df$act1)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc



# export reg table
coefs = exp(coef(ag))
stargazer(ag, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)



# base Ambazonian Separatists (Cameroon) ------ 
asc <- glm(df$`actor1_Ambazonian Separatists (Cameroon)` ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor + road_den + rwi +
                  factor(year) + factor(country), data = df, family = "binomial")

#lr test 

asc_et <- glm(df$`actor1_Ambazonian Separatists (Cameroon)` ~ 1, data = df, family = "binomial")
lr_asc <- anova(asc_et, asc, test = "Chisq")
stargazer(lr_asc, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_Ambazonian Separatists (Cameroon)`, fitted(asc), g = 3)



#model checks
pred_re <- predict(asc, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc = table(df$`actor1_Ambazonian Separatists (Cameroon)`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
missing_classerr <- mean(pred_reg != df$`actor1_Ambazonian Separatists (Cameroon)`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, df$`actor1_Ambazonian Separatists (Cameroon)`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export reg table
coefs = exp(coef(asc))
stargazer(asc, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)


# base Boko Haram ------ 
bh <- glm(df$`actor1_Boko Haram` ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor + road_den + rwi +
             factor(year) + factor(country), data = df, family = "binomial")

#lr test 

bh_et <- glm(df$`actor1_Boko Haram` ~ 1, data = df, family = "binomial")
lr_bh <- anova(bh_et, bh, test = "Chisq")
stargazer(lr_bh, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_Boko Haram`, fitted(bh), g = 3)




#model checks
pred_re <- predict(bh, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc <- table(df$`actor1_Boko Haram`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
missing_classerr <- mean(pred_reg != dt$`actor1_Boko Haram`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_Boko Haram`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export table
coefs = exp(coef(bh))
stargazer(bh, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

# rrr
coef = exp(bh$coefficients)
stargazer(bh, type="latex", coef=coef, p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

# base Communal Militia (Nigeria) ------ 
cmn <- glm(df$`actor1_Communal Militia (Nigeria)` ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor + road_den + rwi +
             factor(year) + factor(country), data = df, family = "binomial")
#lr test
cmn_et <- glm(df$`actor1_Communal Militia (Nigeria)` ~ 1, data = df, family = "binomial")
lr_cmn <- anova(cmn_et, cmn, test = "Chisq")
stargazer(lr_cmn, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_Communal Militia (Nigeria)`, fitted(cmn), g = 3)



#model checks
pred_re <- predict(cmn, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc = table(df$`actor1_Communal Militia (Nigeria)`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
missing_classerr <- mean(pred_reg != dt$`actor1_Communal Militia (Nigeria)`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_Communal Militia (Nigeria)`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export reg table
coefs = exp(coef(cmn))
stargazer(cmn, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)



# base Fulani Militia ------ 
fm <- glm(df$`actor1_Fulani Militia` ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor + road_den + rwi +
             factor(year) + factor(country), data = df, family = "binomial")
#lr test
fm_et <- glm(df$`actor1_Fulani Militia` ~ 1, data = df, family = "binomial")
lr_fm <- anova(fm_et, fm, test = "Chisq")
lr_fm
stargazer(lr_fm, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_Fulani Militia`, fitted(fm), g = 3)


#model checks
pred_re <- predict(fm, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc = table(df$`actor1_Fulani Militia`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
missing_classerr <- mean(pred_reg != dt$`actor1_Fulani Militia`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_Fulani Militia`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export reg table
coefs = exp(coef(fm))
stargazer(fm, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)


# base Islamic State ------ 
isis <- glm(df$`actor1_Islamic State` ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor + road_den + rwi +
             factor(year) + factor(country), data = df, family = "binomial")

#lr test
is_et <- glm(df$`actor1_Islamic State` ~ 1, data = df, family = "binomial")
lr_is <- anova(is_et, isis, test = "Chisq")
stargazer(lr_is, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_Islamic State`, fitted(isis), g = 3)



#model checks
pred_re <- predict(isis, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc = table(df$`actor1_Islamic State`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
missing_classerr <- mean(pred_reg != dt$`actor1_Islamic State`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_Islamic State`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export table
coefs = exp(coef(isis))
stargazer(isis, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

# base JNIM: Group for Support of Islam and Muslims ------ 
jnim <- glm(df$`actor1_JNIM: Group for Support of Islam and Muslims` ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor + road_den + rwi +
             factor(year) + factor(country), data = df, family = "binomial")

#lr test
jnim_et <- glm(df$`actor1_JNIM: Group for Support of Islam and Muslims` ~ 1, data = df, family = "binomial")
lr_jnim <- anova(jnim_et, jnim, test = "Chisq")
stargazer(lr_jnim, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_JNIM: Group for Support of Islam and Muslims`, fitted(jnim), g = 3)

#model checks
pred_re <- predict(jnim, df, type="response")
pred_reg <- factor(ifelse(pred_re > 0.5, 1, 0))
mc = table(df$`actor1_JNIM: Group for Support of Islam and Muslims`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
missing_classerr <- mean(pred_reg != dt$`actor1_JNIM: Group for Support of Islam and Muslims`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_JNIM: Group for Support of Islam and Muslims`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export table
coefs = exp(coef(jnim))
stargazer(jnim, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)


# base Katiba Macina ------ 
km <- glm(df$`actor1_Katiba Macina` ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor + road_den + rwi +
             factor(year) + factor(country), data = df, family = "binomial")


#lr test
km_et <- glm(df$`actor1_Katiba Macina` ~ 1, data = df, family = "binomial")
lr_km <- anova(km_et, km, test = "Chisq")
stargazer(lr_km, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)


#hms 
hoslem.test(df$`actor1_Katiba Macina`, fitted(km), g = 5)

#model checks
pred_re <- predict(km, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc = table(df$`actor1_Katiba Macina`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
missing_classerr <- mean(pred_reg != dt$`actor1_Katiba Macina`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_Katiba Macina`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc


# export table
coefs = exp(coef(km))
stargazer(km, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

# upt(with religious variables) all groups -----
ag <- glm(df$act1 ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor +
             road_den + rwi + christiansmax + muslimmax + factor(year) + factor(country), data = df, family = "binomial")

#model checks
pred_re <- predict(ag, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
table(df$act1, pred_reg)
missing_classerr <- mean(pred_reg != df$act1)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, df$act1)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export reg table
stargazer(ag, type="latex", p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)
# export table
coefs = exp(coef(ag))
stargazer(ag, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)


# upt Ambazonian Separatists (Cameroon) ------ 
asc <- glm(df$`actor1_Ambazonian Separatists (Cameroon)` ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor +
             road_den + rwi + christiansmax + muslimmax + factor(year) + factor(country), data = df, family = "binomial")
#lr test 

asc_et <- glm(df$`actor1_Ambazonian Separatists (Cameroon)` ~ 1, data = df, family = "binomial")
lr_asc <- anova(asc_et, asc, test = "Chisq")
stargazer(lr_asc, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_Ambazonian Separatists (Cameroon)`, fitted(asc))



#model checks
pred_re <- predict(asc, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc = table(df$`actor1_Ambazonian Separatists (Cameroon)`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
missing_classerr <- mean(pred_reg != df$`actor1_Ambazonian Separatists (Cameroon)`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, df$`actor1_Ambazonian Separatists (Cameroon)`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export reg table
stargazer(asc, type="latex", p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)
# export table
coefs = exp(coef(asc))
stargazer(asc, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

# upt Boko Haram ------ 
bh <- glm(df$`actor1_Boko Haram` ~ gdp + pop_den + demid + distance + elevation + v2xel_locelec + inf_mor 
          + road_den + rwi + christiansmax + muslimmax +
            factor(year) + factor(country), data = df, family = "binomial")
#lr test 

bh_et <- glm(df$`actor1_Boko Haram` ~ 1, data = df, family = "binomial")
lr_bh <- anova(bh_et, bh, test = "Chisq")
stargazer(lr_bh, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_Boko Haram`, fitted(bh), g = 3)


#model checks
pred_re <- predict(bh, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc <- table(df$`actor1_Boko Haram`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
missing_classerr <- mean(pred_reg != dt$`actor1_Boko Haram`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_Boko Haram`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc
# export reg table
stargazer(bh, type="latex", p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)
# export table
coefs = exp(coef(bh))
stargazer(bh, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

# upt Communal Militia (Nigeria) ------ 
cmn <- glm(df$`actor1_Communal Militia (Nigeria)` ~ gdp + pop_den + demid + distance + elevation +
             v2xel_locelec + inf_mor + road_den + rwi + christiansmax + muslimmax + 
             factor(year) + factor(country), data = df, family = "binomial")
#lr test
cmn_et <- glm(df$`actor1_Communal Militia (Nigeria)` ~ 1, data = df, family = "binomial")
lr_cmn <- anova(cmn_et, cmn, test = "Chisq")
stargazer(lr_cmn, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_Communal Militia (Nigeria)`, fitted(cmn), g = 3)



#model checks
pred_re <- predict(cmn, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc = table(df$`actor1_Communal Militia (Nigeria)`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
#model checks
pred_re <- predict(cmn, dt, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
table(dt$`actor1_Communal Militia (Nigeria)`, pred_reg)
missing_classerr <- mean(pred_reg != dt$`actor1_Communal Militia (Nigeria)`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_Communal Militia (Nigeria)`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export reg table
stargazer(cmn, type="latex", p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

coefs = exp(coef(cmn))
stargazer(cmn, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)



# upt Fulani Militia ------ 
fm <- glm(df$`actor1_Fulani Militia` ~ gdp + pop_den + demid + distance + elevation +
            v2xel_locelec + inf_mor + road_den + rwi  + christiansmax + muslimmax +
            factor(year) + factor(country), data = df, family = "binomial")
#lr test
fm_et <- glm(df$`actor1_Fulani Militia` ~ 1, data = df, family = "binomial")
lr_fm <- anova(fm_et, fm, test = "Chisq")
stargazer(lr_fm, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_Fulani Militia`, fitted(fm), g = 3)


#model checks
pred_re <- predict(fm, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc = table(df$`actor1_Fulani Militia`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
#model checks
pred_re <- predict(fm, dt, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
table(dt$`actor1_Fulani Militia`, pred_reg)
missing_classerr <- mean(pred_reg != dt$`actor1_Fulani Militia`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_Fulani Militia`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export reg table
stargazer(fm, type="latex", p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

coefs = exp(coef(fm))
stargazer(fm, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)


# upt Islamic State ------ 
isis <- glm(df$`actor1_Islamic State` ~ gdp + pop_den + demid + distance + elevation +
              v2xel_locelec + inf_mor + road_den + rwi + christiansmax + muslimmax +
              factor(year) + factor(country), data = df, family = "binomial")
#lr test
is_et <- glm(df$`actor1_Islamic State` ~ 1, data = df, family = "binomial")
lr_is <- anova(is_et, isis, test = "Chisq")
lr_is
stargazer(lr_is, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_Islamic State`, fitted(isis))



#model checks
pred_re <- predict(isis, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc = table(df$`actor1_Islamic State`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
#model checks
pred_re <- predict(isis, dt, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
table(dt$`actor1_Islamic State`, pred_reg)
missing_classerr <- mean(pred_reg != dt$`actor1_Islamic State`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_Islamic State`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc


# export reg table
stargazer(isis, type="latex", p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

coefs = exp(coef(isis))
stargazer(isis, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)


# upt JNIM: Group for Support of Islam and Muslims ------ 
jnim <- glm(df$`actor1_JNIM: Group for Support of Islam and Muslims` ~ gdp + pop_den + demid + distance + elevation 
            + v2xel_locelec + inf_mor + road_den + rwi + christiansmax + muslimmax +
              factor(year) + factor(country), data = df, family = "binomial")

#lr test
jnim_et <- glm(df$`actor1_JNIM: Group for Support of Islam and Muslims` ~ 1, data = df, family = "binomial")
lr_jnim <- anova(jnim_et, jnim, test = "Chisq")
lr_jnim
stargazer(lr_jnim, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)

#hms 
hoslem.test(df$`actor1_JNIM: Group for Support of Islam and Muslims`, fitted(jnim))

#model checks
pred_re <- predict(jnim, df, type="response")
pred_reg <- factor(ifelse(pred_re > 0.5, 1, 0))
mc = table(df$`actor1_JNIM: Group for Support of Islam and Muslims`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
#model checks
pred_re <- predict(jnim, dt, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
table(dt$`actor1_JNIM: Group for Support of Islam and Muslims`, pred_reg)
missing_classerr <- mean(pred_reg != dt$`actor1_JNIM: Group for Support of Islam and Muslims`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_JNIM: Group for Support of Islam and Muslims`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# export reg table
stargazer(jnim, type="latex", p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

coefs = exp(coef(jnim))
stargazer(jnim, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

# upt Katiba Macina ------ 
km <- glm(df$`actor1_Katiba Macina` ~ gdp + pop_den + demid + distance + elevation +
            v2xel_locelec + inf_mor + road_den + rwi + christiansmax + muslimmax +
            factor(year) + factor(country), data = df, family = "binomial")

#lr test
km_et <- glm(df$`actor1_Katiba Macina` ~ 1, data = df, family = "binomial")
lr_km <- anova(km_et, km, test = "Chisq")
lr_km
stargazer(lr_km, type="latex",p.auto=T, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F)


#hms 
hoslem.test(df$`actor1_Katiba Macina`, fitted(km), g = 5)

#model checks
pred_re <- predict(km, df, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
mc = table(df$`actor1_Katiba Macina`, pred_reg)
#error rate
t = (mc[1, 2] + mc[2, 1]) / sum(mc)
t
#model checks
pred_re <- predict(km, dt, type="response")
pred_reg <- ifelse(pred_re > 0.5, 1, 0)
table(dt$`actor1_Katiba Macina`, pred_reg)
missing_classerr <- mean(pred_reg != dt$`actor1_Katiba Macina`)
print(paste('Accuracy = ', 1-missing_classerr))
#ROC-AUC curve
ROCpred <- prediction(pred_reg, dt$`actor1_Katiba Macina`)
ROCper <- performance(ROCpred, measure = "tpr", x.measure = "fpr")
auc <- performance(ROCpred, measure = "auc")
auc <- auc@y.values[[1]]
auc


# export reg table
stargazer(km, type="latex", p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)

coefs = exp(coef(km))
stargazer(km, type="latex", coef=list(coefs), p.auto=FALSE, out="model.tex", ci=F, column.sep.width = "3pt", min.max = F, mean.sd = F)








# plots and maps -----
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(dplyr)
library(ggmap)
library(maps)
library(mapdata)


tm_shape(map1) +
  tm_polygons(col = "Country")+
  tm_style("classic")+
  tm_fill()+
  tm_borders() +
  tm_layout(frame = F)

tm_shape(df) +
  tm_polygons(col = "pop_den", style = "quantile", palette = "Greys") +
  tm_fill()+
  tm_borders() +
  tm_layout(frame = F)


tm_shape(df22) +
  tm_polygons(col = "pop_den", style = "quantile", palette = "Greys") +
  tm_fill()+
  tm_borders() +
  tm_layout(frame = F)

terr.sf<- st_as_sf(terr.s)

tr_03_22 = terr.sf %>% 
  filter(year %in% c(2010, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))


tr_03_22is <- tr_03_22 %>% 
  filter(actor1 %in% "Islamic State")

tm_shape(map1) +
  tm_polygons(col = "Country") +
  tm_shape(tr_03_22is) +
  tm_symbols(col = "black", size = "count") +
  tm_facets(by = "year", ncol = 3, free.coords = FALSE) +
  tm_layout(bg.color = "lightblue")+
  tm_layout(inner.margins = c(0,0,0,0))
  #tm_layout(main.title = " Yearly count evolution for Islamic state", title.position=c("center", "TOP"), title.fontfamily ="sans")

tr_03_22jn <- tr_03_22 %>% 
  filter(actor1 %in% "JNIM: Group for Support of Islam and Muslims")


tm_shape(map1) +
  tm_polygons(col = "Country") +
  tm_shape(tr_03_22jn) +
  tm_symbols(col = "black", size = "count") +
  tm_facets(by = "year", ncol = 3, free.coords = FALSE) +
  tm_layout(bg.color = "lightblue") +
  tm_layout(inner.margins = c(0,0,0,0))
  #tm_layout(main.title = " Yearly count evolution for JNIM", title.position=c("center", "TOP"), title.fontfamily ="mono")

tr_03_22bh <- tr_03_22 %>% 
  filter(actor1 %in% "Boko Haram")

tm_shape(map1) +
  tm_polygons(col = "Country") +
  tm_shape(tr_03_22bh) +
  tm_symbols(col = "black", size = "count") +
  tm_facets(by = "year", ncol = 3, free.coords = FALSE) +
  tm_layout(bg.color = "lightblue") +
  tm_layout(inner.margins = c(0,0,0,0))
  #tm_layout(main.title = " Yearly count evolution for Boko Haram", title.position=c("center", "TOP"), title.fontfamily ="mono")


tr_03_22as <- tr_03_22 %>% 
  filter(actor1 %in% "Ambazonian Separatists (Cameroon)")

tm_shape(map1) +
  tm_polygons(col = "Country") +
  tm_shape(tr_03_22as) +
  tm_symbols(col = "black", size = "count") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE) +
  tm_layout(bg.color = "lightblue") +
  tm_layout(inner.margins = c(0,0,0,0))
  #tm_layout(main.title = " Yearly count evolution for Ambazonian Separatists", title.position=c("center", "TOP"), title.fontfamily ="mono")


tr_03_22cm <- tr_03_22 %>% 
  filter(actor1 %in% "Communal Militia (Nigeria)")

tm_shape(map1) +
  tm_polygons(col = "Country") +
  tm_shape(tr_03_22cm) +
  tm_symbols(col = "black", size = "count") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE) +
  tm_layout(bg.color = "lightblue") +
  tm_layout(main.title = " Yearly count evolution for Communal Militia", title.position=c("center", "TOP"), title.fontfamily ="mono")



tr_03_22 = terr.sf %>% 
  filter(year %in% c(2003, 2005, 2010, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))



tm_shape(map1) +
  tm_polygons(col = "Country") +
  tm_layout(bg.color = "lightblue")+
  tm_shape(tr_03_22) +
  tm_symbols(col = "black", size = "count") +
  tm_facets(by = "year", nrow = 3, free.coords = FALSE) +
  tm_layout(main.title = " Yearly count evolution for terror incidences in WA", title.position=c("center", "TOP"), title.fontfamily ="mono")
   

tm_shape(mapp) +
  tm_polygons(col = "Country") +
  tm_layout(bg.color = "lightblue")+
  tm_shape(tr_03_22) +
  tm_symbols(col = "red", size = "fatalities") +
  tm_facets(by = "year", nrow = 3, free.coords = FALSE) +
  tm_layout(main.title = " Yearly fatality evolution for terror incidences in WA", title.position=c("center", "TOP"), title.fontfamily ="mono")




# study area --------------------------------
shp <- "C:/Users/ACER/Documents/Terror/data/shp files/wa shp"
setwd(shp)
waf <- read_sf("waadmin2.shp")

map<- waf[is.element(waf$ADM0_NAME ,c("Burkina Faso", "Cameroon",
                                      "Chad", "Mali", "Niger", "Nigeria")), ]

map <- map |> 
  dplyr::select(c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME", "geometry"))

colnames(map) <- c("Country", "State", "community", "geometry")


mapp <- st_join(fnet, map, left =T, largest=T)

mapp <- mapp |> 
  dplyr::select(c("Country", "State", "community", "id"))


tm_shape(mapp) +
  tm_polygons()+
  tm_borders() +
  tm_layout(frame = F)

library(rnaturalearth)


# Load a world map dataset from rnaturalearth
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Filter the world map to focus on Africa
africa_map <- world_map[world_map$region_un == "Africa", ]

ps = sequential_hcl(5, palette = "GnBu")
pa = sequential_hcl(5, palette = "Light Grays")


ggplot() +
  geom_sf(data = africa_map,fill=pa[3]) +
  geom_sf(data = map2, fill = ps[3], alpha = 1, col="black") +
  geom_sf_text(aes(label=admin), data = africa_map, check_overlap = F, position = "jitter", size = 2, fontface = "bold")+
  theme_void()+
  labs(caption = "(Selected countries highlighted in light green.)")+
  #labs(caption = "(They include; Benin, Burkina Faso, Cameroon, Chad, Niger, Nigeria and Togo.)")+
  ggtitle("Selected Countries") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))
  #labs(title = "Study Area in Africa")


library("colorspace")
hcl_palettes(plot = TRUE)
