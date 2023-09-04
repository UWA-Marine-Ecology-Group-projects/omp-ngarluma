
###
# Project: MBH Template
# Data:    Bathy derivatives
# Task:    MBH Design Preparation - Dampier
# author:  Kingsley Griffin
# date:    July 2022
##

library(raster)

# read in covariate rasters and clean up
preds <- readRDS('output/covariate_rasters.rds')
plot(preds)
preddf <- as.data.frame(preds, xy = TRUE)
preddf <- preddf[is.na(preddf$slope) != TRUE, ]
# preddf$in_aumpa <- ifelse(preddf$aumpa_Area_KM2 > 0, 1, 0)
summary(preddf)
colnames(preddf)

saveRDS(preddf, "output/covariate_df.rds")

### SET BASIC SITE/SURVEY PLAN
## number of bruvs/boss etc..
## 40 BOSS
## depths into 0-30m and then meso reef 30-60m categories

nbruv <- 40

## Build in design shape (stratification biases)
# convert covariates to categorical layers with breaks that define areas of interest, or break evenly by quantiles
# then define stratification bias by weighting the number of samples that will fall within each category


## for each covariate, take a look at the range and distribution of values (histogram)
## then set the 'splits' that will define the categories
## and set the weighting for each category
## multiply this by the number of drops

# hist(preddf$site)
# site_split <- data.frame(zones = unique(preds$site), 
#                          split = c(0.21, 0.21, 0.21, 0.19, 0.18))
# site_split$zbruv <- site_split$split * nbruv

hist(preddf$depth, breaks = 10)                                                 # uneven distribution
bathy_cuts <- c(-35, -32, -28)                                                  # create shallow and deep categories
bathy_cuts
cat_bathy  <- cut(preds$depth, breaks = bathy_cuts, na.rm = TRUE)
plot(stack(preds$depth, cat_bathy))                                             # compare categorical with original data
bathy_split <- data.frame(zones = unique(cat_bathy),
                          split = c(0.5, 0.5))                                  # split weighting for sampling among categories
bathy_split$zbruv <- bathy_split$split * nbruv                                  # n samples in each zone
bathy_split

hist(preds$slope)
slope_qs   <- c(0, 0.92, 0.98, 1)
slope_cuts <- quantile(preds$slope, slope_qs)
slope_cuts
cat_slope  <- cut(preds$slope, breaks = slope_cuts, na.rm = TRUE)
plot(stack(preds$slope, cat_slope))
slope_split <- data.frame(zones = unique(cat_slope),
                          split = c(0.2, 0.5, 0.3))
slope_split$zbruv <- slope_split$split * nbruv
slope_split

hist(preds$roughness)
roughness_qs   <- c(0, 0.85, 0.96, 1)
roughness_cuts <- quantile(preds$roughness, roughness_qs)
cat_roughness  <- cut(preds$roughness, breaks = roughness_cuts, na.rm = TRUE)
plot(stack(preds$roughness, cat_roughness))
rough_split <- data.frame(zones = unique(cat_roughness),
                          split = c(0.2, 0.4, 0.4))
rough_split$zbruv <- rough_split$split * nbruv
rough_split

hist(preds$detrended)
detrend_qs   <- c(0, 0.1, 0.9, 1)
detrend_cuts <- quantile(preds$detrended, detrend_qs)
cat_detrend  <- cut(preds$detrended, breaks = detrend_cuts, na.rm = TRUE)
plot(stack(preds$detrended, cat_detrend))
detrend_split <- data.frame(zones = unique(cat_detrend),
                          split = c(0.4, 0.2, 0.4))
detrend_split$zbruv <- detrend_split$split * nbruv
detrend_split

# create inclusion probability rasters for levels of each covariate
# choose carefully here - if you can make do with less rasters for selection, do that
inp_rasts        <- stack(cat_bathy, cat_slope)
names(inp_rasts) <- c("cat_bathy", "cat_slope")
plot(inp_rasts)

# convert to data frame
icr_df    <- as.data.frame(inp_rasts, xy = TRUE)

# calculate inclusion probabilities for each variable
bath_lvl_sum  <- table(icr_df$cat_bathy)                                       # count of cells of each level
bath_p_strata <- bath_lvl_sum / sum(bath_lvl_sum)                              # proportion of raster covered by each level
bath_inclp    <- bathy_split$split / bath_p_strata                             # alter inclusion prob based on target sample numbers

slope_lvl_sum  <- table(icr_df$cat_slope)
slope_p_strata <- slope_lvl_sum / sum(slope_lvl_sum)
slope_inclp    <- slope_split$split / slope_p_strata

# rough_lvl_sum  <- table(icr_df$cat_rough)
# rough_p_strata <- rough_lvl_sum / sum(rough_lvl_sum)
# rough_inclp    <- rough_split$split / rough_p_strata

# detrend_lvl_sum  <- table(icr_df$cat_detrend)
# detrend_p_strata <- detrend_lvl_sum / sum(detrend_lvl_sum)
# detrend_inclp    <- detrend_split$split / detrend_p_strata

# site_lvl_sum  <- table(icr_df$cat_site)
# site_p_strata <- site_lvl_sum / sum(site_lvl_sum)
# site_inclp    <- site_split$split / site_p_strata

# translate this onto inclusion probability rasters for each layer - leaving this in long format for easy interp
for(lev in 1:length(bath_inclp)){
  inp_rasts$cat_bathy[inp_rasts$cat_bathy == lev] <- bath_inclp[lev]
}

for(lev in 1:length(slope_inclp)){
  inp_rasts$cat_slope[inp_rasts$cat_slope == lev] <- slope_inclp[lev]
}

# for(lev in 1:length(rough_inclp)){
#   inp_rasts$cat_rough[inp_rasts$cat_rough == lev] <- rough_inclp[lev]
# }

# for(lev in 1:length(detrend_inclp)){
#   inp_rasts$cat_detrend[inp_rasts$cat_detrend == lev] <- detrend_inclp[lev]
# }
# 
# for(lev in 1:length(site_inclp)){
#   inp_rasts$cat_site[inp_rasts$cat_site == lev] <- site_inclp[lev]
# }

# scale the layers so there is equal influence of each
for(rasti in 1:nlayers(inp_rasts)){
  inp_rasts[[rasti]] <- inp_rasts[[rasti]] / sum(rasti[], na.rm = TRUE)
}

plot(inp_rasts)
inp_overall <- sum(inp_rasts)
plot(inp_overall)
cellStats(inp_overall, 'sum') # can't remember why we check this?
inp_overall[] <- inp_overall[] / sum(inp_overall[], na.rm = TRUE)
cellStats(inp_overall, 'sum') # to make it equal 1?


saveRDS(inp_overall, "output/inclusionp_rast.rds")

