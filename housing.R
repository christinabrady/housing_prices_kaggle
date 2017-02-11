options(stringsAsFactors = FALSE)
setwd("Documents/kaggle/housing/")
library(ReporteRs)
library(ggvis)
library(dplyr)

house <- read.csv("AmesHousing.csv")

cleanColNames <- function(dat){
	tolower(gsub("\\.", "_", colnames(dat)))
}
colnames(house) <- cleanColNames(house)

nomVar <- c("ms_subclass", "ms_zoning", "street", "alley", "land_contour", "lot_config", "neighborhood", "condition_1", "condition_2", "bldg_type", "house_style", "roof_style", "roof_matl", "exterior_1st", "exterior_2nd", "mas_vnr_type", "foundation", "heating", "central_air", "garage_type", "misc_feature", "sale_type", "sale_condition", "electrical")
ordVar <- c("lot_shape", "utilities", "land_slope", "overall_qual", "overall_cond", "exter_qual", "exter_cond", "bsmt_qual", "bsmt_cond", "bsmt_exposure", "bsmtfin_type_1", "bsmtfin_type_2", "heating_qc", "electrical", "kitchen_qual", "functional", "fireplace_qu", "garage_finish", "garage_qual", "garage_cond", "paved_drive", "pool_qc", "fence")
disVar <- c("year_built", "year_remod_add", "bsmt_full_bath", "bsmt_half_bath", "bedroom_abvgr", "kitchen_abvgr", "totrms_abvgrd", "fireplaces", "garage_yr_blt", "garage_cars", "mo_sold", "yr_sold")
conVar <- c("lot_frontage", "lot_area", "mas_vnr_area", "bsmtfin_sf_1", "bsmtfin_sf_2", "bsmt_unf_sf", "total_bsmt_sf", "x1st_flr_sf", "x2nd_flr_sf", "low_qual_fin_sf", "gr_liv_area", "garage_area", "wood_deck_sf", "open_porch_sf", "enclosed_porch", "screen_porch", "pool_area", "misc_val")


### plotting functions ###
plotting_boxes <- function(dat, var, doc){
	print(var)
	form <- paste("saleprice~", var)
	freqs <- table(house[, var])
	names <- unlist(lapply(1:length(freqs), function(i) sprintf("%s (%s)", names(freqs)[i], freqs[i])))
	doc <- addParagraph(doc, value = var)
	doc <- addPlot(doc, function(){
		with(dat, boxplot(as.formula(form), main = var, names = names, las = 3))
		})
}

plotting_histograms <- function(dat, var, doc){
	print(var)
	doc <- addParagraph(doc, value = var)
	doc <- addPlot(doc, function(){
		hist(dat[, var], 200, main = var)
		})
}

plotting_dots <- function(dat, var, doc){
	print(var)
	doc <- addParagraph(doc, value = var)
	doc <- addPlot(doc, function(){
		plot(x = dat[, var], y = dat$saleprice, main = var)
		})
}
### exploration
doc <- docx()
lapply(nomVar, function(vs) plotting_boxes(house, vs, doc))
lapply(ordVar, function(vs) plotting_boxes(house, vs, doc))
lapply(disVar, function(vs) plotting_boxes(house, vs, doc))
lapply(conVar, function(vs) plotting_histograms(house, vs, doc))
lapply(conVar, function(vs) plotting_dots(house, vs, doc))

writeDoc(doc, "housing_viz.docx")

oneoccur <- unlist(lapply(c(nomVar, ordVar, disVar), function(cl){
	tb <- table(house[, cl])
	if(sum(tb > 1) == length(tb)){
		return(NULL)
	}else{
		return(cl)
	}
	}))

qualitylevels <- c("Ex", "Gd", "TA", "Fa", "Po")

### ms_subclass has 1 level with 1 observation. In terms of saleprice, it fits best in subclass 050 (close to the median)
### so I recoded it to that class
house$ms_subclass[house$ms_subclass == 150] <- 50

### neighborhood
house <- house[order(house$saleprice), ]
plot(house$saleprice)
### install ggvis in order to use tooltip

### condition_2
### based on distributions of saleprice based on condition 2, set to Norm, PosA, PosN and Other
house$condition_2[house$condition_2 %in% c("Artery", "Feedr", "RRAe", "RRNe", "RRNn")] <- "Other"

### roof matl:
### most are concentrated in composite shingles, which has a lot of outliers, tar, wood shingles and wood shakes have higher value though small number
### try combining others into composite shingles
house$roof_matl[house$roof_matl %in% c("ClyTile", "Membrane", "Metal", "Roll")] <- "Compshg"

### exterior_1st: a lot of variance. not used
### exterior_2nd: a lot of variance. not used
### mas_vnr_type: 1 CBlock, fits in the range of None. 23 blank fit within the range of stone
house$mas_vnr_type[house$mas_vnr_type == ""] <- "Stone"
house$mas_vnr_type[house$mas_vnr_type == "CBlock"] <- "None"

### heating: floor and wall fit in with Grav, OthW fits in with GasW
house$heating[house$heating == "OthW"] <- "GasW"
house$heating[house$heating %in% c("Floor", "Wall")] <- "Grav"

### misc_feature: too many nas
### sale_type: too much variance
### utilities: only three don't have public utilities and they still fit in the range of those that do.
### bsmt_qual: poor and blank got with fair
house$bsmt_qual[house$bsmt_qual == "Po"] <- "Fa"
house$bsmt_qual[house$bsmt_qual == ""] <- "Fa"
house$bsmt_qual <- factor(house$bsmt_qual, levels = qualitylevels)
### bsmt_cond: combine TA, good, excellent and poor, fair, blank
house$bsmt_cond[house$bsmt_cond %in% c("TA", "Gd", "Ex")] <- "Good or above"
house$bsmt_cond[house$bsmt_cond %in% c("Po", "Fa", "")] <- "sucks"
house$bsmt_cond <- factor(house$bsmt_cond, levels = c("Good or above", "sucks"))
## bsmtfin_type_1: GLQ or not seems to make a difference
house$bsmtfin_type_1[house$bsmtfin_type_1 != "GLQ"] <- "Not GLQ"

## electrical: the blank seems to be circuit breaker and mix does poorly
house$electrical[house$electrical == ""] <- "SBrkr"
house$electrical[house$electrical == "Mix"] <- "FuseP"

## kitchen_qual: combine poor with fair
house$kitchen_qual[house$kitchen_qual == "Po"] <- "Fa"
house$kitchen_qual <- factor(house$kitchen_qual, qualitylevels[1:4])

### garage_qual: 158 Nas, maybe if I combine it with existence of a garage
house$garage_qual[house$garage_qual == ""] <- "TA"
house$garage_qual <- factor(house$garage_qual, qualitylevels)

house$garage_cond[house$garage_cond == ""] <- "TA"
house$garage_cond <- factor(house$garage_cond, qualitylevels)

house$age <- 2017 - house$year_built

### bedroom_abvgr: no clear relationship, but factor so that it doesn't turn it into a continuous variable and include the 1 8 bedroom in with the six
house$bedroom_abvgr[house$bedroom_abvgr >= 6] <- "6 or more"
house$bedroom_abvgr <- factor(house$bedroom_abvgr, levels = c(0:5, "6 or more"))

## totrms_abvgrd: factor and combine 13, 14, 15 in with 16 or more
house$totrms_abvgrd[house$totrms_abvgrd >= 12] <- "12 or more"
house$totrms_abvgrd <-factor(house$totrms_abvgrd, levels = c(2:11, "12 or more"))

## fireplaces: factor and combine 3 and 4
house$fireplaces[house$fireplaces >= 3] <- "3 or more"
house$fireplaces <- factor(house$fireplaces, levels = c(0:2, "3 or more"))

## garage_yr_blt: turn into continuous variable
house$garage_age <- 2017 - house$garage_yr_blt

## garage_cars: 1 house has a 5 car garage which has more square footage than the living area: look at garage area as a function of living area?
# library(scatterplot3d)
# areasub <- subset(house, lot_area < 4000)
# # scatterplot3d(areasub$lot_area, areasub$total_bsmt_sf, areasub$saleprice)
#
# library(rgl)
# plot3d(areasub$lot_area, areasub$total_bsmt_sf, areasub$saleprice, col="red", size=3)

#####################
##### modeling ######
#####################
# ms_zoning (agr only has 2, C has 25 and I has 2)
# land_contour (different medians, but a lot of overlap in variance)
# neighborhood
# building type
# land_slope
# overall_qual**
# overall_cond** (maybe 1, 5, 9, group 2-4 and 6-8)
# exter_qual
# exter_cond
# bsmt_qual
# bsmt_cond
# year_built (change to age, maybe group by 5 years or decades)
# year_remod_add needs transformation, if same as built year, no remodeling
# gr_liv_area**
# garage_area**
# central_air
# sale_type (combine)
# sale_condition (some overlap with sale_type)
# x1st_flr_sf
# gr_liv_area
# garage_cars (combine 4 and 5)
# fireplaces (combine 3 and 4)
# totrms_abvgrd (combine 12 and higher)
# bsmt_full_bath (combine 2 and 3)
# paved_drive
# garage_finish (combine two blanks with Unf)
# kitchen_qual (combine poor and fair)
# heating_qc
# bsmt_qual (combine blank, Fa, Po)
# exter_qual
# NO pool_area
#
# histograms show some crazy outliers to be eliminated

#### first try without any transformations
## basic cleaning:
cleanhouse <- subset(house, lot_area < 40000)
nas <- unlist(lapply(1:ncol(cleanhouse), function(i) sum(is.na(house[, i]))))
cleanhouse <- cleanhouse[, which(nas < 100)]
cleanhouse$remodeled_age <- cleanhouse$year_remod_add - cleanhouse$year_built
cleanhouse$year_remod_add <- NULL
cleanhouse$order <- NULL

### combine levels with 1 occurence
### factor numeric ordinal variables



smp <- sample(1:nrow(cleanhouse), size = round(nrow(cleanhouse)*0.7))
training <- cleanhouse[smp, ]
testing <- cleanhouse[-smp, ]

randomlm <- function(n){
	vars <- sample(1:(ncol(training)-1), n)
	tmp <- training[, vars]
	tmp$saleprice <- training$saleprice
	mod <- lm(saleprice~ ., data = tmp, na.action = na.omit)
	cf <- summary(mod)$coefficients
	sigvars <- rownames(cf[cf[, 4] < 0.005, ])
	return(list(sigvars, summary(mod)$adj.r))
}

ranmods <- replicate(20, randomlm(15))
highr <- unlist(apply(ranmods, 2, function(md) return(ifelse(md[[2]]> 0.75, 1, 0)))
highmods <- ranmods[, as.logical(highr)]
sigvars <- table(unlist(apply(highmods, 2, function(md) return(md[[1]]))))

randmods10 <- replicate(20, randomlm(10))
highr10 <- unlist(apply(randmods10, 2, function(md) return(ifelse(md[[2]]> 0.75, 1, 0))))
highmods10 <- randmods10[, as.logical(highr10)]
sigvars10 <- table(unlist(apply(highmods10, 2, function(md) return(md[[1]]))))

randmods20 <- replicate(20, randomlm(20))
highr20 <- unlist(apply(randmods20, 2, function(md) return(ifelse(md[[2]]> 0.75, 1, 0))))
highmods20 <- randmods20[, as.logical(highr20)]
sigvars20 <- table(unlist(apply(highmods20, 2, function(md) return(md[[1]]))))

training_sub <- training[, c("saleprice", "garage_cars", "misc_val", "x1st_flr_sf", "bedroom_abvgr", "house_style",
														"overall_qual", "year_built", "bldg_type", "bsmt_unf_sf", "condition_2",
														"lot_area", "overall_cond", "sale_condition", "bsmt_full_bath", "exter_qual",
														"full_bath", "garage_area", "gr_liv_area", "heating_qc", "kitchen_qual", "land_contour",
														"lot_config", "mas_vnr_area", "ms_subclass", "total_bsmt_sf", "bsmt_qual")]
mod20 <- lm(saleprice~ ., training_sub)
mod20pred <- predict(mod20_4, testing[, c("garage_cars", "misc_val", "x1st_flr_sf", "bedroom_abvgr", "house_style",
														"overall_qual", "year_built", "bldg_type", "bsmt_unf_sf", "condition_2",
														"lot_area", "overall_cond", "sale_condition", "bsmt_full_bath", "exter_qual",
														"full_bath", "garage_area", "gr_liv_area", "heating_qc", "kitchen_qual", "land_contour",
														"lot_config", "mas_vnr_area", "ms_subclass", "total_bsmt_sf", "bsmt_qual")])
rmse <- sqrt(sum(testing$saleprice - mod20_4pred)^2/nrow(testing))
