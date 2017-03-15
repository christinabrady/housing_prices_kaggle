options(stringsAsFactors = FALSE)
setwd("Documents/kaggle/housing/")
source("~/Documents/codebase/auto/ckit.R")
library(ReporteRs)
library(ggvis)
library(dplyr)
library(corrplot)

house <- read.csv("AmesHousing.csv")

cleanColNames <- function(dat){
	tolower(gsub("\\.", "", colnames(dat)))
}
colnames(house) <- cleanColNames(house)

### categorize variable types for plotting, according to documentation
nomVar <- c("mssubclass", "mszoning", "street", "alley", "landcontour", "lotconfig", "neighborhood", "condition1", "condition2", "bldgtype", "housestyle", "roofstyle", "roofmatl", "exterior1st", "exterior2nd", "masvnrtype", "foundation", "heating", "centralair", "garagetype", "miscfeature", "saletype", "salecondition", "electrical")
ordVar <- c("lotshape", "utilities", "landslope", "overallqual", "overallcond", "exterqual", "extercond", "bsmtqual", "bsmtcond", "bsmtexposure", "bsmtfintype1", "bsmtfintype2", "heatingqc", "electrical", "kitchenqual", "functional", "fireplacequ", "garagefinish", "garagequal", "garagecond", "paveddrive", "poolqc", "fence")
disVar <- c("yearbuilt", "yearremodadd", "bsmtfullbath", "bsmthalfbath", "bedroomabvgr", "kitchenabvgr", "totrmsabvgrd", "fireplaces", "garageyrblt", "garagecars", "mosold", "yrsold")
conVar <- c("lotfrontage", "lotarea", "masvnrarea", "bsmtfinsf1", "bsmtfinsf2", "bsmtunfsf", "totalbsmtsf", "x1stflrsf", "x2ndflrsf", "lowqualfinsf", "grlivarea", "garagearea", "wooddecksf", "openporchsf", "enclosedporch", "screenporch", "poolarea", "miscval")


### plotting functions ###
plotBoxes <- function(dat, var, doc){
	print(var)
	form <- paste("saleprice~", var)
	freqs <- table(house[, var])
	names <- unlist(lapply(1:length(freqs), function(i) sprintf("%s (%s)", names(freqs)[i], freqs[i])))
	doc <- addParagraph(doc, value = var)
	doc <- addPlot(doc, function(){
		with(dat, boxplot(as.formula(form), main = var, names = names, las = 3))
		})
}

plotHistograms <- function(dat, var, doc){
	print(var)
	doc <- addParagraph(doc, value = var)
	doc <- addPlot(doc, function(){
		hist(dat[, var], 200, main = var)
		})
}

plotDots <- function(dat, var, doc){
	print(var)
	doc <- addParagraph(doc, value = var)
	doc <- addPlot(doc, function(){
		plot(x = dat[, var], y = dat$saleprice, main = var)
		})
}
### exploration
# doc <- docx()
# lapply(nomVar, function(vs) plotBoxes(house, vs, doc))
# lapply(ordVar, function(vs) plotBoxes(house, vs, doc))
# lapply(disVar, function(vs) plotBoxes(house, vs, doc))
# lapply(conVar, function(vs) plotHistograms(house, vs, doc))
# lapply(conVar, function(vs) plotDots(house, vs, doc))
#
# writeDoc(doc, "housingviz.docx")

### How many and which categorical variables have levels with only one observation?
### These will cause problems when splitting the data into training and test sets and running regressions. ###
oneoccur <- unlist(lapply(c(nomVar, ordVar, disVar), function(cl){
	tb <- table(house[, cl])
	if(sum(tb > 1) == length(tb)){
		return(NULL)
	}else{
		return(cl)
	}
	}))

qualitylevels <- c("Ex", "Gd", "TA", "Fa", "Po")

dataManip <- function(dat){
	### mssubclass has 1 level with 1 observation. In terms of saleprice, it fits best in subclass 050 (close to the median)
	### so I recoded it to that class
	dat$mssubclass[dat$mssubclass == 150] <- 50

	dat$mszoning[dat$mszoning == "I (all)"] <- "C (all)"
	### condition2
	### based on distributions of saleprice based on condition 2, set to Norm, PosA, PosN and Other
	dat$condition2[dat$condition2 %in% c("Artery", "Feedr", "RRAe", "RRNe", "RRNn")] <- "Other"

	### roof matl:
	### most are concentrated in composite shingles, which has a lot of outliers, tar, wood shingles and wood shakes have higher value though small number
	### try combining others into composite shingles
	dat$roofmatl[dat$roofmatl %in% c("ClyTile", "Membrane", "Metal", "Roll")] <- "Compshg"

	### exterior1st: a lot of variance. not used
	### exterior2nd: a lot of variance. not used
	### masvnrtype: 1 CBlock, fits in the range of None. 23 blank fit within the range of stone
	dat$masvnrtype[dat$masvnrtype == ""] <- "Stone"
	dat$masvnrtype[dat$masvnrtype == "CBlock"] <- "None"

	### heating: floor and wall fit in with Grav, OthW fits in with GasW
	dat$heating[dat$heating == "OthW"] <- "GasW"
	dat$heating[dat$heating %in% c("Floor", "Wall")] <- "Grav"

	### miscfeature: too many nas
	### saletype: VWD becomes ConLi whose median is the closest
	dat$saletype[dat$saletype == "VWD"] <- "ConLI"
	### utilities: only three don't have public utilities and they still fit in the range of those that do.
	### bsmtqual: poor and blank got with fair
	dat$bsmtqual[dat$bsmtqual == "Po"] <- "Fa"
	dat$bsmtqual[dat$bsmtqual == ""] <- "Fa"
	dat$bsmtqual[is.na(dat$bsmtqual)] <- "no basement"
	dat$bsmtqual <- factor(dat$bsmtqual, levels = c(qualitylevels, "no basement"))

	### bsmtcond: combine TA, good, excellent and poor, fair, blank
	dat$bsmtcond[dat$bsmtcond %in% c("TA", "Gd", "Ex")] <- "Good or above"
	dat$bsmtcond[dat$bsmtcond %in% c("Po", "Fa", "")] <- "sucks"
	dat$bsmtcond <- factor(dat$bsmtcond, levels = c("Good or above", "sucks"))

	## bsmtfintype1: GLQ or not seems to make a difference
	dat$bsmtfintype1[dat$bsmtfintype1 != "GLQ"] <- "Not GLQ"

	## electrical: the blank seems to be circuit breaker and mix does poorly
	dat$electrical[dat$electrical == ""] <- "SBrkr"
	dat$electrical[dat$electrical == "Mix"] <- "FuseP"

	## kitchenqual: combine poor with fair
	dat$kitchenqual[dat$kitchenqual == "Po"] <- "Fa"
	dat$kitchenqual <- factor(dat$kitchenqual, qualitylevels[1:4])

	### garagequal: 158 Nas, maybe if I combine it with existence of a garage
	dat$garagequal[dat$garagequal == ""] <- "TA"
	dat$garagequal <- factor(dat$garagequal, qualitylevels)

	dat$garagecond[dat$garagecond == ""] <- "TA"
	dat$garagecond <- factor(dat$garagecond, qualitylevels)

	## year built: turn into a continous variable
	dat$age <- 2017 - dat$yearbuilt

	### bedroomabvgr: no clear relationship, but factor so that it doesn't turn it into a continuous variable and include the 1 8 bedroom in with the six
	dat$bedroomabvgr[dat$bedroomabvgr >= 6] <- "6 or more"
	dat$bedroomabvgr <- factor(dat$bedroomabvgr, levels = c(0:5, "6 or more"))

	## totrmsabvgrd: factor and combine 13, 14, 15 in with 16 or more
	dat$totrmsabvgrd[dat$totrmsabvgrd >= 12] <- "12 or more"
	dat$totrmsabvgrd <-factor(dat$totrmsabvgrd, levels = c(2:11, "12 or more"))

	## fireplaces: factor and combine 3 and 4
	dat$fireplaces[dat$fireplaces >= 3] <- "3 or more"
	dat$fireplaces <- factor(dat$fireplaces, levels = c(0:2, "3 or more"))

	## garageyrblt: turn into continuous variable: age
	dat$garageage <- 2017 - dat$garageyrblt
	dat$remodeledage <- dat$yearremodadd - dat$yearbuilt
	dat$yearremodadd <- NULL
	dat$order <- NULL

	dat

}

cleanhouse <- dataManip(house)
## garagecars: 1 house has a 5 car garage which has more square footage than the living area: look at garage area as a function of living area?
with(house, boxplot(grlivarea~garagecars)) ### in living area and price, it is similar to one car garage, 92 year old single family home and the garage is unfinished


#####################
##### modeling ######
#####################
#### first try without any transformations
## basic cleaning:
cleanhouse <- subset(house, lotarea < 40000)
nas <- unlist(lapply(1:ncol(cleanhouse), function(i) sum(is.na(house[, i]))))
cleanhouse <- cleanhouse[, which(nas < 100)]

smp <- sample(1:nrow(cleanhouse), size = round(nrow(cleanhouse)*0.7))
training <- cleanhouse[smp, ]
testing <- cleanhouse[-smp, ]

### test my assumptions + number of predictors
randomlm <- function(n){
	vars <- sample(1:(ncol(training)-1), n)
	tmp <- training[, vars]
	tmp$saleprice <- training$saleprice
	mod <- lm(saleprice~ ., data = tmp, na.action = na.omit)
	cf <- summary(mod)$coefficients
	sigvars <- rownames(cf[cf[, 4] < 0.005, ])
	return(list(sigvars, summary(mod)$adj.r))
}

set.seed(314)

ranmods <- replicate(30, randomlm(10))
highr <- unlist(apply(ranmods, 2, function(md) return(ifelse(md[[2]]> 0.75, 1, 0))))
highmods <- ranmods[, as.logical(highr)]
sigvars <- table(unlist(apply(highmods, 2, function(md) return(md[[1]]))))

randmods15 <- replicate(30, randomlm(15))
highr15 <- unlist(apply(randmods15, 2, function(md) return(ifelse(md[[2]]> 0.75, 1, 0))))
highmods15 <- randmods15[, as.logical(highr15)]
sigvars15 <- table(unlist(apply(highmods15, 2, function(md) return(md[[1]]))))

randmods20 <- replicate(30, randomlm(20))
highr20 <- unlist(apply(randmods20, 2, function(md) return(ifelse(md[[2]]> 0.75, 1, 0))))
highmods20 <- randmods20[, as.logical(highr20)]
sigvars20 <- table(unlist(apply(highmods20, 2, function(md) return(md[[1]]))))

randmods30 <- replicate(30, randomlm(30))
highr30 <- unlist(apply(randmods30, 2, function(md) return(ifelse(md[[2]]> 0.75, 1, 0))))
highmods30 <- randmods30[, as.logical(highr30)]
sigvars30 <- table(unlist(apply(highmods30, 2, function(md) return(md[[1]]))))


### 20 predictors improves the minimun R2, 10 predictors sucks, changing the number of replications changes the significant variables

### from looking at the visualizations of the data, the following variables look like good predictors
mypreds <- c("mszoning", "street", "landcontour", "condition1", "roofstyle", "foundation",
	"centralair", "saletype", "electrical", "landslope", "overallqual", "exterqual", "bsmtqual",
	"heatingqc", "electrical", "kitchenqual", "paveddrive", "bsmtfullbath",
	"totrmsabvgrd", "fireplaces", "garagecars", "totalbsmtsf", "x1stflrsf", "grlivarea",
	"lotarea")

### potential transformations:

## maybe a collapsed bldgtype, collapsed overallcond, collapsed extercond, collapsed bsmtfintype1, collapsed bedroomabvgr
## is bsmtexposure correlated with anything else?
## is garage area correlated with garagecars? Yes! 0.89

### there seems to be a floor on price by age: log of age is negatively correlated with log of saleprice

mod1 <- lm(as.formula(sprintf("saleprice ~ %s", paste(mypreds, collapse = "+"))), data = training)
par(mfrow = c(2,2))
plot(mod1)
summary(mod1)

mod2 <- lm(as.formula(sprintf("log(saleprice) ~ %s", paste(mypreds, collapse = "+"))), data = training)
par(mfrow = c(2,4))
plot(mod1)
plot(mod2)
summary(mod2) ### slightly improves r2 and more variables are significant


### trim down even further:
mypreds2 <- c("mszoning", "landcontour", "condition1", "roofstyle", "foundation",
	"centralair", "saletype", "electrical", "landslope", "overallqual", "exterqual", "bsmtqual",
	"heatingqc", "electrical", "kitchenqual", "paveddrive", "bsmtfullbath",
	"fireplaces", "garagecars", "totalbsmtsf", "grlivarea",
	"lotarea")
mod3 <- lm(as.formula(sprintf("log(saleprice) ~ %s", paste(mypreds2, collapse = "+"))), data = training)
summary(mod3)

### fucking change colnames to fit the fucking submission file colnames
colnames(training) <- gsub("", "", colnames(training))
mypreds2 <- gsub("", "", mypreds2)
mod3 <- lm(as.formula(sprintf("log(saleprice) ~ %s", paste(mypreds2, collapse = "+"))), data = training)

rmse <- sqrt(mean((log(testing$saleprice) - predict(mod3, testing))^2, na.rm = TRUE))

### test mod3
kaggletest <- read.csv("test.csv")
colnames(kaggletest) <- cleanColNames(kaggletest)
ret <- data.frame(Id = kaggletest$id, SalePrice = predict(mod3, kaggletest))
