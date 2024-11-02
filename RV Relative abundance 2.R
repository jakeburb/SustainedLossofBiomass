#########################################################################
#                      RV SURVEY RELATIVE ABUNDANCE                     #
#########################################################################
#              Authors: N. Rolland with help from D. Ricard             #
#########################################################################

required.packages <- c("rioja", "vegan", "reshape2")
installed.packages <- data.frame(installed.packages())
`%nin%` = Negate(`%in%`)
not.installed <- required.packages[required.packages %nin% installed.packages$Package]
if(length(not.installed)>0) {install.packages(not.installed, repos="http://mirror.its.dal.ca/cran/")}

rm(list = ls())
library(gulf)
library(reshape2)
library(rioja)
library(vegan)
cat("\f")
clg()

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
fp <- getwd()

rv_survey <- read.surveys(survey = "rv") # temp function during Covid-19

strata <- 415:439

years <- 1971:2021

setwd("../..")
fp_sp <- getwd()

species_list <- read.csv(paste0(fp_sp, "/Liste of species.csv"), sep = ",")

#sp_demersal <- subset(species_list, Group_1 == "demersal")
#sp_pelagic <- subset(species_list, Group_1 == "pelagic")
#sp_commercial <- subset(species_list, Commercial == 1)
#sp_non_commercial <- subset(species_list, Commercial == 0)

sp_crabs <- c(2510, 2511, 2513, 2520, 2523, 2525, 2526, 2527, 2528, 2532)
sp_lobster <- 2550
#sp_shrimps <- c(2100, 2200, 2210, 2211, 2212, 2600, 2700)
sp_shrimps <- taxonomic.group(2100)$species.to.include
#sp_seacucumbers <- c(6600, 6611)

#sp <- species_list$Cat_species_RV
#sp <- c(species_list$Cat_species_RV, sp_crabs, sp_lobster)
sp <- c(species_list$Cat_species_RV, sp_crabs, sp_lobster, taxonomic.group(2100)$species.to.include)

#Pull only good set cards
set<-rv.good.sets(years) 
table(set$year)
set<-subset(set, stratum=strata)
table(set$stratum)

#Load catch cards
cat <- subset(rv$cat, year = years, stratum = strata) 
#excel(cat)

#correct shrimp 2210 error in 1971 and 1973 (0 catch coded as 9999)
cat$weight.caught[cat$species == 2210 | cat$weight.caught == 9999] <- 0

#make minor corrections to catch card
#taxonomic.group(713)
#taxonomic.group(200)
#taxonomic.group(50)
cat$species[cat$species == 727] <- 713 #White Barracudina has two species numbers in the Oracle database, and was entered in 2019 with the second one 

##Herring Correction
#sp <- species_list$Cat_species_RV
oo <- which(cat$species %in% sp)
cat_sp <- cat[oo,]

cat_sp_l <- cat_sp[cat_sp$species != 60, ]
cat_sp_l <- cat_sp_l[cat_sp_l$species != 2550, ]
cat_sp_h <- cat_sp[cat_sp$species == 60, ]
cat_sp_lob <- cat_sp[cat_sp$species == 2550, ]

cat_sp_l_adj <- adjust(cat_sp_l, set)

oo <- (set$year < 1993)
set_h_before <- set[oo,]
oo <- (set$year > 1992)
set_h_after <- set[oo,]

cat_sp_h_adj_before <- adjust(cat_sp_h, set_h_before)
cat_sp_h_adj_after <- adjust(cat_sp_h, set_h_after, vessel = F, day.night = F, distance = T)

set_sp_lob <- set[set$year != 1977, ]
cat_sp_lob_adj <- adjust(cat_sp_lob, set_sp_lob)

cat_sp_adj <- rbind(cat_sp_l_adj, cat_sp_h_adj_before, cat_sp_h_adj_after, cat_sp_lob_adj)

# temporary correct for database error for shrimp with duplicates in 2004 and 2016 for species 2414, for now remove the line with the lowest catch

verify.key(cat_sp_adj)

#[1] "Year = 2004, vessel code = N, cruise number = 446, set number = 90, species = 2414, Duplicate index key found." 
#[2] "Year = 2004, vessel code = N, cruise number = 446, set number = 91, species = 2414, Duplicate index key found." 
#[3] "Year = 2004, vessel code = N, cruise number = 446, set number = 92, species = 2414, Duplicate index key found." 
#[4] "Year = 2016, vessel code = T, cruise number = 661, set number = 168, species = 2414, Duplicate index key found."

cat_sp_adj <- cat_sp_adj[-40986,]
cat_sp_adj <- cat_sp_adj[-41007,]
cat_sp_adj <- cat_sp_adj[-41029,]
cat_sp_adj <- cat_sp_adj[-77156,]

# now merge set and cat

set_sp_adj <- merge.catch(set, cat_sp_adj)
#excel(set_sp_adj)

## wide data frame

my.wide.df <- dcast(cat_sp_adj$year~cat_sp_adj$species, data=cat_sp_adj, value.var=c("weight.caught"), sum)
my.wide.df[1:10,]

#Merge shrimp species into one and remove others
shrimp_col <- which(colnames(my.wide.df) %in% taxonomic.group(2100)$species.to.include)
shrimp <- which(colnames(my.wide.df) == 2100)

for (i in 1:nrow(my.wide.df)) {
  
  my.wide.df[i,shrimp] <- sum(my.wide.df[i, shrimp_col])

}

shrimp_col_other <- shrimp_col[-1] 
my.wide.df <- my.wide.df[,-shrimp_col_other]
                       
## turn into a matrix

my.mat <- as.matrix(my.wide.df[ ,2:ncol(my.wide.df)])
dimnames(my.mat)[[1]] <- years
dimnames(my.mat)[[2]] <- species.str(dimnames(my.mat)[[2]])

## vector with yearly totals

year.vec <- rowSums(my.mat)

## new matrix with yearly percentages

my.perc.mat <- my.mat / year.vec * 100
rowSums(my.perc.mat) 

# remove less abundant taxa

mx <- apply(my.perc.mat, 2, max)
#spec_selected_barplot <- my.perc.mat[, mx > 3.3999]
spec_selected_barplot <- my.perc.mat[, mx > 3.3]

# temporary correction for species name
#colnames(spec_selected_barplot)<- c("Atlantic cod","White hake","Redfish sp.", "Atlantic halibut", "Greenland halibut", "American plaice", "Yellowtail flounder",
#                                    "Winter flounder", "Atlantic herring", "Rainbow smelt", "Capelin", "Atlantic mackerel", "Spiny dogfish", "Black dogfish")

#colnames(spec_selected_barplot) <- c("Atlantic cod", "White hake", "Redfish sp.", "Atlantic halibut", "Greenland halibut", "American plaice", 
#                                     "Yellowtail flounder","Winter flounder", "Atlantic herring","Rainbow smelt", "Capelin", "Atlantic mackerel",
#                                     "Spiny dogfish", "Black dogfish", "Snow crab", "American lobster")

colnames(spec_selected_barplot)<- c("Winter Flounder","Atlantic Cod","American Plaice","Black Dogfish","White Hake","Rainbow Smelt","Spiny Dogfish",
                                     "Yellowtail Flounder","Redfish sp.","Atlantic Herring","Snow Crab","Greenland Halibut","Capelin",
                                     "Decapod sp.","Atlantic Mackerel","Atlantic Halibut","American Lobster") 

# Plot stacked Bar Plot

output <- file.path(fp, paste0("Relative Abundance Bar Plot (all species) with crab and lobster (update Feb 2023) - ", min(years), "-", max(years), ".png"))

png(output, width = 4500, height = 2000, res = 300)

par(mar = c(5, 5, 2, 2))

clr = rev(rainbow(ncol(spec_selected_barplot), s = 1, v = 1, start = 0, end = 0.7, alpha = 0.5))

barplot(t(spec_selected_barplot), space = 0, col = clr, beside = FALSE, legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", inset = c(0.0,0), cex = 1),
        xpd = FALSE, las = 1,
        xlim = c(1.9, length(years)+6), ylim = c(0, 100))

text(cex = 1, x = -3, y = 50, "Relative abundance (%)", xpd = TRUE, srt = 90, font = 2)
text(cex = 1, x = (max(years)-min(years))/2, y = -10, "Year", xpd = TRUE, srt = 0, font = 2)

dev.off()

# remove less abundant taxa

mx <- apply(my.perc.mat, 2, max)
#spec_selected_straplot <- my.perc.mat[, mx > 3.3999]
spec_selected_straplot <- my.perc.mat[, mx > 3.3]

# temporary correction for species name
#colnames(spec_selected_straplot)<- c("Atlantic cod","White hake","Redfish sp.", "Atlantic halibut", "Greenland halibut", "American plaice", "Yellowtail flounder",
#                                    "Winter flounder", "Atlantic herring", "Rainbow smelt", "Capelin", "Atlantic mackerel", "Spiny dogfish", "Black dogfish")

#colnames(spec_selected_straplot)<- c("Atlantic cod","White hake","Redfish sp.", "Atlantic halibut", "Greenland halibut", "American plaice",
#                                     "Yellowtail flounder","Winter flounder", "Atlantic herring","Rainbow smelt", "Capelin", "Atlantic mackerel",
#                                     "Spiny dogfish", "Black dogfish", "Snow crab", "American lobster")

colnames(spec_selected_straplot) <- c("Atlantic Cod","White Hake","Redfish spp.","Atlantic Halibut","Greenland Halibut","American Plaice",
                                      "Yellowtail Flounder","Winter Flounder","Atlantic Herring","Rainbow Smelt","Capelin","Atlantic Mackerel",
                                      "Spiny Dogfish","Black Dogfish","Shrimp spp.","Snow Crab","American Lobster")

# Plot stratigraphic diagram with cluster analysis and zones

diss <- dist(sqrt(my.perc.mat/100)^2)
clust <- chclust(diss, method = "coniss")
bstick(clust)

output <- file.path(fp, paste0("Relative Abundance (selected species) with crab and lobster (update Feb 2023) - ", min(years), "-", max(years), ".png"))

png(output, width = 3800, height = 2500, res = 300)

par(mar = c(5, 5, 2, 1))

#sm.fun <- function(x, y, i, nm) {
#  tmp <- data.frame(x=y, y=x)
#  tmp <- na.omit(tmp)
#  lo <- lowess(tmp, f=0.3)
#  lines(lo$y, lo$x, col="blue", lwd=1)
#}

x <- strat.plot(spec_selected_straplot, yvar = as.numeric(rownames(spec_selected_straplot)),
                scale.percent = TRUE, graph.widths = 1, minmax = NULL, scale.minmax = TRUE,
                xLeft = 0.07, xRight = 0.7, yBottom = 0.075, yTop = 0.88,
                title = "", cex.title = 1.8,
                y.axis = TRUE, x.axis = TRUE, min.width = 10, ylim = NULL, y.rev = FALSE,
                y.tks = seq(min(years), max(years), 2), ylabel = "", cex.ylabel = 1.5, cex.yaxis = 0.9,
                xSpace = 0.008, x.pc.inc = 10, x.pc.lab = TRUE, x.pc.omit0 = TRUE,
                wa.order = "bottomleft", plot.line = TRUE,
                col.line = "black", lwd.line = 1, plot.bar = TRUE,
                lwd.bar = 1, col.bar = "darkgrey", sep.bar = FALSE, bar.back = FALSE,
                plot.poly = TRUE, col.poly = "#7570B340", col.poly.line = NA,
                lwd.poly = 1, plot.symb = FALSE, symb.pch = 19, symb.cex = 1,
                x.names = NULL, cex.xlabel = 0.8, srt.xlabel = 45,
                mgp = NULL, cex.axis = 0.9, clust = NULL, clust.width = 0.1,
                orig.fig = NULL, exag = FALSE, exag.mult = 5, col.exag = "grey90",
                exag.alpha = 0.2, fun2 = NULL, add = FALSE) #fun1 = sm.fun, 

mtext("Relative biomass (%)", side = 1, line = 3.8, cex = 1.3, font = 2)
mtext("Year", side = 2, adj = 0.45, padj = -4.4, cex = 1.3, font = 2)

dca <- decorana(my.perc.mat, iweigh = 1)
sc <- scores(dca, display = "sites", choices = 1:2)
y <- strat.plot(sc, xLeft = 0.7, yvar = as.numeric(rownames(spec_selected_straplot)), y.rev = FALSE, xRight = 0.99, y.axis = FALSE,
           clust = clust, clust.width = 0.08, add = TRUE, srt.xlabel = 45, cex.xlabel = 0.8, x.pc.omit0 = TRUE, cex.axis = 0.9,
           plot.poly=TRUE, col.poly = "#1B9E7740", yTop = 0.88, yBottom = 0.075)

#addZone(x, 1977.5, 1971, col = rgb(1,0,0,0.1))

addClustZone(x, clust, 5, col = "darkred")
addClustZone(y, clust, 5, col = "darkred")

dev.off()

######

setEPS()

output <- file.path(fp, paste0("Relative Abundance (selected species) with crab and lobster (update Feb 2023) - ", min(years), "-", max(years), ".eps"))

postscript(output)

par(mar = c(5, 5, 2, 1))

x <- strat.plot(spec_selected_straplot, yvar = as.numeric(rownames(spec_selected_straplot)),
                scale.percent = TRUE, graph.widths = 1, minmax = NULL, scale.minmax = TRUE,
                xLeft = 0.07, xRight = 0.7, yBottom = 0.075, yTop = 0.88,
                title = "", cex.title = 1.8,
                y.axis = TRUE, x.axis = TRUE, min.width = 10, ylim = NULL, y.rev = FALSE,
                y.tks = seq(min(years), max(years), 2), ylabel = "", cex.ylabel = 1.5, cex.yaxis = 0.9,
                xSpace = 0.008, x.pc.inc = 10, x.pc.lab = TRUE, x.pc.omit0 = TRUE,
                wa.order = "bottomleft", plot.line = TRUE,
                col.line = "black", lwd.line = 1, plot.bar = TRUE,
                lwd.bar = 1, col.bar = "darkgrey", sep.bar = FALSE, bar.back = FALSE,
                plot.poly = TRUE, col.poly = "grey30", col.poly.line = NA,
                lwd.poly = 1, plot.symb = FALSE, symb.pch = 19, symb.cex = 1,
                x.names = NULL, cex.xlabel = 0.8, srt.xlabel = 45,
                mgp = NULL, cex.axis = 0.9, clust = NULL, clust.width = 0.1,
                orig.fig = NULL, exag = FALSE, exag.mult = 5, col.exag = "grey90",
                exag.alpha = 0.2, fun2 = NULL, add = FALSE) #fun1 = sm.fun, 

mtext("Relative biomass (%)", side = 1, line = 3.8, cex = 1.3, font = 2)
mtext("Year", side = 2, adj = 0.45, padj = -4.4, cex = 1.3, font = 2)

dca <- decorana(my.perc.mat, iweigh = 1)
sc <- scores(dca, display = "sites", choices = 1:2)
y <- strat.plot(sc, xLeft = 0.7, yvar = as.numeric(rownames(spec_selected_straplot)), y.rev = FALSE, xRight = 0.99, y.axis = FALSE,
                clust = clust, clust.width = 0.08, add = TRUE, srt.xlabel = 45, cex.xlabel = 0.8, x.pc.omit0 = TRUE, cex.axis = 0.9,
                plot.poly=TRUE, col.poly = "grey30", yTop = 0.88, yBottom = 0.075)

#addZone(x, 1977.5, 1971, col = rgb(1,0,0,0.1))

addClustZone(x, clust, 5, col = "darkred")
addClustZone(y, clust, 5, col = "darkred")

dev.off()
