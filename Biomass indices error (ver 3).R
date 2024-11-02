#########################################################################
#                    RV SURVEY BIOMASS INDICES ERROR                    #
#########################################################################
#                           Author N. Rolland                           #
#########################################################################

rm(list = ls())
library(gulf)
library(lattice)
library(FactoMineR) #for CA analysis
library(data.table)
cat("\f") 
clg()

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
fp <- getwd()

rv_survey <- read.surveys(survey = "rv")

strata <- 415:439

years <- 1971:2021

setwd("../..")
fp_sp <- getwd()

species_list <- read.csv(paste0(fp_sp, "/Liste of species.csv"), sep = ",")

sp_demersal <- subset(species_list, Group_1 == "demersal")
sp_pelagic <- subset(species_list, Group_1 == "pelagic")
sp_commercial <- subset(species_list, Commercial == 1)
sp_non_commercial <- subset(species_list, Commercial == 0)

sp_crabs <- c(2510, 2511, 2513, 2520, 2523, 2525, 2526, 2527, 2528, 2532)
sp_lobster <- 2550
sp_shrimps <- taxonomic.group(2100)$species.to.include
#sp_seacucumbers <- c(6600, 6611)
sp <- c(species_list$Cat_species_RV, sp_crabs, sp_lobster, taxonomic.group(2100)$species.to.use)

##################################################################
# Load set card for all years and strata and apply modifications #
##################################################################

#Pull only good set cards
set<-rv.good.sets(years) 
table(set$year)
set<-subset(set, stratum=strata)
table(set$stratum)

#Load catch cards
cat <- subset(rv$cat, year = years, stratum = strata) 

########################################################
# load data if already extracted other go to next step #
########################################################

load(paste0(fp, "/res.RData"))

#####################################################################
# this will extract the smean for each species if no RData provided #
#####################################################################

res <- data.frame(matrix(ncol = 1, nrow = length(years)))
colnames(res) <- c("year")
res$year <- years

  for (i in 1:length(sp)){
    
    print(species.str(sp[i]))
  
    if (sp[i] == 2100) {
      
      oo <- which(cat$species %in% sp_shrimps)
      cat_sp <- cat[oo,]
      cat_sp$weight.caught[cat_sp$species == 2210 | cat_sp$weight.caught == 9999] <- 0
      
    } else {
    
      oo <- which(cat$species %in% sp[i])
      cat_sp <- cat[oo,]
      
    }
    
    if (sp[i] == 60 | sp[i] == 2550){
      
      if (sp[i] == 60) {
        
      oo <- (set$year < 1993)
      set_before <- set[oo,]
      oo <- (set$year > 1992)
      set_after <- set[oo,]
      
      cat_adj_before <- adjust(cat_sp, set_before)
      set_adj_before <- merge.catch(set_before, cat_adj_before)
      
      cat_adj_after <- adjust(cat_sp, set_after, vessel = F, day.night = F, distance = T)
      set_adj_after <- merge.catch(set_after, cat_adj_after)
      
      set_adj_sp <- rbind(set_adj_before, set_adj_after)

      } else {
        
      cat_sp$weight.caught[cat_sp$year == 1977 | cat_sp$weight.caught == 9999] <- 0 
      cat_sp_lob_adj <- adjust(cat_sp, set)
      set_adj_sp <- merge.catch(set, cat_sp_lob_adj, var = "weight.caught", species.code = T)

      }
      
    } else {
      
      if (sp[i] == 2100) {
        
        cat_adj_sp <- adjust(cat_sp, set)
        verify.key(cat_adj_sp)
        
        #[1] "Year = 2004, vessel code = N, cruise number = 446, set number = 90, species = 2414, Duplicate index key found." 
        #[2] "Year = 2004, vessel code = N, cruise number = 446, set number = 91, species = 2414, Duplicate index key found." 
        #[3] "Year = 2004, vessel code = N, cruise number = 446, set number = 92, species = 2414, Duplicate index key found." 
        #[4] "Year = 2016, vessel code = T, cruise number = 661, set number = 168, species = 2414, Duplicate index key found."
        #Duplicate for 4 sets, will for now remove the lines from the catch card until correction is made in Oracle
        cat_adj_sp <- cat_adj_sp[-1941,]
        cat_adj_sp <- cat_adj_sp[-1950,]
        cat_adj_sp <- cat_adj_sp[-1958,]
        cat_adj_sp <- cat_adj_sp[-14332,]
        set_adj_sp <- merge.catch(set, cat_adj_sp, var = "weight.caught", species.code = T)

        #Merge shrimp species into one and remove others
        shrimp_col <- which(colnames(set_adj_sp) %in% taxonomic.group(2100)$species.to.include)
        shrimp <- which(colnames(set_adj_sp) == 2100)
        
        for (j in 1:nrow(set_adj_sp)) {
          
          set_adj_sp[j,shrimp] <- sum(set_adj_sp[j, shrimp_col])
          
        }
        
        shrimp_col_other <- shrimp_col[-1] 
        set_adj_sp <- set_adj_sp[,-shrimp_col_other]
        
        colnames(set_adj_sp)[shrimp] <- "weight.caught"
        
      } else {
      
      cat_adj_sp <- adjust(cat_sp, set)
      set_adj_sp <- merge.catch(set, cat_adj_sp, var = "weight.caught", species.code = T)
      
      }
    }
    
    smean_weight_sp <- smean(set_adj_sp, var = "weight.caught", by = "year")
    
    # data will be normalized between 0 and 1
    smean_weight_sp$diff <- (smean_weight_sp$mean - min(smean_weight_sp$mean))/(max(smean_weight_sp$mean)-min(smean_weight_sp$mean))
    
    # built matrix
    res <- cbind(res, smean_weight_sp$diff)
    #names(res)[names(res) == "smean_weight_sp$diff"] <- species.str(language = "english", sp[i])
    names(res)[names(res) == "smean_weight_sp$diff"] <- sp[i]
    
    }
  
#save(res, file = paste0(fp, "/res.RData"))

####################################################################
# Select species with at least 10 occurrences over the time series #
####################################################################

res_sel <- (colSums(res != 0))
res_sel <- subset(res_sel, res_sel > 9)
res_sel <- res_sel[-1]
res_sel <- as.data.frame(res_sel)

res_sel <- res[, which(colnames(res) %in% as.numeric(rownames(res_sel)))]

res_norm <- t(res_sel)
res_norm[is.nan(res_norm)] <- 0
res_norm <- res_norm[rowSums(res_norm[])>0,]

###############################################################
# Correspondence analysis to sort species abundance over time #
###############################################################

ord <- FactoMineR::CA(t(res_norm), ncp = 5, graph = T)
ord_res <- ord$col$coord[,1]

#sort species base on CA score 1

ord_sort <- sort(ord_res, decreasing = T)
ord_sort <- row.names(as.data.frame(ord_sort))

# number of species considered

length(ord_sort)

#sort entire table based on CA score 1

test <- setcolorder(as.data.table(t(res_norm)), as.character(ord_sort))
res_norm <- t(test)
row.names(res_norm) <- species.str(row.names(res_norm))

####################
# Prepare for plot #
####################

lattice.options(default.theme = standard.theme(color = FALSE))

mycolorkey <- list(at = seq(floor(min(res_norm)), ceiling(max(res_norm)), by = 0.01),
                   labels = list(at = seq(floor(min(res_norm)), ceiling(max(res_norm)), by = 0.1)), space = "right")

cols <- colorRampPalette(c("white", "#7570B3"))

output <- file.path(fp, paste0("RV survey total biomass indices by size error (final round 2)", " - ", min(years),"-",max(years),".png"))

png(output, width = 3500, height = 4500, res = 300)

par(mar = c(0, 0, 0, 0))

levelplot(t(res_norm),
          pretty = FALSE,
          cuts = 10,
          contour = FALSE,
          colorkey = mycolorkey,
          scales=list(x=list(at=seq(1, 49, by = 2))),
          col.regions = cols,
          xlab = "Year",
          ylab = "")

dev.off()

###############################
# Export file for Illustrator #
###############################

setEPS()

output_eps <- file.path(fp, paste0("RV survey total biomass indices by size error (final round 2)", " - ", min(years),"-",max(years),".eps"))

#postscript(paste0("RV survey total biomass indices by size error (final)", " - ", min(years),"-",max(years),".eps"), horizontal = FALSE, onefile = FALSE, paper = "special")
postscript(output_eps, horizontal = FALSE, onefile = FALSE, paper = "special")

levelplot(t(res_norm),
          pretty = FALSE,
          cuts = 10,
          contour = FALSE,
          colorkey = mycolorkey,
          scales=list(x=list(at=seq(1, 49, by = 2))),
          col.regions = cols,
          xlab = "Year",
          ylab = "")

dev.off()


###################
#                 #
# #### #   # ###  #
# #    ##  # #  # #
# ###  # # # #  # #
# #    #  ## #  # #
# #### #   # ###  #
#                 #
###################




#########
#color.ramp.length <- 100
#negative.length <- round(abs(range(res_temp)) / 
#                           diff(range(res_temp)) * 
#                           color.ramp.length)
#positive.length <- color.ramp.length - negative.length


heat.colors(100)


####

diverge.color <- function(start.color,end.color,min.value,max.value,mid.value=0,mid.color="ivory")
  
{
  # based on ideas from Maureen Kennedy, Nick Povak, and Alina Cansler
  
  # creates a palette for the current session for a divergent-color
  # graphic with a non-symmetric range
  # "cuts" = the number of slices to be made in the range above and below "mid.value"
  
  ramp1 <- colorRampPalette(c(start.color,mid.color))
  ramp2 <- colorRampPalette(c(mid.color,end.color))
  
  # now specify the number of values on either side of "mid.value"
  
  max.breaks <- round(max.value - mid.value)
  min.breaks <- round(mid.value - min.value)
  
  num.breaks <- max(max.breaks,min.breaks)
  
  low.ramp <- ramp1(num.breaks)
  high.ramp <- ramp2(num.breaks)
  
  # now create a combined ramp from the higher values of "low.ramp" and 
  # the lower values of "high.ramp", with the longer one using all values 
  # high.ramp starts at 2 to avoid duplicating zero
  
  myColors <- c(low.ramp[(num.breaks-min.breaks):num.breaks],high.ramp[2:max.breaks])
  
  myColors
}
test<-diverge.color("red", "blue", min.value = -1, max.value =  15, mid.value =  0, "white")
