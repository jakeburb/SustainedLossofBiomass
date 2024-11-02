####################################
##Marine Fish Trends Analysis 2022#####
######By: Jacob Burbank#########

rm(list = ls())
library(gulf)
library(lattice)
cat("\f")
clg()

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
fp <- getwd()

###################TOTAL BIOMASS ANALYSES#################
######Pull species list and subset by species group###
strata <- 415:439

years <- 1971:2021

species_list <- read.csv(paste0("W:/MarFish/Shared/Marine fish trends/Liste of species.csv"), sep = ",")

sp_demersal <- subset(species_list, Group_1 == "demersal")
sp_pelagic <- subset(species_list, Group_1 == "pelagic")
sp_commercial <- subset(species_list, Commercial == 1)
sp_non_commercial <- subset(species_list, Commercial == 0)

sp_crabs <- c(2510, 2511, 2513, 2520, 2523, 2525, 2526, 2527, 2528, 2532)
sp_lobster <- 2550
sp_shrimps <- c(2100, 2200, 2210, 2211, 2212, 2600, 2700)
sp_seacucumbers <- c(6600, 6611)

rv<-read.surveys(survey = "rv")

###########All Fish########
###First Estimate and plot total biomass (kg/tow) and abundance (number/tow)#####
#Pull only good set cards
set<-rv.good.sets(years) 
table(set$year)
set<-subset(set, stratum=strata)
table(set$stratum)

#Load catch cards
cat<-subset(rv$cat, year = years, stratum = strata)
#cat<-read.card(year=years, stratum=strata, card.type = "catch")

#make minor corrections to catch card
taxonomic.group(713)
taxonomic.group(200)
taxonomic.group(50)
cat$species[cat$species == 727] <- 713 #White Barracudina has two species numbers in the Oracle database, and was entered in 2019 with the second one 


#adjust catch data
adjustedcatchcards<-adjust(cat, set.card = set)
#adjust length data
#adjustedlengthcards<-adjust(lengthcards, set.card = set)

#Merge all fish catch cards
#mergedALLfishcatch<-merge.catch(set, catch.card=adjustedcatchcards, species = species_list$Cat_species_RV)
#str(mergedALLfishcatch)
#mergedALLfishcatch[is.na(mergedALLfishcatch)]= 0
#Merge all fish length cards
#mergedALLfishlength<-merge.length(set, length.card=adjustedlengthcards, species = species_list$Cat_species_RV)

#Specify rows with weight caught and number caught
#rows_weight.caught <- grep("weight.caught", names(mergedALLfishcatch))
#rows_number.caught <- grep("number.caught", names(mergedALLfishcatch))

#Sume weight caught and number caught for all fish
#mergedALLfishcatch$allfish.weight.caught <- rowSums(mergedALLfishcatch[,rows_weight.caught])
#mergedALLfishcatch$allfish.number.caught <- rowSums(mergedALLfishcatch[,rows_number.caught])

##Herring Correction
sp <- species_list$Cat_species_RV
oo <- which(cat$species %in% sp)
cat_sp <- cat[oo,]

if (60 %in% sp){
  
  cat_sp_l <- cat_sp[cat_sp$species != 60,]
  cat_sp_h <- cat_sp[cat_sp$species == 60, ]
  
  cat_sp_l_adj <- adjust(cat_sp_l, set)
  
  oo <- (set$year < 1993)
  set_h_before <- set[oo,]
  oo <- (set$year > 1992)
  set_h_after <- set[oo,]
  
  cat_sp_h_adj_before <- adjust(cat_sp_h, set_h_before)
  cat_sp_h_adj_after <- adjust(cat_sp_h, set_h_after, vessel = F, day.night = F, distance = T)
  
  cat_sp_adj <- rbind(cat_sp_l_adj, cat_sp_h_adj_before, cat_sp_h_adj_after)
  
  set_sp_adj <- merge.catch(set, cat_sp_adj, species = species_list$Cat_species_RV)
  
} else {
  
  cat_sp_adj <- adjust(cat_sp, set)
  set_sp_adj <- merge.catch(set, cat_sp_adj, species = species_list$Cat_species_RV)
  
}

if (length(sp) > 1){
  
  rows_weight.caught <- grep("weight.caught", names(set_sp_adj))
  rows_number.caught <- grep("number.caught", names(set_sp_adj))
  
  set_sp_adj$allfish.weight.caught <- rowSums(set_sp_adj[,rows_weight.caught])
  set_sp_adj$allfish.number.caught <- rowSums(set_sp_adj[,rows_number.caught])
  
  smean_set_sp_adj_weight <- smean(set_sp_adj, var = "allfish.weight.caught", by = "year")
  smean_set_sp_adj_number <- smean(set_sp_adj, var = "allfish.number.caught", by = "year")
  
} else {
  
  smean_set_sp_adj_weight <- smean(set_sp_adj, var = "weight.caught", by = "year")
  smean_set_sp_adj_number <- smean(set_sp_adj, var = "number.caught", by = "year")
  
}

#Stratified mean for all fish
#smean_set_sp_adj_weight <- smean(mergedALLfishcatch, var = "allfish.weight.caught", by = "year")
#smean_set_sp_adj_number <- smean(mergedALLfishcatch, var = "allfish.number.caught", by = "year")

###Mean, min and max of total biomass by weight and number over time series for all fish
#Weight
mean(smean_set_sp_adj_weight$mean)
smean_set_sp_adj_weight[which.max(smean_set_sp_adj_weight$mean),]
smean_set_sp_adj_weight[which.min(smean_set_sp_adj_weight$mean),]
#Number
mean(smean_set_sp_adj_number$mean)
smean_set_sp_adj_number[which.max(smean_set_sp_adj_number$mean),]
smean_set_sp_adj_number[which.min(smean_set_sp_adj_number$mean),]

## Plot

#output <- file.path(fp, paste0("RV survey total abundance indices v4b All fish ",
#                               min(years),"-",max(years), ".png"))

output <- file.path(fp, paste0("RV survey total abundance indices v4b All fish ",
                               min(years),"-",max(years), ".eps"))

#png(output, width = 3500, height = 2500, res = 300)
postscript(output)

#par(mar = c(5, 5.5, 2, 5.5))
par(mar = c(5, 5.5, 0.5, 5.5))

plot(smean_set_sp_adj_weight$year, smean_set_sp_adj_weight$mean, xaxs = "i", yaxs = "i", xaxt = 'n', yaxt = 'n', bty = 'n',
     pch = '', ylab = '', xlab = '', ylim = c(0, round(max(smean_set_sp_adj_weight$upper.cl)+4.99, -1)))
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_number$year, smean_set_sp_adj_number$mean, type = "l", lty = 2, xlab = "", ylab = "", col = "#1B9E77",
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_number$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_number[,"lower.cl"], rev(smean_set_sp_adj_number[,"upper.cl"])),
                               border = "grey", col = "#1B9E7740"))
    
    points(smean_set_sp_adj_number$year, smean_set_sp_adj_number$mean, col = "#1B9E77", pch = 1, cex = 0.8)
    
    y_by <- round(max(smean_set_sp_adj_number$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 200, 20,
                   ifelse(y_by > 200 & y_by <= 500, 50,
                          ifelse(y_by > 500 & y_by <= 1000, 100,
                                 ifelse(y_by > 1000 & y_by <= 2000, 250,
                                        ifelse(y_by > 2000 & y_by <= 5000, 500,
                                               ifelse(y_by > 5000 & y_by <= 1000, 750, 1000))))))
    
    axis(4, at = seq(0, round(max(smean_set_sp_adj_number$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Abundance (number per tow)", side = 4, line = 3.5, cex = 1.6, font = 2, padj = 0.5)
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_weight$year, smean_set_sp_adj_weight$mean, type = "l", xlab = "", ylab = "", col = "#D95F02", #7570B3
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_weight$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_weight[,"lower.cl"], rev(smean_set_sp_adj_weight[,"upper.cl"])),
                               border = "grey", col = "#D95F0240")) #7570B340"
    
    points(smean_set_sp_adj_weight$year, smean_set_sp_adj_weight$mean, col = "#D95F02", pch = 19, cex = 0.8) #7570B3
    
    axis(1, at = seq(min(years), max(years), by = 3), cex.axis = 1.2)
    
    y_by <- round(max(smean_set_sp_adj_weight$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 20, 2,
                   ifelse(y_by > 20 & y_by <= 50, 5,
                          ifelse(y_by > 50 & y_by <= 100, 10,
                                 ifelse(y_by > 100 & y_by <= 200, 25,
                                        ifelse(y_by > 200 & y_by <= 500, 50,
                                               ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
    
    axis(2, at = seq(0, round(max(smean_set_sp_adj_weight$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
    mtext("Biomass (kg per tow)", side = 2, line = 3.5, cex = 1.6, font = 2)
    
    #title(paste0("RV survey total biomass indices - ", sp_text, " - ",  min(years), "-", max(years)))
    mtext("All fish", side = 3, line = -1.5, font = 2, cex = 1.5)
    
    dev.off()
    


    
    
    
################Commercial Fish Only########
    
    ##Herring Correction
    sp <- species_list$Cat_species_RV
    oo <- which(cat$species %in% sp)
    cat_sp <- cat[oo,]
    
    if (60 %in% sp){
      
      cat_sp_l <- cat_sp[cat_sp$species != 60,]
      cat_sp_h <- cat_sp[cat_sp$species == 60, ]
      
      cat_sp_l_adj <- adjust(cat_sp_l, set)
      
      oo <- (set$year < 1993)
      set_h_before <- set[oo,]
      oo <- (set$year > 1992)
      set_h_after <- set[oo,]
      
      cat_sp_h_adj_before <- adjust(cat_sp_h, set_h_before)
      cat_sp_h_adj_after <- adjust(cat_sp_h, set_h_after, vessel = F, day.night = F, distance = T)
      
      cat_sp_adj <- rbind(cat_sp_l_adj, cat_sp_h_adj_before, cat_sp_h_adj_after)
      
      set_sp_adjCOM <- merge.catch(set, cat_sp_adj, species = sp_commercial$Cat_species_RV)
      
    } else {
      
      cat_sp_adj <- adjust(cat_sp, set)
      set_sp_adjCOM <- merge.catch(set, cat_sp_adj, species = sp_commercial$Cat_species_RV)
      
    }
    
    if (length(sp) > 1){
      
      rows_weight.caughtCOM <- grep("weight.caught", names(set_sp_adjCOM))
      rows_number.caughtCOM <- grep("number.caught", names(set_sp_adjCOM))
      
      set_sp_adjCOM$allfish.weight.caughtCOM <- rowSums(set_sp_adjCOM[,rows_weight.caughtCOM])
      set_sp_adjCOM$allfish.number.caughtCOM <- rowSums(set_sp_adjCOM[,rows_number.caughtCOM])
      
      smean_set_sp_adj_weightCOM <- smean(set_sp_adjCOM, var = "allfish.weight.caughtCOM", by = "year")
      smean_set_sp_adj_numberCOM <- smean(set_sp_adjCOM, var = "allfish.number.caughtCOM", by = "year")
      
    } else {
      
      smean_set_sp_adj_weightCOM <- smean(set_sp_adjCOM, var = "weight.caught", by = "year")
      smean_set_sp_adj_numberCOM <- smean(set_sp_adjCOM, var = "number.caught", by = "year")
      
    }
    #Stratified mean for all fish
    #smean_set_sp_adj_weightCOM <- smean(mergedALLfishcatchCOM, var = "allfish.weight.caughtCOM", by = "year")
    #smean_set_sp_adj_numberCOM <- smean(mergedALLfishcatchCOM, var = "allfish.number.caughtCOM", by = "year")

###Mean, min and max of total biomass by weight and number over time series for commercial fish
#Weight
mean(smean_set_sp_adj_weightCOM$mean)
smean_set_sp_adj_weightCOM[which.max(smean_set_sp_adj_weightCOM$mean),]
smean_set_sp_adj_weightCOM[which.min(smean_set_sp_adj_weightCOM$mean),]
#Number
mean(smean_set_sp_adj_numberCOM$mean)
smean_set_sp_adj_numberCOM[which.max(smean_set_sp_adj_numberCOM$mean),]
smean_set_sp_adj_numberCOM[which.min(smean_set_sp_adj_numberCOM$mean),]
    
    
    ## Plot
    
    #output <- file.path(fp, paste0("RV survey total abundance indices v4b Commercial fish ",
    #                               min(years),"-",max(years), ".png"))
    output <- file.path(fp, paste0("RV survey total abundance indices v4b Commercial fish ",
                                   min(years),"-",max(years), ".eps"))
    
    #png(output, width = 3500, height = 2500, res = 300)
    postscript(output)
    
    #png(output, width = 3500, height = 2500, res = 300)
    
    #par(mar = c(5, 5.5, 2, 5.5))
    par(mar = c(5, 5.5, 0.5, 5.5))
    
    plot(smean_set_sp_adj_weightCOM$year, smean_set_sp_adj_weightCOM$mean, xaxs = "i", yaxs = "i", xaxt = 'n', yaxt = 'n', bty = 'n',
         pch = '', ylab = '', xlab = '', ylim = c(0, round(max(smean_set_sp_adj_weightCOM$upper.cl)+4.99, -1)))
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_numberCOM$year, smean_set_sp_adj_numberCOM$mean, type = "l", lty = 2, xlab = "", ylab = "", col = "#1B9E77",
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_numberCOM$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_numberCOM[,"lower.cl"], rev(smean_set_sp_adj_numberCOM[,"upper.cl"])),
                               border = "grey", col = "#1B9E7740"))
    
    points(smean_set_sp_adj_numberCOM$year, smean_set_sp_adj_numberCOM$mean, col = "#1B9E77", pch = 1, cex = 0.8)
    
    y_by <- round(max(smean_set_sp_adj_numberCOM$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 200, 20,
                   ifelse(y_by > 200 & y_by <= 500, 50,
                          ifelse(y_by > 500 & y_by <= 1000, 100,
                                 ifelse(y_by > 1000 & y_by <= 2000, 250,
                                        ifelse(y_by > 2000 & y_by <= 5000, 500,
                                               ifelse(y_by > 5000 & y_by <= 1000, 750, 1000))))))
    
    axis(4, at = seq(0, round(max(smean_set_sp_adj_numberCOM$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Abundance (number per tow)", side = 4, line = 3.5, cex = 1.6, font = 2, padj = 0.5)
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_weightCOM$year, smean_set_sp_adj_weightCOM$mean, type = "l", xlab = "", ylab = "", col = "#D95F02", #7570B3
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_weightCOM$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_weightCOM[,"lower.cl"], rev(smean_set_sp_adj_weightCOM[,"upper.cl"])),
                               border = "grey", col = "#D95F0240")) #7570B340"
    
    points(smean_set_sp_adj_weightCOM$year, smean_set_sp_adj_weightCOM$mean, col = "#D95F02", pch = 19, cex = 0.8) #7570B3
    
    axis(1, at = seq(min(years), max(years), by = 3), cex.axis = 1.2)
    
    y_by <- round(max(smean_set_sp_adj_weightCOM$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 20, 2,
                   ifelse(y_by > 20 & y_by <= 50, 5,
                          ifelse(y_by > 50 & y_by <= 100, 10,
                                 ifelse(y_by > 100 & y_by <= 200, 25,
                                        ifelse(y_by > 200 & y_by <= 500, 50,
                                               ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
    
    axis(2, at = seq(0, round(max(smean_set_sp_adj_weightCOM$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
    mtext("Biomass (kg per tow)", side = 2, line = 3.5, cex = 1.6, font = 2)
    
    #title(paste0("RV survey total biomass indices - ", sp_text, " - ",  min(years), "-", max(years)))
    mtext("Commercial fish", side = 3, line = -1.5, font = 2, cex = 1.5)
    
    dev.off()
    
#####Non Commercial####
#Merge all fish catch cards
mergedALLfishcatchNCOM<-merge.catch(set, catch.card=adjustedcatchcards, species = sp_non_commercial$Cat_species_RV)
    
#Specify rows with weight caught and number caught
rows_weight.caughtNCOM <- grep("weight.caught", names(mergedALLfishcatchNCOM))
rows_number.caughtNCOM <- grep("number.caught", names(mergedALLfishcatchNCOM))
    
#Sum weight caught and number caught for all fish
mergedALLfishcatchNCOM$allfish.weight.caughtNCOM <- rowSums(mergedALLfishcatchNCOM[,rows_weight.caughtNCOM])
mergedALLfishcatchNCOM$allfish.number.caughtNCOM <- rowSums(mergedALLfishcatchNCOM[,rows_number.caughtNCOM])
    
#Stratified mean for all fish
smean_set_sp_adj_weightNCOM <- smean(mergedALLfishcatchNCOM, var = "allfish.weight.caughtNCOM", by = "year")
smean_set_sp_adj_numberNCOM <- smean(mergedALLfishcatchNCOM, var = "allfish.number.caughtNCOM", by = "year")
    
###Mean, min and max of total biomass by weight and number over time series for all fish
#Weight
mean(smean_set_sp_adj_weightNCOM$mean)
smean_set_sp_adj_weightNCOM[which.max(smean_set_sp_adj_weightNCOM$mean),]
smean_set_sp_adj_weightNCOM[which.min(smean_set_sp_adj_weightNCOM$mean),]
#Number
mean(smean_set_sp_adj_numberNCOM$mean)
smean_set_sp_adj_numberNCOM[which.max(smean_set_sp_adj_numberNCOM$mean),]
smean_set_sp_adj_numberNCOM[which.min(smean_set_sp_adj_numberNCOM$mean),]

    
    ## Plot
    
#    output <- file.path(fp, paste0("RV survey total abundance indices v4b Non-Commercial fish ",
#                                   min(years),"-",max(years), ".png"))
    output <- file.path(fp, paste0("RV survey total abundance indices v4b Non-Commercial fish ",
                                   min(years),"-",max(years), ".eps"))
    
#    png(output, width = 3500, height = 2500, res = 300)
    postscript(output)
    
    #par(mar = c(5, 5.5, 2, 5.5))
    par(mar = c(5, 5.5, 0.5, 5.5))
    
    plot(smean_set_sp_adj_weightNCOM$year, smean_set_sp_adj_weightNCOM$mean, xaxs = "i", yaxs = "i", xaxt = 'n', yaxt = 'n', bty = 'n',
         pch = '', ylab = '', xlab = '', ylim = c(0, round(max(smean_set_sp_adj_weightNCOM$upper.cl)+4.99, -1)))
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_numberNCOM$year, smean_set_sp_adj_numberNCOM$mean, type = "l", lty = 2, xlab = "", ylab = "", col = "#1B9E77",
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_numberNCOM$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_numberNCOM[,"lower.cl"], rev(smean_set_sp_adj_numberNCOM[,"upper.cl"])),
                               border = "grey", col = "#1B9E7740"))
    
    points(smean_set_sp_adj_numberNCOM$year, smean_set_sp_adj_numberNCOM$mean, col = "#1B9E77", pch = 1, cex = 0.8)
    
    y_by <- round(max(smean_set_sp_adj_numberNCOM$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 200, 20,
                   ifelse(y_by > 200 & y_by <= 500, 50,
                          ifelse(y_by > 500 & y_by <= 1000, 100,
                                 ifelse(y_by > 1000 & y_by <= 2000, 250,
                                        ifelse(y_by > 2000 & y_by <= 5000, 500,
                                               ifelse(y_by > 5000 & y_by <= 1000, 750, 1000))))))
    
    axis(4, at = seq(0, round(max(smean_set_sp_adj_numberNCOM$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Abundance (number per tow)", side = 4, line = 3.5, cex = 1.6, font = 2, padj = 0.5)
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_weightNCOM$year, smean_set_sp_adj_weightNCOM$mean, type = "l", xlab = "", ylab = "", col = "#D95F02", #7570B3
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_weightNCOM$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_weightNCOM[,"lower.cl"], rev(smean_set_sp_adj_weightNCOM[,"upper.cl"])),
                               border = "grey", col = "#D95F0240")) #7570B340"
    
    points(smean_set_sp_adj_weightNCOM$year, smean_set_sp_adj_weightNCOM$mean, col = "#D95F02", pch = 19, cex = 0.8) #7570B3
    
    axis(1, at = seq(min(years), max(years), by = 3), cex.axis = 1.2)
    
    y_by <- round(max(smean_set_sp_adj_weightNCOM$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 20, 2,
                   ifelse(y_by > 20 & y_by <= 50, 5,
                          ifelse(y_by > 50 & y_by <= 100, 10,
                                 ifelse(y_by > 100 & y_by <= 200, 25,
                                        ifelse(y_by > 200 & y_by <= 500, 50,
                                               ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
    
    axis(2, at = seq(0, round(max(smean_set_sp_adj_weightNCOM$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
    mtext("Biomass (kg per tow)", side = 2, line = 3.5, cex = 1.6, font = 2)
    
    #title(paste0("RV survey total biomass indices - ", sp_text, " - ",  min(years), "-", max(years)))
    mtext("Non-Commercial fish", side = 3, line = -1.5, font = 2, cex = 1.5)
    
    dev.off()
    


###########Pelagic##########
    ##Herring Correction
    sp <- species_list$Cat_species_RV
    oo <- which(cat$species %in% sp)
    cat_sp <- cat[oo,]
    
    if (60 %in% sp){
      
      cat_sp_l <- cat_sp[cat_sp$species != 60,]
      cat_sp_h <- cat_sp[cat_sp$species == 60, ]
      
      cat_sp_l_adj <- adjust(cat_sp_l, set)
      
      oo <- (set$year < 1993)
      set_h_before <- set[oo,]
      oo <- (set$year > 1992)
      set_h_after <- set[oo,]
      
      cat_sp_h_adj_before <- adjust(cat_sp_h, set_h_before)
      cat_sp_h_adj_after <- adjust(cat_sp_h, set_h_after, vessel = F, day.night = F, distance = T)
      
      cat_sp_adj <- rbind(cat_sp_l_adj, cat_sp_h_adj_before, cat_sp_h_adj_after)
      
      set_sp_adjP<- merge.catch(set, cat_sp_adj, species = sp_pelagic$Cat_species_RV)
      
    } else {
      
      cat_sp_adj <- adjust(cat_sp, set)
      set_sp_adjP <- merge.catch(set, cat_sp_adj, species = sp_pelagic$Cat_species_RV)
      
    }
    
    if (length(sp) > 1){
      
      rows_weight.caughtP <- grep("weight.caught", names(set_sp_adjP))
      rows_number.caughtP <- grep("number.caught", names(set_sp_adjP))
      
      set_sp_adjP$allfish.weight.caughtP <- rowSums(set_sp_adjP[,rows_weight.caughtP])
      set_sp_adjP$allfish.number.caughtP <- rowSums(set_sp_adjP[,rows_number.caughtP])
      
      smean_set_sp_adj_weightP <- smean(set_sp_adjP, var = "allfish.weight.caughtP", by = "year")
      smean_set_sp_adj_numberP <- smean(set_sp_adjP, var = "allfish.number.caughtP", by = "year")
      
    } else {
      
      smean_set_sp_adj_weightP <- smean(set_sp_adjP, var = "weight.caught", by = "year")
      smean_set_sp_adj_numberP <- smean(set_sp_adjP, var = "number.caught", by = "year")
      
    }
    #Stratified mean for all fish
    #smean_set_sp_adj_weightCOM <- smean(mergedALLfishcatchCOM, var = "allfish.weight.caughtCOM", by = "year")
    #smean_set_sp_adj_numberCOM <- smean(mergedALLfishcatchCOM, var = "allfish.number.caughtCOM", by = "year")
    

###Mean, min and max of total biomass by weight and number over time series for all fish
#Weight
mean(smean_set_sp_adj_weightP$mean)
smean_set_sp_adj_weightP[which.max(smean_set_sp_adj_weightP$mean),]
smean_set_sp_adj_weightP[which.min(smean_set_sp_adj_weightP$mean),]
#Number
mean(smean_set_sp_adj_numberP$mean)
smean_set_sp_adj_numberP[which.max(smean_set_sp_adj_numberP$mean),]
smean_set_sp_adj_numberP[which.min(smean_set_sp_adj_numberP$mean),]
    
    ## Plot
    
#    output <- file.path(fp, paste0("RV survey total abundance indices v4b Pelagic fish ",
#                                   min(years),"-",max(years), ".png"))
    output <- file.path(fp, paste0("RV survey total abundance indices v4b Pelagic fish ",
                                   min(years),"-",max(years), ".eps"))
    
#    png(output, width = 3500, height = 2500, res = 300)
    postscript(output)
    #par(mar = c(5, 5.5, 2, 5.5))
    par(mar = c(5, 5.5, 0.5, 5.5))
    
    plot(smean_set_sp_adj_weightP$year, smean_set_sp_adj_weightP$mean, xaxs = "i", yaxs = "i", xaxt = 'n', yaxt = 'n', bty = 'n',
         pch = '', ylab = '', xlab = '', ylim = c(0, round(max(smean_set_sp_adj_weightP$upper.cl)+4.99, -1)))
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_numberP$year, smean_set_sp_adj_numberP$mean, type = "l", lty = 2, xlab = "", ylab = "", col = "#1B9E77",
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_numberP$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_numberP[,"lower.cl"], rev(smean_set_sp_adj_numberP[,"upper.cl"])),
                               border = "grey", col = "#1B9E7740"))
    
    points(smean_set_sp_adj_numberP$year, smean_set_sp_adj_numberP$mean, col = "#1B9E77", pch = 1, cex = 0.8)
    
    y_by <- round(max(smean_set_sp_adj_numberP$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 200, 20,
                   ifelse(y_by > 200 & y_by <= 500, 50,
                          ifelse(y_by > 500 & y_by <= 1000, 100,
                                 ifelse(y_by > 1000 & y_by <= 2000, 250,
                                        ifelse(y_by > 2000 & y_by <= 5000, 500,
                                               ifelse(y_by > 5000 & y_by <= 1000, 750, 1000))))))
    
    axis(4, at = seq(0, round(max(smean_set_sp_adj_numberP$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Abundance (number per tow)", side = 4, line = 3.5, cex = 1.6, font = 2, padj = 0.5)
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_weightP$year, smean_set_sp_adj_weightP$mean, type = "l", xlab = "", ylab = "", col = "#D95F02", #7570B3
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_weightP$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_weightP[,"lower.cl"], rev(smean_set_sp_adj_weightP[,"upper.cl"])),
                               border = "grey", col = "#D95F0240")) #7570B340"
    
    points(smean_set_sp_adj_weightP$year, smean_set_sp_adj_weightP$mean, col = "#D95F02", pch = 19, cex = 0.8) #7570B3
    
    axis(1, at = seq(min(years), max(years), by = 3), cex.axis = 1.2)
    
    y_by <- round(max(smean_set_sp_adj_weightP$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 20, 2,
                   ifelse(y_by > 20 & y_by <= 50, 5,
                          ifelse(y_by > 50 & y_by <= 100, 10,
                                 ifelse(y_by > 100 & y_by <= 200, 25,
                                        ifelse(y_by > 200 & y_by <= 500, 50,
                                               ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
    
    axis(2, at = seq(0, round(max(smean_set_sp_adj_weightP$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
    mtext("Biomass (kg per tow)", side = 2, line = 3.5, cex = 1.6, font = 2)
    
    #title(paste0("RV survey total biomass indices - ", sp_text, " - ",  min(years), "-", max(years)))
    mtext("Pelagic fish", side = 3, line = -1.5, font = 2, cex = 1.5)
    
    dev.off()
    
    
#################Demersal############    
    
#Merge all fish catch cards
mergedALLfishcatchD<-merge.catch(set, catch.card=adjustedcatchcards, species = sp_demersal$Cat_species_RV)
    
#Specify rows with weight caught and number caught
rows_weight.caughtD <- grep("weight.caught", names(mergedALLfishcatchD))
rows_number.caughtD <- grep("number.caught", names(mergedALLfishcatchD))
    
#Sum weight caught and number caught for all fish
mergedALLfishcatchD$allfish.weight.caughtD <- rowSums(mergedALLfishcatchD[,rows_weight.caughtD])
mergedALLfishcatchD$allfish.number.caughtD <- rowSums(mergedALLfishcatchD[,rows_number.caughtD])
    
#Stratified mean for all fish
smean_set_sp_adj_weightD <- smean(mergedALLfishcatchD, var = "allfish.weight.caughtD", by = "year")
smean_set_sp_adj_numberD <- smean(mergedALLfishcatchD, var = "allfish.number.caughtD", by = "year")
   
###Mean, min and max of total biomass by weight and number over time series for all fish
#Weight
mean(smean_set_sp_adj_weightD$mean)
smean_set_sp_adj_weightD[which.max(smean_set_sp_adj_weightD$mean),]
smean_set_sp_adj_weightD[which.min(smean_set_sp_adj_weightD$mean),]
#Number
mean(smean_set_sp_adj_numberD$mean)
smean_set_sp_adj_numberD[which.max(smean_set_sp_adj_numberD$mean),]
smean_set_sp_adj_numberD[which.min(smean_set_sp_adj_numberD$mean),]
    
    
    ## Plot
    
#    output <- file.path(fp, paste0("RV survey total abundance indices v4b Demersal fish ",
#                                   min(years),"-",max(years), ".png"))
    output <- file.path(fp, paste0("RV survey total abundance indices v4b Demersal fish ",
                                   min(years),"-",max(years), ".eps"))
    #png(output, width = 3500, height = 2500, res = 300)
    postscript(output)
    
    #par(mar = c(5, 5.5, 2, 5.5))
    par(mar = c(5, 5.5, 0.5, 5.5))
    
    plot(smean_set_sp_adj_weightD$year, smean_set_sp_adj_weightD$mean, xaxs = "i", yaxs = "i", xaxt = 'n', yaxt = 'n', bty = 'n',
         pch = '', ylab = '', xlab = '', ylim = c(0, round(max(smean_set_sp_adj_weightD$upper.cl)+4.99, -1)))
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_numberD$year, smean_set_sp_adj_numberD$mean, type = "l", lty = 2, xlab = "", ylab = "", col = "#1B9E77",
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_numberD$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_numberD[,"lower.cl"], rev(smean_set_sp_adj_numberD[,"upper.cl"])),
                               border = "grey", col = "#1B9E7740"))
    
    points(smean_set_sp_adj_numberD$year, smean_set_sp_adj_numberD$mean, col = "#1B9E77", pch = 1, cex = 0.8)
    
    y_by <- round(max(smean_set_sp_adj_numberD$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 200, 20,
                   ifelse(y_by > 200 & y_by <= 500, 50,
                          ifelse(y_by > 500 & y_by <= 1000, 100,
                                 ifelse(y_by > 1000 & y_by <= 2000, 250,
                                        ifelse(y_by > 2000 & y_by <= 5000, 500,
                                               ifelse(y_by > 5000 & y_by <= 1000, 750, 1000))))))
    
    axis(4, at = seq(0, round(max(smean_set_sp_adj_numberD$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Abundance (number per tow)", side = 4, line = 3.5, cex = 1.6, font = 2, padj = 0.5)
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_weightD$year, smean_set_sp_adj_weightD$mean, type = "l", xlab = "", ylab = "", col = "#D95F02", #7570B3
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_weightD$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_weightD[,"lower.cl"], rev(smean_set_sp_adj_weightD[,"upper.cl"])),
                               border = "grey", col = "#D95F0240")) #7570B340"
    
    points(smean_set_sp_adj_weightD$year, smean_set_sp_adj_weightD$mean, col = "#D95F02", pch = 19, cex = 0.8) #7570B3
    
    axis(1, at = seq(min(years), max(years), by = 3), cex.axis = 1.2)
    
    y_by <- round(max(smean_set_sp_adj_weightD$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 20, 2,
                   ifelse(y_by > 20 & y_by <= 50, 5,
                          ifelse(y_by > 50 & y_by <= 100, 10,
                                 ifelse(y_by > 100 & y_by <= 200, 25,
                                        ifelse(y_by > 200 & y_by <= 500, 50,
                                               ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
    
    axis(2, at = seq(0, round(max(smean_set_sp_adj_weightD$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
    mtext("Biomass (kg per tow)", side = 2, line = 3.5, cex = 1.6, font = 2)
    mtext("Demersal fish", side = 3, line = -1.5, font = 2, cex = 1.5)
    
    dev.off()
  
    
#################Lobster############    
    
#Merge  catch cards
adjustedcatchcardsL<-subset(adjustedcatchcards, year != "1977")
mergedALLfishcatchL<-merge.catch(set, catch.card=adjustedcatchcardsL, species= "2550")
    
#Stratified mean for lobster
smean_set_sp_adj_weightL <- smean(mergedALLfishcatchL, var = "weight.caught", by = "year")
smean_set_sp_adj_numberL <- smean(mergedALLfishcatchL, var = "number.caught", by = "year")

###Mean, min and max of total biomass by weight and number over time series for all fish
#Weight
mean(smean_set_sp_adj_weightL$mean)
smean_set_sp_adj_weightL[which.max(smean_set_sp_adj_weightL$mean),]
smean_set_sp_adj_weightL[which.min(smean_set_sp_adj_weightL$mean),]
#Number
mean(smean_set_sp_adj_numberL$mean)
smean_set_sp_adj_numberL[which.max(smean_set_sp_adj_numberL$mean),]
smean_set_sp_adj_numberL[which.min(smean_set_sp_adj_numberL$mean),]
    
    
    ## Plot
    
#    output <- file.path(fp, paste0("RV survey total abundance indices v4b Lobster",
#                                   min(years),"-",max(years), ".png"))
    output <- file.path(fp, paste0("RV survey total abundance indices v4b Lobster",
                                   min(years),"-",max(years), ".eps"))
    
#    png(output, width = 3500, height = 2500, res = 300)
    postscript(output)
    
    #par(mar = c(5, 5.5, 2, 5.5))
    par(mar = c(5, 5.5, 0.5, 5.5))
    
    plot(smean_set_sp_adj_weightL$year, smean_set_sp_adj_weightL$mean, xaxs = "i", yaxs = "i", xaxt = 'n', yaxt = 'n', bty = 'n',
         pch = '', ylab = '', xlab = '', ylim = c(0, round(max(smean_set_sp_adj_weightL$upper.cl)+4.99, -1)))
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_numberL$year, smean_set_sp_adj_numberL$mean, type = "l", lty = 2, xlab = "", ylab = "", col = "#1B9E77",
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_numberL$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_numberL[,"lower.cl"], rev(smean_set_sp_adj_numberL[,"upper.cl"])),
                               border = "grey", col = "#1B9E7740"))
    
    points(smean_set_sp_adj_numberL$year, smean_set_sp_adj_numberL$mean, col = "#1B9E77", pch = 1, cex = 0.8)
    
    y_by <- round(max(smean_set_sp_adj_numberL$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 200, 20,
                   ifelse(y_by > 200 & y_by <= 500, 50,
                          ifelse(y_by > 500 & y_by <= 1000, 100,
                                 ifelse(y_by > 1000 & y_by <= 2000, 250,
                                        ifelse(y_by > 2000 & y_by <= 5000, 500,
                                               ifelse(y_by > 5000 & y_by <= 1000, 750, 1000))))))
    
    axis(4, at = seq(0, round(max(smean_set_sp_adj_numberL$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Abundance (number per tow)", side = 4, line = 3.5, cex = 1.6, font = 2, padj = 0.5)
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_weightL$year, smean_set_sp_adj_weightL$mean, type = "l", xlab = "", ylab = "", col = "#D95F02", #7570B3
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_weightL$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_weightL[,"lower.cl"], rev(smean_set_sp_adj_weightL[,"upper.cl"])),
                               border = "grey", col = "#D95F0240")) #7570B340"
    
    points(smean_set_sp_adj_weightL$year, smean_set_sp_adj_weightL$mean, col = "#D95F02", pch = 19, cex = 0.8) #7570B3
    
    axis(1, at = seq(min(years), max(years), by = 3), cex.axis = 1.2)
    
    y_by <- round(max(smean_set_sp_adj_weightL$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 20, 2,
                   ifelse(y_by > 20 & y_by <= 50, 5,
                          ifelse(y_by > 50 & y_by <= 100, 10,
                                 ifelse(y_by > 100 & y_by <= 200, 25,
                                        ifelse(y_by > 200 & y_by <= 500, 50,
                                               ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
    
    axis(2, at = seq(0, round(max(smean_set_sp_adj_weightL$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
    mtext("Biomass (kg per tow)", side = 2, line = 3.5, cex = 1.6, font = 2)
    
    #title(paste0("RV survey total biomass indices - ", sp_text, " - ",  min(years), "-", max(years)))
    mtext("Lobster", side = 3, line = -1.5, font = 2, cex = 1.5)
    
    dev.off()
    
    ##########Crabs################
    #Merge all fish catch cards
    mergedALLfishcatchC<-merge.catch(set, catch.card=adjustedcatchcards, species = sp_crabs)
    
    #Specify rows with weight caught and number caught
    rows_weight.caughtC <- grep("weight.caught", names(mergedALLfishcatchC))
    rows_number.caughtC <- grep("number.caught", names(mergedALLfishcatchC))
    
    #Sum weight caught and number caught for all fish
    mergedALLfishcatchC$allfish.weight.caughtC <- rowSums(mergedALLfishcatchC[,rows_weight.caughtC])
    mergedALLfishcatchC$allfish.number.caughtC <- rowSums(mergedALLfishcatchC[,rows_number.caughtC])
    
    #Stratified mean for all fish
    smean_set_sp_adj_weightC <- smean(mergedALLfishcatchC, var = "allfish.weight.caughtC", by = "year")
    smean_set_sp_adj_numberC <- smean(mergedALLfishcatchC, var = "allfish.number.caughtC", by = "year")
  
      ## Plot
    
#    output <- file.path(fp, paste0("RV survey total abundance indices v4b Crabs",
#                                   min(years),"-",max(years), ".png"))
    output <- file.path(fp, paste0("RV survey total abundance indices v4b Crabs",
                                   min(years),"-",max(years), ".eps"))
    
#    png(output, width = 3500, height = 2500, res = 300)
    postscript(output)
    #par(mar = c(5, 5.5, 2, 5.5))
    par(mar = c(5, 5.5, 0.5, 5.5))
    
    plot(smean_set_sp_adj_weightC$year, smean_set_sp_adj_weightC$mean, xaxs = "i", yaxs = "i", xaxt = 'n', yaxt = 'n', bty = 'n',
         pch = '', ylab = '', xlab = '', ylim = c(0, round(max(smean_set_sp_adj_weightC$upper.cl)+4.99, -1)))
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_numberC$year, smean_set_sp_adj_numberC$mean, type = "l", lty = 2, xlab = "", ylab = "", col = "#1B9E77",
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_numberC$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_numberC[,"lower.cl"], rev(smean_set_sp_adj_numberC[,"upper.cl"])),
                               border = "grey", col = "#1B9E7740"))
    
    points(smean_set_sp_adj_numberC$year, smean_set_sp_adj_numberC$mean, col = "#1B9E77", pch = 1, cex = 0.8)
    
    y_by <- round(max(smean_set_sp_adj_numberC$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 200, 20,
                   ifelse(y_by > 200 & y_by <= 500, 50,
                          ifelse(y_by > 500 & y_by <= 1000, 100,
                                 ifelse(y_by > 1000 & y_by <= 2000, 250,
                                        ifelse(y_by > 2000 & y_by <= 5000, 500,
                                               ifelse(y_by > 5000 & y_by <= 1000, 750, 1000))))))
    
    axis(4, at = seq(0, round(max(smean_set_sp_adj_numberL$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Abundance (number per tow)", side = 4, line = 3.5, cex = 1.6, font = 2, padj = 0.5)
    
    par(new = TRUE)
    
    plot(smean_set_sp_adj_weightC$year, smean_set_sp_adj_weightC$mean, type = "l", xlab = "", ylab = "", col = "#D95F02", #7570B3
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_weightC$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_weightC[,"lower.cl"], rev(smean_set_sp_adj_weightC[,"upper.cl"])),
                               border = "grey", col = "#D95F0240")) #7570B340"
    
    points(smean_set_sp_adj_weightC$year, smean_set_sp_adj_weightC$mean, col = "#D95F02", pch = 19, cex = 0.8) #7570B3
    
    axis(1, at = seq(min(years), max(years), by = 3), cex.axis = 1.2)
    
    y_by <- round(max(smean_set_sp_adj_weightC$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 20, 2,
                   ifelse(y_by > 20 & y_by <= 50, 5,
                          ifelse(y_by > 50 & y_by <= 100, 10,
                                 ifelse(y_by > 100 & y_by <= 200, 25,
                                        ifelse(y_by > 200 & y_by <= 500, 50,
                                               ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
    
    axis(2, at = seq(0, round(max(smean_set_sp_adj_weightC$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
    mtext("Biomass (kg per tow)", side = 2, line = 3.5, cex = 1.6, font = 2)
    mtext("Crabs", side = 3, line = -1.5, font = 2, cex = 1.5)
    
    dev.off()
    
    
    
##########Shrimps################
    #Merge all fish catch cards
    mergedALLfishcatchS<-merge.catch(set, catch.card=adjustedcatchcards, species = sp_shrimps)
    
    #Specify rows with weight caught and number caught
    rows_weight.caughtS <- grep("weight.caught", names(mergedALLfishcatchS))
    rows_number.caughtS <- grep("number.caught", names(mergedALLfishcatchS))
    
    #Sum weight caught and number caught for all fish
    mergedALLfishcatchS$allfish.weight.caughtS <- rowSums(mergedALLfishcatchS[,rows_weight.caughtS])
    mergedALLfishcatchS$allfish.number.caughtS <- rowSums(mergedALLfishcatchS[,rows_number.caughtS])
    
    mergedALLfishcatchS$allfish.weight.caughtS[mergedALLfishcatchS$year %in% c(1971:1979)] <- 0
    mergedALLfishcatchS$allfish.number.caughtS[mergedALLfishcatchS$year %in% c(1971:1979)] <- 0
    
    #Stratified mean for all fish
    smean_set_sp_adj_weightS <- smean(mergedALLfishcatchS, var = "allfish.weight.caughtS", by = "year")
    smean_set_sp_adj_numberS <- smean(mergedALLfishcatchS, var = "allfish.number.caughtS", by = "year")
    
    ###Mean, min and max of total biomass by weight and number over time series for all fish
    #Weight
    mean(smean_set_sp_adj_weightS$mean)
    smean_set_sp_adj_weightS[which.max(smean_set_sp_adj_weightS$mean),]
    smean_set_sp_adj_weightS[which.min(smean_set_sp_adj_weightS$mean),]
    #Number
    mean(smean_set_sp_adj_numberS$mean)
    smean_set_sp_adj_numberS[which.max(smean_set_sp_adj_numberS$mean),]
    smean_set_sp_adj_numberS[which.min(smean_set_sp_adj_numberS$mean),]
    
    ## Plot
    
#    output <- file.path(fp, paste0("RV survey total abundance indices v4b Shrimps ",
#                                   min(years),"-",max(years), ".png"))
    output <- file.path(fp, paste0("RV survey total abundance indices v4b Shrimps ",
                                   min(years),"-",max(years), ".eps"))
    #png(output, width = 3500, height = 2500, res = 300)
    postscript(output)
    #par(mar = c(5, 5.5, 2, 5.5))
    par(mar = c(5, 5.5, 0.5, 5.5))
    
    plot(smean_set_sp_adj_weightS$year, smean_set_sp_adj_weightS$mean, xaxs = "i", yaxs = "i", xaxt = 'n', yaxt = 'n', bty = 'n',
         pch = '', ylab = '', xlab = '', ylim = c(0, round(max(smean_set_sp_adj_weightS$upper.cl)+4.99, -1)))
    
    par(new = TRUE)
    
    #plot(smean_set_sp_adj_numberS$year, smean_set_sp_adj_numberS$mean, type = "l", lty = 2, xlab = "", ylab = "", col = "#1B9E77",
    #     xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_numberS$upper.cl)+4.99, -1)),
    #     panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_numberS[,"lower.cl"], rev(smean_set_sp_adj_numberS[,"upper.cl"])),
    #                           border = "grey", col = "#1B9E7740"))
    
    #points(smean_set_sp_adj_numberS$year, smean_set_sp_adj_numberS$mean, col = "#1B9E77", pch = 1, cex = 0.8)
    
    #y_by <- round(max(smean_set_sp_adj_numberS$upper.cl)+4.99, -1)
    #y_by <- ifelse(y_by <= 200, 20,
    #               ifelse(y_by > 200 & y_by <= 500, 50,
    #                      ifelse(y_by > 500 & y_by <= 1000, 100,
    #                             ifelse(y_by > 1000 & y_by <= 2000, 250,
    #                                    ifelse(y_by > 2000 & y_by <= 5000, 500,
    #                                           ifelse(y_by > 5000 & y_by <= 1000, 750, 1000))))))
    
    #axis(4, at = seq(0, round(max(smean_set_sp_adj_numberS$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    #mtext("Biomass (number per tow)", side = 4, line = 3.5, cex = 1.6, font = 2, padj = 0.5)
    
    #par(new = TRUE)
    
    plot(smean_set_sp_adj_weightS$year, smean_set_sp_adj_weightS$mean, type = "l", xlab = "", ylab = "", col = "#D95F02", #7570B3
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, round(max(smean_set_sp_adj_weightS$upper.cl)+4.99, -1)),
         panel.first = polygon(c(years, rev(years)), c(smean_set_sp_adj_weightS[,"lower.cl"], rev(smean_set_sp_adj_weightS[,"upper.cl"])),
                               border = "grey", col = "#D95F0240")) #7570B340"
    
    points(smean_set_sp_adj_weightS$year, smean_set_sp_adj_weightS$mean, col = "#D95F02", pch = 19, cex = 0.8) #7570B3
    
    axis(1, at = seq(min(years), max(years), by = 3), cex.axis = 1.2)
    
    y_by <- round(max(smean_set_sp_adj_weightS$upper.cl)+4.99, -1)
    y_by <- ifelse(y_by <= 20, 2,
                   ifelse(y_by > 20 & y_by <= 50, 5,
                          ifelse(y_by > 50 & y_by <= 100, 10,
                                 ifelse(y_by > 100 & y_by <= 200, 25,
                                        ifelse(y_by > 200 & y_by <= 500, 50,
                                               ifelse(y_by > 500 & y_by <= 1000, 100, 200))))))
    
    axis(2, at = seq(0, round(max(smean_set_sp_adj_weightS$upper.cl)+4.99, -1), by = y_by), cex.axis = 1.2, las = 1)
    
    mtext("Year", side = 1, line = 3.5, cex = 1.6, font = 2)
    mtext("Biomass (kg per tow)", side = 2, line = 3.5, cex = 1.6, font = 2)
    mtext("Shrimps", side = 3, line = -1.5, font = 2, cex = 1.5)
    
    dev.off()
    