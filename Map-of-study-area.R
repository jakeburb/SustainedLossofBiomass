###Please Contact Corresponding Author Jacob Burbank for help with code####
###########Jacob.Burbank@dfo-mpo.gc.ca##########
##  make a strata map swith depth values
before <- ls()

library(gulf)

## map of the Gulf RV strata
f2.n <- "sGSL-RV-4T-strata-map.pdf"

library(sf)
library(ggplot2)
library(tidyverse)

mar.atlas.path <- "../../../GitHub/Maritimes-SUMMER-Atlas"

boundaries <- read_sf(file.path(mar.atlas.path, "AC/AC_1M_BoundaryPolygons_shp/AC_1M_BoundaryPolygons.shp"))

boundaries_simple <- boundaries %>%
  filter(
    POL_DIV %in% c(
      "Quebec", "Newfoundland and Labrador" ,
      "New Brunswick", "Nova Scotia",
      "Prince Edward Island"
    ),
    SELECTION %in% c("sparse","dense") #
  ) %>%
  st_transform(4326)

## strata statistics
st <- stratum.info()
idx <- which(st$stratum %in% c(401:403,415:429,431:439))
st <- st[idx,]
st$depth.range.fathom <- c("<50","<50","<50",">100","51-100","<50","<50","<50","<50","<50","<50","<50","<50",">100","51-100","<50","<50","<50","<50","<50","<50","<50","<50","<50","51-100","51-100",">100")
st$depth.range.meter <- c("<92","<92","<92",">183","93-183","<92","<92","<92","<92","<92","<92","<92","<92",">183","93-183","<92","<92","<92","<92","<92","<92","<92","<92","<92","93-183","93-183",">183")
st$depth.min.fathom <- c(11,11,11,101,51,11,11,11,11,11,11,11,11,101,51,11,11,11,11,11,11,11,11,11,51,51,101)


vars <- c("stratum","depth.range.fathom","depth.range.meter","area","trawlable.units")

strata.cols <- data.frame(min.depth=c(11,51,101,151), max.depth=c(50,100,150,200), col=c("lightblue","blue","darkblue","snow1"))

st$depth.color <- strata.cols[match(st$depth.min.fathom, strata.cols$min.depth),"col"]

atlas.path <- "../../GulfAtlas/Mapping/shapefiles/"
st.p <- read_sf(file.path(atlas.path, "Gulf-Sept-RV-strata.shp"))


g <- ggplot(data = boundaries_simple) +
  geom_sf(fill="cornsilk", color=grey(0.8)) +
  geom_sf(data=st.p) +
  xlim(-66.3,-60) + ylim(45.7,49.2) +
  xlab("Longitude (\u{B0}W)") + ylab("Latitude (\u{B0}N)") +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "black", fill=NA, size=1))

rm(list= ls()[!(ls() %in% before)]) ## clean up after ourselves
