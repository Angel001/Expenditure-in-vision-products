library(ggplot2)
library(SmarterPoland)
library(dplyr)
library(RColorBrewer)
library(rgdal)

data <- getEurostatRCV(kod = "hlth_sha_hp")
vp <- filter(data, icha_hp == "HP42", unit == "EUR_HAB") ## Retail sale and other suppliers of optical glasses and other vision products
vp <- filter(vp, !(geo %in% c("JP", "US", "AU", "KR", "CA", "NZ")))
vp <- droplevels(vp)
vp$eurCat <- cut(vp$value, breaks = seq(0, 110, 10))
vp <- filter(vp, !is.na(value))

vp1 <- vp %>% group_by(geo) %>% summarise(time = max(as.numeric(as.character(time)))) ## Getting last year with data available, this is the table
vp1$time <- as.factor(vp1$time)
vp2 <- left_join(vp1, vp) ## This is the data frame with values per country with the last year with available data 

## Get shapefiles from eurostat
download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2010_03M_SH.zip", "NUTS2010.zip")
unzip("NUTS2010.zip")
EU_NUTS <- readOGR(dsn = "./NUTS_2010_03M_SH/NUTS_2010_03M_SH/data", layer = "NUTS_RG_03M_2010")
## proj4string(EU_NUTS) #projection info, change to mercator
EU_NUTS <- spTransform(EU_NUTS, CRS("+proj=merc +a=6378137 +b=6378137
                                    +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0
                                    +k=1.0 +units=m +nadgrids=@null +no_defs"))
EU_NUTS@data$id <- row.names(EU_NUTS@data)
eu.points <- fortify(EU_NUTS, region="id")
eudf <- left_join(eu.points, EU_NUTS@data, by="id")

df <- left_join(eudf, vp2, by = c("NUTS_ID" = "geo"))
df$NUTS_ID <- as.factor(df$NUTS_ID)
df <- filter(df, STAT_LEVL_ == 0)
df <- filter(df, long > -1.5e6, long < 5e6, lat > 3.5e6, lat < 1.15e7) ## Limits for continental Europe

df <- droplevels(df)
lbl <- levels(df$eurCat)
lbl <- sapply(lbl, function(x) substr(x, 2, nchar(x) - 1), USE.NAMES = F)
lbl <- sapply(lbl, function(x) gsub(",", " - ", x), USE.NAMES = F)

ggplot(df, aes(x = long, y = lat, group = group, label = time)) + 
        geom_polygon(aes(x=long, y=lat, group=group, fill = eurCat)) +
        geom_path(colour = "gray25") +
        ggtitle("Retail sale and other suppliers of\noptical glasses and other vision products") +
        scale_fill_brewer(name = "Euros per inhabitant", palette = "Reds", labels = lbl) +
        coord_fixed() +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_rect(fill = "gray85")
        )

