#library(lubridate)
#t <- read.table("/media/baikal/Asiakirjat/data/GDELT/GDELT.1979-2012.reduced/RUS.ALL.select.outfile.txt", sep = "\t")
#colnames(t) <- c("Day", "Actor1Code", "Actor2Code", "EventCode", "QuadCategory", 
#                 "GoldsteinScale", "Actor1Geo_Lat", "Actor1Geo_Long", "Actor2Geo_Lat", "Actor2Geo_Long", 
#                 "ActionGeo_Lat", "ActionGeo_Long")
#t$Day <- ymd(t$Day)
#save(t, file = "gdeltRus.Rdata")

load("gdeltRus.Rdata")

tail(sort(table(t$Actor1Code)), 20)

str(t)


library(lubridate)
library(ggplot2)
x <- data.frame(table(year(t$Day)))
ggplot(x, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


library(RgoogleMaps)
library(ggmap)
library(mapproj)
library(plyr)

# Reshape the data so we get the counts of events for each location
t$count <- 1
df <- ddply(t, .(ActionGeo_Lat, ActionGeo_Long), summarize, count = sum(count))

# lets define the scope of our map
lat <- c(40, 70)
lon <- c(20, 100)
center = c(lat = mean(lat), lon = mean(lon))
zoom <- min(MaxZoom(range(lat), range(lon)))

# And download a map file!
map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 3, maptype = "terrain", 
               source = "google")
ggmap(map) + geom_point(data = df, aes(x = ActionGeo_Long, y = ActionGeo_Lat, 
                                             size = count), alpha = 0.3)



t$count <- 1
# selecting the events we are interested in
t2 <- t[t$EventCode > 139 & t$EventCode < 146, ]

# Keep only rows with agents that interest us. Combined these
t3a <- t2[grep("OPP|EDU|REL|CVL|ELI|LAB|IGO|NGO|NGM", t2$Actor1Code), ]
t3b <- t2[grep("OPP|EDU|REL|CVL|ELI|LAB|IGO|NGO|NGM", t2$Actor2Code), ]
t3 <- rbind(t3a, t3b)
# agents: OPP, EDU, REL, CVL, ELI, LAB, IGO, NGO, NGM

df2 <- ddply(t3, .(ActionGeo_Lat, ActionGeo_Long), summarize, count = sum(count))
map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 3, maptype = "terrain", 
               source = "google", color = "bw")
print(ggmap(map) + geom_point(data = df2, aes(x = ActionGeo_Long, y = ActionGeo_Lat, 
                                              size = count), alpha = 0.8, pch = 21, colour = "black", fill = "red2") + 
        scale_size(range = c(2, 7)))




t3$recent <- 0
t3$Day <- as.Date(t3$Day)
t3$recent[t3$Day > ("2011-07-01")] <- 1
tryCatch(detach("package:Hmisc"), error = function(e) NULL)

df2 <- ddply(t3, .(ActionGeo_Lat, ActionGeo_Long, recent), summarize, count = sum(count))
mos <- get_map(location = "moscow", zoom = 10, maptype = "terrain", source = "google")
print(ggmap(mos) + geom_point(data = df2, aes(x = ActionGeo_Long, y = ActionGeo_Lat, 
                                              size = count, fill = recent), alpha = 0.8, pch = 21, colour = "black") + 
        scale_size(range = c(4, 20)) + facet_wrap(~recent))


####################################3
#######################################

#Limit the data to the last year recorded
t3$recent <- 0
t3$Day <- as.Date(t3$Day)
t3$recent[t3$Day>("2011-07-01")] <- 1
t3 <- t3[t3$recent==1,]

#to avoid conflicts between plyr and Hmisc. If anyone knows a better way of doing this, please let me know!
tryCatch(detach("package:Hmisc"),error=function(e) NULL)


df2 <- ddply(t3,.(Actor1Geo_Lat,Actor1Geo_Long,Actor2Geo_Lat,Actor2Geo_Long,ActionGeo_Lat,ActionGeo_Long),summarize, count=sum(count))
df2 <- df2[complete.cases(df2),]


#remove links with America and southern hemisphere
df2 <- df2[df2$Actor1Geo_Lat>0&df2$Actor2Geo_Lat>0&df2$Actor1Geo_Long>0&df2$Actor2Geo_Long>0,]
#remove Generic Russia
df2 <- df2[df2$Actor2Geo_Lat!=60&df2$Actor2Geo_Long!=100,]
df2 <- df2[df2$Actor1Geo_Lat!=60&df2$Actor1Geo_Long!=100,]


#place points and edges in separate data frames
pointsDf <- df2[,5:7]
colnames(pointsDf)[3] <- "count2"
edgesDf <- df2[,c(1:4,7)]

a <- paste0(edgesDf[,1],edgesDf[,2])#remove points were start and end are the same
b <- paste0(edgesDf[,3],edgesDf[,4])
edgesDf <- edgesDf[!a==b,]


library(sna)
library(Hmisc)

edgeMaker <- function(whichRow, len = 1, curved = TRUE){
  fromC <- c(edgesDf[whichRow,2],edgesDf[whichRow,1])  # Origin
  toC <- c(edgesDf[whichRow,4],edgesDf[whichRow,3]) # Terminus
  weight <- edgesDf[whichRow, 5]  # Terminus
  
  # Add curve:
  graphCenter <- c(50,50)#colMeans(edgesDf[,1:2])  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
  }  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
  
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$weight <- weight
  edge$Group <- whichRow
  return(edge)
}
allEdges <- lapply(1:nrow(edgesDf), edgeMaker, len = 100, curved = TRUE)
allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^

#     col <- rep(gray(8/10),length(labels))
#     col2 <- rep(gray(1/10),length(labels))
#     col[labels==term] <- "red3"
#     col2[labels==term] <- "red4"

new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$legend <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, 0, -1), unit = "lines",
                                         valid.unit = 3L, class = "unit")

map <-get_map(location=c(lon=mean(lon), lat=mean(lat)),zoom=3,maptype="terrain",color="bw")

p <- ggmap(map)
#p <- ggplot(allEdges,environment=environment())  
p <- p + geom_path(data=allEdges, aes(x = x, y = y,group = Group,  # Edges with gradient
                                      size=(weight-1),colour=Sequence),alpha=.6)  # and taper
p <- p + geom_point(data = data.frame(pointsDf),  # Add points
                    aes(x = ActionGeo_Long, y = ActionGeo_Lat,size=log(count2)), pch = 21,
                    colour = "black", fill = "red2")  # Customize gradient 
p <- p + scale_colour_gradient(low = "red3", high = "white", guide = "none")
p <- p + scale_size(range = c(.6, 5), guide = "none")  # Customize taper
p <- p + new_theme_empty  # Clean up plot
p <- p + xlim(min(allEdges$x-1),max(allEdges$x+1))
p <- p + ylim(20,65)
print(p)
ggsave("latestImage.png", p, h = 4, w = 8, type = "cairo-png")
