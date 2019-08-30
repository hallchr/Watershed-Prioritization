#To open excel in R
library(openxlsx)
# To plot using ggplot2
library(ggplot2)
#To plot side by side or on top of each other
library(gridExtra)
#To use date_break functinoallity
library(scales)
library(lubridate)
library(rkt)
library(EnvStats)
library(zoo)
library(dplyr)
library(Kendall)
library(boot)
library(magrittr)
library(ggpubr)
library(ggthemes)
library(plotly)
library(psych)
library(shiny)
library(sp)
library(rgdal)
library(raster)
library(ggspatial)



#remove.packages("ggplot2")


#install.packages(c("sp", "rgdal", "raster"))
#install.packages("ggspatial")

#path of spreadsheet
dbPath1 <- "TMDL_Stream_2010-2019_V3.xlsx"

# load the workbook

wb <- loadWorkbook(dbPath1)

#load the worksheets
SARU_Excel <-  read.xlsx(dbPath1, "SARU", detectDates = T)
LS_1_Excel <-  read.xlsx(dbPath1, "LS_1", detectDates = T)
JO_1_Excel <-  read.xlsx(dbPath1, "JO_1", detectDates = T)
PM_1_Excel <-  read.xlsx(dbPath1, "PM_1", detectDates = T)
NCLD_Excel <-  read.xlsx(dbPath1, "NCLD", detectDates = T)
HC_1_Excel <-  read.xlsx(dbPath1, "HC_1", detectDates = T)
HC_2_Excel <-  read.xlsx(dbPath1, "HC_2", detectDates = T)
NC_1_Excel <-  read.xlsx(dbPath1, "NC_1", detectDates = T)
WC_1_Excel <-  read.xlsx(dbPath1, "WC_1", detectDates = T)
QC_1_Excel <- read.xlsx(dbPath1, "QC_1", detectDates = T)
NC_2_Excel <- read.xlsx(dbPath1, "NC_2", detectDates = T)
PR_2_Excel <- read.xlsx(dbPath1, "PR_2", detectDates = T)
MD_1_Excel <- read.xlsx(dbPath1, "MD_1", detectDates = T)
WD_1_Excel <- read.xlsx(dbPath1, "WD_1", detectDates = T)
PA_1_Excel <- read.xlsx(dbPath1, "PA_1", detectDates = T)
PA_2_Excel <- read.xlsx(dbPath1, "PA_2", detectDates = T)
BY_1_Excel <- read.xlsx(dbPath1, "BY_1", detectDates = T)
MH_1_Excel <- read.xlsx(dbPath1, "MH_1", detectDates = T)
# Combine Worksheets
AllCountsAmbient <- rbind(SARU_Excel, LS_1_Excel, JO_1_Excel, PM_1_Excel,
                          NCLD_Excel, HC_1_Excel,HC_2_Excel, NC_1_Excel, WC_1_Excel, QC_1_Excel,
                          NC_2_Excel, PR_2_Excel, MD_1_Excel, WD_1_Excel, PA_1_Excel, PA_2_Excel, BY_1_Excel, MH_1_Excel)
#Creat Year and Month Vectors

AllCountsAmbient$Month <- month(AllCountsAmbient$Date, label = TRUE, abbr = TRUE)

#get objects in correct form
AllCountsAmbient$pH <- as.numeric(AllCountsAmbient$pH)
AllCountsAmbient$Turbidity <- as.numeric(AllCountsAmbient$Turbidity)

AllCountsAmbient$Temp.Type <- ifelse(AllCountsAmbient$TEMP < 16, "below", "above")
AllCountsAmbient$DO.Type <- ifelse(AllCountsAmbient$DO > 9.5, "below", "above")
AllCountsAmbient$Turbidity.Type <- ifelse(AllCountsAmbient$Turbidity < 4, "below", "above")

AllCountsAmbient2019 <- subset(AllCountsAmbient, Year %in% "2019")
AllCountsAmbient2018 <- subset(AllCountsAmbient, Year %in% "2018")
AllCountsAmbient2018_2019 <- subset(AllCountsAmbient, Year %in% c("2018", "2019"))


AllCountsAmbient$CurrentYear <- '2010-2018'
AllCountsAmbient$CurrentYear[AllCountsAmbient$Year == '2019'] <- '2019'

Basins <- rgdal::readOGR(dsn = "inputs", layer = "Monitoring_Basins_Project")
Streams <- raster::shapefile("inputs/Streams_BUGA_WGS.shp")
Sample_Points <- rgdal::readOGR(dsn = "inputs", layer = "2018_Ambient_Sample_Locations")

Basinsdf <- fortify(Basins)
Streamsdf <- fortify(Streams)
Sample_Pointsdf <- as.data.frame(Sample_Points)

Sample_Pointsdf$watershed <- c("Perry Creek", "Lower North Creek", "Lower North Creek", "Lower North Creek",
                               "Little Swamp Creek", "Horse Creek", "Horse Creek", "Lower North Creek", 
                               "Lower Sammamish River", "Lower North Creek", "Perry Creek",
                               "Upper North Creek","Parr Creek", "Upper North Creek")

Sample_Map <- ggplot() +
  geom_path(data = Basinsdf, aes(x = long, y = lat, group = group, color = factor(group)), fill = id, colour = "black") +
  geom_line(data = Streamsdf, aes(x = long, y = lat, group = group), fill = id, colour = "blue") +
  geom_point(data = Sample_Pointsdf, aes(x = Longitude, y = Latitude, group = watershed, colour = factor(watershed), size = 2)) +
  theme_void() +
  coord_map()
#______________________________________________FINAL GRAPHS_____________________________________________________________________________________________________________________



# 2019

ggplot(subset(AllCountsAmbient, Year == "2019"), aes(x=Date, y=TEMP, group = Site, colour = Temp.Type)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red",) + 
  geom_line(size=1) + facet_wrap(~Site, ncol = 2) + labs(title=NULL, subtitle=NULL, y="Temperature (Degrees Celcius)", x= "Date", caption = "Ambient Locations, 2010-2018") +
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%y")) +  theme_bw() + theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                                                                                                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=16), axis.title.x = element_text(size = 18),
                                                                                                     legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))

cols <- c(below = "red", above = "blue")
DO_2019 <- ggplot(subset(AllCountsAmbient, Year == "2019"), aes(x=Date, y=DO, group = Site, colour = DO.Type)) + geom_point(size=3, shape=21, fill = factor(AllCountsAmbient$DO.Type)) + geom_hline(yintercept = 9.5, color = "red",) + 
  geom_line(size=1) + facet_wrap(~Site, ncol = 2) + labs(title=NULL, subtitle=NULL, y="Temperature (Degrees Celcius)", x= "Date", caption = "Ambient Locations, 2010-2018") +
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%y")) +  theme_bw() + theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                                                                                                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=16), axis.title.x = element_text(size = 18),
                                                                                                     legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))

DO_2019 + scale_colour_manual(values = cols)


#2019 specific

#DO_____________________________________________________________

#Plotly
f <- list(
  family = "helvetica",
  size = 18,
  color = "Black"
)
y <- list(
  title = "2019 Dissolved Oxygen (mg/L)",
  titlefont = f
)
y2 <- list(
  title = "2018 Dissolved Oxygen (mg/L)",
  titlefont = f
)
x <- list(
  title = "Temperature (Degrees Celcius)",
  titlefont = f
)

plot_ly(subset(AllCountsAmbient, Year %in% "2019"), y = ~DO, color = ~Site, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y)


DO_2019 <- plot_ly(subset(AllCountsAmbient, Year %in% "2019"), y = ~DO, x = ~Site, legendgroup =~Year, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y)%>%
  layout(yaxis = list(autorange = FALSE,
                      range = c(7, 18)))

DO_2018 <- plot_ly(subset(AllCountsAmbient, Year %in% "2018"), y = ~DO, x = ~Site, legendgroup = ~Year, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y2)%>%
  layout(yaxis = list(autorange = FALSE,
                      range = c(7, 18)))



#compare2018_2019
subplot(DO_2018, DO_2019, nrows = 1, shareX = T)


ggplot(subset(AllCountsAmbient2019, Site %in% c("HC-1", "HC-2", "NC-1", "WC-1", "NC_2", "QC_2")), aes(x=Date, y=DO, group = Site, colour = DO.Type)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red") + 
  geom_line(size=1) + facet_wrap(~Site, ncol = 1) + labs(title=NULL, subtitle=NULL, y="DO (mg/L)", x= "Date", caption = "Ambient Locations, 2019") +
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%y")) +  theme_bw() + theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                                                                                                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=16), axis.title.x = element_text(size = 18),
                                                                                                     legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))


ggplot(subset(AllCountsAmbient2019, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(x=Date, y=DO, group = Site, colour = DO.Type)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 9.5, color = "red") + 
  geom_line(size=1) + facet_wrap(~Site, ncol = 1) + labs(title=NULL, subtitle=NULL, y="DO (mg/L)", x= "Date", caption = "TMDL Locations, 2019") +
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                                                                                                    axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=16), axis.title.x = element_text(size = 18),
                                                                                                    legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))


#compare 2018_2019
ggplot(subset(AllCountsAmbient2018_2019, Season == "Summer"), aes(Site, DO)) + 
  stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") + facet_wrap(~Year, ncol = 2) +
  geom_boxplot(fill = "gray97", width = 0.4, color = "darkcyan")  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Dissolved Oxygen (mg/L)") + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")



ggplot(AllCountsAmbient2018_2019, aes(Site, DO)) + 
  stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") + facet_wrap(~Year, ncol = 2) +
  geom_boxplot(fill = "gray97", width = 0.4, color = "darkcyan")  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Dissolved Oxygen (mg/L)") + geom_hline(yintercept = 9.5, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

#by basin

f <- list(
  family = "helvetica",
  size = 18,
  color = "Black"
)
x <- list(
  title = "Monitoring Basin",
  titlefont = f
)
y <- list(
  title = "DO (mg/L)",
  titlefont = f
  
)

plot_ly(AllCountsAmbient, y = ~DO, color = ~Monitoring.Basin, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y, showlegend = FALSE) %>%
  layout(xaxis = x)

ggplot(subset(AllCountsAmbient, Season == "Summer"), aes(Monitoring.Basin, TEMP)) + 
  stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") +
  geom_boxplot(fill = "gray97", width = 0.4, color = "darkcyan")  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees Celsius)") + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")





#TEMP________________
#ggplot2
ggplot(subset(AllCountsAmbient2019, Site %in% c("HC-1", "HC-2", "NC-1", "WC-1", "NC_2", "QC_2")), aes(x=Date, y=TEMP, group = Site, colour = Temp.Type)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red") + 
  geom_line(size=1) + facet_wrap(~Site, ncol = 1) + labs(title=NULL, subtitle=NULL, y="Temperature (Degrees Celcius)", x= "Date", caption = "Ambient Locations, 2010-2018") +
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%y")) +  theme_bw() + theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                                                                                                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=16), axis.title.x = element_text(size = 18),
                                                                                                     legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))


ggplot(subset(AllCountsAmbient2019, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(x=Date, y=TEMP, group = Site, colour = Temp.Type)) + geom_point(size=3, shape=21, fill="white") + geom_hline(yintercept = 16, color = "red") + 
  geom_line(size=1) + facet_wrap(~Site, ncol = 1) + labs(title=NULL, subtitle=NULL, y="Temperature (Degrees Celcius)", x= "Date", caption = "TMDL Locations, 2010-2018") +
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%y")) + theme_bw() + theme(axis.title.y = element_text(size=22), title = element_text(size = 18), axis.text.x = element_text(size = 15, angle = 65, vjust = 0.6),
                                                                                                    axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=16), axis.title.x = element_text(size = 18),
                                                                                                    legend.position = "none",  strip.text.x = element_text(size=12, colour = "steelblue3"))

#compare2018_2019
ggplot(subset(AllCountsAmbient2018_2019, Season == "Summer"), aes(Site, TEMP)) + 
  stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") + facet_wrap(~Year, ncol = 2) +
  geom_boxplot(fill = "gray97", width = 0.4, color = "darkcyan")  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees Celsius)") + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")


ggplot(subset(AllCountsAmbient, Season == "Summer"), aes(Site, TEMP)) + 
  stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") + facet_wrap(~CurrentYear, ncol = 2) +
  geom_boxplot(fill = "gray97", width = 0.4, color = "darkcyan")  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees Celsius)") + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

#by basin

ggplot(subset(AllCountsAmbient, Season == "Summer"), aes(Monitoring.Basin, TEMP)) + 
  stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") + facet_wrap(~CurrentYear, ncol = 2) +
  geom_boxplot(fill = "gray97", width = 0.4, color = "darkcyan")  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Temperature (Degrees Celsius)") + geom_hline(yintercept = 16, color="red", size=1) + 
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none") 



#Sediment________________________________


pturb2019 <- ggplot(AllCountsAmbient2019, aes(Site, Turbidity)) + 
  stat_boxplot(geom = 'errorbar', width = 0.4, color = "cyan4") +
  geom_boxplot(fill = "gray94", width = 0.5, color = "cyan4")  +
  labs(title = "e. Turbidity (NTU)", subtitle=NULL, y=NULL, x=NULL) + scale_y_log10(breaks=c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 150)) +
  theme(axis.title.y = element_text(size=7), title = element_text(size = 15), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_few() + theme(axis.title.y = element_text(size=7), title = element_text(size = 8), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                      axis.text.y = element_text(size = 10), legend.title = element_text(size=18), legend.text = element_text(size=15)) + coord_flip()

pturb2019 <- ggplotly(pturb)

pturb2019

f <- list(
  family = "helvetica",
  size = 18,
  color = "Black"
)
y <- list(
  title = "2019 Turbidity (NTU)",
  titlefont = f
)
x <- list(
  title = "Site",
  titlefont = f
)

plot_ly(AllCountsAmbient2019, y = ~Turbidity, color = ~Site, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y)

plot_ly(AllCountsAmbient2019, y = ~Turbidity, x = ~Site, color = ~Precipitation, type = "box", boxpoints = 'outliers',
        boxmean = TRUE) %>% 
  layout(yaxis = y) %>%
  layout(xaxis = x) %>%
  layout(boxmode = "group")

plot_ly(AllCountsAmbient, y = ~Turbidity, x = ~Site, color = ~Precipitation, type = "box", boxpoints = 'outliers',
        boxmean = TRUE) %>% 
  layout(yaxis = list(type = "log")) %>%
  layout(xaxis = x) %>%
  layout(boxmode = "group")

plot_ly(AllCountsAmbient2019, y = ~TSS, color = ~Site, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y)




ggplot(AllCountsAmbient2019, aes(Site, Turbidity, na.rm=TRUE)) + geom_boxplot(aes(fill=Precipitation), alpha=0.4,na.rm=TRUE) + geom_hline(yintercept = 4, color = "orange2", size = 1) + geom_hline(yintercept = 70, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, caption = "Source: In-Situ Monitoring, 2010-2018", y="Turbidity (NTU)") + scale_y_log10(breaks=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15))

#compare by basin

ggplot(AllCountsAmbient, aes(Monitoring.Basin, Turbidity, na.rm=TRUE)) + geom_boxplot(aes(fill=Precipitation), alpha=0.4,na.rm=TRUE) + geom_hline(yintercept = 4, color = "orange2", size = 1) + geom_hline(yintercept = 70, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, caption = "Source: In-Situ Monitoring, 2010-2018", y="Turbidity (NTU)") + scale_y_log10(breaks=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15))

#Metals_______________

Zn <- ggplot(AllCountsAmbient2019, aes(Site, Zn, na.rm=TRUE)) + geom_boxplot(aes(fill=Site), alpha=0.4,na.rm=TRUE) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Zn (ug/L)") + scale_y_log10(breaks=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")

Cu <- ggplot(AllCountsAmbient2019, aes(Site, Cu, na.rm=TRUE)) + geom_boxplot(aes(fill=Site), alpha=0.4,na.rm=TRUE) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Cu (ug/L)") + scale_y_log10(breaks=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")

Pb <- ggplot(AllCountsAmbient2019, aes(Site, Pb, na.rm=TRUE)) + geom_boxplot(aes(fill=Site), alpha=0.4,na.rm=TRUE) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, caption = "Source: Quarterly Monitoring, 2019", y="Pb (ug/L)") + scale_y_log10(breaks=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")

grid.arrange(Zn, Cu, Pb)

#_________________Nutrients

TN <- ggplot(AllCountsAmbient2019, aes(Site, TN, na.rm=TRUE)) + geom_boxplot(aes(fill=Site), alpha=0.4,na.rm=TRUE) +geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="TN (mg/L)") +
  theme_bw() + theme(axis.title.y = element_text(size=16), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")

TP <- ggplot(AllCountsAmbient2019, aes(Site, TP, na.rm=TRUE)) + geom_boxplot(aes(fill=Site), alpha=0.4,na.rm=TRUE) +geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, caption = "Source: Quarterly Monitoring, 2019", y="TP (mg/L)") +
  theme_bw() + theme(axis.title.y = element_text(size=16), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")

grid.arrange(TN, TP)

TN_Basin <- ggplot(AllCountsAmbient2019, aes(Monitoring.Basin, TN, na.rm=TRUE)) + geom_boxplot(aes(fill=Monitoring.Basin), alpha=0.4,na.rm=TRUE) +geom_hline(yintercept = 0.58, color = "orange2", size = 1) + geom_hline(yintercept = 0.98, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="TN (mg/L)") +
  theme_bw() + theme(axis.title.y = element_text(size=16), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")

TP_Basin <- ggplot(AllCountsAmbient2019, aes(Monitoring.Basin, TP, na.rm=TRUE)) + geom_boxplot(aes(fill=Monitoring.Basin), alpha=0.4,na.rm=TRUE) +geom_hline(yintercept = 0.04, color = "orange2", size = 1) + geom_hline(yintercept = 0.178, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, caption = "Source: Quarterly Monitoring, 2019", y="TP (mg/L)") +
  theme_bw() + theme(axis.title.y = element_text(size=16), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")
grid.arrange(TN_Basin, TP_Basin, Sample_Map, nrow = 2)
#TSS_______________________

ggplot(AllCountsAmbient2019, aes(Site, TSS, na.rm=TRUE)) + geom_boxplot(aes(fill=Site), alpha=0.4,na.rm=TRUE) +geom_hline(yintercept = 33, color = "orange2", size = 1) + #geom_hline(yintercept = 275, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, caption = "Source: Quarterly Monitoring, 2019", y="TSS (mg/L)") +
  theme_bw() + theme(axis.title.y = element_text(size=16), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")

#fecals

Fecal_2019 <-ggplot(subset(AllCountsAmbient2019, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform), label = sprintf("%0.2f", round(fecalmeans, digits = 2))) + 
  stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") +
  geom_boxplot(fill = "gray97", width = 0.4, color = "darkcyan")  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform (CFU)") + geom_hline(yintercept = 50, color="red", size=1) + 
  geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none") + stat_compare_means()

#compare 2018 & 2019
ggplot(subset(AllCountsAmbient2018_2019, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform), label = sprintf("%0.2f", round(fecalmeans, digits = 2))) + 
  stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") + facet_wrap(~Year, ncol = 2) +
  geom_boxplot(fill = "gray97", width = 0.4, color = "darkcyan")  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform (CFU)") + geom_hline(yintercept = 50, color="red", size=1) + 
  geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")

#compare 2010-2018 to 2019

ggplot(subset(AllCountsAmbient, Site %in% c("LS-1", "JO-1", "NCLD-1", "PM-1", "SARU")), aes(Site, Fecal.Coliform), label = sprintf("%0.2f", round(fecalmeans, digits = 2))) + 
  stat_boxplot(geom = 'errorbar', width = 0.25, color = "darkcyan") + facet_wrap(~CurrentYear, ncol = 2) +
  geom_boxplot(fill = "gray97", width = 0.4, color = "darkcyan")  +
  stat_summary(fun.y= "mean", colour="black", geom="point", 
               shape=18, size=3,show_guide = FALSE) + 
  labs(title = NULL, subtitle=NULL, y="Fecal Coliform (CFU)") + geom_hline(yintercept = 50, color="red", size=1) + 
  geom_hline(yintercept = 200, color="blue", size=1) + scale_y_log10(breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15)) +
  theme_bw() + theme(axis.title.y = element_text(size=15), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=18), legend.text = element_text(size=15), legend.position = "none")
#by basin

f <- list(
  family = "helvetica",
  size = 18,
  color = "Black"
)
x <- list(
  title = "Monitoring Basin",
  titlefont = f
)
y <- list(
  title = "Fecal Coliform (CFU)",
  titlefont = f,
  type = "log"
  
)



plot_ly(AllCountsAmbient, y = ~Fecal.Coliform, color = ~Monitoring.Basin, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y, showlegend = FALSE) %>%
  layout(xaxis = x)%>%
  layout(shapes=list(type='line', x0 = "Horse Creek", x1 = "Upper North Creek", y0=50, y1=50, line=list(dash='dot', width=1, color = "red")),
         title = 'Fecal Coliform (CFU) by Monitoring Basin',
         xaxis = list(title = "Monitoring Basin", showgrid = TRUE),
         yaxis = list(title = "Fcal Coliform (CFU)", showgrid = TRUE))


plot_ly(AllCountsAmbient, x =~Monitoring.Basin, y = ~Fecal.Coliform, color = ~Site, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y) %>%
  layout(xaxis = x)%>%
  layout(boxmode = "group")


#comparison to 2018


#ggspatial
Basins <- rgdal::readOGR(dsn = "inputs", layer = "Monitoring_Basins_Project")
Streams <- raster::shapefile("inputs/Streams_BUGA_WGS.shp")
Sample_Points <- raster::shapefile("inputs/2018_Ambient_Sample_Locations.shp")
Sample_Points <- rgdal::readOGR(dsn = "inputs", layer = "2018_Ambient_Sample_Locations")

Basinsdf <- fortify(Basins)
Streamsdf <- fortify(Streams)
Sample_Pointsdf <- as.data.frame(Sample_Points)


Zn_basin <- ggplot(AllCountsAmbient2019, aes(Monitoring.Basin, Zn, na.rm=TRUE)) + geom_boxplot(aes(fill=Monitoring.Basin), alpha=0.4,na.rm=TRUE) + geom_hline(yintercept = 32, color = "orange2", size = 1) + geom_hline(yintercept = 35, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Zn (ug/L)") + scale_y_log10(breaks=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")

Cu_basin <- ggplot(AllCountsAmbient2019, aes(Monitoring.Basin, Cu, na.rm=TRUE)) + geom_boxplot(aes(fill=Monitoring.Basin), alpha=0.4,na.rm=TRUE) + geom_hline(yintercept = 3.47, color = "orange2", size = 1) + geom_hline(yintercept = 4.61, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, y="Cu (ug/L)") + scale_y_log10(breaks=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")

Pb_basin <- ggplot(AllCountsAmbient2019, aes(Monitoring.Basin, Pb, na.rm=TRUE)) + geom_boxplot(aes(fill=Monitoring.Basin), alpha=0.4,na.rm=TRUE) + geom_hline(yintercept = 0.541, color = "orange2", size = 1) + geom_hline(yintercept = 13.88, color = "red", size=1) +
  labs(title = NULL, subtitle=NULL, caption = "Source: Quarterly Monitoring, 2019", y="Pb (ug/L)") + scale_y_log10(breaks=c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                     axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")

grid.arrange(Zn_basin, Cu_basin, Pb_basin, Sample_Map)
#never got ggspatial to work unfortunately :()
#ggplot() +
#ggspatial::geom_spatial_polygon(data = Basins, mapping = aes(colour = "Name")) +
#coord_map()



#BIBI by Watershed!

f <- list(
  family = "helvetica",
  size = 18,
  color = "Black"
)
x <- list(
  title = "Monitoring Basin",
  titlefont = f
)
y <- list(
  title = "B-IBI (0-100)",
  titlefont = f
  
)

plot_ly(AllCountsAmbient, y = ~BIBI, color = ~Monitoring.Basin, type = "box", jitter = 0.3, pointpos = -1.8, boxpoints = 'all') %>%
  layout( yaxis = y, showlegend = FALSE) %>%
  layout(xaxis = x)