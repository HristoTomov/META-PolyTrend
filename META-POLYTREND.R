library(zoo)
library(sp)
library(raster)

rasterOptions(progress="text")

# Config section

region.countries <- list (  "BHR", ## Bahrain
                            "CYP", ## Cyprus
                            "EGY", ## Egypt
                            "IRN", ## Iran
                            "IRQ", ## Iraq
                            "ISR", ## Israel
                            "JOR", ## Jordan
                            "KWT", ## Kuwait
                            "LBN", ## Lebanon
                            "OMN", ## Oman
                            "PSE", ## Palestine
                            "QAT", ## Qatar
                            "SAU", ## Saudi Arabia
                            "SYR", ## Syria
                            "TUR", ## Turkey
                            "ARE", ## United Arab Emirates
                            "YEM") ## Yemen

# Path to GIMMS NDVI3g dataset
data.path <- "/PATH/TO/NDVI3g/"

# Path to output the results
output.path <- "/PATH/TO/ResultsPT/"

data.date.start <- as.yearmon('1982-01')
data.date.end <-  as.yearmon('2010-12')

# NDVI threshold for the period
ndvi3g.threshold <- 0.1

# Number of growing months
nMonths <- 4

PolyTrend.DIR <- "/PATH/TO/PolyTrend/"
MidEastAnlz.DIR <- "/PATH/TO/META-POLYTREND/"

# End Config

# Load PolyTrend
source(paste(PolyTrend.DIR,"PolyTrend.R",sep=""))

# Load MidEastAnlz files
source(paste(MidEastAnlz.DIR,"readNDVI3g.R",sep=""))
source(paste(MidEastAnlz.DIR,"loadRegion.R",sep=""))
source(paste(MidEastAnlz.DIR,"PTwrapper.R",sep=""))
source(paste(MidEastAnlz.DIR,"GenerateMaps.R",sep=""))

region.ts <- loadRegion(region.countries,data.date.start,data.date.end, data.DIR)
NDVI3g.dates <- getZ(region.ts)
message("Processing dataset...","\n",appendLF=FALSE)
flush.console()

rc.trends <- calc(region.ts, fun=function(x) {PTwrapper(x,s
                  tackDates=data.dates,ndvi3g.threshold=ndvi3g.threshold, 
                  nMonths=nMonths)}, forceapply=TRUE)

names(rc.trends) <- c("TrendType", "Slope", "Direction", "Significance")

# Report
message("Writing the results...","\n",appendLF=FALSE)
flush.console()

output.files <- paste(output.path, format(data.date.start, format="%Y%b"), 
                      format(data.date.end, format="%Y%b"), "." , names(rc.trends), "." , "PolyTrends", sep="")

writeRaster(rc.trends, filename=output.files, bylayer=TRUE, format=output.format, overwrite=TRUE)

output.statfile <- paste(output.path, format(data.date.start, format="%Y%b"), 
                         format(data.date.end, format="%Y%b"), "." , "PolyTrends.txt", sep="")
out<-capture.output(freq(rc.trends))
cat(out,file=output.statfile,sep="\n")

GenerateMaps(output.path, output.name = "TrendType",region.SPDF, 
             map.raster=rc.trends$TrendType,
             map.title=paste("Trend Type", 
                            format(data.date.start, format="%Y%b"), 
                            format(data.date.end, format="%Y%b", sep=" ")), 
             colors=c( "#e60001", "#0170fd","#d0ff72","#fdedce","#cdcdcd"),
             legend.classes = TRUE, legend.cols = 3, 
             legend.lables=c("Cubic","Quadratic","Linear","No Trend", "Concealed"),
             revOrder = TRUE
)

GenerateMaps(output.path, output.name = "TrendSignificance",region.SPDF, 
             map.raster=rc.trends$Significance,
             map.title=paste("Trend Significance", 
                            format(data.date.start, format="%Y%b"), 
                            format(data.date.end, format="%Y%b", sep=" ")), 
             colors=c("#08ae00","#f5b338"),
             legend.classes = TRUE, legend.cols = 2, 
             legend.lables=c("Significant","Not Significant"),
             revOrder = TRUE
)

GenerateMaps(output.path, output.name = "TrendSlope", region.SPDF,
             map.raster=rc.trends$Slope,
             map.title=paste("Trend Slope", 
                            format(data.date.start, format="%Y%b"), 
                            format(data.date.end, format="%Y%b", sep=" ")), 
             colors=brewer.pal(10,"RdYlGn")
             
)

GenerateMaps(output.path, output.name = "TrendDirection", region.SPDF, 
             map.raster=rc.trends$Direction,
             map.title=paste("Trend Direction", 
                            format(data.date.start, format="%Y%b"), 
                            format(data.date.end, format="%Y%b", sep=" ")), 
             colors=c("#a6d226","#d07b1d"),
             legend.classes = TRUE, legend.cols = 2, 
             legend.lables=c("Positive","Negative"),
             revOrder = TRUE
)

GenerateMaps(output.path, output.name = "TrendSamples",region.SPDF, 
             map.raster=rc.trends$TrendType,
             map.title=paste("Trend Samples", 
                             format(data.date.start, format="%Y%b"), 
                            format(data.date.end, format="%Y%b", sep=" ")), 
             colors=c( "#e60001", "#0170fd","#d0ff72","#fdedce","#cdcdcd"),
             legend.classes = TRUE, legend.cols = 3, 
             legend.lables=c("Cubic","Quadratic","Linear","No Trend", "Concealed"),
             revOrder = TRUE,
             prob.samples = TRUE,
             nSamplesPerClass = 2, 
             pdfGrid = c(5,2),
             region.ts = region.ts
)

message("Done.","\n",appendLF=FALSE)
flush.console()

