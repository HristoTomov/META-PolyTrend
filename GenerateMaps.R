GenerateMaps <- function(output.path, output.name, countries.SPDF, 
                         countries.bg = "white", 
                         countries.border = "gray70",
                         map.raster, map.title, scalebar.dist=1000,
                         northarrow.size=3,
                         northarrow.topoffset=1, colors, 
                         legend.classes = FALSE, legend.cols = 2,
                         legend.lables, prob.samples=FALSE, 
                         nSamplesPerClass = 2, revOrder=FALSE,
                         pdfGrid = c(5,2), region.ts) {
        
        pdf(paste(output.path, paste(output.name, "pdf", sep="."),sep="/")) 
        par.old <- par(no.readonly = TRUE)
        on.exit(par(par.old))
        par(mar=c(4, 4, 4, 5))
        
        plot(countries.SPDF, lwd=0.2, border=countries.bg,col=countries.bg, 
             xlim=c(xmin(map.raster) + 1, xmax(map.raster) - 1), 
             ylim=c(ymin(map.raster) + 1, ymax(map.raster) - 1),axes = F)
        
        degAxis(1, col = 'white', col.ticks = 'black', cex.axis = 0.95, font = 3, family = 'serif')
        degAxis(2, col = 'white', col.ticks = 'black', cex.axis = 0.95, font = 3, family = 'serif')
        box()
        if(legend.classes) {
                
                if(revOrder) {
                        plot(map.raster, legend = FALSE, col=rev(colors), axes=F, add=TRUE)
                }else{
                        plot(map.raster, legend = FALSE, col=colors, axes=F, add=TRUE)
                }
                
        } else {
                redvals <- seq(minValue(map.raster),0,length.out = 6)[-6]
                greenvals <- seq(0,maxValue(map.raster),length.out = 6)[-1]
                breakingpoints <-c(redvals,0,greenvals)
                
                plot(map.raster, legend = FALSE, col=colors, breaks = breakingpoints, axes=F, add=TRUE)
                
                plot(map.raster, legend.only=TRUE, col=colors,
                     breaks = breakingpoints,
                     legend.width=1, legend.shrink=0.75,
                     axis.args=list(at=breakingpoints, labels=round(breakingpoints,3), cex.axis=0.6),
                     legend.args=list(text='Slope (NDVI/Year)', side=4, font=2, line=2.5, cex=0.8))
        }
        
        invisible()
        
        grid(lwd = 1.8)
        text(xmin(map.raster) + 0.5, ymin(map.raster) - 3, 
             "Projection: Geographic\nCoordinate System: WGS 1984\n
             Data Source: GIMMS3g/GADM", 
             adj=c(0,0), cex=0.7, col="gray18")
        scalebar(scalebar.dist, xy = c(xmin(map.raster) + 0.5,ymin(map.raster)),  
                 lonlat = TRUE, label = paste(scalebar.dist, "km"), lwd=2)
        
        if(legend.classes) {
                
                colors <- c(colors,"#ffffff")
                legend.lables <- c(legend.lables,"Masked")
                legend("bottomright",  legend = legend.lables, 
                       fill = colors,ncol=legend.cols,cex = 0.8)
        } 
        
        north.coord <- c(xmin(map.raster) + northarrow.topoffset, ymax(map.raster) - northarrow.topoffset)
        SpatialPolygonsRescale(layout.north.arrow(1), offset = north.coord, scale = northarrow.size, plot.grid=F)
        plot(countries.SPDF, lwd=0.8, border="gray70", add=TRUE, axes = F)
        
        mtext(side=3, line=1, map.title, cex=1.2)
        mtext(side=1, "Longitude", line=2.5, cex=0.8)
        mtext(side=2, "Latitude", line=2.5, cex=0.8)
        
        if(prob.samples) {
                
                prob <- probRaster(nSamplesPerClass,map.raster)
                points(prob$xy.samples[,1],prob$xy.samples[,2])
                text(prob$xy.samples[,1],prob$xy.samples[,2], 
                     paste(letters[1:nrow(prob$xy.samples)]), cex = 0.85, pos=4)
                dev.off() 
                
                pdf(paste(output.path, paste(output.name, "Fig", "pdf", sep=".")), 
                    title="PolyTrend Samples", width=8.27, height=11.7)
                par(mfrow = pdfGrid, oma=c(2,5,5,0), mar=c(3,3,2,2))
                
                figDates <- unique(as.vector(format(getZ(region.ts),"%y")))
                ndvi3g.vals <- sapply(prob$cell.samples, function(x) aggregateProb(
                                            region.ts[x], getZ(region.ts)))
                sapply(1:ncol(ndvi3g.vals), function(x) plot(
                              PolyTrend(Y = ndvi3g.vals[,x], alpha = 0.05),
                                    fig.text=paste(letters[x],")", sep=""), fig.dates=figDates))
                
                mtext(side=3, line=1, map.title, cex=1.4, adj = 0.06, outer = TRUE)
                dev.off() 
                
                output.statfile <- paste(output.path, 
                                 format(data.date.start, format="%Y%b"), 
                                 format(data.date.end, format="%Y%b"), ".", 
                                                 "SamplesNDVI.txt", sep="")
                out<-capture.output(list("ndvi3g.vals"=ndvi3g.vals, "Years" =figDates))
                cat(out,file=output.statfile,sep="\n")

        } else {
                
                dev.off() 
        }
}

probRaster <- function(nSamples,rst) {
        xy.samples <- vector(mode="numeric", length=0)
        cell.samples <- vector(mode="numeric", length=0)
        unq <- unique(rst)
        for(i in 1:length(unq))
        {
                unq.values <-which(values(rst)==rev(unq[i]))
                rnd.samples <- sample(unq.values, nSamples)
                coord.sample <- xyFromCell(rst, rnd.samples)
                
                xy.samples <- rbind(xy.samples,coord.sample)
                cell.samples <- cbind(cell.samples,rnd.samples)
                
        }
        return(list("xy.samples" = xy.samples, "cell.samples" = cell.samples))
}

aggregateProb <- function(x,stackDates) {
        
        ndvi3g.ts <-zoo(as.vector(x), stackDates)
        ndvi3g.ts <- aggregate(ndvi3g.ts, as.yearmon, mean)
        by.years <- split( as.vector(ndvi3g.ts), as.vector(format(time(ndvi3g.ts), "%y")) )
        ndvi3g.ts <- as.vector(sapply(by.years, function(x) mean(sort(x, decreasing = TRUE)[1:4])))
        return(ndvi3g.ts)               
}
