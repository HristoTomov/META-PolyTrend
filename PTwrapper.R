PTwrapper <- function(x, stackDates, ndvi3g.threshold, nMonths) {
        
        if(any(is.na(x))) { ## filter NA
                
                return(c(NA, NA, NA, NA))                
        } else {
                
                ndvi3g.ts <-zoo(as.vector(x), stackDates)
                
                if (any(as.vector(rollapply(ndvi3g.ts, 24, mean, by = 24)) < ndvi3g.threshold)){ 
                        return(c(NA, NA, NA, NA))
                } else {
                        
                        ndvi3g.ts <- aggregate(ndvi3g.ts, as.yearmon, mean)
                        by.years <- split( as.vector(ndvi3g.ts), as.vector(format(time(ndvi3g.ts), "%y")) )
                        ndvi3g.ts <- vapply(by.years, function(x) mean( sort(x, decreasing = TRUE)[1:nMonths]), FUN.VALUE=double(1))
                        ndvi3g.ts <- unname(ndvi3g.ts)
                        PT <- PolyTrend(Y = ndvi3g.ts, alpha = 0.05)
                        return(c(PT$TrendType, PT$Slope, PT$Direction, PT$Significance))
                }
        } ## end na check
} ## end
