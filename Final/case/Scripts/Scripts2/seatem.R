
library('data.table')
library('raster')
library('rasterVis')
library('rworldmap')
library('marmap')
library('oce')
library('mapview')
library('Kendall')
library('wql')
library('snow')

sstReynolds <- fread(input = "datos/sst.csv", sep = ",", header = TRUE, showProgress = TRUE, data.table = FALSE, stringsAsFactors = FALSE)

head(sstReynolds)

colnames(sstReynolds) <- c("lon", "lat", "time", "sst")

sstReynolds$time <- paste(sstReynolds$time, "-01", sep = "")

sstReynoldsSPDF <- sstReynolds
coordinates(sstReynoldsSPDF) <- c("lon", "lat")

wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

proj4string(sstReynoldsSPDF) <- wgs.84

CreateRasterBrickSST <- function(sst_spdf, from, to, xmin, xmax, ymin, ymax) {
  
  e <- extent(xmin, xmax, ymin, ymax)

  c <- crop(sst_spdf, e)

  time <- unique(sst_spdf$time)
  
  rasterList <- list()
  
  from <- strptime(from, "%Y-%m-%d")
  to <- strptime(to, "%Y-%m-%d")
  
  names <- character()
  
  for (i in 1:length(time)) {
    
    if(strptime(time[i], "%Y-%m-%d") >= from & strptime(time[i], "%Y-%m-%d") <= to) {
      
      message(paste(time[i]))  
      
      # Subset
      c2 <- c[which(c$time == time[i]), -3]
      
      # Create SpatialPixelsDataFrame
      spgrid <- SpatialPixelsDataFrame(points = c2, data = c2@data)
      
      # Create raster
      r <- raster(spgrid, layer = 2, values = TRUE)
      
      # Add raster to list
      if(length(rasterList) == 0) {
        rasterList <- r
      } else {
        rasterList <- append(rasterList, r)
      }
      
      # Add name to vector
      if(length(names) == 0) {
        names <- paste('SST_', time[i], sep = "")
      } else {
        names <- append(names, paste('SST_', time[i], sep = ""))
      }
    }
    
  }
  

  b <- brick(rasterList)
  names(b) <- names
  
  message("Finished!")

  return(b)
  
}


sst.rb.From1960To2017 <- CreateRasterBrickSST(sst_spdf = sstReynoldsSPDF,
                                              from = '1960-01-01',
                                              to = '2017-12-01',
                                              xmin = -70, 
                                              xmax = 60, 
                                              ymin = -60, 
                                              ymax = -20) 

writeRaster(x = sst.rb.From1960To2017, filename = paste("sst.rb.From1960To2017", ".tif", sep = ""), format = "GTiff", overwrite = TRUE)

paises <- getMap(resolution = 'high')

offset = 0.25
bati <- getNOAA.bathy(lon1 = -70 - offset, lon2 = 60 + offset, lat1 = -60 - offset, lat2 = -20 + offset, resolution = 10, keep = TRUE)

bati_raster <- as.raster(bati)

isobatas <- rasterToContour(bati_raster, levels = c(-50, -200, -500, -1000, -2000, -3000, -4000, -5000, -6000))

PlotRaster <- function(raster, plotBati = TRUE, plotPaises = TRUE) {
  
  colTemp <- oceColors9A(n = 128)
  
  plot(raster, colNA = "#272822", useRaster = TRUE, interpolate = TRUE, col = colTemp, alpha = 1)
  if (plotBati) plot.bathy(bati, image = FALSE, shallowest.isobath = 0, deepest.isobath = -8500, step = 200, lwd = 0.1, add = TRUE)
  if(plotPaises) plot(paises, col = "black", border = 'white', lwd = 0.2, add = TRUE)
  
}


PlotRaster(raster = sst.rb.From1960To2017$SST_1961.03.01)


mapView(sst.rb.From1960To2017$SST_1961.03.01, legend = TRUE, col.regions = oceColors9A(n = 256), layer.name = "sst", alpha.regions = 0.8) 


AnalisisMannKendall <- function(rb, cluster = TRUE, write = FALSE) {
  
  rasterList <- list()
  
  if (cluster) {
    
    message("Empezando Cluster...")
    
    beginCluster()
    
    message("Calculando tau...")
    fun.tau <- function(x) calc(x, function(y) MannKendall(y)$tau) # tau
    raster.tau <- clusterR(rb, fun.tau)
    message("Terminado!")
    
    message("Calculando sl...")
    fun.sl <- function(x) calc(x, function(y) MannKendall(y)$sl) # pvalue
    raster.sl <- clusterR(rb, fun.sl)
    message("Terminado!")
    
    message("Calculando pendiente de sen...")
    fun.sen <- function(x) calc(x, function(y) mannKen(y)$sen.slope) # pendiente Sen   
    raster.sen <- clusterR(rb, fun.sen) 
    message("Terminado!")
    
    endCluster()
    message("Fin de Cluster!")
    
  } else {
    
    message("Calculando tau...")
    raster.tau <- calc(rb, function(x) {MannKendall(x)$tau}) # tau
    message("Terminado!")
    message("Calculando sl...")
    raster.sl <- calc(rb, function(x) {MannKendall(x)$sl}) # pvalue
    message("Terminado!")
    message("Calculando pendiente de sen...")
    raster.sen <- calc(rb, function(x) {mannKen(x)$sen.slope}) # pendiente Sen
    message("Terminado!")
    
  }
  
  rasterList[[1]] <- raster.tau
  rasterList[[2]] <- raster.sl
  rasterList[[3]] <- raster.sen

  b <- brick(rasterList)
  names(b) <- c("tau", "sl", "sen")
  
  if (write) {
    
    message("Escribiendo a un archivo...")
    name <- paste(names(rb)[1], "-", tail(names(rb))[6], sep = "")
    writeRaster(x = raster.tau, filename = paste("rasterTau_", name, ".tif", sep = ""), format = "GTiff", overwrite = TRUE)
    writeRaster(x = raster.sl, filename = paste("rasterSl_", name, ".tif", sep = ""), format = "GTiff", overwrite = TRUE)
    writeRaster(x = raster.sen, filename = paste("rasterSen_", name, ".tif", sep = ""), format = "GTiff", overwrite = TRUE)
    message("Terminado!")
  }
  
  message("Fin")
  gc() # liberar memoria
  return(b)
  
}

sst.rb.From1960To2017.mk <- AnalisisMannKendall(rb = sst.rb.From1960To2017, cluster = TRUE, write = TRUE)

sst.rb.From1960To2017.mk.tau <- sst.rb.From1960To2017.mk$tau
sst.rb.From1960To2017.mk.tau[which(values(sst.rb.From1960To2017.mk$tau == 1))] <- NA
PlotRaster(sst.rb.From1960To2017.mk.tau) # tau

sst.rb.From1960To2017.mk.sl <- sst.rb.From1960To2017.mk$sl
sst.rb.From1960To2017.mk.sl[which(values(sst.rb.From1960To2017.mk$sl == 1))] <- NA
PlotRaster(sst.rb.From1960To2017.mk.sl) # sl

sst.rb.From1960To2017.mk.sen <- sst.rb.From1960To2017.mk$sen
PlotRaster(sst.rb.From1960To2017.mk.sen) # sen
plot(rasterToContour(sst.rb.From1960To2017.mk.sen), add = TRUE, col = "#272822", lwd = 0.5)


Resultados <- function(mk, nombreArchivo) {
  
  tau <- as.vector(extract(mk, 1:ncell(mk), layer = 1, nl = 1))
  sl <- as.vector(extract(mk, 1:ncell(mk), layer = 2, nl = 1))
  sen <- as.vector(extract(mk, 1:ncell(mk), layer = 3, nl = 1))
  
  # Armar data.frame
  df <- data.frame("ID" = as.character(1:length(tau)),
                   "tau" = tau,
                   "sl" = sl,
                   "sen" = sen)
  print(head(df))
  
  message("Escribiendo a un archivo...")
  write.table(df, paste(nombreArchivo, ".csv", sep = ""), sep = ",", row.names = FALSE)
  message("Terminado!")
  
  return(df)
  
}

df <- Resultados(mk = sst.rb.From1960To2017.mk, nombreArchivo = 'prueba')

hist(df$tau, 
     breaks = seq(min(df$tau), max(df$tau, na.rm = TRUE), length.out = 40), 
     border = "#272822", col = "#785DA7", 
     main = "Histograma de Tau",
     xlab = "tau",
     ylab = "Frecuencia")

hist(df$sl, 
     breaks = seq(min(df$sl), max(df$sl, na.rm = TRUE), length.out = 40), 
     border = "#272822", col = "#785DA7", 
     main = "Histograma de Sl",
     xlab = "sl",
     ylab = "Frecuencia")

hist(df$sen, 
     breaks = seq(min(df$sen, na.rm = TRUE), max(df$sen, na.rm = TRUE), length.out = 40), 
     border = "#272822", col = "#785DA7", 
     main = "Histograma de Sen",
     xlab = "sen",
     ylab = "Frecuencia")


AnalisisTendencia <- function(mk, df = df, tendencia = 1, pvalor = 0.05, write = FALSE, nombreArchivo = "nombArchivo") {
  
  message("Consultando sen y pvalor...")
  if (tendencia == 1) {celdas <- subset(df, sen > 0 & sl < pvalor, select = "ID")}
  if (tendencia == 0) {celdas <- subset(df, sen == 0 & sl < pvalor, select = "ID")}
  if (tendencia == -1) {celdas <- subset(df, sen < 0 & sl < pvalor, select = "ID")}
  message("Terminado...")
  
  message("Construyendo nuevo raster...")
  sen <- mk$sen
  sen[1:ncell(sen)] <- NA
  sen[as.numeric(as.character(celdas$ID))] <- mk$sen[as.numeric(as.character(celdas$ID))]
  message("Terminado...")
  
  if (write) {
    message("Escribiendo archivo...")
    writeRaster(x = sen, filename = paste(nombreArchivo, "_rasterSen.tif", sep = ""), format = "GTiff", overwrite = TRUE)
    message("Terminado...")
  }
  
  message("Fin")
  return(sen)
}

sen1 <- AnalisisTendencia(mk = sst.rb.From1960To2017.mk, df = df, tendencia = 1, pvalor = 0.05)
PlotRaster(raster = sen1)

sen2 <- AnalisisTendencia(mk = sst.rb.From1960To2017.mk, df = df, tendencia = -1, pvalor = 0.05)
PlotRaster(raster = sen2)

sen3 <- AnalisisTendencia(mk = sst.rb.From1960To2017.mk, df = df, tendencia = 0, pvalor = 0.05)
PlotRaster(raster = sen3)


PlotSerieTiempoCelda <- function(r, mk, xy, i = NULL, f = 1, nomVarY, nomVarX) {
  
  if (!is.null(i)) {
    
    ts_cell <- as.vector(extract(r, i))
    
    tau <- as.character(round(mk$tau[i], digits = 5))
    sl <- as.character(round(mk$sl[i], digits = 5))
    sen <- as.character(round(mk$sen[i], digits = 5))
    
  } else {
    
    ts_cell <- as.vector(extract(r, xy))
    
    tau <- as.character(round(as.vector(extract(mk$tau, xy)), digits = 5))
    sl <- as.character(round(as.vector(extract(mk$sl, xy)), digits = 5))
    sen <- as.character(round(as.vector(extract(mk$sen, xy)), digits = 5))
    
  }
  
  par(bg = "#D3D7CF")
  plot(x = 1:length(ts_cell), y = ts_cell, pch = 19, col = "#785DA7",
       xlab = paste(nomVarX), 
       ylab = paste(nomVarY), 
       ylim = c(min(minValue(r)), max(maxValue(r))), 
       main = paste("Celda ", i, sep = ""))
  lines(x = 1:length(ts_cell), y = ts_cell, lwd = 0.5, col = "#785DA7")
  
  lines(lowess(x = 1:length(ts_cell), y = ts_cell, f = f), col = "#E12669", lwd = 2) # ajuste suavizado
 
  text(x = length(ts_cell)/2, y = min(minValue(r)), pos = 3, 
       labels = paste("tau = ", tau, " | p-value = ", sl, " | sen = ", sen, sep = ""), 
       cex = 1, col = "#272822")
  par(bg = "white")
  
}



click() 

PlotSerieTiempoCelda(r = sst.rb.From1960To2017, mk = sst.rb.From1960To2017.mk, xy = cbind(-51.57595, -37.79489), f = 1, nomVarY = "SST (ºC)", nomVarX = "Tiempo")


