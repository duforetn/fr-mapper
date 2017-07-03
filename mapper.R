require(sp)
require(fields)

# return a grid on which clusters could be projected on using 'map' function
createGrid = function(gadmFile, npixels_lon = 100, npixels_lat = 100)
{
    load(gadmFile)
    att <- attributes(gadm)
    Poly <- att$polygons
    lon.min <- att$bbox[1, 'min']
    lon.max <- att$bbox[1, 'max']
    lat.min <- att$bbox[2, 'min']
    lat.max <- att$bbox[2, 'max']
    lon.pix=seq(from=lon.min,to=lon.max, length=npixels_lon)
    lat.pix=seq(from=lat.min,to=lat.max, length=npixels_lat)
    grid=make.surface.grid( list( lon.pix,lat.pix))
    return(grid)
}


#This function returns an array of booleans telling if a pixel is in or out the polygon
createConstraints = function(myGrid, gadmFile){
    load(gadmFile)
    myPolygon = attributes(gadm)$polygons
    listPolygon <- slot(myPolygon[[1]], "Polygons")
    sizePoly <- NULL
    for (i in 1:length(listPolygon)) sizePoly <- c(sizePoly, nrow(slot(listPolygon[i][[1]], 'coords')))
    coordsPolygon <- slot(listPolygon[which.max(sizePoly)][[1]], 'coords')
    areIn <- point.in.polygon(myGrid[, 1], myGrid[, 2], coordsPolygon[, 1], coordsPolygon[, 2])
    return(areIn)
}

# Main mapping function
maps <- function(Coords, value, grid, constraints, colrp, Norm = F, min = -10, log = F, levels = NULL, cex = 1, draw.points = T, ...){

        if (Norm) value <- (value - min(value))/(max(value) - min(value))
        if (log) value <- log(value)
        if (log) levels <- log(levels)

        MyFit <- Krig(Coords, as.numeric(value))
        FitOverGrid <- predict(MyFit, grid)
        FitGrid <- as.surface(grid, FitOverGrid)
        if (class(constraints)!= "NULL") { FitGrid[[8]][ !constraints ] = NA }
        FitGrid[[8]][ FitGrid[[8]] < min ] = NA
        surface(FitGrid, col = colrp, levels = levels, ...)
        if (draw.points) points(Coords, cex = cex, ...)

}

