
#create grid as clusters
createClusters = function(coords, nClusterLon = 20, nClusterLat = 20) {
    lon.min <- min(coords[, 'lon'])
    lon.max <- max(coords[, 'lon'])
    lat.min <- min(coords[, 'lat'])
    lat.max <- max(coords[, 'lat'])
    lon.clusters=seq(from=lon.min, to=lon.max, length=nClusterLon)
    lat.clusters=seq(from=lat.min, to=lat.max, length=nClusterLat)
    grid = cbind(rep(lon.clusters, nClusterLat), rep(lat.clusters, each = nClusterLon))
    colnames(grid) = c('lon', 'lat')
    return(grid)
}

#Assign a cluster to a given point
toCluster = function(x, clusters) {
    return(
            which(abs(clusters[, 'lat']-x['lat'])==min(abs(x['lat']-clusters[, 'lat'])) & abs(clusters[, 'lon']-x['lon'])==min(abs(x['lon']-clusters[, 'lon'])))
    )
}

smooth = function(coords, value, threshold = 0, myFunction = mean, nClusterLon = 20, nClusterLat = 20, trim = TRUE) {

    grid = createClusters(coords, nClusterLon, nClusterLat)
    myClust = function(x) toCluster(x, grid)
    clustering = apply(coords, 1, myClust)    

    smoothedValues = rep(0, nClusterLon*nClusterLat)
    for (c in 1:length(smoothedValues)) {
        c_index = which(clustering == c)
        smoothedValues[c] = ifelse((length(c_index) > threshold), yes = myFunction(value[c_index]), no = NA)
     }
    result = cbind(grid, smoothedValues)
    colnames(result) = c('lon', 'lat', 'value')
    torm = which(is.na(result[, 'value']))
    if (trim) result <- result[-torm, ]
    return(result)
}
