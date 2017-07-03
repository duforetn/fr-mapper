#Colours 
colrp <- colorRampPalette(c('lightyellow', "yellow", "orange", "red1", 'red3', "brown"))(50)

#Load a .RData administrative file from http://gadm.org
gadmFile = 'data/FRA_adm0.RData'

#Create the mapper
source('mapper.R')
myGrid = createGrid(gadmFile, 250, 250)
myConstraints = createConstraints(myGrid, gadmFile)

#Data
Data <- read.table("../data/geopointsFRA.dat")
torm <- which(Data[, 1] < 40 | Data[, 1] > 60 | Data[, 2] < -5 | Data[, 2] > 10)
Data <- Data[-torm, ]

coords = Data[, 2:1]
colnames(coords) = c("lon", 'lat')
value = Data[, 3]

#Smoothing
source('smoothData.R')
smoothedValues = smooth(coords, value, 25, 25, myFunction = median, threshold = 1)

#Plot
maps(smoothedValues[, c('lon', 'lat')], smoothedValues[, 'value'], myGrid, myConstraints, colrp, draw.points = FALSE)

