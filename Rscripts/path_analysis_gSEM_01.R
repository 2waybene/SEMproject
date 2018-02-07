library(gSEM)
##' ## Load the sample acrylic data set
data(acrylic)
## Run semi-gSEM principle one
ans <- sgSEMp1(acrylic, predictor = "IrradTot", response = "YI")
## Extract relations between IrradTot and IAD2
cf <- path(ans, from = "IrradTot", to = "IAD2")
print(cf)

plot(ans, cutoff = 0.1)



setwd("x:/project2018/Diagram_Wu/workingDir")
dt <- "x:/project2018/Diagram_Wu/data/GSE58144_levels_activities.csv"


dm.1 <- read.csv(dt)


dm.1 <- dm.1[,-1]

ans <- sgSEMp1(dm.1, predictor = "GATA2", response = "PGR")

ans <- sgSEMp1(dm.1, predictor = "GATA2", response = "SOX17")


plot(ans, cutoff = 0.1)

