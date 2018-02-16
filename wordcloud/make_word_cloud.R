##===============================
##  make_word_cloud.R
##  Jianying Li
##  For Thuy-Ai's project
##  For paper: NAR - 01112018
##===============================
library(wordcloud2)
library(tm)
library(SnowballC)
library(wordcloud)


##================================
##  Get data
##================================

setwd("x:/project2018/p53_manuscript/WordCloud/")
d <- read.csv("Jianying_wordcloud_for_4_or_all_20180110.csv")

d.4.wordcloud <- d[,c(1,3)]
colnames(d.4.wordcloud) <- c ("word", "freq")
table(d.4.wordcloud$freq)




##==================================================
##  Minimum four or more counts -- 123 genes
##==================================================
head(d.4.wordcloud)
d.4 <- d.4.wordcloud[which(d.4.wordcloud$freq >=4),]
table(d.4$freq)
d.4$freq <- d.4$freq-3

## with changing the "size", all 51 are visiable.

set.seed(1234)
wordcloud2(d.4, size = 0.4, minRotation = -pi/16, maxRotation = -pi/6, minSize = 1 , rotateRatio = 0)



##==================================================
##  Try to print to emf file
##  FIXME: nothing works from here on
##        (jyl, 01/16/18)
##==================================================

library(devEMF)
emf (file = "Try_EMF_cistrom_4up.emf", width=12, height=8)
set.seed(1234)
wordcloud2(d.4, size = 0.4, minRotation = -pi/16, maxRotation = -pi/6, minSize = 1 , rotateRatio = 0)
dev.off()

myWordCloudEMF (d.4, size = 0.4, minRotation = -pi/16, maxRotation = -pi/6, minSize = 1 , rotateRatio = 0)
##================
##  my function
##================

myWordCloudEMF <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                            fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                            minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                            rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                            widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if (class(data) == "table") {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
 
   htmlwidgets::saveWidget("wordcloud2", settings, 
              width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                    browser.padding = 0, browser.fill = TRUE))
  
  #chart = htmlwidgets::createWidget("wordcloud2", settings, 
  #                                  width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            #browser.padding = 0, browser.fill = TRUE))
  chart
}


