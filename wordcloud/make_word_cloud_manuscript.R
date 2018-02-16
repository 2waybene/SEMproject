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



##=========================
##  All genes
##=========================
setwd("x:/project2018/p53_manuscript/WordCloud/")
d <- read.csv("Jianying_wordcloud_for_all_cistrom_20180110.csv")

d.4.wordcloud <- d[,c(1,3)]
colnames(d.4.wordcloud) <- c ("word", "freq")
table(d.4.wordcloud$freq)

set.seed(1234)
wordcloud2(d.4, size = 0.4, minRotation = -pi/16, maxRotation = -pi/6, minSize = 1 , rotateRatio = 0)

##  There are unseen characters, need to filter them out!!
d.all.wordcloud <- d[c(1:943),c(1,3)]
colnames(d.all.wordcloud) <- c ("word", "freq")
table(d.all.wordcloud$freq)
pal2 <- brewer.pal(8,"Dark2")

png("wordcloud_cistrom_all_windows.png", width=1280,height=800)

wordcloud(d.all.wordcloud$word,d.all.wordcloud$freq, scale=c(8,.2),min.freq=1,
          max.words=Inf, random.order=FALSE, rot.per=0, colors=pal2)


dev.off()




##  okay, can I make a ball shape plot??
##==============================
##  let's get a ball shape
##==============================

table(d.all.wordcloud$freq)

d.mod <- d.all.wordcloud

d.mod <- d.mod[order(d.mod$freq, decreasing = TRUE),]
table(d.mod$freq)

prominent.d <- head(d.mod,9)
upper.m.d   <- d.mod[c(10:25),]
median.m.d  <- d.mod[c(26:45),]
lower.m.d   <- d.mod[c(46:85),]
down.d      <- d.mod[c(86:378),]
d.d.d       <- d.mod[c(379:943),]


prominent.d$freq <- prominent.d$freq*30 + ceiling(runif(9)*100)
upper.m.d$freq   <- upper.m.d$freq*20 + ceiling(runif(16)*40)
median.m.d$freq  <- median.m.d$freq*12+ ceiling(runif(20)*20)
lower.m.d$freq   <- lower.m.d$freq*7 + ceiling(runif(40)*10)
down.d$freq      <- down.d$freq*2+ ceiling(runif(293)*5)



new.d <- rbind(prominent.d,upper.m.d ,median.m.d,lower.m.d ,down.d ,d.d.d)

png("wordcloud_cistrom_all_ball_shape.png", width=1280,height=800)
wordcloud(new.d$word, new.d$freq, scale=c(8,.2),min.freq=1,
          max.words=Inf, random.order=FALSE, rot.per=0, colors=pal2)
dev.off()

set.seed(1234)
wordcloud2(new.d , size = 0.4, minRotation = -pi/16, maxRotation = -pi/6, minSize = 1 , rotateRatio = 0)
wordcloud2(d.mod , size = 0.4, minRotation = -pi/16, maxRotation = -pi/6, minSize = 1 , rotateRatio = 0)


