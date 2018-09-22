setwd("C:/Users/Craig/Dropbox/_UNF/Craig Thesis/R-Roughness/R/fig/L1")

#if (!require('iotools')) install.packages('iotools'); library('iotools')
if (!require('bmp')) install.packages('bmp'); library('bmp')
if (!require('pixmap')) install.packages('pixmap'); library('pixmap')
if (!require('imager')) install.packages('imager'); library('imager')

r <- read.bmp("RGB_1mm_Gridded.bmp")
p <- pixmapRGB(r)
# plot(p, xaxs="i", yaxs="i")
# box(lwd=2)

z <- read.table("Z_1mm_gridded.txt", header=T, sep=" ")
#z <- t(z)
z <- as.matrix(z) # Flip image about x-axis
colnames(z) <- 1:length(z[1,])
c <- mean(z, na.rm=T)
z <- (z-c)*1e4
# 
# x.s <- dev.size()[1]
# y.s <- dev.size()[2]
# 
# #W <- 0.6
# H <- 0.6
# OP <- 0.05
# IP <- 0.05
# 
# #W.W <- 1-W-2*OP-IP
# H.W <- 1-H-2*OP-IP
# W.W <- H.W*(y.s)/x.s
# 
# W <- 1-W.W-2*OP-IP*(y.s)/x.s
# 
# m <- matrix(c(
#       OP, OP+W, OP+IP+H.W, 1-OP,
#       OP, OP+W, OP, OP+H.W,
#       1-IP-W.W, 1-IP, OP+IP+H.W, 1-OP
#       ), ncol=4, byrow=T)
# 
# #split.screen(rbind(c(0.1,0.292,0.1, 0.98), c(0.312, 0.95, 0.1, 0.98)))
# #split.screen(rbind(c(0.1,0.292,0.1,0.768), c(0.312,0.95,0.1,0.768),c(0.312,0.95,0.788,0.98)))
# split.screen(m)
# screen(1)
# par(mar=c(0,0.1,0,0))
# #plot(p)
# image(z, axes=F, useRaster=T)
# usr <- locator(1)
# abline(h=usr$y,v=usr$x, lwd=2, lty=2)
# x <- round(usr$x*length(z[,1]),0)
# y <- round(usr$y*length(z[1,]),0)
# #text(usr$x, usr$y, paste0("(",x,",",y,",",z[x,y],")"), pos=4)
# box(lwd=2)
# screen(2)
# par(mar=c(0.1,0.1,0,0))
# #plot(z[usr$y,], type="l", axes=F)
# #plot(z[,x], type="l", axes=F, ylim=c(-0.003,0.003))
# #plot(1:length(z[,1]), z[,x], type="l", axes=F, ylim=c(-0.003,0.003), xaxs="i", yaxs="i")
# plot(z[,y], axes=F, ylim=c(-40,20), xaxs="i", yaxs="i", type="l", lwd=2)
# axis(1)
# axis(2)
# abline(v=x, lwd=2, lty=2)
# #abline(v=y)
# box(lwd=2)
# screen(3)
# par(mar=c(0,0,0,0.05))
# plot(z[x,], 1:length(z[1,]),type="l", axes=F, xlim=c(20,-40), lwd=2, xaxs="i", yaxs="i")
# axis(4)
# axis(3)
# abline(h=y, lwd=2, lty=2)
# box(lwd=2)
# close.screen(all.screens = TRUE)
# 
# loc <- function(){
#   l <- locator(1)
#   l.x <- round(l$x*314,0)
#   l.y <- round(l$y*214,0)
#   val <- z[l.x, l.y]
#   print(val)
# }