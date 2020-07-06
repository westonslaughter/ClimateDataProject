#### FINAL RUN ####
## Author: Wes Slaughter
## Org: Sierra Streams Institute

library(plyr)
library(tidyverse)
library(vegan)
library(zoo)

setwd("C:/Users/Wes/Documents/SSI/Climate Data Project/FINALRUN")

# Bring in All Variable dataset


# Water Quality (WQ), Climate (CL), Bugs (BMI)
# Land cover (LC), and Veg Data (V)

DF.raw <- read.csv("AllVars.csv")
DF.raw <- DF.raw[,-1] %>% subset(WY < 2017)


##### 1: Setting up dataframes for PCAs #####
        
# Imputation (i) package 
# .i = suffix for 'imputed'
library(mice)


# Universal Metrics (Site, WY, Month)
uni <- c(1:3,125)
uni.i <- c(1:3,21)

              #### Bugs ####
#Bug counts
bmi.abund <- c(21:106)
#Bug summary info
bmi.info <- c(107:124)
# Bug only df
bmi.only <- na.omit(DF.raw[c(uni,bmi.abund,bmi.info)])
# Bug abundance only df
bmi.abun.only <- na.omit(DF.raw[c(uni,bmi.abund)])


              ### Imputation ###          (No bugs)
                        
                        DF.raw$EColi <- na_if(DF.raw$EColi,-Inf)
                        
                        Df.m <- DF.raw[,-c(21:106,107:124)]
              DF.i.raw <- parlmice(Df.m,
                                   m=5,
                                   cluster.seed=1)
              bwplot(DF.i.raw)
              DF.i <- complete(DF.i.raw)
              write.csv(DF.i,"AllVars_i.csv")

              #### Water Quality ####
              
# wq variables
wq <- c(4:13)
# wq only df
wq.only <- DF.raw[c(uni,wq)]
# wq imputed
wq.only.i <- DF.i[c(uni.i,wq)]


              #### Climate ####

# cl variables
cl <- c(14:17)
# cl only df
cl.only <- DF.raw[c(uni,cl)]
# cl only df imputed
cl.only.i <- DF.i[c(uni.i,cl)]
              
              #### Land Cover ####

# lc variables
lc <- c(18:20)
# lc only df                  
lc.only <- DF.raw[c(uni,lc)]
# lc only imputed 
lc.only.i <- DF.i[c(uni.i,lc)]


## Lag effects

# Bugs
library(data.table)





                  #### 2: PCA ####

          # Variance finder: which(apply(iris, 2, var) == 0)  

#### Overall PCA ####
master.i <- apply(DF.i[,-21],2,as.numeric,header=TRUE)
master.pc <- prcomp(master.i,center = TRUE, scale = TRUE)

# sink("Overall_nobmi_i_PC.txt")
# summary(master.pc)
# sink()

# Plot

plot(master.pc$x[,1],master.pc$x[,2], 
     xlab="PC1 (28.63%)", ylab = "PC2 (12.51%)", 
     main = "All Non-bmi Vars PCA \nPC1 / PC2")

#### Overall PCA (with Bugs) ####
master.i.b <- merge(bmi.only,DF.i,by=c("Site","WY","Month"))
master.i.b <- apply(master.i.b[-c(4,51,126)],2,as.numeric,header=TRUE)
master.pc.b <- prcomp(master.i.b,center = TRUE, scale = TRUE)

# LAG DATAFRAME #

# Create a 'lagged' dataframe, see if there are lag effects
non.bug <- as.data.table(master.i.b[,c(108:121)])
wqcl.lag <- as.data.frame(non.bug[, unlist(lapply(.SD, shift, n = 1:3), recursive = FALSE)])
bmi.match <- as.data.frame(master.i.b[,c(108:121)])

bmi.lag.df <- cbind(master.i.b,wqcl.lag)

# Report

# sink("allVariable_i_PC.txt")
# summary(master.pc.b)
# sink()

# Plot
plot(master.pc.b$x[,1],master.pc.b$x[,2], 
     xlab="PC1 (11.64%)", ylab = "PC2 (7.79%)",
     ylim = c(-25,10),
     main = "All Variables and Bugs PCA \nPC1 / PC2")

#### Bug PCA ####
bmi.only <- apply(bmi.only[,-4],2,as.numeric,header=TRUE)

bmi.pc <- prcomp(bmi.only[,-50],center = TRUE, scale = TRUE)

# Report

# sink("bmi_PC.txt")
# summary(bmi.pc)
# sink()

# Plot
plot(bmi.pc$x[,1],bmi.pc$x[,2], 
     xlab="PC1 (10.88%)", ylab = "PC2 (8.80%)", 
     ylim=c(-10,25),
     main = "Bug Only PCA \nPC1 / PC2")

#### Bug Info Only PCA ####
bmi.info.pc <- prcomp(bmi.only[,c(90:107)],center = TRUE, scale = TRUE)

#Report

# sink("bmi_info_PC.txt")
# summary(bmi.info.pc)
# sink()

# Plot

plot(bmi.info.pc$x[,1],bmi.info.pc$x[,2], 
     xlab="PC1 (40.86%)", ylab = "PC2 (19.40%)", 
     ylim=c(-10,25),
     main = "Bug Info PCA \nPC1 / PC2")

#### BMI abundance only PCA ####

bmi.abun.only <- apply(bmi.abun.only[,-4],2,as.numeric,header=TRUE)

bmi.abun.pc <- prcomp(bmi.abun.only[,-50],center = TRUE, scale = TRUE)

# Report 

# sink("bmi_abundance_PC.txt")
# summary(bmi.abun.pc)
# sink()

#   PC1      PC2
# 0.08988 0.0561

# Plot
plot(bmi.abun.pc$x[,1],bmi.abun.pc$x[,2], 
     xlab="PC1 (8.98%)", ylab = "PC2 (5.61%)", 
     ylim=c(-15,25),
     xlim=c(-15,25),
     main = "Bug Abundance PCA \nPC1 / PC2")

#### Bug Info Only PCA ####
bmi.info.pc <- prcomp(bmi.only[,c(90:107)],center = TRUE, scale = TRUE)

#Report

# sink("bmi_info_PC.txt")
# summary(bmi.info.pc)
# sink()

# Plot

plot(bmi.info.pc$x[,1],bmi.info.pc$x[,2], 
     xlab="PC1 (40.86%)", ylab = "PC2 (19.40%)", 
     ylim=c(-10,25),
     main = "Bug Info PCA \nPC1 / PC2")

##### Bugs with All Vars, & Var Lag Effects PCA ####

bmi.lag <- na.omit(apply(bmi.lag.df,2,as.numeric,header=TRUE))

bmi.lag.pc <- prcomp(bmi.lag,center = TRUE, scale = TRUE)
bmi.only.lag.pc <- prcomp(bmi.lag[,c(1:106)],center = TRUE, scale = TRUE)

#Report

 # sink("bmi_lag_PC.txt")
 # summary(bmi.lag.pc)
 # sink()

# Plot
plot(bmi.only.lag.pc$x[,1],bmi.only.lag.pc$x[,2], 
     xlab="PC1 (12.19%)", ylab = "PC2 (6.79%)", 
     ylim=c(-20,15),
     main = "Bug PCA w Vars and Lag Vars \nPC1 / PC2")


#### WQ PCA ####
wq.only.i <- apply(wq.only.i[-4],2,as.numeric,header=TRUE)

wq.i.pc <- prcomp(wq.only.i,center = TRUE, scale = TRUE)

#Report

# sink("wq_i_PC.txt")
# summary(wq.i.pc)
# sink()

#Plot

plot(wq.i.pc$x[,1],wq.i.pc$x[,2], 
     xlab="PC1 (25.72%)", ylab = "PC2 (13.18%)",
     main = "Water Quality (Imputed) Variables PCA \nPC1 / PC2")

#### Climate PCA ####
cl.only.i <- apply(cl.only.i[-4],2,as.numeric,header=TRUE)

cl.i.pc <- prcomp(cl.only.i,center = TRUE, scale = TRUE)

# Report

# sink("cl_i_PC.txt")
# summary(cl.i.pc)
# sink()

# Plot

plot(cl.i.pc$x[,1],cl.i.pc$x[,2], 
     xlab="PC1 (47.33%)", ylab = "PC2 (16.47%)", 
     main = "Climate Variables PCA \nPC1 / PC2")

                #### 3: Vectors ####

# For WQ,CL,LC vectors for BMI pca, need to do envfit of 
# seperate dataframes of those variable sets, because 
# bugs are only June and October.

# NOTE: color scheme default:
        # Cl: Red
        # WQ: Blue, Alt: #b19cd9 (light purple)
        # LC: Gray,
        # Bugs: Sea-green (#1fb78a)
        # Bug Info: Blue-Gray (#6699cc)

    #### Bug Vectors ####

lc.vec.bmi <- envfit(bmi.pc,master.i.b[,c(121:123)])
cl.vec.bmi <- envfit(bmi.pc,master.i.b[,c(117:120)])
wq.vec.bmi <- envfit(bmi.pc,master.i.b[,c(107:116)])    
info.b.vec.bmi <- envfit(bmi.pc,master.i.b[,c(89:106)])
bmi.vec.bmi <- envfit(bmi.pc, master.i.b[,c(5:88)])

plot(lc.vec.bmi, col="gray")
plot(cl.vec.bmi, col="red")
plot(wq.vec.bmi, col="#b19cd9") #b19cd9 <- purple, when light scheme needed for vis
plot(info.b.vec.bmi, col="#6699cc")

#
plot(bmi.vec.bmi, col="#1fb78a")

  #### Bug Lag Vectors ####

# lag0 (oiginal)
wq.vec.lag0.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(107:116)])
cl.lag0.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(117:120)])

#Lag1
cl.lag1.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(151,154,157,160)])
wq.lag1.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(124,127,130,133,136,139,142,145,148)])

#lag two
cl.lag2.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(152,155,158,161)])
wq.lag2.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(125,128,131,134,137,140,143,146,149)]) 

# Lag Three
cl.lag3.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(153,156,159,162)])
wq.lag3.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(126,129,132,135,138,141,144,147,150)]) 

#info
info.lag0.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(89:106)])

plot(cl.lag1.vec.bmi,col="#c51b8a")
plot(wq.lag1.vec.bmi, col="#00AFBB")


  #### WQ/CL/LC Vectors ####

wq.vec <- envfit(master.pc,master.i[,c(4:13)])
cl.vec <- envfit(master.pc,master.i[,c(14:17)])
lc.vec <- envfit(master.pc,master.i[,c(18:20)])


plot(wq.vec, col="blue")
plot(cl.vec, col="red")
plot(lc.vec, col="grey")

## Vector/Variable Summary: Corrplot, Reports ##
library(corrplot)

# Corr Plot of Overall PCA (no bugs)
corrplot(master.pc$rotation)
# Corr Plot of Bug Info PCA (Bug family PCA unreadable)
corrplot(bmi.info.pc$rotation)


# 
# sink("vectorReport.txt")
# print("Master Imputed Vectors")
# wq.vec
# cl.vec
# lc.vec
# print("BMI Vectors")
# wq.vec.bmi
# cl.vec.bmi
# lc.vec.bmi
# bmi.vec.bmi
# info.b.vec.bmi
# print("Lag effect Vectors")
# print("Lag Zero (original)")
# wq.vec.lag0.bmi
# cl.lag0.vec.bmi 
# 
# print("Lag One")
# cl.lag1.vec.bmi
# wq.lag1.vec.bmi
# 
# print("Lag Two")
# cl.lag2.vec.bmi 
# wq.lag2.vec.bmi
# 
# print("Lag Three")
# cl.lag3.vec.bmi 
# wq.lag3.vec.bmi 
# 
# print("End Report")
# sink()

#### 4: Visualizations ####
scl <- 3

#### Color Palletes ####
# Gradient Yellow-Purple (Site, n=18)
gcol <- read.csv("/Users/Wes/Documents/SSI/Climate Data Project/Climate Data Raw/Water Quality/ForR/SiteGradient.csv",header = FALSE)
grad.col <- as.character(as.factor(gcol$V1))

# Gradient Blues (WY, n=20)
wcol <-rev(c('#ff7b00',	'#f3760b',	'#e87117',	'#dd6d23',	'#d2682f',	'#c7643b',	'#bc5f47',	'#b15b53',	'#a6565f',	'#9b526b',	'#8f4d77',	'#844983',	'#79448f',	'#6e409b',	'#633ba7',	'#5837b3',	'#4d32bf',	'#422ecb',	'#3729d7',	'#2c25e3'))

#### Overviews, with Site, WY colorations ####

# Will be focusing now on the PCA of just WQ variables, overlain with all vecs
# This is so the observable relationships of explanatory or dependent vectors
# are not as influenced by their existence in PCA itself



########## Overview: Site, WY, Month Colored Points #############
# Below are options for 3 plots: Site/WY/Month, 
# so choose what you run accordingly

wq.only.i <- as.data.frame(wq.only.i)
# grad col, for months
grad.col.mo <- grad.col[c(16:18,1:6,14:16)]
mo.fac <- factor(unique(wq.only.i$Month), levels=c(1,2,3,4,5,6,7,8,9,10,11,12))

#Plot
plot(wq.i.pc$x[,1],wq.i.pc$x[,2], type="n",
     xlab="PC1 (25.72%)", ylab = "PC2 (13.18%)",
     main = "Water Quality (Imputed) Variables PCA \nPC1 / PC2")

# Origin lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")

#(1) Add points, color mapped to site
points(wq.i.pc$x[,1],wq.i.pc$x[,2], col = grad.col[wq.only.i$Site])

#(2) Add points color mapped to WY
wq.only.i$WY <- as.factor(wq.only.i$WY)
points(wq.i.pc$x[,1],wq.i.pc$x[,2], col = wcol[wq.only.i$WY])

#(3) Add points color mapped to month
points(wq.i.pc$x[,1],wq.i.pc$x[,2], col = grad.col.mo[wq.only.i$Month])

# Legend

#(1) Site Leg
legend("topright", legend = levels(as.factor(wq.only.i$Site)), pch=20,
       cex=.25, pt.cex = 2.2, col=grad.col[unique(wq.only.i$Site)], title="Water Quality Site")
#(2) WY Leg
legend("topright", legend = levels(as.factor(wq.only.i$WY)), pch=20,
       cex=.25, pt.cex = 2.2, col=wcol[unique(wq.only.i$WY)], title="Water Year")

#(3) Month legend
legend("topright", legend = levels(as.factor(wq.only.i$Month)), pch=20,
       cex=.35, pt.cex = 2.2, col=grad.col.mo[mo.fac], title="Month")


# Vecs
plot(wq.vec, col="blue")
plot(cl.vec, col="red")
plot(lc.vec,col="gray")

#plot(info.b.vec.bmi,col="#6699cc")

## QA/QC: text map, to check that vis is mapping correctly ##
#    text(wq.i.pc$x[,1],wq.i.pc$x[,2], display = "species", scaling = scl, cex = 0.8, col = "darkcyan")

 
 
######### Overview, All Data, Site/WY/Mo Color, Hulls ################
 
 
# PLOT 

plot(wq.i.pc$x[,1],wq.i.pc$x[,2], type="n",
      xlab="PC1 (25.72%)", ylab = "PC2 (13.18%)",
      main = "Water Quality (Imputed) Variables PCA \nPC1 / PC2")
 
 # Origin lines
 abline(v=0, lty=2, col="grey50")
 abline(h=0, lty=2, col="grey50")
 
 #Add Hulls, color mapped to site
 ordihull(wq.i.pc, 
          group=wq.only.i$Site,
          col = grad.col)
 
 #Add Hulls color mapped to WY
 ordihull(wq.i.pc,
          group=wq.only.i$WY,
          col = wcol)
 
 # Add hulls color mapped to month
 ordihull(wq.i.pc,
          group=wq.only.i$Month,
          col = grad.col.mo)
 
 # Legends
 
 # Site Leg
 legend("topright", legend = levels(as.factor(wq.only.i$Site)), pch=20,
        cex=.25, pt.cex = 2.2, col=grad.col[unique(wq.only.i$Site)], title="Water Quality Site")
 
 # WY Leg
 legend("topright", legend = levels(as.factor(wq.only.i$WY)), pch=20,
        cex=.25, pt.cex = 2.2, col=wcol[unique(wq.only.i$WY)], title="Water Quality Site")
 
 # Month Legend
 legend("topright", legend = levels(as.factor(wq.only.i$Month)), pch=20,
        cex=.25, pt.cex = 2.2, col=grad.col.mo[unique(wq.only.i$Month)], title="Water Quality Month")
 
 # Vecs
 plot(wq.vec, col="blue")
 plot(cl.vec, col="red")
 plot(lc.vec,col="gray")
 
 # plot(info.b.vec.bmi,col="#6699cc")
 
 ## QA/QC: text map, to check that vis is mapping correctly ##
 #  text(wq.i.pc$x[,1],wq.i.pc$x[,2], display = "species", scaling = scl, cex = 0.8, col = "darkcyan")

 
########## Overview, All Data, Site/WY/Mo Color, Centroids ##############
 
#### Ordicenter Function
 ordicenter <- function (ord, groups, display = "sites", w = weights(ord, display), 
                         show.groups, ...) 
 {
   weights.default <- function(object, ...) NULL
   pts <- scores(ord, display = display, ...)
   w <- eval(w)
   if (length(w) == 1) 
     w <- rep(1, nrow(pts))
   if (is.null(w)) 
     w <- rep(1, nrow(pts))
   if (!missing(show.groups)) 
   {
     take <- groups %in% show.groups
     pts <- pts[take, , drop = FALSE]
     groups <- groups[take]
     w <- w[take]
   }
   out <- seq(along = groups)
   inds <- names(table(groups))
   for (is in inds) 
   {
     gr <- out[groups == is]
     if (length(gr) > 1)
     {
       X <- pts[gr, ]
       W <- w[gr]
       ave <- apply(X, 2, weighted.mean, w = W)
       vegan:::ordiArgAbsorber(ave[1], ave[2], labels = is, FUN = text, ...)
     }
     if (length(gr) == 1)
     {
       X <- pts[gr, ]
       W <- w[gr]
       vegan:::ordiArgAbsorber(X[1], X[2], labels = is, FUN = text, ...)
     }
   }
   invisible()
 } 
 
####

 # PLOT 
 plot(wq.i.pc$x[,1],wq.i.pc$x[,2], type="n",
      xlab="PC1 (25.72%)", ylab = "PC2 (13.18%)",
      main = "Water Quality (Imputed) Variables PCA \nPC1 / PC2")
 
 # Origin lines
 abline(v=0, lty=2, col="grey50")
 abline(h=0, lty=2, col="grey50")
 
 # Vecs
 plot(wq.vec, col="blue")
 plot(cl.vec, col="red")
 plot(lc.vec,col="gray")
 
 # plot(info.b.vec.bmi,col="#6699cc")
 
 #Add Site Centroids
 
 ordicenter(wq.i.pc, 
            groups= wq.only.i$Site,
            cex=1.3)
 
 
 #Add WY Centroids 
 ordicenter(wq.i.pc, 
            groups = wq.only.i$WY,
            cex=1.4)
 
 
 # Add Month centroids
 ordicenter(wq.i.pc, 
            groups= wq.only.i$Month,
            cex=1.3)
 
 ## QA/QC: text map, to check that vis is mapping correctly ##
 #   text(wq.i.pc$x[,1],wq.i.pc$x[,2], display = "species", scaling = scl, cex = 0.8, col = "darkcyan")
 
 
################### HULLS: EACH WY BY ALL Sites ##################
library(ggpubr)
 
wq.only.i$SiteWY <- paste(wq.only.i$WY,wq.only.i$Site) 

# Water Year with All Sites, Ordihull
DecompHulls <- function(Year){
        plot(wq.i.pc$x[,1], wq.i.pc$x[,2],type="n",
            xlab="PC1 (32.18%)", ylab = "PC2 (12.88%)",
            ylim=c(-10,13),
            xlim=c(-12,12),
            main = paste("Water Quality and Climate\nwith Imputed Values",Year))
  
            # origin lines
            abline(v=0, lty=2, col="grey50")
            abline(h=0, lty=2, col="grey50")
            
  
            # Legend
            legend("topright", legend = levels(as.factor(wq.only.i$Site)), pch=20,
            cex=.25, pt.cex = 2.2, col=grad.col[unique(wq.only.i$Site)], title="Water Quality Site")
          
            # Vectors
            plot(wq.vec, col="blue")
            plot(cl.vec, col="red")
            plot(lc.vec,col="gray")
            # plot(info.b.vec.bmi,col="#6699cc")
            
          # Hulls 
          for (Place in wq.only.i$Site){
          ordihull(wq.i.pc,
              group = wq.only.i$SiteWY == paste(Year,Place),
              col=grad.col[Place])}
  
          # Outer hull remover (optional, and in poor taste)
          # ordihull(wq.i.pc,
          #          group =wq.only.i$SiteWY == "2001",
          #          col="white")
} 

# Run for all years
# FUTURE NOTE: should automate this w/ for loop
# tried, but wasted time
# ggarrange? would take along time, even if  functional (so far, isnt)

DecompHulls(2001)
DecompHulls(2002)
DecompHulls(2003)
DecompHulls(2004)
DecompHulls(2005)
DecompHulls(2006)
DecompHulls(2007)
DecompHulls(2008)
DecompHulls(2009)
DecompHulls(2010)
DecompHulls(2011)
DecompHulls(2012)
DecompHulls(2013)
DecompHulls(2014)
DecompHulls(2015)
DecompHulls(2016)
DecompHulls(2017)
DecompHulls(2018)
DecompHulls(2019)



################### HULLS: EACH SITE BY ALL WY ##################
# locX = site

# Function
DecompHullsSite <- function(locX){
  plot(wq.i.pc$x[,1], wq.i.pc$x[,2],type="n",
       xlab="PC1 (32.18%)", ylab = "PC2 (12.88%)",
       # ylim=c(-10,13),
       # xlim=c(-12,12),
       main = paste("Water Quality and Climate\n (i) All WYs, Site",locX))

  
  
  # origin lines
  abline(v=0, lty=2, col="grey50")
  abline(h=0, lty=2, col="grey50")
  
  # Hulls 
  # color is mapped....creatively
  
  for (yr in wq.only.i$WY){
    ordihull(wq.i.pc,
             group = wq.only.i$SiteWY == paste(yr,locX),
             col = wcol[as.numeric(#must be a number
                              str_remove(#removing leading 0s, "^0+"
                                    substr(# take the number for the year, works as index
                                        yr,3,4), "^0+"))]
    )}
  
  # Vectors
  plot(wq.vec, col="blue")
  plot(cl.vec, col="red")
  plot(lc.vec,col="gray")
  plot(info.b.vec.bmi,col="#6699cc")
  
  # Legend
  legend("topright", legend = levels(as.factor(wq.only.i$WY)), pch=20,
         cex=.25, pt.cex = 2.2, col=wcol[unique(wq.only.i$WY)], title="Water Quality Site")

  
 
} 

DecompHullsSite(1)
DecompHullsSite(2)
DecompHullsSite(3)
DecompHullsSite(4)
DecompHullsSite(5)
DecompHullsSite(6)
DecompHullsSite(7)
DecompHullsSite(8)
DecompHullsSite(9)
DecompHullsSite(10)
DecompHullsSite(11)
DecompHullsSite(12)
DecompHullsSite(13)
DecompHullsSite(14)
DecompHullsSite(15)
DecompHullsSite(16)
DecompHullsSite(17)
DecompHullsSite(18)


################### CENTROIDS: EACH WY BY ALL Sites ##################

DecompCenY <- function(Year){
  plot(wq.i.pc$x[,1], wq.i.pc$x[,2],type="n",
       xlab="PC1 (32.18%)", ylab = "PC2 (12.88%)",
       ylim=c(-10,13),
       xlim=c(-12,12),
       main = paste("Water Quality and Climate\nwith Imputed Values",Year))
  
  # origin lines
  abline(v=0, lty=2, col="grey50")
  abline(h=0, lty=2, col="grey50")
  
  
  # Legend
  legend("topright", legend = levels(as.factor(wq.only.i$Site)), pch=20,
         cex=.25, pt.cex = 2.2, col=grad.col[unique(wq.only.i$Site)], title="Water Quality Site")
  
  # Vectors
  plot(wq.vec, col="blue")
  plot(cl.vec, col="red")
  plot(lc.vec,col="gray")
  # plot(info.b.vec.bmi,col="#6699cc")
  
  # Hulls 
  for (Place in wq.only.i$Site){
    ordicenter(wq.i.pc,
             groups = wq.only.i$SiteWY == paste(Year,Place),
             col=grad.col[Place]
             )}
  
} 


DecompCenY(2001)
DecompCenY(2002)
DecompCenY(2003)
DecompCenY(2004)
DecompCenY(2005)
DecompCenY(2006)
DecompCenY(2007)
DecompCenY(2008)
DecompCenY(2009)
DecompCenY(2010)
DecompCenY(2011)
DecompCenY(2012)
DecompCenY(2013)
DecompCenY(2014)
DecompCenY(2015)
DecompCenY(2016)
DecompCenY(2017)
DecompCenY(2018)
DecompCenY(2019)


################### CENTROIDS: EACH SITE BY ALL WY ##################
library(stringr)

DecompCenSi <- function(Place){
  plot(wq.only.i$x[,1], wq.only.i$x[,2],type="n",
       xlab="PC1 (32.18%)", ylab = "PC2 (12.88%)",
       ylim=c(-10,13),
       xlim=c(-12,12),
       main = paste("Water Quality and Climate\nwith Imputed Site",Place))
  
  # origin lines
  abline(v=0, lty=2, col="grey50")
  abline(h=0, lty=2, col="grey50")
  
  
  # Legend
  legend("topright", legend = levels(as.factor(wq.only.i$WY)), pch=20,
         cex=.25, pt.cex = 2.2, col=wcol[unique(wq.only.i$WY)], title="Water Year")
  
  # Vectors
  plot(wq.vec, col="blue")
  plot(cl.vec, col="red")
  plot(lc.vec,col="gray")
  
  # Centroids
  # Reason that Site worked is because the sites also work as a color index,
  # 2001 means nothing to line 614
  
  # col is a wy index made by substr of WY, a cheap shot
  # 2001 -> 1, 2002 -> 2, so on
  
  for (yr in wq.only.i$WY){
    ordicenter(wq.i.pc,
               groups = wq.only.i$SiteWY == paste(yr,Place),
               col=wcol[as.numeric(str_remove(substr(yr,3,4), "^0+"))]
    )}
  
}


DecompCenSi(1)
DecompCenSi(2)
DecompCenSi(3)
DecompCenSi(4)
DecompCenSi(5)
DecompCenSi(6)
DecompCenSi(7)
DecompCenSi(8)
DecompCenSi(9)
DecompCenSi(10)
DecompCenSi(11)
DecompCenSi(12)
DecompCenSi(13)
DecompCenSi(14)
DecompCenSi(15)
DecompCenSi(16)
DecompCenSi(17)
DecompCenSi(18)




################### CENTROIDS: EACH WY BY ALL Sites ##################

DecompCenY <- function(Year){
  plot(wq.i.pc$x[,1], wq.i.pc$x[,2],type="n",
       xlab="PC1 (32.18%)", ylab = "PC2 (12.88%)",
       ylim=c(-10,13),
       xlim=c(-12,12),
       main = paste("Water Quality and Climate\nwith Imputed Values",Year))
  
  # origin lines
  abline(v=0, lty=2, col="grey50")
  abline(h=0, lty=2, col="grey50")
  
  
  # Legend
  legend("topright", legend = levels(as.factor(wq.only.i$Site)), pch=20,
         cex=.25, pt.cex = 2.2, col=grad.col[unique(wq.only.i$Site)], title="Water Quality Site")
  
  # Vectors
  plot(wq.vec, col="blue")
  plot(cl.vec, col="red")
  plot(lc.vec,col="gray")
  plot(info.b.vec.bmi,col="#6699cc")
  
  # Hulls 
  for (Place in wq.only.i$Site){
    ordicenter(wq.i.pc,
               groups = wq.only.i$SiteWY == paste(Year,Place),
               col=grad.col[Place]
    )}
  
} 


DecompCenY(2001)
DecompCenY(2002)
DecompCenY(2003)
DecompCenY(2004)
DecompCenY(2005)
DecompCenY(2006)
DecompCenY(2007)
DecompCenY(2008)
DecompCenY(2009)
DecompCenY(2010)
DecompCenY(2011)
DecompCenY(2012)
DecompCenY(2013)
DecompCenY(2014)
DecompCenY(2015)
DecompCenY(2016)
DecompCenY(2017)
DecompCenY(2018)
DecompCenY(2019)





##### BMI Dataset, Lag Effects ######
bugmask <- as.character(c(rep("#FFFFFF00",15)))
bmi.wcol = wcol[1:15]

# Plot
plot(bmi.only.lag.pc$x[,1],bmi.only.lag.pc$x[,2],type="n",
     xlab="PC1 (10.99%)", ylab = "PC2 (8.89%)", 
     ylim=c(-20,10),
     main = "Bug PCA w Vars and Lag Vars \nPC1 / PC2")

points(bmi.lag.pc$x[,1],bmi.lag.pc$x[,2], display = "sites",
                           col=bmi.wcol[bmiWY])

plot(wq.vec.lag0.bmi ,col='blue')
plot(cl.lag0.vec.bmi,col='red')

plot(cl.lag1.vec.bmi,col="#c51b8a")
plot(wq.lag1.vec.bmi, col="#00AFBB")

plot(info.lag0.vec.bmi, col="darkgrey")





############### BMI Abundance Dataset: Year Masking  ####################################
bmi.abun.only <- as.data.frame(bmi.abun.only)
bmi.abun.only$SiteWY <- paste(bmi.abun.only$WY,bmi.abun.only$Site)

# wcol.bmi<- wcol[1:15]

# 2014

bmi.mask14 <-c(rep("#FFFFFF00",13),"#274C77","#FFFFFF00")
bmiWY <- as.factor(bmi.abun.only$WY)

plot(bmi.abun.pc$x[,1],bmi.abun.pc$x[,2],type="n",
     xlab="PC1 (8%)", ylab = "PC2 (5%)",
     ylim = c(-20,25),
     xlim=c(-8,30),
     main = "BMI Dataset with Imputed WQ and Climate Vectors\n 2014 'Mask'")

# Text TEST
# text(bmi.abun.pc$x[,1],bmi.abun.pc$x[,2], display = "sites", scaling = scl, cex = 0.8, col=bmi.mask[bmiWY],bg = bmi.mask[bmiWY],)

# Points
with(bmi.abun.only, points(bmi.abun.pc$x[,1],bmi.abun.pc$x[,2], display = "sites",
                           col = bmi.mask14[bmiWY]))


# Potential site shape code sketch below

# SiteLeg <- as.factor(bmi.wqcut.rel[,-c(83)]$Site)
# 
# legend("topright",legend=levels(bmiSite),
#        pch = unique(bmi.wqcut.rel$Site),
#        cex=.33,pt.cex=.8)

#### BMI WY Plots ####
bugmask <- as.character(c(rep("#FFFFFF00",15)))

BMIyr <- function (yr){
  Bcol <- replace(bugmask,yr,"#274C77")
  
  plot(bmi.abun.pc$x[,1],bmi.abun.pc$x[,2],type="n",
       xlab="PC1 (8%)", ylab = "PC2 (5%)",
       ylim = c(-20,25),
       xlim=c(-8,30),
       main = paste("BMI Dataset with Imputed WQ and Climate Vectors\n","'",yr)) 
  
  
  plot(wq.vec.bmi, col="#90008080")
  plot(cl.vec.bmi, col="#900000FF")
  plot(lc.vec.bmi,col="#909400D3")
  plot(info.b.vec.bmi,col="darkgray")
  
  with(bmi.abun.only, points(bmi.abun.pc$x[,1],bmi.abun.pc$x[,2], display = "sites",
                             col = Bcol[bmiWY]))

  
  plot()
}



BMIyr(01)
BMIyr(02)
BMIyr(03)
BMIyr(04)
BMIyr(05)
BMIyr(06)
BMIyr(07)
BMIyr(08)
BMIyr(09)
BMIyr(10)
BMIyr(11)
BMIyr(12)
BMIyr(13)
BMIyr(14)
BMIyr(15)

# Month Masking BMI
bugmask.mo <- bugmask[c(1:12)]

BMImo <- function (mo){
  Bcol <- replace(bugmask.mo,mo,"#274C77")
  
  plot(bmi.abun.pc$x[,1],bmi.abun.pc$x[,2],type="n",
       xlab="PC1 (8%)", ylab = "PC2 (5%)",
       ylim = c(-20,25),
       xlim=c(-8,30),
       main = paste("BMI Dataset with Imputed WQ and Climate Vectors\n","Month:",mo)) 
  
  
  plot(wq.vec.bmi, col="#90008080")
  plot(cl.vec.bmi, col="#900000FF")
  plot(lc.vec.bmi,col="#909400D3")
  plot(info.b.vec.bmi,col="#80D3D3D3")
  
  with(bmi.abun.only, points(bmi.abun.pc$x[,1],bmi.abun.pc$x[,2], display = "sites",
                             col = Bcol[unique(bmi.abun.only$Month)]))
  
  
  plot()
}



BMImo(06)
BMImo(10)
BMImo(11)
BMImo(12)


################## APPENDIX : Z-Scores Plots #########################

library(Rmisc)
 
#Calculate z scores
 DF.i.z <- as.data.frame(apply(DF.i[,c(4:20)], 2, scale)) %>%
 cbind(DF.i[,c(1:3)])
 
# No 2020, because data incomplete- maybe later.
 
 DF.i.z <- subset(DF.i.z[,c(18:20,1:17)],
                  WY < 2020)
 
 DF.i.m <- ddply(DF.i.z, c("WY"), summarize,
                            # Water Quality
                            mPO4 = mean(PO4, na.rm = TRUE), 
                            uPO4 = CI(PO4)[1],              
                            lPO4 = CI(PO4)[3],
                            mNO3 = mean(NO3, na.rm = TRUE),
                            uNO3 = CI(NO3)[1],
                            lNO3 = CI(NO3)[3],
                            mAir = mean(Air, na.rm = TRUE),
                            uAir = CI(Air)[1],
                            lAir = CI(Air)[3],
                            mpH = mean(pH, na.rm = TRUE),
                            upH = CI(pH)[1],
                            lpH = CI(pH)[3],
                            mCond = mean(Cond, na.rm = TRUE),
                            uCond = CI(Cond)[1],
                            lCond = CI(Cond)[3],
                            mTur = mean(Tur, na.rm = TRUE),
                            uTur = CI(Tur)[1],
                            lTur = CI(Tur)[3],
                            mO2 = mean(O2, na.rm = TRUE),
                            uO2 = CI(O2)[1],
                            lO2 = CI(O2)[3],
                            mH2Otemp = mean(H2Otemp, na.rm = TRUE),
                            uH2Otemp = CI(H2Otemp)[1],
                            lH2Otemp = CI(H2Otemp)[3],
                            mTotalColiform = mean(TotalColiform, na.rm = TRUE),
                            uTotalColiform = CI(TotalColiform)[1],
                            lTotalColiform = CI(TotalColiform)[3],
                            mEColi = mean(EColi, na.rm = TRUE),
                            uEColi = CI(EColi)[1],
                            lEColi = CI(EColi)[3],
                 
                            # Climate
                            mtmn = mean(tmn, na.rm = TRUE),
                            utmn = CI(tmn)[1],
                            ltmn = CI(tmn)[3],
                            mppt = mean(ppt, na.rm = TRUE),
                            uppt= CI(ppt)[1],
                            lppt = CI(ppt)[3],
                            mcwd = mean(cwd, na.rm = TRUE),
                            ucwd = CI(cwd)[1],
                            lcwd = CI(cwd)[3],
                            mtmx = mean(tmx, na.rm = TRUE),
                            utmx = CI(tmx)[1],
                            ltmx = CI(tmx)[3],
                 
                            # Land Cover
                            mNDVI = mean(NDVI, na.rm = TRUE),
                            uNDVI = CI(NDVI)[1],
                            lNDVI = CI(NDVI)[3],
                            mPctUrb = mean(PctUrb, na.rm = TRUE),
                            uPctUrb = CI(PctUrb)[1],
                            lPctUrb = CI(PctUrb)[3],
                            mPctImp = mean(PctImp, na.rm = TRUE),
                            uPctImp = CI(PctImp)[1],
                            lPctImp = CI(PctImp)[3],
                           )

 # Nitrate
 
 # Basic Plot
 NO3det.effplot <- ggplot(DF.i.m, aes(x = WY, y = mNO3)) #mNO3 = your target resposne variablesof choice
 NO3det.effplot 
 
 # Layer on mean value lines, with CI ribbons
 NO3eff <- NO3det.effplot + 
    geom_line(aes(y = mNO3), col="red", size = 1.1) + 
    geom_ribbon(aes(ymin = lNO3, ymax = uNO3, col="NO3"), alpha = 0.2)+
    
    geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
    geom_ribbon(aes(ymin = (lppt), ymax = (uppt), col="PPT"), alpha = 0.2) +
    
    geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
    geom_ribbon(aes(ymin = lNDVI, ymax = uNDVI, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
    
    geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
    geom_ribbon(aes(ymin = ltmx, ymax = utmx, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
    
    geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
    geom_ribbon(aes(ymin = ltmn, ymax = utmn, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
    
    geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
    geom_ribbon(aes(ymin = (lcwd), ymax = (ucwd),col="CWD"), fill = "slateblue2", alpha = 0.2) +
    
    # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
    # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
    
    
    ylab("Nitrate and Climate Metric z-scores")+
    scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                                "slateblue2"),
                       labels=c("NO3","PPT","NDVI","TMX","TMN","CWD"),
                       breaks=c("NO3","PPT","NDVI","TMX","TMN","CWD"),
                       guide = guide_legend(),
                       name="Variable")+
   scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
    
    theme(
       panel.grid = element_blank(),
       axis.text = element_text(size = 14, colour = "black"),
       axis.title = element_text(size = 16, colour = "black"),
    )
 
 NO3eff
 
 # Phosphate
  
  # Basic Plot
  PO4det.effplot <- ggplot(DF.i.m, aes(x = WY, y = mPO4)) #mNO3 = your target resposne variablesof choice
  PO4det.effplot 
  
  # Layer on mean value lines, with CI ribbons
  PO4eff <- PO4det.effplot + 
     geom_line(aes(y = mPO4), col="red", size = 1.1) + 
     geom_ribbon(aes(ymin = lPO4, ymax = uPO4, col="PO4"), alpha = 0.2)+
     
     geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
     geom_ribbon(aes(ymin = (lppt), ymax = (uppt), col="PPT"), alpha = 0.2) +
     
     geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
     geom_ribbon(aes(ymin = lNDVI, ymax = uNDVI, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
     
     geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmx, ymax = utmx, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
     
     geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmn, ymax = utmn, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
     
     geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
     geom_ribbon(aes(ymin = (lcwd), ymax = (ucwd),col="CWD"), fill = "slateblue2", alpha = 0.2) +
     
     # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
     # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
     
     
     ylab("Phosphates and Climate Metric z-scores")+
     scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                                 "slateblue2"),
                        labels=c("PO4","PPT","NDVI","TMX","TMN","CWD"),
                        breaks=c("PO4","PPT","NDVI","TMX","TMN","CWD"),
                        guide = guide_legend(),
                        name="Variable")+
    scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
     
     theme(
        # panel.grid = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
     )
  
  PO4eff

 # H20 Temp
  
  # Basic Plot
  Tempdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mH2Otemp)) #mNO3 = your target resposne variablesof choice
  Tempdet.effplot 
  
  # Layer on mean value lines, with CI ribbons
  Tempeff <- Tempdet.effplot + 
     geom_line(aes(y = mH2Otemp), col="red", size = 1.1) + 
     geom_ribbon(aes(ymin = lH2Otemp, ymax = uH2Otemp, col="H2O"), alpha = 0.2)+
     
     geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
     geom_ribbon(aes(ymin = (lppt), ymax = (uppt), col="PPT"), alpha = 0.2) +
     
     geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
     geom_ribbon(aes(ymin = lNDVI, ymax = uNDVI, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
     
     geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmx, ymax = utmx, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
     
     geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmn, ymax = utmn, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
     
     geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
     geom_ribbon(aes(ymin = (lcwd), ymax = (ucwd),col="CWD"), fill = "slateblue2", alpha = 0.2) +
     
     # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
     # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
     
     
     ylab("H2O Temperature and Climate Metric z-scores")+
     scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                                 "slateblue2"),
                        labels=c("H2O","PPT","NDVI","TMX","TMN","CWD"),
                        breaks=c("H2O","PPT","NDVI","TMX","TMN","CWD"),
                        guide = guide_legend(),
                        name="Variable")+
    scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
     
     theme(
        # panel.grid = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
     )
  
  Tempeff
  
  # pH
  
  
  # Basic Plot
  pHdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mpH)) #mNO3 = your target resposne variablesof choice
  pHdet.effplot 
  
  # Layer on mean value lines, with CI ribbons
  pHeff <- pHdet.effplot + 
     geom_line(aes(y = mpH), col="red", size = 1.1) + 
     geom_ribbon(aes(ymin = lpH, ymax = upH, col="pH"), alpha = 0.2)+
     
     geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
     geom_ribbon(aes(ymin = (lppt), ymax = (uppt), col="PPT"), alpha = 0.2) +
     
     geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
     geom_ribbon(aes(ymin = lNDVI, ymax = uNDVI, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
     
     geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmx, ymax = utmx, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
     
     geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmn, ymax = utmn, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
     
     geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
     geom_ribbon(aes(ymin = (lcwd), ymax = (ucwd),col="CWD"), fill = "slateblue2", alpha = 0.2) +
     
     # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
     # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
     
     
     ylab("pH and Climate Metric z-scores")+
     scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                                 "slateblue2"),
                        labels=c("pH","PPT","NDVI","TMX","TMN","CWD"),
                        breaks=c("pH","PPT","NDVI","TMX","TMN","CWD"),
                        guide = guide_legend(),
                        name="Variable")+
    scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
     
     theme(
        # panel.grid = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
     )
  
  pHeff
  
# Conductivity
  
  
  # Basic Plot
  Conddet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mCond)) #mNO3 = your target resposne variablesof choice
  Conddet.effplot 
  
  # Layer on mean value lines, with CI ribbons
  Condeff <- Conddet.effplot + 
     geom_line(aes(y = mCond), col="red", size = 1.1) + 
     geom_ribbon(aes(ymin = lCond, ymax = uCond, col="Cond"), alpha = 0.2)+
     
     geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
     geom_ribbon(aes(ymin = (lppt), ymax = (uppt), col="PPT"), alpha = 0.2) +
     
     geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
     geom_ribbon(aes(ymin = lNDVI, ymax = uNDVI, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
     
     geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmx, ymax = utmx, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
     
     geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmn, ymax = utmn, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
     
     geom_line(aes(y = -(mcwd)),col="slateblue2", size = 1.1) +
     geom_ribbon(aes(ymin = -(lcwd), ymax = -(ucwd),col="CWD"), fill = "slateblue2", alpha = 0.2) +
     
     # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
     # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
     

      ylab("Conductivity and Climate Metric z-scores")+
      scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                                  "slateblue2","black"),
                         labels=c("Cond","PPT","NDVI","TMX","TMN","CWD","PctUrb"),
                         breaks=c("Cond","PPT","NDVI","TMX","TMN","CWD","PctUrb"),
                         guide = guide_legend(),
                         name="Variable")+
    scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
     
     theme(
        # panel.grid = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
     )
  
  Condeff

 # O2
  
  
  # Basic Plot
  O2det.effplot <- ggplot(DF.i.m, aes(x = WY, y = mO2)) #mNO3 = your target resposne variablesof choice
  O2det.effplot 
  
  # Layer on mean value lines, with CI ribbons
  O2eff <- O2det.effplot + 
     geom_line(aes(y = mO2), col="red", size = 1.1) + 
     geom_ribbon(aes(ymin = lO2, ymax = uO2, col="O2"), alpha = 0.2)+
     
     geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
     geom_ribbon(aes(ymin = (lppt), ymax = (uppt), col="PPT"), alpha = 0.2) +
     
     geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
     geom_ribbon(aes(ymin = lNDVI, ymax = uNDVI, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
     
     geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmx, ymax = utmx, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
     
     geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmn, ymax = utmn, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
     
     geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
     geom_ribbon(aes(ymin = (lcwd), ymax = (ucwd),col="CWD"), fill = "slateblue2", alpha = 0.2) +
     
     # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
     # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
     
     
     ylab("Dissolved O2 and Climate Metric z-scores")+
     scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                                 "slateblue2"),
                        labels=c("O2","PPT","NDVI","TMX","TMN","CWD"),
                        breaks=c("O2","PPT","NDVI","TMX","TMN","CWD"),
                        guide = guide_legend(),
                        name="Variable")+
    scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
     
     theme(
        # panel.grid = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
     )
  
  O2eff
  

# E Coli
  
  
  # Basic Plot
  ECdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mEColi)) #mNO3 = your target resposne variablesof choice
  ECdet.effplot 
  
  # Layer on mean value lines, with CI ribbons
  ECeff <- ECdet.effplot + 
     geom_line(aes(y = mEColi), col="red", size = 1.1) + 
     geom_ribbon(aes(ymin = lEColi, ymax = uEColi, col="E. Coli"), alpha = 0.2)+
     
     geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
     geom_ribbon(aes(ymin = (lppt), ymax = (uppt), col="PPT"), alpha = 0.2) +
     
     geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
     geom_ribbon(aes(ymin = lNDVI, ymax = uNDVI, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
     
     geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmx, ymax = utmx, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
     
     geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmn, ymax = utmn, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
     
     geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
     geom_ribbon(aes(ymin = (lcwd), ymax = (ucwd),col="CWD"), fill = "slateblue2", alpha = 0.2) +
     
     # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
     # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
     
     
     ylab("E. Coli and Climate Metric z-scores")+
     scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                                 "slateblue2"),
                        labels=c("E. Coli","PPT","NDVI","TMX","TMN","CWD"),
                        breaks=c("E. Coli","PPT","NDVI","TMX","TMN","CWD"),
                        guide = guide_legend(),
                        name="Variable")+
    scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
     
     theme(
        # panel.grid = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
     )
  
  ECeff
  

# Total Coliform
  
  # Basic Plot
  TCdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mTotalColiform)) #mNO3 = your target resposne variablesof choice
  TCdet.effplot 
  
  # Layer on mean value lines, with CI ribbons
  TCeff <- TCdet.effplot + 
     geom_line(aes(y = mTotalColiform), col="red", size = 1.1) + 
     geom_ribbon(aes(ymin = lTotalColiform, ymax = uTotalColiform, col="TotalColiform"), alpha = 0.2)+
     
     geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
     geom_ribbon(aes(ymin = (lppt), ymax = (uppt), col="PPT"), alpha = 0.2) +
     
     geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
     geom_ribbon(aes(ymin = lNDVI, ymax = uNDVI, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
     
     geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmx, ymax = utmx, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
     
     geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmn, ymax = utmn, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
     
     geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
     geom_ribbon(aes(ymin = (lcwd), ymax = (ucwd),col="CWD"), fill = "slateblue2", alpha = 0.2) +
     
     # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
     # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
     
     
     ylab("Total Coliform and Climate Metric z-scores")+
     scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                                 "slateblue2"),
                        labels=c("TotalColiform","PPT","NDVI","TMX","TMN","CWD"),
                        breaks=c("TotalColiform","PPT","NDVI","TMX","TMN","CWD"),
                        guide = guide_legend(),
                        name="Variable")+
    scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
     
     theme(
        # panel.grid = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
     )
  
  TCeff
  
  
  # Air Temp
  
  # Basic Plot
  Airdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mAir)) #mNO3 = your target resposne variablesof choice
  Airdet.effplot 
  
  # Layer on mean value lines, with CI ribbons
  Aireff <- Airdet.effplot + 
     geom_line(aes(y = mAir), col="red", size = 1.1) + 
     geom_ribbon(aes(ymin = lAir, ymax = uAir, col="Air"), alpha = 0.2)+
     
     geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
     geom_ribbon(aes(ymin = (lppt), ymax = (uppt), col="PPT"), alpha = 0.2) +
     
     geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
     geom_ribbon(aes(ymin = lNDVI, ymax = uNDVI, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
     
     geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmx, ymax = utmx, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
     
     geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
     geom_ribbon(aes(ymin = ltmn, ymax = utmn, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
     
     geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
     geom_ribbon(aes(ymin = (lcwd), ymax = (ucwd),col="CWD"), fill = "slateblue2", alpha = 0.2) +
     
     # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
     # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
     
     
     ylab("Air and Climate Metric z-scores")+
     scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                                 "slateblue2"),
                        labels=c("Air","PPT","NDVI","TMX","TMN","CWD"),
                        breaks=c("Air","PPT","NDVI","TMX","TMN","CWD"),
                        guide = guide_legend(),
                        name="Variable")+
    scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
     
     theme(
        # panel.grid = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
     )
  
  Aireff

library(ggpubr)     

ZPlots <- ggarrange(PO4eff,NO3eff,O2eff,ECeff,TCeff,
                    Condeff,pHeff,Tempeff,Aireff,nrow = 1,ncol = 1) 

  ggexport(ZPlots,"ZPlots.pdf")


#### Z Plots, no CI Ribbon (CLEAN) ####

# Nitrate

# Basic Plot
NO3det.effplot <- ggplot(DF.i.m, aes(x = WY, y = mNO3)) #mNO3 = your target resposne variablesof choice
NO3det.effplot 

# Layer on mean value lines, with CI ribbons
NO3eff <- NO3det.effplot + 
  geom_line(aes(y = mNO3), col="red", size = 1.1) + 
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NO3"), fill="#00000000", alpha = 0.0)+
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), fill= "#00000000", alpha = 0.0) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.0) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.0) +
  
  geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMN"), fill = "goldenrod3", alpha = 0.0) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  
  
  ylab("Nitrate and Climate Metric z-scores")+
  scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2"),
                     labels=c("NO3","PPT","NDVI","TMX","TMN","CWD"),
                     breaks=c("NO3","PPT","NDVI","TMX","TMN","CWD"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
  )

NO3eff

# Phosphate

# Basic Plot
PO4det.effplot <- ggplot(DF.i.m, aes(x = WY, y = mPO4)) #mNO3 = your target resposne variablesof choice
PO4det.effplot 

# Layer on mean value lines, with CI ribbons
PO4eff <- PO4det.effplot + 
  geom_line(aes(y = mPO4), col="red", size = 1.1) + 
  geom_ribbon(aes(ymin = 0, ymax = 0, col="PO4"), alpha = 0.2)+
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), alpha = 0.2) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
  
  geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0),col="CWD"), fill = "slateblue2", alpha = 0.2) +
  
  # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
  # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
  
  
  ylab("Phosphates and Climate Metric z-scores")+
  scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2"),
                     labels=c("PO4","PPT","NDVI","TMX","TMN","CWD"),
                     breaks=c("PO4","PPT","NDVI","TMX","TMN","CWD"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
  )

PO4eff

# H20 Temp

# Basic Plot
Tempdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mH2Otemp)) #mNO3 = your target resposne variablesof choice
Tempdet.effplot 

# Layer on mean value lines, with CI ribbons
Tempeff <- Tempdet.effplot + 
  geom_line(aes(y = mH2Otemp), col="red", size = 1.1) + 
  geom_ribbon(aes(ymin = 0, ymax = 0, col="H2O"), alpha = 0.2)+
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), alpha = 0.2) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
  
  geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0),col="CWD"), fill = "slateblue2", alpha = 0.2) +
  
  # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
  # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
  
  
  ylab("H2O Temperature and Climate Metric z-scores")+
  scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2"),
                     labels=c("H2O","PPT","NDVI","TMX","TMN","CWD"),
                     breaks=c("H2O","PPT","NDVI","TMX","TMN","CWD"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
  )

Tempeff

# pH


# Basic Plot
pHdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mpH)) #mNO3 = your target resposne variablesof choice
pHdet.effplot 

# Layer on mean value lines, with CI ribbons
pHeff <- pHdet.effplot + 
  geom_line(aes(y = mpH), col="red", size = 1.1) + 
  geom_ribbon(aes(ymin = 0, ymax = 0, col="pH"), alpha = 0.2)+
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), alpha = 0.2) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
  
  geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0),col="CWD"), fill = "slateblue2", alpha = 0.2) +
  
  # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
  # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
  
  
  ylab("pH and Climate Metric z-scores")+
  scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2"),
                     labels=c("pH","PPT","NDVI","TMX","TMN","CWD"),
                     breaks=c("pH","PPT","NDVI","TMX","TMN","CWD"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
  )

pHeff

# Conductivity


# Basic Plot
Conddet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mCond)) #mNO3 = your target resposne variablesof choice
Conddet.effplot 

# Layer on mean value lines, with CI ribbons
Condeff <- Conddet.effplot + 
  geom_line(aes(y = mCond), col="red", size = 1.1) + 
  geom_ribbon(aes(ymin = 0, ymax = 0, col="Cond"), alpha = 0.2)+
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), alpha = 0.2) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
  
  geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0),col="CWD"), fill = "slateblue2", alpha = 0.2) +
  
  # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
  # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
  
  
  ylab("Conductivity and Climate Metric z-scores")+
  scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2","black"),
                     labels=c("Cond","PPT","NDVI","TMX","TMN","CWD","PctUrb"),
                     breaks=c("Cond","PPT","NDVI","TMX","TMN","CWD","PctUrb"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
  )

Condeff

# O2
# Basic Plot
O2det.effplotA <- ggplot(DF.i.m, aes(x = WY, y = mO2)) #mNO3 = your target resposne variablesof choice
O2det.effplotA 

# Layer on mean value lines, with CI ribbons
O2eff <- O2det.effplotA + 
  geom_line(aes(y = mO2), col="red", size = 1.1) + 
  geom_ribbon(aes(ymin = 0, ymax = 0, col="O2"), alpha = 0.2)+
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), alpha = 0.2) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
  
  geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0),col="CWD"), fill = "slateblue2", alpha = 0.2) +
  
  # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
  # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
  
  
  ylab("Dissolved O2 and Climate Metric z-scores")+
  scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2"),
                     labels=c("Cond","PPT","NDVI","TMX","TMN","CWD"),
                     breaks=c("Cond","PPT","NDVI","TMX","TMN","CWD"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
  )

O2eff


# E Coli


# Basic Plot
ECdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mEColi)) #mNO3 = your target resposne variablesof choice
ECdet.effplot 

# Layer on mean value lines, with CI ribbons
ECeff <- ECdet.effplot + 
  geom_line(aes(y = mEColi), col="red", size = 1.1) + 
  geom_ribbon(aes(ymin = 0, ymax = 0, col="E. Coli"), alpha = 0.2)+
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), alpha = 0.2) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
  
  geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0),col="CWD"), fill = "slateblue2", alpha = 0.2) +
  
  # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
  # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
  
  
  ylab("E. Coli and Climate Metric z-scores")+
  scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2"),
                     labels=c("E. Coli","PPT","NDVI","TMX","TMN","CWD"),
                     breaks=c("E. Coli","PPT","NDVI","TMX","TMN","CWD"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
  )

ECeff


# Total Coliform

# Basic Plot
TCdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mTotalColiform)) #mNO3 = your target resposne variablesof choice
TCdet.effplot 

# Layer on mean value lines, with CI ribbons
TCeff <- TCdet.effplot + 
  geom_line(aes(y = mTotalColiform), col="red", size = 1.1) + 
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TotalColiform"), alpha = 0.2)+
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), alpha = 0.2) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
  
  geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0),col="CWD"), fill = "slateblue2", alpha = 0.2) +
  
  # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
  # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
  
  
  ylab("Total Coliform and Climate Metric z-scores")+
  scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2"),
                     labels=c("TotalColiform","PPT","NDVI","TMX","TMN","CWD"),
                     breaks=c("TotalColiform","PPT","NDVI","TMX","TMN","CWD"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
  )

TCeff


# Air Temp

# Basic Plot
Airdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mAir)) #mNO3 = your target resposne variablesof choice
Airdet.effplot 

# Layer on mean value lines, with CI ribbons
Aireff <- Airdet.effplot + 
  geom_line(aes(y = mAir), col="red", size = 1.1) + 
  geom_ribbon(aes(ymin = 0, ymax = 0, col="Air"), alpha = 0.2)+
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), alpha = 0.2) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
  
  geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0),col="CWD"), fill = "slateblue2", alpha = 0.2) +
  
  # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
  # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
  
  
  ylab("Air and Climate Metric z-scores")+
  scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2"),
                     labels=c("Air","PPT","NDVI","TMX","TMN","CWD"),
                     breaks=c("Air","PPT","NDVI","TMX","TMN","CWD"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
  )

Aireff

library(ggpubr)     
ZPlots <- ggarrange(PO4eff,NO3eff,O2eff,ECeff,TCeff,Condeff,pHeff,Tempeff,Aireff,nrow = 1,ncol = 1)  
ggexport(ZPlots,"ZPlots.pdf")

# Just Climate Vars #

# Air Temp

# Basic Plot
Climdet.effplot <- ggplot(DF.i.m, aes(x = WY, y = mppt), fill="#00000000") 
Climdet.effplot 

# Layer on mean value lines, with CI ribbons
CLIMeff <- Climdet.effplot + 
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), alpha = 0.2) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
  
  geom_line(aes(y = mtmn),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMN"), fill = "goldenrod3", alpha = 0.2) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0),col="CWD"), fill = "slateblue2", alpha = 0.2) +
  
  # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
  # geom_ribbon(aes(ymin = -(lPctUrb), ymax = -(uPctUrb), col="PctUrb"), fill = "black", alpha = 0.2) +
  
  
  ylab("Air and Climate Metric z-scores")+
  scale_color_manual(values=c("blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2"),
                     labels=c("PPT","NDVI","TMX","TMN","CWD"),
                     breaks=c("PPT","NDVI","TMX","TMN","CWD"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
  )

CLIMeff+ggtitle("Climate and Land Cover Variables")+
  theme(plot.title = element_text(hjust = 0.5))
