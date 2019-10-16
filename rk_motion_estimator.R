#####################################################################
## estimate absolute motion out of fMRI realignment parameter file ##
## by rkessler1990@gmail.com ########################################
## version 0.1 ######################################################
#####################################################################
library(RColorBrewer)

data_folder <- 'D:/motionEstimator/data/hildesheim_debus/Adults/'

file_list <-list.files(path = data_folder, pattern = '*_*', all.files = FALSE,
                  full.names = FALSE, recursive = FALSE,
                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

mean.sts = numeric()
median.sts = numeric()
min.sts = numeric()
max.sts = numeric()
single.sts = numeric()

for (i in 1:length(file_list)){
  data = read.csv(paste(data_folder,file_list[i], collapse = "", sep = ""), sep = '', header = FALSE, col.names = c('x','y','z','pitch','roll','yaw'))
  
  # calculate differences between successive x y and z values
  diff.x     <- diff(data$x)
  diff.y     <- diff(data$y)
  diff.z     <- diff(data$z)
  diff.pitch <- diff(data$pitch)
  diff.roll  <- diff(data$roll)
  diff.yaw   <- diff(data$yaw)
  
  # summate translation and rotation
  diff.x_ges      <- diff.x + diff.pitch * 65 * pi / 180
  diff.y_ges      <- diff.y + diff.roll  * 65 * pi / 180
  diff.z_ges      <- diff.z + diff.yaw   * 65 * pi / 180
  
  # scan to scan motion according to Mazaika 2011 and an average cortical distance of 65mm according to Marco Wilke et al 
  scan.to.scan <- sqrt(diff.x**2 + diff.y**2 + diff.z**2 + ( 65 * pi / 180 )**2 * (diff.pitch**2 + diff.roll**2 + diff.yaw**2))
  single.sts <- cbind(single.sts, scan.to.scan)

  # plot this subject time series for motion
  plot(scan.to.scan, type = "l", col='black')
  
}
  
# boxplot over subjects
boxplot(single.sts, notch = TRUE, names = file_list,  col=(c("lightgreen","lightblue")),
        ylab = "motion | mm", outline = FALSE, las=2)
title("scan to scan motion of all subjects")

# boxplot with all outliners
boxplot(single.sts, notch = TRUE, names = file_list,  col=(c("lightgreen","lightblue")),
        ylab = "motion | mm", outline = TRUE, las=2)
title("scan to scan motion of all subjects")

#######################################
# point plot / stripchart of all subs #
#######################################

# you need to update the plotting parameters here before using

options(scipen=999) # disable scientific notation in R

dims <- dim(single.sts)
iterations = dims[1]
variables = dims[2]
df2 <- matrix(ncol=variables, nrow=iterations)
for(i in 1:iterations){
  df2[i,] <- single.sts[i,] #runif(2)
}
df2 <- data.frame(output) #, col.names = file_list)

## logarithmic strip chart

stripchart(output,
           group.names = file_list,
           method = "jitter",
           jitter = 0.4, # jitter expansion
           vertical = TRUE,
           pch = 16,  # marker style
           cex = 0.75,# point size
           col=(c("lightgreen","lightblue")),
           las=2,
           ylab = "motion | mm",
           ylim = c(0.01,10),
           log = "y"
           )

# plot the means
points(colMeans(single.sts), pch = 16)  # "p", col = "black") #, lty = "..")
# plot the threshold
abline(h = 0.35, col = "red", lty = 3) 

## non-log strip chart
stripchart(output,
           group.names = file_list,
           method = "jitter",
           jitter = 0.4, # jitter expansion
           vertical = TRUE,
           pch = 16,  # marker style
           cex = 0.75,# point size
           col=(c("lightgreen","lightblue")),
           las=2,
           ylab = "motion | mm",
           ylim = c(0,10),
           #log = "y"
)
# plot the means
points(colMeans(single.sts), pch = 16)  # "p", col = "black") #, lty = "..")
# plot the threshold
abline(h = 0.35, col = "red", lty = 3) 


# sts over time plot
cols = c(brewer.pal(12, name = "Paired"),brewer.pal(8, name = "Dark2"))
#par(xpd = TRUE)
ts.plot(single.sts, col = cols, ylab = "motion | mm")
#legend('right',4, file_list, fill = cols, xpd = TRUE)
title('scan to scan motion over time')

# print some interesting values
mean = colMeans(single.sts)
sum = colSums(single.sts)
out = cbind(mean,sum)
out
