#####################################################################
## estimate absolute motion out of fMRI realignment parameter file ##
## by rkesslerx@gmail.com ###########################################
## version 0.1 ######################################################
#####################################################################
rm(list=ls())  # remove all objects from current workspace
library(RColorBrewer)
library(viridis)  # Load viridis for the viridis palette
library(ggplot2)
library(ggdist)
library(gghalves)
library(ggridges)
library(tidyverse)
library(ggrain)
library(dplyr)


# user input
raw_dir <- '/Users/roman/GitHub/emprise3t/data/raw/'  # where raw data is located
result_dir <- '/Users/roman/GitHub/emprise3t/data/interim/'  # where results are saved
plot_dir <- '/Users/roman/GitHub/emprise3t/plots/qa/motion/' # where plots are saved
file_pattern <- 'rp_arun-.*\\.txt'  # pattern to find the motion parameter files in subdirectories

# INFO:
# the subject, session, and run information is taken from the filepath!
# strings must contain sub-, ses-, and run-

# Create the directory and its parent directories if they do not exist
dir.create(paste0(plot_dir, "single_participant/"), recursive = TRUE)

file_list <-list.files(path = raw_dir, pattern = file_pattern, all.files = FALSE,
                  full.names = FALSE, recursive = TRUE,
                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# Create an empty dataframe to fill with all values
df <- data.frame(fwd = numeric(),
                 frame = numeric(),
                 sub = character(),
                 ses = character(),
                 run = character(),
                 stringsAsFactors = TRUE)

for (i in 1:length(file_list)){
  data = read.csv(paste(raw_dir,file_list[i], collapse = "", sep = ""), sep = '', header = FALSE, col.names = c('x','y','z','pitch','roll','yaw'))
  
  # get sub, ses, run
  sub_value <- gsub(".*sub-(\\d+).*", "sub-\\1", file_list[i])  # Extract sub value
  ses_value <- gsub(".*ses-(\\d+).*", "ses-\\1", file_list[i])  # Extract ses value
  run_value <- gsub(".*run-(\\d+).*", "run-\\1", file_list[i])  # Extract run value
  sub_ses_run_str = paste0(sub_value, "_", ses_value, "_", run_value)
  
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
  #sts <- cbind(sts, scan.to.scan)

  # plot this subject time series for motion
  #plot(scan.to.scan, type = "l", col='black')
  
  tryCatch(
    {
    # Create a new dataframe for the current iteration
    new_df <- data.frame(fwd = scan.to.scan,
                         frame = seq(1,length(scan.to.scan)),
                         sub = sub_value,
                         ses = ses_value,
                         run = run_value)
    
    # plot fwd time series
    p1 <- ggplot(new_df, aes(x=frame, y=fwd)) +
      geom_line() +
      geom_hline(yintercept = 1.5, linetype='dotted', col="darkred") +
      geom_hline(yintercept = 3, linetype='solid', col="darkred", lwd=1.5) +
      geom_text(aes(0,3,label = "voxel size", vjust = +2, hjust = -1), 
                color="darkred") +
      labs(title=paste0("frame-wise displacement (", 
                        sub_ses_run_str, ")"),
           y="frame-wise displacement | mm")
    
    ggsave(filename=paste0(plot_dir, "single_participant/fwd_", sub_ses_run_str, ".png"),
           plot=p1, dpi=200,
           height = 6, width = 18, unit = "cm")
    
    # wide to long transformation of raw motion parameters
    axis_order <- c("x", "y", "z", "pitch", "roll", "yaw")
    long_data <- data %>%
      mutate(frame = row_number()) %>%
      pivot_longer(cols = c(x, y, z, pitch, roll, yaw),
                   names_to = "axis",
                   values_to = "displacement") %>%
      mutate(axis = factor(axis, levels = axis_order))
    
    # plot raw motion time series
    p2 <- ggplot(long_data, aes(x=frame, y=displacement, color=axis)) +
      geom_line() +
      geom_hline(yintercept = 1.5, linetype='dotted', col="darkred") +
      geom_hline(yintercept = 3, linetype='solid', col="darkred", lwd=1.5) +
      geom_text(aes(0,3,label = "voxel size", vjust = +2, hjust = -1), 
                color="darkred") +
      labs(title=paste0("motion parameters (", 
                        sub_ses_run_str, ")"),
           y="displacement | mm || deg")
    
    ggsave(filename=paste0(plot_dir, "single_participant/motion_", sub_ses_run_str, ".png"),
           plot=p2, dpi=200,
           height = 6, width = 18, unit = "cm")
    
    
    # Append the new dataframe to the existing dataframe
    df <- rbind(df, new_df)
    },
    error = function(e) {
      cat(paste("Error:", e$message, "\n"))
      cat(paste(sub_value, " ", ses_value, " ", run_value, "does not fit\n"))
      
    }
  )
  
}
  
# boxplot over subjects

#ggplot(data=df, aes(x=ses, y=fwd, fill=run)) +
#  geom_boxplot(outlier.shape = NA) + # no outliers
#  scale_y_continuous(limits = quantile(df$fwd, c(0.0, 0.9))) + # to scale to not show outlier scales
#  labs(y = "frame wise displacement | mm") +
#  scale_fill_viridis(discrete=TRUE, alpha=0.5) +
#  facet_grid(sub ~ .)

# raincould plot
p3 <- ggplot(data = df, aes(x = ses, y = fwd, color = run)) + #, fill = run
  geom_rain(alpha = 0.5, 
            point.args = rlang::list2(size=0.005),
            point.args.pos = rlang::list2(
              position = position_jitter(width = 0.15, height = 0, 
                                         seed = 42)),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(x = -.35), 
              width = 0.3
            ),
            violin.args.pos = rlang::list2(
              position = position_nudge(x = 0.2), 
              width = 0.4
            ),
            violin.args = rlang::list2(side="r")
            ) +
  facet_grid(sub ~ .) +
  scale_y_log10() +
  geom_hline(yintercept = 3, linetype='dashed', 
             col="darkred", lwd=0.5) +
  labs(title="frame-wise motion",
       y = "frame-wise displacement | mm",
       x = "session")


ggsave(filename=paste0(plot_dir, "fwd.png"),
       plot=p3, dpi=300,
       height = 30, width = 30, unit = "cm")


# save data and summary statistics
write.csv(df, 
          paste0(result_dir, "fwd.csv"), 
          row.names=FALSE)

#define quantiles of interest
q = seq(from=0.0, to=1, by=0.1)

#calculate quantiles by grouping variable
df_summary <- df %>%
  group_by(sub,ses,run) %>%
  summarize(quant00 = quantile(fwd, probs = q[1]),
            quant10 = quantile(fwd, probs = q[2]),
            quant20 = quantile(fwd, probs = q[3]),
            quant30 = quantile(fwd, probs = q[4]),
            quant40 = quantile(fwd, probs = q[5]),
            quant50 = quantile(fwd, probs = q[6]),
            quant60 = quantile(fwd, probs = q[7]),
            quant70 = quantile(fwd, probs = q[8]),
            quant80 = quantile(fwd, probs = q[9]),
            quant90 = quantile(fwd, probs = q[10]),
            quant100 = quantile(fwd, probs = q[11])
            )

write.csv(df_summary, 
          paste0(result_dir, "fwd_quantiules.csv"), 
          row.names=FALSE)
