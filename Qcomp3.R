library(dataRetrieval)
library(lubridate)
library(tidyverse)
library(zoo)
library(cowplot)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(scales)


# Read CSV files
ppt_data <- read.csv("merged_ppt_data.csv", header = TRUE, sep =",")
jul_hms <- read.csv("09JUL2022.csv", header = TRUE, sep =",")
aug_hms <- read.csv("03AUG2022.csv", header = TRUE, sep =",")
sep_hms <- read.csv("10SEP2022.csv", header = TRUE, sep =",")

# Define Function Parameters
get_nwis_q <- function(site_number, start_date, end_date) {
  
  # Create a site number vector
  site_numbers <- c(site_number)
  
  # Retrieve instantaneous discharge data
  discharge_data <- readNWISdata(
    siteNumbers = site_numbers,
    parameterCd = "00060",  # 00060 represents instantaneous discharge
    startDate = start_date,
    endDate = end_date,
    tz = "MST",
    service = "iv"  # Set service to "iv" for instantaneous values
  )
  
  return(discharge_data)
}

# Set Site Number and Time Range
site_number <- "08380500"  # Replace
start_date <- "2022-07-01"  # Replace
end_date <- "2022-10-01"    # Replace

# Retrieve Instantaneous PPT
nwis_q <- get_nwis_q(site_number, start_date, end_date)

# Convert Date and Time
nwis_q$dateTime <- as.POSIXct(nwis_q$dateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
ppt_data$dateTime <- as.POSIXct(ppt_data$dateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
jul_hms$DateTime <- as.POSIXct(paste(jul_hms$Date, jul_hms$Time), format = "%d-%b-%y %H:%M", tz = "MST")
aug_hms$DateTime <- as.POSIXct(paste(aug_hms$Date, aug_hms$Time), format = "%d-%b-%y %H:%M", tz = "MST")
sep_hms$DateTime <- as.POSIXct(paste(sep_hms$Date, sep_hms$Time), format = "%d-%b-%y %H:%M", tz = "MST")

# Filter data frames for the specified date range
nwis_q <- subset(nwis_q, dateTime >= "2022-07-01" & dateTime < "2022-10-01")
ppt_data <- subset(ppt_data, dateTime >= "2022-07-01" & dateTime < "2022-10-01")


# Melt the data to long format for ggplot
melted_data <- gather(ppt_data, gauge, Value, -dateTime)

# Convert inches to millimeters
melted_data$Value <- melted_data$Value * 25.4

# Convert cfs to cms
nwis_q$Obs_Outflow <- nwis_q$X_00060_00000 * 0.0283168
jul_hms$Pre_Outflow <- jul_hms$Pre_Outflow * 0.0283168
jul_hms$Post_Outflow <- jul_hms$Post_Outflow * 0.0283168
aug_hms$Pre_Outflow <- aug_hms$Pre_Outflow * 0.0283168
aug_hms$Post_Outflow <- aug_hms$Post_Outflow * 0.0283168
sep_hms$Pre_Outflow <- sep_hms$Pre_Outflow * 0.0283168
sep_hms$Post_Outflow <- sep_hms$Post_Outflow * 0.0283168

# Discharge Hydrograph
p1 <- ggplot(nwis_q) +
  geom_rect(
    xmin = as.POSIXct("2022-07-09 10:00:00", tz = "MST"),
    xmax = as.POSIXct("2022-07-10 10:00:00", tz = "MST"),
    ymin = log(0),
    ymax = log(20),
    fill = NA,
    alpha = 0.3,
    color = "black",
    linetype = "dashed",
    linewidth = 0.1
  ) +
  geom_rect(
    xmin = as.POSIXct("2022-08-03 06:00:00", tz = "MST"),
    xmax = as.POSIXct("2022-08-04 06:00:00", tz = "MST"),
    ymin = log(0),
    ymax = log(20),
    fill = NA,
    alpha = 0.3,
    color = "black",
    linetype = "dashed",
    linewidth = 0.1
  ) +
  geom_rect(
    xmin = as.POSIXct("2022-09-10 08:00:00", tz = "MST"),
    xmax = as.POSIXct("2022-09-11 08:00:00", tz = "MST"),
    ymin = log(0),
    ymax = log(20),
    fill = NA,
    alpha = 0.3,
    color = "black",
    linetype = "dashed",
    linewidth = 0.1
  ) +
  geom_line(aes(dateTime, Obs_Outflow, color = "Observed")) +
  scale_y_continuous(position = "left",
                     trans = 'log10',
                     labels = scales::label_number(),
                     limits = c(0.1, 900),
                     breaks = c(0.1,1, 10, 100),
                     minor_breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
                                      1, 2, 3, 4, 5, 6, 7, 8, 9,
                                      10, 20, 30, 40, 50, 60, 70, 80, 90, 200),
                     expand = c(0, 0)) +
  scale_color_manual(values = c("midnightblue")) +
  labs(y = expression(paste("Discharge (", m^"3", "s"^"-1",")")), x = "") +
  #labs(y = expression(atop("Discharge", paste("m"^"3", "s"^"-1"))), x = "") +
  #labs(y = expression(atop("Discharge", paste("(m"^"3", "s"^"-1", ")"))), x = "") +
  theme_minimal() +
  scale_x_datetime(expand = c(0,0)) +
  theme(axis.title.y.left = element_text(hjust = 0),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        #panel.border = element_rect(color = "black", fill = NA)
        )

# Print the plot
p1

# Precipitation hyetograph
p2 <- ggplot(melted_data) +
  geom_step(aes(dateTime, Value), color = "steelblue", linewidth = 1) +
  scale_y_reverse(position = "right",
                  limits = c(61, 0),
                  breaks = seq(0, 10, by = 5),
                  minor_breaks = c(),
                  labels = seq(0, 10, by = 5),
                  expand = c(0, 0)) +
  guides(x = guide_axis(angle = 90)) +
  geom_hline(yintercept = 10, linetype = "solid", color = "black", linewidth = 0.1)  +
  #labs(y = expression(paste("Precipitation (", "mm ", "5min"^"-1",")")), x = "") +
  labs(y = expression(paste("Precipitation (", "mm",")")), x = "") +
  #labs(y = expression(atop("Precipitation", "mm 5min"^"-1")), x = "") +
  #labs(y = expression(atop("Precipitation", paste("(mm 5min", phantom(')')^'-1', ')'))), x = "") +
  scale_x_datetime(expand = c(0,0)) +
  theme_minimal() +
  theme(axis.title.y.right = element_text(hjust = 0),
        legend.position = "right",
        legend.justification = c(0.75, 0.5),
        legend.title = element_blank(),
        axis.text.x = element_blank(),    # Remove x-axis text
        axis.ticks.x = element_blank(),   # Remove x-axis ticks
        axis.line.x = element_blank(),    # Remove x-axis line
        panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor.x = element_blank())  # Remove minor vertical grid lines

p2

# Combine plots using cowplot
aligned_plots <- align_plots(p1, p2, align = "hv", axis = "tblr")
out <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
print(out)


# Specify the start and end date for the window
jul_start <- as.POSIXct("2022-07-09 10:00:00", tz = "MST")
jul_end <- as.POSIXct("2022-07-10 10:00:00", tz = "MST")

# Create a subset of the data for the specified date range
jul_subset_hms <- jul_hms[jul_hms$DateTime >= jul_start & jul_hms$DateTime <= jul_end, ]
jul_subset_ppt <- ppt_data[ppt_data$dateTime >= jul_start & ppt_data$dateTime <= jul_end, ]
jul_melted_subset <- gather(jul_subset_ppt, gauge, Value, -dateTime)
jul_melted_subset$Value <- jul_melted_subset$Value * 25.4

# Match and update Obs_Outflow values in jul_hms
jul_subset_hms$Obs_Outflow <- nwis_q$Obs_Outflow[match(jul_subset_hms$DateTime, nwis_q$dateTime)]
jul_subset_hms$Obs_Outflow <- na.approx(jul_subset_hms$Obs_Outflow)

# Plot Discharge Comparison 
p3 <- ggplot(jul_subset_hms, aes(x = DateTime)) +
  geom_hline(yintercept = 555*0.028316832, linetype = "dashed", color = "grey")  +
  geom_hline(yintercept = 1490*0.028316832, linetype = "dashed", color = "grey")  +
  geom_hline(yintercept = 2520*0.028316832, linetype = "dashed", color = "grey")  +
  #geom_hline(yintercept = 3880*0.028316832, linetype = "dashed", color = "grey")  +
  geom_line(aes(y = Post_Outflow, color = "Post-Fire"), linewidth = 0.7) +
  geom_line(aes(y = Pre_Outflow, color = "Pre-Fire"), linewidth = 0.7) +
  geom_line(aes(y = Obs_Outflow, color = "Observed"), linewidth = 0.7) +
  labs(title = "",
       x = "",
       y = "") +
  scale_color_manual(name = "", values = c("Observed" = "midnightblue", "Pre-Fire" = "forestgreen", "Post-Fire" = "orangered2")) +
  scale_y_continuous(position = "left",
                     trans = 'log10',
                     labels = scales::label_number(),
                     limits = c(0.1, 900),
                     breaks = c(0.1,1, 10, 100),
                     minor_breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
                                      1, 2, 3, 4, 5, 6, 7, 8, 9,
                                      10, 20, 30, 40, 50, 60, 70, 80, 90, 200),
                     expand = c(0, 0)) +
  labs(y = expression(paste("Discharge (", m^"3", "s"^"-1",")")), x = "") +
  #labs(y = expression(atop("Discharge", paste("m"^"3", "s"^"-1"))), x = "") +
  #labs(y = expression(atop("Discharge", paste("(m"^"3", "s"^"-1", ")"))), x = "") +
  theme_minimal() + 
  scale_x_datetime(
    date_breaks = "day",
    date_labels = "%b %d",
    #date_minor_breaks = "1 hours"
    ) +
  theme(axis.title.y.left = element_text(hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank()
        #panel.border = element_rect(color = "black", fill = NA)
        )

p3

# Add annotations
#p3 <- p3 +
#  annotate("text", x = as.POSIXct("2022-07-09 11:00:00"), y = 555*0.028316832, label = "1/2 AEP", hjust = 0, vjust = 1.5, size = 3, alpha = 0.7) +
#  annotate("text", x = as.POSIXct("2022-07-09 11:00:00"), y = 1490*0.028316832, label = "1/5 AEP", hjust = 0, vjust = 1.5, size = 3, alpha = 0.7) +
#  annotate("text", x = as.POSIXct("2022-07-09 11:00:00"), y = 2520*0.028316832, label = "1/10 AEP", hjust = 0, vjust = 1.5, size = 3, alpha = 0.7)
#  #annotate("text", x = as.POSIXct("2022-07-09 11:00:00"), y = 3880*0.028316832, label = "1/25 AEP", hjust = 0, vjust = 1.5, size = 3, alpha = 0.7)

#p3

# Plot Hietograph Comparison
p4 <- ggplot(jul_melted_subset) +
  geom_step(aes(dateTime, Value, color = gauge), linewidth = 0.7) +
  scale_y_reverse(position = "right",
                  limits = c(61, 0),
                  breaks = seq(0, 10, by = 5),
                  minor_breaks = c(),
                  labels = seq(0, 10, by = 5),
                  expand = c(0, 0)) +
  scale_color_brewer(palette = "PuBu") +
  guides(x = guide_axis(angle = 90)) +
  labs(y = "", x = "") +
  theme_minimal() +
  geom_hline(yintercept = 10, linetype = "solid", color = "black", linewidth = 0.2)  +
  #geom_hline(yintercept = 7, linetype = "dashed", color = "grey")  +
  geom_hline(yintercept = 9, linetype = "dashed", color = "grey")  +
  #geom_hline(yintercept = 13, linetype = "dashed", color = "grey")  +
  theme(axis.title.y.right = element_text(hjust = 0),
        axis.text.y = element_blank(),  # Remove y-axis labels
        legend.position = "none",
        legend.justification = c(0.75, 0.5),
        legend.title = element_blank(),
        axis.text.x = element_blank(),    # Remove x-axis text
        axis.ticks.x = element_blank(),   # Remove x-axis ticks
        axis.line.x = element_blank(),    # Remove x-axis line
        panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor.x = element_blank())  # Remove minor vertical grid lines

p4

# Add annotations
#p4 <- p4 +
  #annotate("text", x = as.POSIXct("2022-07-09 11:00:00"), y = 7, label = "1/1 Annual Exceedance Probability (AEP)", hjust = 1) +
  #annotate("text", x = as.POSIXct("2022-07-09 11:00:00"), y = 9, label = "1/2 Annual Exceedance Probability (AEP)", hjust = 0, vjust = -.5, size = 3, alpha = 0.5)

p4

# Combine plots using cowplot
aligned_plots <- align_plots(p3, p4, align = "hv", axis = "tblr")
out2 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
print(out2)


# Specify the start and end date for the window
aug_start <- as.POSIXct("2022-08-03 06:00:00", tz = "MST")
aug_end <- as.POSIXct("2022-08-04 06:00:00", tz = "MST")

# Create a subset of the data for the specified date range
aug_subset_hms <- aug_hms[aug_hms$DateTime >= aug_start & aug_hms$DateTime <= aug_end, ]
aug_subset_ppt <- ppt_data[ppt_data$dateTime >= aug_start & ppt_data$dateTime <= aug_end, ]
aug_melted_subset <- gather(aug_subset_ppt, gauge, Value, -dateTime)
aug_melted_subset$Value <- aug_melted_subset$Value * 25.4

# Match and update Obs_Outflow values in aug_hms
aug_subset_hms$Obs_Outflow <- nwis_q$Obs_Outflow[match(aug_subset_hms$DateTime, nwis_q$dateTime)]
aug_subset_hms$Obs_Outflow <- na.approx(aug_subset_hms$Obs_Outflow)

# Plot Discharge Comparison 
p5 <- ggplot(aug_subset_hms, aes(x = DateTime)) +
  geom_hline(yintercept = 555*0.028316832, linetype = "dashed", color = "grey")  +
  geom_hline(yintercept = 1490*0.028316832, linetype = "dashed", color = "grey")  +
  geom_hline(yintercept = 2520*0.028316832, linetype = "dashed", color = "grey")  +
  #geom_hline(yintercept = 3880*0.028316832, linetype = "dashed", color = "grey")  +
  geom_line(aes(y = Post_Outflow, color = "Post-Fire"), linewidth = 0.7) +
  geom_line(aes(y = Pre_Outflow, color = "Pre-Fire"), linewidth = 0.7) +
  geom_line(aes(y = Obs_Outflow, color = "Observed"), linewidth = 0.7) +
  labs(title = "",
       x = "",
       y = "") +
  scale_color_manual(name = "", values = c("Observed" = "midnightblue", "Pre-Fire" = "forestgreen", "Post-Fire" = "orangered2")) +
  scale_y_continuous(position = "left",
                     trans = 'log10',
                     labels = scales::label_number(),
                     limits = c(0.1, 900),
                     breaks = c(0.1,1, 10, 100),
                     minor_breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
                                      1, 2, 3, 4, 5, 6, 7, 8, 9,
                                      10, 20, 30, 40, 50, 60, 70, 80, 90, 200),
                     expand = c(0, 0)) +
  labs(y = "",
       x = "") +
  theme_minimal() + 
  scale_x_datetime(
    date_breaks = "day",
    date_labels = "%b %d") +
  theme(axis.title.y.right = element_text(hjust = 0),
        axis.text.y = element_blank(),  # Remove y-axis labels
        legend.position = "none",
        legend.justification = c(0.75, 0.5),
        legend.title = element_blank()) # Remove minor vertical grid lines
        #panel.border = element_rect(color = "black", fill = NA))  


p5

# Plot Hietograph Comparison
p6 <- ggplot(aug_melted_subset) +
  geom_step(aes(dateTime, Value, color = gauge), linewidth = 0.7) +
  scale_y_reverse(position = "right",
                  limits = c(61, 0),
                  breaks = seq(0, 10, by = 5),
                  minor_breaks = c(),
                  labels = seq(0, 10, by = 5),
                  expand = c(0, 0)) +
  scale_color_brewer(palette = "PuBu") +
  guides(x = guide_axis(angle = 90)) +
  labs(y = "", x = "") +
  geom_hline(yintercept = 10, linetype = "solid", color = "black", linewidth = 0.2)  +
  #geom_hline(yintercept = 7, linetype = "dashed", color = "grey")  +
  geom_hline(yintercept = 9, linetype = "dashed", color = "grey")  +
  #geom_hline(yintercept = 13, linetype = "dashed", color = "grey")  +
  theme_minimal() +
  theme(axis.title.y.right = element_text(hjust = 0),
        axis.text.y = element_blank(),  # Remove y-axis labels
        legend.position = "none",
        legend.justification = c(0.75, 0.5),
        legend.title = element_blank(),
        axis.text.x = element_blank(),    # Remove x-axis text
        axis.ticks.x = element_blank(),   # Remove x-axis ticks
        axis.line.x = element_blank(),    # Remove x-axis line
        panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor.x = element_blank())  # Remove minor vertical grid lines

p6

# Combine plots using cowplot
aligned_plots <- align_plots(p5, p6, align = "hv", axis = "tblr")
out3 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
print(out3)

# Specify the start and end date for the window
sep_start <- as.POSIXct("2022-09-10 10:00:00", tz = "MST")
sep_end <- as.POSIXct("2022-09-11 10:00:00", tz = "MST")

# Create a subset of the data for the specified date range
sep_subset_hms <- sep_hms[sep_hms$DateTime >= sep_start & sep_hms$DateTime <= sep_end, ]
sep_subset_ppt <- ppt_data[ppt_data$dateTime >= sep_start & ppt_data$dateTime <= sep_end, ]
sep_melted_subset <- gather(sep_subset_ppt, gauge, Value, -dateTime)
sep_melted_subset$Value <- sep_melted_subset$Value * 25.4

# Match and update Obs_Outflow values in sep_hms
sep_subset_hms$Obs_Outflow <- nwis_q$Obs_Outflow[match(sep_subset_hms$DateTime, nwis_q$dateTime)]
sep_subset_hms$Obs_Outflow <- na.approx(sep_subset_hms$Obs_Outflow)

# Plot Hietograph Comparison 
p7 <- ggplot(sep_subset_hms, aes(x = DateTime)) +
  geom_hline(yintercept = 555*0.028316832, linetype = "dashed", color = "grey")  +
  geom_hline(yintercept = 1490*0.028316832, linetype = "dashed", color = "grey")  +
  geom_hline(yintercept = 2520*0.028316832, linetype = "dashed", color = "grey")  +
  #geom_hline(yintercept = 3880*0.028316832, linetype = "dashed", color = "grey")  +
  geom_line(aes(y = Post_Outflow, color = "Post-Fire"), linewidth = 0.7) +
  geom_line(aes(y = Pre_Outflow, color = "Pre-Fire"), linewidth = 0.7) +
  geom_line(aes(y = Obs_Outflow, color = "Observed"), linewidth = 0.7) +
  labs(title = "",
       x = "",
       y = "") +
  scale_color_manual(name = "", values = c("Observed" = "midnightblue", "Pre-Fire" = "forestgreen", "Post-Fire" = "orangered2")) +
  scale_y_continuous(position = "left",
                     trans = 'log10',
                     labels = scales::label_number(),
                     limits = c(0.1, 900),
                     breaks = c(0.1,1, 10, 100),
                     minor_breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
                                      1, 2, 3, 4, 5, 6, 7, 8, 9,
                                      10, 20, 30, 40, 50, 60, 70, 80, 90, 200),
                     expand = c(0, 0)) +
  labs(y = "",
       x = "") +
  theme_minimal() + 
  scale_x_datetime(
    date_breaks = "day",
    date_labels = "%b %d") +
  theme(axis.title.y.right = element_text(hjust = 0),
        axis.text.y = element_blank(),  # Remove y-axis labels
        legend.position = "none",
        legend.justification = c(0.75, 0.5),
        legend.title = element_blank())  # Remove minor vertical grid lines

p7

# Add annotations
#p7 <- p7 +
  #annotate("text", x = as.POSIXct("2022-09-11 06:00:00"), y = 555*0.028316832, label = "1/2 AEP", hjust = 0, vjust = 1.5, size = 3, alpha = 0.5) +
  #annotate("text", x = as.POSIXct("2022-09-11 06:00:00"), y = 1490*0.028316832, label = "1/5 AEP", hjust = 0, vjust = 1.5, size = 3, alpha = 0.5) +
  #annotate("text", x = as.POSIXct("2022-09-11 06:00:00"), y = 2520*0.028316832, label = "1/10 AEP", hjust = 0, vjust = 1.5, size = 3, alpha = 0.5)
  #annotate("text", x = as.POSIXct("2022-09-11 04:00:00"), y = 3880*0.028316832, label = "1/25 AEP", hjust = 0, vjust = -0.5, size = 3, alpha = 0.5)

p7

# Plot Hietograph Comparison
p8 <- ggplot(sep_melted_subset) +
  geom_step(aes(dateTime, Value, color = gauge), linewidth = 0.7) +
  scale_y_reverse(position = "right",
                  limits = c(61, 0),
                  breaks = seq(0, 10, by = 5),
                  minor_breaks = c(),
                  labels = seq(0, 10, by = 5),
                  expand = c(0, 0)) +
  scale_color_brewer(palette = "PuBu") +
  guides(x = guide_axis(angle = 90)) +
  geom_hline(yintercept = 10, linetype = "solid", color = "black", linewidth = 0.2)  +
  #labs(y = expression(paste("Precipitation (", "mm ", "5min"^"-1",")")), x = "") +
  labs(y = expression(paste("Precipitation (", "mm",")")), x = "") +
  #labs(y = expression(atop("Precipitation", "mm 5min"^"-1")), x = "") +
  #labs(y = expression(atop("Precipitation", paste("(mm 5min", phantom(')')^'-1', ')'))), x = "") +
  theme_minimal() +
  geom_hline(yintercept = 9, linetype = "dashed", color = "grey") +
  theme(axis.title.y.right = element_text(hjust = 0),
        legend.position = "none",
        legend.justification = c(0.75, 0.5),
        legend.title = element_blank(),
        axis.text.x = element_blank(),    # Remove x-axis text
        axis.ticks.x = element_blank(),   # Remove x-axis ticks
        axis.line.x = element_blank(),    # Remove x-axis line
        panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor.x = element_blank())  # Remove minor vertical grid lines
        #panel.border = element_rect(color = "black", fill = NA))  

p8

# Combine plots using cowplot
aligned_plots <- align_plots(p7, p8, align = "hv", axis = "tblr")
out4 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
print(out4)

# Combine the plots
zoom <- ggarrange(out2, out3, out4, ncol = 3, legend = "none")
print(zoom)

# Create a custom legend
custom_legend <- ggplot() +
  geom_line(aes(x = 1, y = 1, color = "Pre-Fire Model"), linewidth = 0.7) +
  geom_line(aes(x = 2, y = 1, color = "Post-Fire Model"), linewidth = 0.7) +
  geom_line(aes(x = 3, y = 1, color = "Observed Flow"), linewidth = 0.7) +
  scale_color_manual(name = "",  # Empty string for no legend title
                     values = c("Pre-Fire Model" = "forestgreen", "Post-Fire Model" = "orangered2", "Observed Flow" = "midnightblue"),
                     limits = c("Pre-Fire Model", "Post-Fire Model", "Observed Flow")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11))



# Overlay the custom legend on top of the zoomed-in plots
zoom_legend <- ggdraw(zoom) +
  draw_plot(custom_legend, x = 0, y = 0.11, width = 1, height = 1)

# Print the final plot
print(zoom_legend)

# Combine the legend
final <- ggarrange(out, zoom_legend, ncol = 1, legend = "none", heights = c(1, 2))

# Print the final combined plot
print(final)

# Load the image
hmsimage <- readPNG("HMS.png")  # Replace with the path to your image
im_A <- ggplot() + background_image(hmsimage) + 
  theme(plot.margin = margin(t=2, l=1, r=1, b=2, unit = "cm"))


# Arrange the plots side by side
map_plot <- ggarrange(final, im_A,
                      ncol = 2, widths = c(2,1))
print(map_plot)
