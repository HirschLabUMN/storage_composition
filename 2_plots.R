### Script written by Jonathan Renk
### 9 Jun 2021

### Figure 1b-c ###
## Clearing the global environment
rm(list=ls(all=TRUE))

## Setting up the working directory
getwd()
setwd("/Users/jonathanrenk/Desktop/Storage Work/Thermobutton_data/")

## Loading packages
library(lubridate)
library(ggplot2)

#loading in the data
# planting 2018
p_18 <- read.csv("plant_2018.csv", header=T, stringsAsFactors=F)
str(p_18)

# convert date to Date class
p_18$date_time = dmy_hms(paste(p_18$Date, p_18$Time))

#loading in the vertical lines
v_line <- read.csv("v_lines_p18.csv", header=T, stringsAsFactors=F)
# convert date to Date class
v_line$date_time = dmy_hms(paste(v_line$Date, v_line$Time))

#making figures
ggplot(data=p_18, aes(x=date_time, y=Temperature)) +
  geom_line(color = "gray", size = 1, alpha = 0.80) +
  labs(y = "Temperature (\u00B0C)", x = "Date (month)", title = "2018 Storage Temperature") +
  geom_vline(data = v_line,
             aes(xintercept = date_time, color = Hybrid)) +
  theme_classic()
ggsave("p18_temp_v1.pdf", width = 7, height = 2, units = c("in"))

#loading in the data
# planting 2019
p_19 <- read.csv("plant_2019.csv", header=T, stringsAsFactors=F)
str(p_19)

# convert date to Date class
p_19$date_time = dmy_hms(paste(p_19$Date, p_19$Time))

#loading in the vertical lines
v_line <- read.csv("v_lines_p19.csv", header=T, stringsAsFactors=F)
# convert date to Date class
v_line$date_time = dmy_hms(paste(v_line$Date, v_line$Time))

#making figures
ggplot(data=p_19, aes(x=date_time, y=Temperature)) +
  geom_line(color = "gray", size = 1, alpha = 0.80) +
  labs(y = "Temperature (\u00B0C)", x = "Date (month)", title = "2019 Storage Temperature") +
  geom_vline(data = v_line,
             aes(xintercept = date_time, color = Drying)) +
  theme_classic()
ggsave("p19_temp_v1.pdf", width = 7, height = 2, units = c("in"))

### Figure 3 ###
## Clearing the global environment
rm(list=ls(all=TRUE))

## Setting up the working directory
getwd()
setwd("/Users/jonathanrenk/Desktop/Storage Work/")

## Loading packages
library(ggplot2)

## Loading in the data
data <- read.csv("data/krn_averages.csv")

str(data)
summary(data)

## Setting variables as factors for hybrid, sample, year, hotplate, rep, temp, and storage
data[,1] <- as.factor(data[,1])
data[,2] <- as.factor(data[,2])
data[,3] <- as.factor(data[,3])
data[,4] <- as.factor(data[,4])
data[,11] <- as.factor(data[,11])

str(data)

## Subsetting the year with kernels
krn_yr1 <- data.frame(data [ which(data$Year == '1'),])
krn_yr2 <- data.frame(data [ which(data$Year == '2'),])

# KRN1
krn_yr1$Storage <- ordered(krn_yr1$Storage, levels = c("NS", "1", "2", "3", "4", "5", "6", "7", "8"))

ggplot(data = krn_yr1, aes(Storage, Crude_protein, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Crude Protein (%)", x="Storage Duration (months)", title="2018 Planting") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Crude.protein_krn_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

ggplot(data = krn_yr1, aes(Storage, Starch_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Starch dwb (%)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Starch_krn_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

ggplot(data = krn_yr1, aes(Storage, Asparagine_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Asp (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Asp_krn_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

ggplot(data = krn_yr1, aes(Storage, Glucose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Glu (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Glu_krn_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

ggplot(data = krn_yr1, aes(Storage, Fructose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Fru (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Fru_krn_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

ggplot(data = krn_yr1, aes(Storage, Sucrose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Suc (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Suc_krn_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

# KRN2
krn_yr2$Storage <- ordered(krn_yr2$Storage, levels = c("NS", "1", "2", "3", "4", "7", "8"))

ggplot(data = krn_yr2, aes(Storage, Crude_protein, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Crude Protein (%)", x="Storage Duration (months)", title="2019 Planting") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Crude.protein_krn2_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

ggplot(data = krn_yr2, aes(Storage, Starch_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Starch dwb (%)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Starch_krn2_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

ggplot(data = krn_yr2, aes(Storage, Asparagine_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Asp (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Asp_krn2_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

ggplot(data = krn_yr2, aes(Storage, Glucose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Glu (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Glu_krn2_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

ggplot(data = krn_yr2, aes(Storage, Fructose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Fru (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Fru_krn2_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

ggplot(data = krn_yr2, aes(Storage, Sucrose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Suc (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Suc_krn2_v2.pdf", width = 3.5, height = 1.75, units = c("in"))

### Figure 4 ###
## Loading in the data
data <- read.csv("data/wk_averages.csv")

str(data)
summary(data)

## Setting variables as factors for hybrid, sample, year, hotplate, rep, temp, and storage
data[,1] <- as.factor(data[,1])
data[,2] <- as.factor(data[,2])
data[,3] <- as.factor(data[,3])
data[,4] <- as.factor(data[,4])
data[,13] <- as.factor(data[,13])

str(data)

## Subsetting the year with kernels
wk_yr1 <- data.frame(data [ which(data$Year == '1'),])
wk_yr2 <- data.frame(data [ which(data$Year == '2'),])

### WK Yr1 ###
wk_yr1$Storage <- ordered(wk_yr1$Storage, levels = c("NS", "1", "2", "3", "4", "5", "6", "7", "8"))

ggplot(data = wk_yr1, aes(Storage, DML.Percent, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="DML (%)", x="Storage Duration (months)", title="2018 Planting") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("DML_wk1_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr1, aes(Storage, Kernel.Moisture, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Kernel Moisture (%)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Kernel.Moisture_wk1_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr1, aes(Storage, Crude_protein, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Crude Protein (%)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Crude.protein_wk1_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr1, aes(Storage, Starch_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Starch dwb (%)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Starch_wk1_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr1, aes(Storage, Asparagine_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Asp (g/100g flour)", x="Storage Duration (months)", title="2018 Planting") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Asp_wk1_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr1, aes(Storage, Glucose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Glu (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Glu_wk1_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr1, aes(Storage, Fructose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Fru (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Fru_wk1_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr1, aes(Storage, Sucrose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Suc (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Suc_wk1_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

### WK Yr2 ###
wk_yr2$Storage <- ordered(wk_yr2$Storage, levels = c("NS", "1", "2", "3", "4", "7", "8"))

ggplot(data = wk_yr2, aes(Storage, DML.Percent, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="DML (%)", x="Storage Duration (months)", title="2019 Planting") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("DML_wk2_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr2, aes(Storage, Kernel.Moisture, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Kernel Moisture (%)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Kernel.Moisture_wk2_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr2, aes(Storage, Crude_protein, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Crude Protein (%)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Crude.protein_wk2_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr2, aes(Storage, Starch_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Starch dwb (%)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Starch_wk2_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr2, aes(Storage, Asparagine_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Asp (g/100g flour)", x="Storage Duration (months)", title="2019 Planting") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Asp_wk2_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr2, aes(Storage, Glucose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Glu (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Glu_wk2_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr2, aes(Storage, Fructose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Fru (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Fru_wk2_v2.pdf", width = 3.5, height = 2.62, units = c("in"))

ggplot(data = wk_yr2, aes(Storage, Sucrose_g.100g_dwb, group = Hybrid.Temp)) +
  geom_line(aes(color=Hybrid.Temp)) + 
  geom_point(aes(color=Hybrid.Temp)) +
  labs(y="Suc (g/100g flour)", x="Storage Duration (months)") +
  theme_bw() +
  theme(legend.position = "none") 
ggsave("Suc_wk2_v2.pdf", width = 3.5, height = 2.62, units = c("in"))
