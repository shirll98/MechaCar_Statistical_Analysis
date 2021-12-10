# import libraries dplyr and tidyverse
library(dplyr)
library(tidyverse)
# read in mecha csv file 
mechaCarData <- read_csv("MechaCar_mpg.csv")
# look at the first rows of imported data 
head(mechaCarData) 

# linear regression model 
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + AWD + ground_clearance,data=mechaCarData) 

# generate summary statistics
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + AWD + ground_clearance,data=mechaCarData)) 

#import suspension coil data
suspension_coil_data <- read_csv("Suspension_Coil.csv") #import suspension coil data
head(suspension_coil_data)

#create total summary table
total_summary <- suspension_coil_data %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') 
total_summary

lot_summary <- suspension_coil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') 
lot_summary

#randomly sample 50 data points
global_sample_table <- suspension_coil_data %>% sample_n(50) #randomly sample 50 data points

#import all coil data into ggplot2
plt <- ggplot(suspension_coil_data,aes(x=PSI)) 

#visualize distribution with density plot
plt + geom_density() 

#import sample coil data into ggplot2
plt <- ggplot(global_sample_table,aes(x=PSI)) 
#visualize distribution with density plot
plt + geom_density() 

#import sample coil data into ggplot2
plt <- ggplot(global_sample_table,aes(x=log10(PSI))) 
#visualize distribution with density plot
plt + geom_density() 

#compare sample versus population mean
t.test(global_sample_table$PSI,mu=mean(suspension_coil_data$PSI)) 

#create a sample table of 25 data points with Lot 1
psi_lot1_sample <- suspension_coil_data %>% subset(Manufacturing_Lot=='Lot1') %>% sample_n(25) 
#compare Lot1 sample versus population mean
t.test(psi_lot1_sample$PSI,mu=mean(suspension_coil_data$PSI)) 

#create a sample table of 25 data points with Lot 2
psi_lot2_sample <- suspension_coil_data %>% subset(Manufacturing_Lot=='Lot2') %>% sample_n(25)
#compare Lot2 sample versus population mean
t.test(psi_lot2_sample$PSI,mu=mean(suspension_coil_data$PSI)) 

#create a sample table of 25 data points with Lot 3
psi_lot3_sample <- suspension_coil_data %>% subset(Manufacturing_Lot=='Lot3') %>% sample_n(25) 

#compare Lot3 sample versus population mean
t.test(psi_lot3_sample$PSI,mu=mean(suspension_coil_data$PSI)) 

