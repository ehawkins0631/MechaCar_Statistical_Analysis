library(dplyr)

library(jsonlite)


##Delivery 1
##Import and read in the Mechacar_mpg.csv file as a dataframe
##Import CSV and read the file in a dataframe

Mechacar <- read.csv(file='MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

##Perform linear regression using the lm() function. IN the lm() function, pass in all six variables (i.e., columns)
##and add the dataframe you created in Step 4 as the data parameter.
##Use the Linear Regression Function

lm (mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = Mechacar)

##Run a multiple Linear Regression RScript and use the Summary Function for multiple Linear Regression

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data = Mechacar))


##Delivery 2

Suspension_Coil <- read.csv(file='Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

total_summary <- Suspension_Coil %>% summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI))

lot_summary <- Suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI))


t.test(Suspension_Coil$PSI,mu = 1500)

t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

t.test(subset(Suspension_Coil,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
