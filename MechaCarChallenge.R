# MPG Regression

# read csv file
MechaCar_mpg <- read_csv("MechaCar_mpg.csv")
suspension <- read_csv("Suspension_Coil.csv")
head(MechaCar_mpg)

# normality test
shapiro.test(MechaCar_mpg$mpg)
#visualize distribution using density plot
ggplot(MechaCar_mpg,aes(mpg)) + geom_density() 

#convert data frame into numeric matrix
used_matrix <- as.matrix(MechaCar_mpg[,c("vehicle length","vehicle weight","spoiler angle", "ground clearance", "mpg")]) 
#calculate r
cor(used_matrix)

# create a multiple linear model
lm(mpg ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance`,data = MechaCar_mpg)
#summarize multiple linear model
summary(model)

#summary statistics table
summarize_sus <- suspension %>%
  summarise(Mean_PSI = mean(PSI), Median_PSI = median(PSI),Variance_PSI = var(PSI),Stdev_PSI =sd(PSI))
# summary of manufactoring lot
group_summarize_sus <- suspension  %>% group_by(Manufacturing_Lot) %>%
  summarise(Mean_PSI = mean(PSI), Median_PSI = median(PSI),Variance_PSI = var(PSI),Stdev_PSI =sd(PSI))

#one-sample t-test
t.test(suspension$PSI, mu=1500)
