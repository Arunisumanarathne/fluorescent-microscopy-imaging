##########################################
# Load the tidyverse library
##########################################

# This line needs to be done every session to load the library
library(tidyverse)

##################################################################
# Merging Day 0 and Day 1 data tables to get the full data frame
##################################################################
# The next step is to merge across all days to get the complete data set for the experiment.
# Combine all data sets into a list (a data collection format that R uses to gather data frames together) called YPD_merged


YPD_merged <- list(YPD_D0_renamed, YPD_D1_renamed)

# Then create a single data frame called YPD_df by merging all the data frames into a single data frame

YPD_df <- Reduce(function(x, y) merge(x, y,
                                      by = "Isolate_name", 
                                      all = TRUE), YPD_merged)



##################################################################
# [Optional] Adding dilution factor to the data
##################################################################
# If the cells were diluted before collecting images, account for the dilution factor(s) here. 
# For example,  before getting the Day 1 images, we did a 1:100 dilution (Protocol 1, Steps 13 and 14). Therefore, we want to multiply all Day 1 counts by 100, the dilution factor. 

# Set the dilution factor. Change as appropriate if it is different than 1:100 
Dilutionfac_D1 <- 100/1 # dilution factor for day 1


# Add two new columns that take into account the dilution factor and then select the required columns to keep

YPD_dil <- YPD_df %>%
  mutate(Avg_Fluor_D1 = Avg_Fluor_D1 * Dilutionfac_D1) %>%
  mutate(Avg_NonFluor_D1 = Avg_NonFluor_D1 * Dilutionfac_D1) %>%
  select(Line_name, Avg_Fluor_D0, Avg_Fluor_D1, 
         Avg_NonFluor_D0, Avg_NonFluor_D1)

# Export the data frame as a .csv file (we will need this CSV file in Step 9)
write.csv(YPD_dil, file = "Data_out/YPD_dil.csv", 
          row.names = FALSE)


################################################################################
# Code for calculating r using the Malthusian parameter of each competitor 
################################################################################
# The selection rate (r) can be calculated as a proxy to competitive fitness by taking the differences in Malthusian parameters of two competitors. 
# Calculate the ratio between the cell count on Day 0 and Day 1 for Fluor and NonFluor. 
# We use the YPD_dil data frame from the step 3.
# Add two new columns, Fluor_ratio and NonFluor_ratio, by dividing Avg_Fluor_D1 by Avg_Fluor_D0 and Avg_NonFluor_D1 by Avg_NonFluor_D0 
# Select Isolate_name, Fluor_ratio and NonFluor_ratio to create a new data frame named YPD_ratio



YPD_ratio <- YPD_dil %>%
  mutate(Fluor_ratio = Avg_Fluor_D1 / Avg_Fluor_D0) %>%
  mutate(NonFluor_ratio = 
           Avg_NonFluor_D1/Avg_NonFluor_D0) %>%
  select(Line_name, Fluor_ratio, NonFluor_ratio)


##########################################################
# Calculating the Malthusian parameter for each competitor 
##########################################################
#Applying natural logarithms for the above ratio will give the Malthusian parameter for each competitor
# Transform the Fluor_ratio and NonFluor_ratio columns into their natural logarithms. Note that in R, “log” actually refers to the natural logarithm, not log base 10

YPD_ln <- YPD_ratio %>%
  mutate_at(vars(Fluor_ratio, NonFluor_ratio), log)


# Calculating the difference between the Malthusian parameters of the competitors will give you the value for r.
# Create a new column, r, by subtracting Fluor from NonFluor 
# Select the columns Real_name and r

YPD_r <- YPD_ln %>%
  mutate(r = NonFluor_ratio - Fluor_ratio) %>%
  select(Isolate_name, r)


# Get the mean of r
YPD_r_mean <- YPD_r %>%
  summarize(mean = mean(r, na.rm = TRUE))

# The number of columns/ data points in the final data frame will depend on the size of the experiment. 
# More days and replicates will add extra columns. 
# You can manipulate the same code to calculate r for all the days/ replicates accordingly.

######################################
# Plotting r with package ggplot2
######################################
# The code below uses the ggplot2 package in R. 
# Axis labels and aesthetics can be changed according to user preferences. 
# Before plotting the data, separate the Real_name column into two columns, Line_name and Replicate.

# Split the “Isolate_name” column 
YPD_r_detailed <- YPD_r %>%
  separate(Isolate_name, into = c("Replicate",  
                                  "Isolate"), sep = "_", remove = FALSE)


# Plot the data
# Change the parameters of the mapping function to alter the appearance of the data points. 
# The labs function allows you to change the axis labels to your preference.
# The theme function is responsible for the overall appearance of the graph, e.g., the position of the axis texts and titles).

YPD_r_plot <- ggplot(data = YPD_r_detailed, mapping = 
                       aes(x = Isolate_name, y = r)) +
  geom_jitter(width = 0.1,
              height = 0, 
              alpha = 0.6, 
              size=1,
              shape = 1,
              stroke = 1) +
  scale_y_continuous(breaks = seq (-3, 3,by = 1), 
                     limits = c (-3, 3)) +
  scale_x_discrete() +
  labs(x = "C. albicans isolate (YPD_10reps)", 
       y = "Competitive fitness (r)") + 
  theme_classic() + 
  theme(axis.text.x = element_text (hjust = 0.5, 
                                    face = "bold"), 
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(hjust = 0.5,
                                    face = "bold"), 
        axis.title.y = element_text (hjust = 0.9, 
                                     vjust = 1, 
                                     face = "bold")) +
  geom_hline(aes(yintercept = mean), 
             color = "darkblue", 
             linetype = "dashed",
             linewidth = 0.3)

# Saving the plot 
# You can specify the plot type, width and height of the plot.
ggsave ("Figures/YPD_r_plot.png",
        plot = p, 
        width = 6, 
        height = 4)


###############################################
# Code for calculating competitive fitness (m) 
##############################################
# We perform a nonlinear least square (NLS) fitting on the above data set (Cell_counts.xlsx), which will estimate the best-fitting parameters for the initial fraction of the competing population (p0) and competitive fitness (m) for each competitor to a nonlinear model. 
# This will give results that are approximately similar to the method described in step 6. 
# In the NLS method, all data from replicates is combined into a single data frame for model fitting.
# Create a single data frame that contains information from all experiment days and replicates  
# We need to rearrange the data for the next analysis step. The easiest way to do this is to export the data frame as a .csv file, save it to your computer, and edit it in Excel. It is also possible to do this in R. 
# We use the YPD_dil.csv from step 2.
# We must create a spreadsheet with Strain, Replicate, day, Fluor, NonFlour and fracNonFluor columns. These exact column names must be used for the NLS model code in the next step to work unless you change the parameter names in the model.
# Save the spreadsheet as YPD_fracNonFlour.csv in the Data_out folder. 
# Values in the fracNonFluor column are calculated by getting the fraction of NonFluor cells (e.g., fracNonFluor = NonFluor/ [Fluor+NonFluor]) (Figure 9).

# Load the spreadsheet into R studio
# Import the new .csv file into R. 

YPD <- read_csv("Data_out/YPD_fracNonFlour.csv")

# Declaring the function for NLS fitting
eqn <- function(p0,mmm,day){
  ((p0*exp(mmm*day))/(1-p0+p0*exp(mmm*day)))
}

# Create empty vectors for “p0”, “mmm”, and “names” 
p0<-c()
mmm<-c()
names<-c()

YPD_nls <- function(dataset){
  nls(fracNonFluor~eqn(p0,mmm,day), data=dataset, 
      start=list(p0=0.4, mmm=0.1))
}

# “start = list(p0 = 0.4, mmm = 0.1)” provides the initial values for the parameters to start the process. 
# If you find issues with model fit you can try and change these parameters depending on your expectations for the initial ratio of non-fluorescing and fluorescing cells (p0) and the competitive fitness of non-fluorescing cells (mmm).

# Subset the dataset by strain
# In this example, we have six isolates (one for each unique competitor); adjust as necessary. 
# Create one data frame for each isolate.

YPD_1 <- subset(YPD, Strain == 1)
YPD_2 <- subset(YPD, Strain == 2)
YPD_3 <- subset(YPD, Strain == 3)
YPD_4 <- subset(YPD, Strain == 4)
YPD_5 <- subset(YPD, Strain == 5)
YPD_6 <- subset(YPD, Strain == 6)

#Fit the NLS model to the data from each isolate. 

YPD_1_par <- compFit_nls(YPD_1)
YPD_2_par <- compFit_nls(YPD_2)
YPD_3_par <- compFit_nls(YPD_3)
YPD_4_par <- compFit_nls(YPD_4)
YPD_5_par <- compFit_nls(YPD_5)
YPD_6_par <- compFit_nls(YPD_6)

# Calculating the Confidence Intervals (CI) in non-linear regression
# We use the confint2 function in the nlstools package (Baty et al., 2015) to calculate 95% confidence intervals for the parameter estimates.

install.packages("nlstools2")
library(nlstools) 

# Calculating the CI
confint1 <- confint2(YPD_1_par)
confint2 <- confint2(YPD_2_par)
confint3 <- confint2(YPD_3_par)
confint4 <- confint2(YPD_4_par)
confint5 <- confint2(YPD_5_par)
confint6 <- confint2(YPD_6_par)

#Initialize an empty data frame
#We want to keep the confidence values related to m from the above matrixes. Therefore, we need to extract those and create a new data frame. The necessary values will be located in the last row of each matrix.
# Create an empty data frame named CI with three columns: Strain (a character column with Strain names), CI2.5 (a double column to store the lower bound of 2.5% CI), and CI97.5 (a double column to store the upper bound of 97.5% CI). This table will store the CI for each strain.

CI <- data.frame (strain=character(), CI2.5=double(),   
                  CI97.5=double()) 

# Create a list of CI

# Create a list named confints containing the names of six CI matrixes created above. Each of these objects contains the CI data for different strains.

confints <- list(confint1, confint2, confint3, confint4, 
                 confint5, confint6)

# Loop through the list and combine CI into one data table
# After the loop completes, the CI data frame will contain six rows, each corresponding to a different strain, with their respective 2.5% and 97.5% confidence intervals.
# The loop iterates over a list of confidence intervals, appending a new row to the data frame CI in each iteration, where each row contains the current index and the last value of the corresponding confidence interval list. If you have a different number of samples than in the example, update the 6 in the for loop as appropriate to your study.

for(i in 1:6){ 
  CI <- rbind(CI, c(i, tail(confints[[i]], n=1))) 
}

# Change the column names of the CI data frame to what we want. 
colnames(CI) <- c("Strain", "CI2.5", "CI97.5")

# Extracting the coefficient values
# We need to extract CI values to create a new dataset. Since it is the second coefficient, we must specify that in the code.
# Extract second coefficient values (mmm) from each fitted model and corresponding Strain name to create a new data frame named Coeff_df. 

Coeff_df <- data.frame(Strain = 1:6, 
                       m = c(coef(YPD_1_par)[2], 
                             coef(YPD_2_par)[2], coef(YPD_3_par)[2], 
                             coef(YPD_4_par)[2], coef(YPD_5_par)[2], 
                             coef(YPD_6_par)[2]))



# Plotting the data using the ggplot2 package in R
# Convert the Strain variable to a factor for plotting purposes.
# Coeff_df$Strain <- as.factor(Coeff_df$Strain)

#Axis labels and aesthetics can be changed according to preferences.

library(tidyverse)

# By changing the parameters of the “mapping” function, you can change the appearance of the data points. 
# “labs” function allows you to change the axis labels to your preference.
# “theme” function is responsible for the overall appearance of the graph (e.g. position of the axis texts and titles)

YPD_comfit_plot <- ggplot() +
  geom_sina(data = Coeff_df,
            mapping = aes(x = Strain, y = m, colour = Strain),
            width = 0.1, height = 0, alpha = 0.6, size=3, shape = 1, stroke = 1) +
  geom_jitter() +
  geom_errorbar(data = CI , aes(x = Strain, ymin = CI2.5, ymax = CI97.5), width = 0.1, position = position_dodge(0.9), color = "black", size = 0.3)+
  scale_y_continuous (breaks = seq (-3, 3, by = 1),   
                      limits = c (-3, 3)) +
  scale_colour_manual(values = c("#38040E","#800E13",
                                 "#880E4F","#E66169", "#996A6A","#B89896")) +
  scale_x_discrete() +
  labs(x = "C. albicans line (YPD_10reps)", 
       y = "Competitive fitness (m)") + 
  theme_classic() + 
  theme(axis.text.x = element_text (hjust = 0.5, 
                                    face =  "bold"),
        axis.text.y = element_text(face =  "bold"),
        axis.title.x = element_text (hjust = 0.5,
                                     face = "bold"),
        axis.title.y = element_text(hjust = 0.9, 
                                    vjust = 1, 
                                    face = "bold")) +
  geom_hline(aes(yintercept = 0), 
             color = "darkblue",
             linetype = "dashed", 
             linewidth = 0.3)

# Saving the plot 
# You can specify the plot type, width and height of the plot.

ggsave ("Figures/ YPD_comfit_plot.PNG", 
        plot = p,
        width = 6, 
        height = 4)







