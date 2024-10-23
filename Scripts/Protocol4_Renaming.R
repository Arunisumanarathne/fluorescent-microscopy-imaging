##########################################################
# Load the tidyverse library
##########################################################
# Install the tidyverse package if you have not already do so. 
# NOTE: You only need to install the package the first time you use it
# NOTE: Lines of code can be run by putting your cursor on the line to be executed and clicking on "Run" above, or hitting "ctrl + enter" ("cmd + return on a Mac). You can also highlight multiple lines of code and run them all together.
install.packages("tidyverse") 

# This line needs to be done every session to load the library
library("tidyverse")

##########################################################
# Load the Cell_counts.xlsx Excel file tab by tab 
##########################################################
# Import the first sheet (Day 1 data) from the Cell_counts.xlsx file located in the Data_in folder and store it as the YPD_D1 data frame.
# The data should all be collected in a single Excel worksheet, with cell counts from each day on different sheets sequentially, starting with the Day 1 data on sheet 1.

# First day of data collection
YPD_D1 <- readxl::read_xlsx ("Data_in/Cell_counts.xlsx", 
                             sheet = 1)

# Second day of data collection
YPD_D2 <- readxl::read_xlsx ("Data_in/Cell_counts.xlsx", 
                             sheet = 2) 

# Repeat the second line of code, changing the "DX" part as appropriate to include additional days of data, depending on your experiment. 
# You should be able to see data tables in the Environment Panel.


##########################################################
# Load the Image_names.xlsx Excel file tab by tab 
##########################################################
# The Image_names.xlsx file should similarly be imported sheet by sheet (increase the number of days, as required)

# First day of data collection
YPD_D1_imagenames <- readxl::read_xlsx ("Data_in/Image_names.xlsx", sheet = 1)

# Second day of data collection
YPD_D2_imagenames <- readxl::read_xlsx ("Data_in/Image_names.xlsx", sheet = 2)

##########################################################
# Removing the unwanted columns from the data tables
##########################################################
# The only essential columns in the YPD_D1 and YPD_D2 data frames are the Filename and two cell counts columns ( Fluor and NonFluor). 
# Select those columns from the data frame. 
# This will overwrite the YPD_D1 and YPD_2 data frames to keep just the selected columns. Do this for the data frame from each day all.

YPD_D1 <- YPD_D1 %>% select(Filename, Fluor, NonFluor)

YPD_D2 <- YPD_D2 %>% select(Filename, Fluor, NonFluor)

# Merge the data frames from Cell_counts.xlsx (YPD_D1) and Image_names.xlsx (YPD_D1_imagenames) to create a single file that contains with real image names. 
# Do this for the data collected from all days. 

YPD_D1_merged <- data.frame(YPD_D1, YPD_D1_imagenames)

YPD_D2_merged <- data.frame(YPD_D2, YPD_D2_imagenames)


