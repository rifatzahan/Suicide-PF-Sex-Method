library(csv)
library(ggplot2)
library(dplyr)
library(purrr)
library(gridExtra)

##############
### Method ###
##############

exportedMale <- read.csv("/Users/muhammadislam/Desktop/Rifat/PF/September/30Run10Kparticles-1/Method/exportedMale.csv", header = T)

# Convert rownames to a column
exportedMale <- exportedMale %>% 
  tibble::rownames_to_column(var = "row_names")


# First, let's split the data by 'Iteration'
list_of_dfs <- split(exportedMale, exportedMale$Iteration)

# Chained inner_join
exportedMale <- list_of_dfs[[1]] %>%
  inner_join(list_of_dfs[[2]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[3]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[4]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[5]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[6]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[7]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[8]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[9]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[10]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[11]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[12]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[13]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[14]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[15]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[16]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[17]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[18]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[19]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[20]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[21]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[22]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[23]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[24]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[25]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[26]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[27]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[28]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[29]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[30]], by = c("row_names", "Year"))




exportedMale <- as.data.frame(cbind(list_of_dfs[[1]], list_of_dfs[[2]], list_of_dfs[[3]],
                      list_of_dfs[[4]], list_of_dfs[[5]], list_of_dfs[[6]],
                      list_of_dfs[[7]], list_of_dfs[[8]], list_of_dfs[[9]],
                      list_of_dfs[[10]]))

#names(exportedMale)

# A helper function to rename duplicated columns
rename_duplicated <- function(df) {
  dupe_names <- names(df)[duplicated(names(df))]
  for (name in dupe_names) {
    name_indices <- which(names(df) == name)
    names(df)[name_indices] <- paste(name, seq_along(name_indices), sep = "_")
  }
  return(df)
}

# Use the helper function to rename duplicated columns
exportedMale <- rename_duplicated(exportedMale)

# Verify that the columns have been renamed
names(exportedMale)

# Select columns containing "Total"
total_cols <- exportedMale %>%
  select(starts_with("Total"))

# Calculate row-wise average
exportedMale$Total_Average <- rowMeans(total_cols, na.rm = TRUE)

# Verify that the columns have been renamed
names(exportedMale)

## Empirical data ##
exportMaleEmpirical <- read.csv("exportedMaleEmpirical.csv", header = T)

exportedMale <- exportedMale %>%
  mutate(color = ifelse(Year_1 > 2010, "after", "before"))

exportMaleEmpirical <- exportMaleEmpirical %>%
  mutate(color = ifelse(Year > 2010, "after", "before"))

p.male <- ggplot(exportedMale, aes(x=Year_1, y=Total_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportMaleEmpirical, aes(x=Year, y=Total, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5), 
                     labels = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Counts of suicide (Male)") + 
  theme_bw() +
  theme(legend.position = "none")

#############
## Poison ###
#############

## Empirical data ##
exportedMaleEmpiricalMethod <- read.csv("exportedMaleEmpiricalMethod.csv", header = T)

exportedMaleEmpiricalMethod <- exportedMaleEmpiricalMethod %>%
  mutate(color = ifelse(Year > 2010, "after", "before"))


# Select columns containing "Poison"
poison_cols <- exportedMale %>%
  select(starts_with("Poison"))

# Calculate row-wise average
exportedMale$Poison_Average <- rowMeans(poison_cols, na.rm = TRUE)

# Verify that the columns have been renamed
names(exportedMale)



p.poison.male <- ggplot(exportedMale, aes(x=Year_1, y=Poison_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedMaleEmpiricalMethod, aes(x=Year, y=Poison, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5), 
                     labels = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Poison") + 
  theme_bw() +
  theme(legend.position = "none")

#############
## Gases ###
#############

# Select columns containing "Gases"
gases_cols <- exportedMale %>%
  select(starts_with("Gases"))

# Calculate row-wise average
exportedMale$Gases_Average <- rowMeans(gases_cols, na.rm = TRUE)

# Verify that the columns have been renamed
names(exportedMale)



p.gases.male <- ggplot(exportedMale, aes(x=Year_1, y=Gases_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedMaleEmpiricalMethod, aes(x=Year, y=Gases, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5), 
                     labels = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Gases") + 
  theme_bw() +
  theme(legend.position = "none")



#############
## Hanging ###
#############

# Select columns containing "Hanging"
hanging_cols <- exportedMale %>%
  select(starts_with("Hanging"))

# Calculate row-wise average
exportedMale$Hanging_Average <- rowMeans(hanging_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedMale)



p.hanging.male <- ggplot(exportedMale, aes(x=Year_1, y=Hanging_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedMaleEmpiricalMethod, aes(x=Year, y=Hanging, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5), 
                     labels = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Hanging") + 
  theme_bw() +
  theme(legend.position = "none")




#############
## Drowning ###
#############

# Select columns containing "Drowning"
drowning_cols <- exportedMale %>%
  select(starts_with("Drowning"))

# Calculate row-wise average
exportedMale$Drowning_Average <- rowMeans(drowning_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedMale)



p.drowning.male <- ggplot(exportedMale, aes(x=Year_1, y=Drowning_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedMaleEmpiricalMethod, aes(x=Year, y=Drowning, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5), 
                     labels = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Drowning") + 
  theme_bw() +
  theme(legend.position = "none")





#############
## Firearms ###
#############

# Select columns containing "Firearms"
firearms_cols <- exportedMale %>%
  select(starts_with("Firearms"))

# Calculate row-wise average
exportedMale$Firearms_Average <- rowMeans(firearms_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedMale)



p.firearms.male <- ggplot(exportedMale, aes(x=Year_1, y=Firearms_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedMaleEmpiricalMethod, aes(x=Year, y=Firearms, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5), 
                     labels = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Firearms") + 
  theme_bw() +
  theme(legend.position = "none")





#############
## Cutting ###
#############

# Select columns containing "Cutting"
cutting_cols <- exportedMale %>%
  select(starts_with("Cutting"))

# Calculate row-wise average
exportedMale$Cutting_Average <- rowMeans(cutting_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedMale)



p.cutting.male <- ggplot(exportedMale, aes(x=Year_1, y=Cutting_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedMaleEmpiricalMethod, aes(x=Year, y=Cutting, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5), 
                     labels = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Cutting") + 
  theme_bw() +
  theme(legend.position = "none")




#############
## Jumping ###
#############

# Select columns containing "Jumping"
jumping_cols <- exportedMale %>%
  select(starts_with("Jumping"))

# Calculate row-wise average
exportedMale$Jumping_Average <- rowMeans(jumping_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedMale)



p.jumping.male <- ggplot(exportedMale, aes(x=Year_1, y=Jumping_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedMaleEmpiricalMethod, aes(x=Year, y=Jumping, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5), 
                     labels = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Jumping") + 
  theme_bw() +
  theme(legend.position = "none")



#############
## Other ###
#############

# Select columns containing "Other"
other_cols <- exportedMale %>%
  select(starts_with("Other"))

# Calculate row-wise average
exportedMale$Other_Average <- rowMeans(other_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedMale)



p.other.male <- ggplot(exportedMale, aes(x=Year_1, y=Other_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedMaleEmpiricalMethod, aes(x=Year, y=Other, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5), 
                     labels = seq(min(exportedMale$Year_1), max(exportedMale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Other") + 
  theme_bw() +
  theme(legend.position = "none")



# Specify the output PDF file and its dimensions
pdf("male_method.pdf", width = 7, height = 10)

grid.arrange(p.poison.male, p.gases.male, 
             p.hanging.male, p.drowning.male, 
             p.firearms.male, p.cutting.male, 
             p.jumping.male, p.other.male, 
             ncol = 2, nrow = 4)

dev.off()

#############
## Female ###
#############

exportedFemale <- read.csv("exportedFemale.csv", header = T)

# Convert rownames to a column
exportedFemale <- exportedFemale %>% 
  tibble::rownames_to_column(var = "row_names")


# First, let's split the data by 'Iteration'
list_of_dfs <- split(exportedFemale, exportedFemale$Iteration)

# Chained inner_join
exportedFemale <- list_of_dfs[[1]] %>%
  inner_join(list_of_dfs[[2]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[3]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[4]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[5]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[6]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[7]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[8]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[9]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[10]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[11]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[12]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[13]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[14]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[15]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[16]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[17]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[18]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[19]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[20]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[21]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[22]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[23]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[24]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[25]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[26]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[27]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[28]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[29]], by = c("row_names", "Year")) %>%
  inner_join(list_of_dfs[[30]], by = c("row_names", "Year"))


exportedFemale <- as.data.frame(cbind(list_of_dfs[[1]], list_of_dfs[[2]], list_of_dfs[[3]],
                                      list_of_dfs[[4]], list_of_dfs[[5]], list_of_dfs[[6]],
                                      list_of_dfs[[7]], list_of_dfs[[8]], list_of_dfs[[9]],
                                      list_of_dfs[[10]]))

names(exportedFemale)



# Use the helper function to rename duplicated columns
exportedFemale <- rename_duplicated(exportedFemale)

# Verify that the columns have been renamed
names(exportedFemale)

# Select columns containing "Total"
total_cols <- exportedFemale %>%
  select(starts_with("Total"))

# Calculate row-wise average
exportedFemale$Total_Average <- rowMeans(total_cols, na.rm = TRUE)

# Verify that the columns have been renamed
names(exportedFemale)

## Empirical data ##
exportFemaleEmpirical <- read.csv("exportedFemaleEmpirical.csv", header = T)

exportedFemale <- exportedFemale %>%
  mutate(color = ifelse(Year_1 > 2010, "after", "before"))

exportFemaleEmpirical <- exportFemaleEmpirical %>%
  mutate(color = ifelse(Year > 2010, "after", "before"))

p.female <- ggplot(exportedFemale, aes(x=Year_1, y=Total_Average/3.25)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportFemaleEmpirical, aes(x=Year, y=Total, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue1", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5), 
                     labels = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Counts of suicide (Female)") + 
  theme_bw() +
  theme(legend.position = "none")




##############################
## Method-specific, Female ###
##############################

## Empirical data ##
exportedFemaleEmpiricalMethod <- read.csv("exportedFemaleEmpiricalMethod.csv", header = T)

exportedFemaleEmpiricalMethod <- exportedFemaleEmpiricalMethod %>%
  mutate(color = ifelse(Year > 2010, "after", "before"))



#############
## Poison ###
#############

# Select columns containing "Poison"
poison_cols <- exportedFemale %>%
  select(starts_with("Poison"))

# Calculate row-wise average
exportedFemale$Poison_Average <- rowMeans(poison_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedFemale)

p.poison.female <- ggplot(exportedFemale, aes(x=Year_1, y=Poison_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedFemaleEmpiricalMethod, aes(x=Year, y=Poison, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5), 
                     labels = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Poison") + 
  theme_bw() +
  theme(legend.position = "none")


#############
## Gases ###
#############

# Select columns containing "Gases"
gases_cols <- exportedFemale %>%
  select(starts_with("Gases"))

# Calculate row-wise average
exportedFemale$Gases_Average <- rowMeans(gases_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedFemale)

p.gases.female <- ggplot(exportedFemale, aes(x=Year_1, y=Gases_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedFemaleEmpiricalMethod, aes(x=Year, y=Gases, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5), 
                     labels = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Gases") + 
  theme_bw() +
  theme(legend.position = "none")



#############
## Hanging ###
#############

# Select columns containing "Gases"
hanging_cols <- exportedFemale %>%
  select(starts_with("Hanging"))

# Calculate row-wise average
exportedFemale$Hanging_Average <- rowMeans(hanging_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedFemale)

p.hanging.female <- ggplot(exportedFemale, aes(x=Year_1, y=Hanging_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedFemaleEmpiricalMethod, aes(x=Year, y=Hanging, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5), 
                     labels = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Hanging") + 
  theme_bw() +
  theme(legend.position = "none")



#############
## Drowning ###
#############

# Select columns containing "Drowning"
drowning_cols <- exportedFemale %>%
  select(starts_with("Drowning"))

# Calculate row-wise average
exportedFemale$Drowning_Average <- rowMeans(drowning_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedFemale)

p.drowning.female <- ggplot(exportedFemale, aes(x=Year_1, y=Drowning_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedFemaleEmpiricalMethod, aes(x=Year, y=Drowning, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5), 
                     labels = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Drowning") + 
  theme_bw() +
  theme(legend.position = "none")



#############
## Firearms ###
#############

# Select columns containing "Firearms"
firearms_cols <- exportedFemale %>%
  select(starts_with("Firearms"))

# Calculate row-wise average
exportedFemale$Firearms_Average <- rowMeans(firearms_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedFemale)

p.firearms.female <- ggplot(exportedFemale, aes(x=Year_1, y=Firearms_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedFemaleEmpiricalMethod, aes(x=Year, y=Firearms, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5), 
                     labels = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Firearms") + 
  theme_bw() +
  theme(legend.position = "none")



#############
## Cutting ###
#############

# Select columns containing "Cutting"
cutting_cols <- exportedFemale %>%
  select(starts_with("Cutting"))

# Calculate row-wise average
exportedFemale$Cutting_Average <- rowMeans(cutting_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedFemale)

p.cutting.female <- ggplot(exportedFemale, aes(x=Year_1, y=Cutting_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedFemaleEmpiricalMethod, aes(x=Year, y=Cutting, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5), 
                     labels = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Cutting") + 
  theme_bw() +
  theme(legend.position = "none")




#############
## Jumping ###
#############

# Select columns containing "Jumping"
jumping_cols <- exportedFemale %>%
  select(starts_with("Jumping"))

# Calculate row-wise average
exportedFemale$Jumping_Average <- rowMeans(jumping_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedFemale)

p.jumping.female <- ggplot(exportedFemale, aes(x=Year_1, y=Jumping_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedFemaleEmpiricalMethod, aes(x=Year, y=Jumping, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5), 
                     labels = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Jumping") + 
  theme_bw() +
  theme(legend.position = "none")



#############
## Other ###
#############

# Select columns containing "Other"
other_cols <- exportedFemale %>%
  select(starts_with("Other"))

# Calculate row-wise average
exportedFemale$Other_Average <- rowMeans(other_cols, na.rm = TRUE)

# Verify that the columns have been renamed
# names(exportedFemale)

p.other.female <- ggplot(exportedFemale, aes(x=Year_1, y=Other_Average)) +
  geom_bin2d(bins = 150) +
  geom_point(data = exportedFemaleEmpiricalMethod, aes(x=Year, y=Other, color = color)) +
  scale_color_manual(values = c("before" = "red", "after" = "black")) +
  scale_fill_gradient(low = "lightblue", high = "blue", guide = "none") +  
  scale_x_continuous(breaks = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5), 
                     labels = seq(min(exportedFemale$Year_1), max(exportedFemale$Year_1), by = 5)) +
  geom_vline(xintercept = 2010, color = "green", linetype = "dashed", linewidth = 1) +
  xlab("Time (in years)") + 
  ylab("Other") + 
  theme_bw() +
  theme(legend.position = "none")





# Specify the output PDF file and its dimensions
pdf("female_method.pdf", width = 7, height = 10)

grid.arrange(p.poison.female, p.gases.female, 
             p.hanging.female, p.drowning.female, 
             p.firearms.female, p.cutting.female, 
             p.jumping.female, p.other.female, 
             ncol = 2, nrow = 4)

dev.off()


# Specify the output PDF file and its dimensions
pdf("deaths.pdf", width = 7, height = 10)

grid.arrange(p.male, p.female, 
             nrow = 2)

dev.off()
####################################
######### Latent States ############
####################################

