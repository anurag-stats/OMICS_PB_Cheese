## ---------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(janitor)
library(purrr)
library(readxl)

#' 
#' 
#' Loading the dataset:
## ---------------------------------------------------------------------------------------------------------------
df_customers <- read.csv("Data/PB.csv")
head(df_customers)

#' 
#' Sample v/s Cheese Eater Type Aroma Liking
## ---------------------------------------------------------------------------------------------------------------
df_customers %>%
  group_by(Sample, Cheese_eater_type) %>%
  summarise(
    mean_aroma_liking = mean(Aroma_Liking_A, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Sample, y = mean_aroma_liking, color = factor(Cheese_eater_type),
             group = Cheese_eater_type)) +
  geom_line() +
  labs(
    y = "Avg Rating",
    color = "Cheese_eater_type"
  ) +
  theme_minimal()

#' 
#' Summary statistics for Aroma Liking
## ---------------------------------------------------------------------------------------------------------------
df_customers %>%
  summarise(
    count = sum(!is.na(Aroma_Liking_A)),
    mean = mean(Aroma_Liking_A, na.rm = TRUE),
    sd = sd(Aroma_Liking_A, na.rm = TRUE),
    min = min(Aroma_Liking_A, na.rm = TRUE),
    q25 = quantile(Aroma_Liking_A, 0.25, na.rm = TRUE),
    median = median(Aroma_Liking_A, na.rm = TRUE),
    q75 = quantile(Aroma_Liking_A, 0.75, na.rm = TRUE),
    max = max(Aroma_Liking_A, na.rm = TRUE)
  )

#' 
#' 
#' Experience in PB Cheese:
## ---------------------------------------------------------------------------------------------------------------
df_customers %>%
  count(Experience_in_plant.based_cheese) %>%
  mutate(prop = n / sum(n))

#' 
#' 
#' Importing the raw data for Cheese chemical compositions:
## ---------------------------------------------------------------------------------------------------------------
df_cheese <- read_excel("Data/1_expanded_filled_uploaded_2.xlsx")
df_cheese <- df_cheese %>%
  rename(PrimaryID = `Primary ID`,
        Unique_Panelist_ID = `Unique Panelist ID`)
head(df_cheese)

#' 
#' Joining cheese and customer (consumer) data:
#' 
## ---------------------------------------------------------------------------------------------------------------
df_cheese_customer <- df_customers %>%
  inner_join(
    df_cheese,
    by = c("Unique_Panelist_ID" = "Unique_Panelist_ID",
           "Sample" = "Class")
  )

#' 
#' Number of null values in each column:
## ---------------------------------------------------------------------------------------------------------------
colSums(is.na(df_cheese_customer))[colSums(is.na(df_cheese_customer)) != 0]

#' 
#' Checking for missingness:
## ---------------------------------------------------------------------------------------------------------------
#setdiff(cols_of_interest, names(df_cheese_customer))
# Columns to check for missingness
cols_of_interest <- c(
  "Unique_Panelist_ID", "Sample_Name_A", "Sample", "Aroma_Liking_A",
  "Cheese_eater_type", "Frequency_of_dairy_cheese_consumption",
  "Frequency_of_PB_cheese", "Frequency_PB", "Diet", "Age",
  "Experience_in_plant.based_cheese", "Q11_1__Cheddar",
  "Q11_2__American", "Q11_3__Parmesan", "Q11_4__Gouda",
  "Q11_5__Mozzarella", "Q11_6__Blue_Cheese", "Q11_7__Pepper_Jack",
  "Q11_8__Feta", "Q11_9__Swiss", "Q11_10__Cream_cheese",
  "Q11___Other", "Q11___Other_COMMENTS", "PrimaryID",
  "Aroma_A", "Feature1_N", "Feature2_N", "Feature3_N", "Feature4_N",
  "Feature5_N", "Feature6_N", "Feature7_N", "Feature8_N", "Feature9_N",
  "Feature10_N", "Feature11_N", "Feature12_N", "Feature13_N",
  "Feature14_N", "Feature15_N", "Feature16_N", "Feature17_N",
  "Feature18_N", "Feature19_N", "Feature20_N", "Feature21_N",
  "Feature22_N", "Feature23_N", "Feature24_N", "Feature25_N",
  "Feature26_N", "Feature27_N", "Feature28_N", "Feature29_N",
  "Feature30_N", "Feature31_N", "Feature32_N", "Feature33_N",
  "Feature34_N", "Feature35_N", "Feature36_N", "Feature37_N",
  "Feature38_N", "Feature39_N", "Feature40_N", "Feature41_N",
  "Feature42_N", "Feature43_N", "Feature44_N", "Feature45_N",
  "Feature46_N", "Feature47_N", "Feature48_N", "Feature49_N",
  "Feature50_N", "Feature51_N"
)

# Rows where at least one of the columns of interest is missing
df_cust_cheese_na <- df_cheese_customer[
  apply(is.na(df_cheese_customer[, cols_of_interest]), 1, any),
]

# Rows where none of the columns of interest is missing
df_cust_chees_non_na <- df_cheese_customer[
  !apply(is.na(df_cheese_customer[, cols_of_interest]), 1, any),
]

# NA counts in the subset with missing values
na_counts <- colSums(is.na(df_cust_cheese_na))
na_counts[na_counts != 0]

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
# Inspect rows where Cheddar preference is missing
# This helps verify whether the missing preference data are tied to a specific sample mapping.
df_cust_cheese_na %>%
  filter(is.na(Q11_1__Cheddar)) %>%
  select(Sample, Sample_Name_A, PrimaryID, Blinding_Code_A) %>%
  distinct()

df_cust_cheese_na %>%
  filter(is.na(Q11_1__Cheddar)) %>%
  select(Sample, Sample_Name_A, PrimaryID, Unique_Panelist_ID) %>%
  distinct()

# Check which sample labels are associated with the missing Cheddar values
df_cust_cheese_na %>%
  filter(is.na(Q11_1__Cheddar)) %>%
  distinct(Sample)

# Inspect rows where Gouda preference is missing
df_cust_cheese_na %>%
  filter(is.na(Q11_4__Gouda)) %>%
  select(Sample, Sample_Name_A, PrimaryID, Blinding_Code_A) %>%
  distinct()



#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
# These values will be used to impute missing cheese preference data,
# for panelists whose preference responses are missing for Sample F.
# The logic here is:
# for each panelist, take the maximum observed value across repeated records,
# then use that value to fill in missing entries for the same panelist.

cust_cheese_tendencies <- df_cust_cheese_na %>%
  group_by(Unique_Panelist_ID) %>%
  summarise(
    across(
      c(
        Experience_in_plant.based_cheese,
        Q11_1__Cheddar,
        Q11_2__American,
        Q11_3__Parmesan,
        Q11_4__Gouda,
        Q11_5__Mozzarella,
        Q11_6__Blue_Cheese,
        Q11_7__Pepper_Jack,
        Q11_8__Feta,
        Q11_9__Swiss,
        Q11_10__Cream_cheese,
        Q11___Other
      ),
      ~ max(., na.rm = TRUE)
    ),
    .groups = "drop"
  )

cols_to_update <- c(
  "Experience_in_plant.based_cheese",
  "Q11_1__Cheddar",
  "Q11_2__American",
  "Q11_3__Parmesan",
  "Q11_4__Gouda",
  "Q11_5__Mozzarella",
  "Q11_6__Blue_Cheese",
  "Q11_7__Pepper_Jack",
  "Q11_8__Feta",
  "Q11_9__Swiss",
  "Q11_10__Cream_cheese",
  "Q11___Other"
)

# Update missing cheese preference values using the panelist-level summaries.
# Only NA values in df_cust_cheese_na are replaced; existing non-missing values stay unchanged.
df_cust_cheese_na <- df_cust_cheese_na %>%
  left_join(
    cust_cheese_tendencies,
    by = "Unique_Panelist_ID",
    suffix = c("", ".impute")
  ) %>%
  mutate(
    across(
      all_of(cols_to_update),
      ~ coalesce(., get(paste0(cur_column(), ".impute")))
    )
  ) %>%
  select(-ends_with(".impute"))

na_counts <- colSums(is.na(df_cust_cheese_na))
na_counts[na_counts != 0]

#' 
#' Dairy Cheese preference missing values have been imputed
#' 
## ---------------------------------------------------------------------------------------------------------------
#Checking for missing values in sample F
df_cust_cheese_na %>%
  filter(Sample == "F") %>%
  select(all_of(cols_to_update)) %>%
  summarise(across(everything(), ~ any(is.na(.))))

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
#Feature_47 has missing values - analyzing other variables for feature 47:
df_cust_cheese_na %>%
  filter(is.na(Feature47_N)) %>%
  select(Sample, Sample_Name_A, PrimaryID, Blinding_Code_A) %>%
  distinct()

#' 
#' All are from Sample G, specifically with primary ID G4.
#' 
## ---------------------------------------------------------------------------------------------------------------
df_cust_cheese_na %>%
  filter(Sample == "G") %>%
  select(PrimaryID, Sample_Name_A, Feature47_N)

#' 
## ---------------------------------------------------------------------------------------------------------------
#Impute missing data for Feature47_N with the average for sample G using the data we do have
#since the chemical compositions are similar within a particular sample.
idx <- is.na(df_cust_cheese_na$Feature47_N) & df_cust_cheese_na$Sample == "G"

g_mean <- mean(df_cheese$Feature47_N[df_cheese$Class == "G"], na.rm = TRUE)

df_cust_cheese_na$Feature47_N[idx] <- g_mean

df_cust_cheese_na[is.na(df_cust_cheese_na$Feature47_N),
                  c("Sample", "Sample_Name_A", "PrimaryID", "Blinding_Code_A")]

sum(is.na(df_cust_cheese_na$Feature47_N))

#' 
#' 
#' Making datasets:
## ---------------------------------------------------------------------------------------------------------------
df_cheese_customer_updated <- bind_rows(df_cust_chees_non_na, df_cust_cheese_na)


# Creating a subject table with one row per subject
cols_subjects <- c(
  "Unique_Panelist_ID", "Cheese_eater_type", "Frequency_of_dairy_cheese_consumption",
  "Frequency_of_PB_cheese", "Frequency_PB", "Diet", "Age",
  "Experience_in_plant.based_cheese",
  "Q11_1__Cheddar", "Q11_2__American", "Q11_3__Parmesan", "Q11_4__Gouda",
  "Q11_5__Mozzarella", "Q11_6__Blue_Cheese", "Q11_7__Pepper_Jack",
  "Q11_8__Feta", "Q11_9__Swiss", "Q11_10__Cream_cheese"
)

subject_preferences_df <- df_cheese_customer_updated %>%
  select(all_of(cols_subjects)) %>%
  distinct()


# Creating a chemicals table with PrimaryID-level data
cols_chemicals <- c(
  "Sample", "Class", "PrimaryID", "Feature1_N", "Feature2_N", "Feature3_N",
  "Feature4_N", "Feature5_N", "Feature6_N", "Feature7_N", "Feature8_N",
  "Feature9_N", "Feature10_N", "Feature11_N", "Feature12_N", "Feature13_N",
  "Feature14_N", "Feature15_N", "Feature16_N", "Feature17_N", "Feature18_N",
  "Feature19_N", "Feature20_N", "Feature21_N", "Feature22_N", "Feature23_N",
  "Feature24_N", "Feature25_N", "Feature26_N", "Feature27_N", "Feature28_N",
  "Feature29_N", "Feature30_N", "Feature31_N", "Feature32_N", "Feature33_N",
  "Feature34_N", "Feature35_N", "Feature36_N", "Feature37_N", "Feature38_N",
  "Feature39_N", "Feature40_N", "Feature41_N", "Feature42_N", "Feature43_N",
  "Feature44_N", "Feature45_N", "Feature46_N", "Feature47_N", "Feature48_N",
  "Feature49_N", "Feature50_N", "Feature51_N"
)

chemical_df <- df_cheese_customer_updated %>%
  select(all_of(cols_chemicals)) %>%
  distinct()


# Response table: subject-primaryID level feedback
feedback_cols <- c(
  "Unique_Panelist_ID", "Sample", "PrimaryID", "Aroma_A",
   "Aroma_Liking_F.x", "Flavor_Liking.x",
  "Aroma_JAR_A", "Comments_aroma_liking", "Comments_aroma_disliking", "Aroma_cheddar_like.x",
  "X1__American_Indian", "X2__Asian__or_Pacific_Islander", "X3__Black__African_American",
  "X4__Hispanic", "X5__White_", "X6__Other", "Blinding_Code_F", "Flavor_JAR",
  "Flavor_Cheddar_like", "Appearance_Liking", "Texture_Liking",
  "Overall_Liking", "Expectation"
)

subject_cheese_response <- df_cheese_customer_updated %>%
  select(all_of(feedback_cols)) %>%
  distinct()


#' 
## ---------------------------------------------------------------------------------------------------------------
write.csv(subject_preferences_df, "Subject_Data.csv", row.names = FALSE)
write.csv(chemical_df, "Cheese_Chemical_Data.csv", row.names = FALSE)
write.csv(subject_cheese_response, "Subject_wise_Cheese_Ratings.csv", row.names = FALSE)

