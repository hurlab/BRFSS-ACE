################################################################################
# ADVERSE CHILDHOOD EXPERIENCES AND CHRONIC HEALTH OUTCOMES
#
# This repository contains the R code for the manuscript titled:
# "ADVERSE CHILDHOOD EXPERIENCES AND CHRONIC HEALTH OUTCOMES: EVIDENCE FROM 33 US STATES 
# IN THE BEHAVIORAL RISK FACTOR SURVEILLANCE SYSTEM, 2019–2023"
################################################################################
################################################################################
# Note: 4/2/2025: The current version is fully working; however, we are 
#                 in process of splitting this one into multiple scripts
#                 for better readability and maintainability.
################################################################################



################################################################################
# SECTION 1
################################################################################

#SET WORKING DIRECTORY
library(rstudioapi) 
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
base_dir = dirname(current_path)
BRFSSdata_dir = paste0(base_dir, "/BRFSSdata/") # Directory where the BRFSS data files are stored

#Lets load the package
required_packages <- c("tidyverse", "gtsummary", "ggpubr", "dplyr","stringr", "psych", "RColorBrewer", "ggplot2","table1", "gt", 
                       "webshot2", "grid","ggcorrplot","DataExplorer", "gridExtra","haven","lubridate", "foreign", "rlang", "glmnet",
                       "GGally","hrbrthemes", "ggcorrplot","naniar", "survey", "car", "pROC", "epitools", "broom", "scales", "MASS",
                       "ROCR", "caret", "kableExtra", "FSA", "tictoc", "flextable", "crosstable", "officer", "corrplot", "usmap")

# Install and load required packages
for (pkg in required_packages) {
  # check whether the package is already installed 
  if (!(pkg %in% installed.packages())) {
    #If the package is not installed, we proceed to install it 
    install.packages(pkg)
  }
  # check if the package can be loaded 
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    #  if so, load it 
    library(pkg, character.only = TRUE)
  }
}


# Functions to use in this script

# Function to display empty cells for non-significant correlations
empty_if_not_significant <- function(cor_matrix, p_mat, threshold = 0.05) {
  # Fill the correlation matrix cells with NA where p-value is greater than the threshold
  cor_matrix[p_mat > threshold] <- NA
  
  # Replace non-significant p-values with NA
  p_mat[p_mat > threshold] <- NA
  
  return(list(cor_matrix = cor_matrix, p_mat = p_mat))
}

# Function to plot bar chart showing counts -it removes NA first
create_bar_plot <- function(data, column, title, x_label) {
  # Remove both string 'NA' and actual NA values
  filtered_data <- data[!is.na(data[[column]]) & data[[column]] != 'NA', ]
  
  # Create the plot with different colors
  ggplot(filtered_data, aes_string(x = column)) +66056
  geom_bar(aes(fill = factor(..x..)), show.legend = FALSE) +  # Different color for each bar
    theme_minimal() +
    labs(title = title, x = x_label, y = "Count") +
    scale_y_continuous(labels = scales::comma) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14), # Make x-axis labels bold and bigger
      axis.text.y = element_text(face = "bold", size = 14), # Make y-axis labels bold and bigger
      axis.title.x = element_text(face = "bold", size = 16), # Make x-axis title bold and bigger
      axis.title.y = element_text(face = "bold", size = 16), # Make y-axis title bold and bigger
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5) # Make plot title bold, bigger, and centered
    )
}


# Function to create a bar plot showing proportions
create_prop_bar_plot <- function(data, variable, plot_title, x_label) {
  
  # Remove both string 'NA' and actual NA values
  filtered_data <- data[!is.na(data[[variable]]) & data[[variable]] != 'NA', ]
  
  # Calculate proportions
  plot_data <- filtered_data %>%
    group_by(!!sym(variable)) %>%
    summarise(count = n()) %>%
    mutate(proportion = count / sum(count))  # Calculate proportion
  
  # Create bar plot with proportions
  ggplot(plot_data, aes(x = !!sym(variable), y = proportion, fill = !!sym(variable))) +  # Fill aesthetic set to same variable as x
    geom_bar(stat = "identity") +  # Bar plot with identity for proportions
    labs(
      title = plot_title,
      x = x_label,
      y = "Proportion",
      fill = x_label  # Set the legend title same as x-axis title
    ) +
    theme_bw() +  # Plain white background
    theme(
      axis.text.x = element_text(face = "bold", size = 16, angle = 45, hjust = 1),  # Adjust x-axis labels
      axis.text.y = element_text(face = "bold", size = 16),  
      axis.title.x = element_text(face = "bold", size = 20),  
      axis.title.y = element_text(face = "bold", size = 20),  
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),  # Center plot title
      legend.title = element_text(size = 16, face = "bold"),  # Adjust legend title size
      legend.text = element_text(size = 14)  # Adjust legend text size
    ) +
    scale_y_continuous(labels = scales::percent)  # Show proportions as percentages
}

#Rename data
rename_columns <- function(df) {
  # Start timing
  tic()
  
  renamed_data <- df %>%
    rename(
      general_health = GENHLTH, 
      physical_health = PHYSHLTH, 
      mental_health = MENTHLTH, 
      heart_attack = CVDINFR4, 
      coronary_heart_disease = CVDCRHD4, 
      stroke = CVDSTRK3,
      asthma = ASTHMA3,
      sex_orient_m = SOMALE,
      skin_cancer = CHCSCNCR,
      other_cancer = CHCOCNCR, 
      sex_orient_f = SOFEMALE,
      copd = CHCCOPD2,  # Chronic Obstructive Pulmonary Disease 
      heavy_drinker = X_RFBING5,
      depressive_disorder = ADDEPEV3, 
      kidney_disease = CHCKDNY2, 
      diabetes = DIABETE4, 
      arthritis = HAVARTH4, 
      transgender = TRNSGNDR,
      marital_status = MARITAL, 
      employment_status = EMPLOY1, 
      weight = WEIGHT2,
      height = HEIGHT3,
      days_you_smoke = SMOKDAY2,
      tobacco_use = USENOW3, 
      average_drink = AVEDRNK3, 
      days_mhealth = X_MENT14D,
      gender = X_SEX, 
      BMI = X_BMI5CAT, 
      education_level = X_EDUCAG, 
      income_level = X_INCOMG,  
      smoking_status = X_SMOKER3,
      age_group = X_AGE_G,
      race = X_RACE,
      survey_weights = X_LLCPWT,
      health_coverage = X_HCVU651,
      veteran_status = VETERAN3,
      bmi_above_25 = X_RFBMI5,
      age = X_AGE80
    )
  
  # Stop timing and store elapsed time
  elapsed_time <- toc(quiet = TRUE)
  cat("After running the code", elapsed_time$callback_msg, "before getting the result \n")
  
  # View the renamed columns
  glimpse(renamed_data)
  
  return(renamed_data)
}

# Recode data using this function
recode_variables <- function(df) {
  # Start timing
  tic()
  
  # Recoding with corrected handling of factor and character types
  recoded_data <- df %>%
    mutate(
      general_health = fct_recode(
        factor(general_health),
        "excellent" = "1",
        "very good" = "2",
        "good" = "3",
        "fair" = "4",
        "poor" = "5"
      ),
      general_health = na_if(as.character(general_health), "7") %>%
        na_if("9") %>% factor() %>% droplevels(),
      
      physical_health = replace(physical_health, physical_health == 88, 0) %>%
        replace(., . %in% c(77, 99), NA),
      mental_health = replace(mental_health, mental_health == 88, 0) %>%
        replace(., . %in% c(77, 99), NA),
      
      across(c(heart_attack, coronary_heart_disease, stroke, asthma, skin_cancer, other_cancer, copd, depressive_disorder,
               kidney_disease, arthritis),
             ~ fct_recode(factor(.),
                          "yes" = "1",
                          "no" = "2")),
      across(c(heart_attack, coronary_heart_disease, stroke, asthma, skin_cancer, other_cancer, copd, depressive_disorder,
               kidney_disease, arthritis),
             ~ na_if(as.character(.), "7") %>% na_if("9") %>% factor() %>% droplevels()),
      
      sex_orient_m = fct_recode(factor(sex_orient_m),
                                "gay" = "1",
                                "straight" = "2",
                                "bisexual" = "3",
                                "somethingelse" = "4"),
      sex_orient_m = na_if(as.character(sex_orient_m), "7") %>%
        na_if("9") %>% factor() %>% droplevels(),
      
      sex_orient_f = fct_recode(factor(sex_orient_f),
                                "lesbian" = "1",
                                "straight" = "2",
                                "bisexual" = "3",
                                "somethingelse" = "4"),
      sex_orient_f = na_if(as.character(sex_orient_f), "7") %>%
        na_if("9") %>% factor() %>% droplevels(),
      
      heavy_drinker = fct_recode(factor(heavy_drinker),
                                 "no" = "1",
                                 "yes" = "2"),
      heavy_drinker = na_if(as.character(heavy_drinker), "9") %>%
        factor() %>% droplevels(),
      
      bmi_above_25 = fct_recode(factor(bmi_above_25),
                                "no" = "1",
                                "yes" = "2"),
      bmi_above_25 = na_if(as.character(bmi_above_25), "9") %>%
        factor() %>% droplevels(),
      
      diabetes = fct_recode(factor(diabetes),
                            "yes" = "1",
                            "gestational" = "2",
                            "no" = "3",
                            "pre-diabetes" = "4"),
      diabetes = na_if(as.character(diabetes), "7") %>%
        na_if("9") %>% factor() %>% droplevels(),
      
      health_coverage = fct_recode(factor(health_coverage),
                                   "yes" = "1",
                                   "no" = "2"),
      health_coverage = na_if(as.character(health_coverage), "7") %>%
        na_if("9") %>% factor() %>% droplevels(),
      
      veteran_status = fct_recode(factor(veteran_status),
                                  "yes" = "1",
                                  "no" = "2"),
      veteran_status = na_if(as.character(veteran_status), "7") %>%
        na_if("9") %>% factor() %>% droplevels(),
      
      marital_status = fct_recode(factor(marital_status),
                                  "married" = "1",
                                  "divorced" = "2",
                                  "widowed" = "3",
                                  "separated" = "4",
                                  "never married" = "5",
                                  "unmarried couple" = "6"),
      marital_status = na_if(as.character(marital_status), "9") %>%
        factor() %>% droplevels(),
      
      employment_status = fct_recode(factor(employment_status),
                                     "employed" = "1",
                                     "self-employed" = "2",
                                     "out of work > 1 yr" = "3",
                                     "out of work < 1 yr" = "4",
                                     "homemaker" = "5",
                                     "student" = "6",
                                     "retired" = "7",
                                     "unable" = "8"),
      employment_status = na_if(as.character(employment_status), "9") %>%
        factor() %>% droplevels(),
      
      income_level = fct_recode(factor(income_level),
                                "< 15k" = "1",
                                "15k – <25k" = "2",
                                "25k – <35k" = "3",
                                "35k – <50k" = "4",
                                "≥ 50k" = "5",
                                "≥ 50k" = "6",
                                "≥ 50k" = "7"),
      income_level = na_if(as.character(income_level), "9") %>%
        factor() %>% droplevels(),
      
      days_you_smoke = fct_recode(factor(days_you_smoke),
                                  "every day" = "1",
                                  "some days" = "2",
                                  "not at all" = "3"),
      days_you_smoke = na_if(as.character(days_you_smoke), "7") %>%
        na_if("9") %>% factor() %>% droplevels(),
      
      tobacco_use = fct_recode(factor(tobacco_use),
                               "every day" = "1",
                               "some days" = "2",
                               "not at all" = "3"),
      tobacco_use = na_if(as.character(tobacco_use), "7") %>%
        na_if("9") %>% factor() %>% droplevels(),
      
      average_drink = replace(average_drink, average_drink == 88, 0) %>%
        replace(., . %in% c(77, 99), NA),
      
      gender = fct_recode(factor(gender),
                          "male" = "1",
                          "female" = "2"),
      BMI = fct_recode(factor(BMI),
                       "underweight" = "1",
                       "normal" = "2",
                       "overweight" = "3",
                       "obese" = "4"),
      education_level = fct_recode(factor(education_level),
                                   "nt grad high sc" = "1",
                                   "grad high sc" = "2",
                                   "attended college/tech" = "3",
                                   "grad college/tech" = "4"),
      education_level = na_if(as.character(education_level), "9") %>%
        factor() %>% droplevels(),
      
      smoking_status = fct_recode(factor(smoking_status),
                                  "cr smoker - evryday" = "1",
                                  "cr smoker - somedays" = "2",
                                  "former smoker" = "3",
                                  "never smoked" = "4"),
      smoking_status = na_if(as.character(smoking_status), "9") %>%
        factor() %>% droplevels(),
      
      age_group = fct_recode(factor(age_group),
                             "18 - 24yrs" = "1",
                             "25 - 34yrs" = "2",
                             "35 - 44yrs" = "3",
                             "45 - 54yrs" = "4",
                             "55 - 64yrs" = "5",
                             "65+ yrs" = "6"),
      race = fct_recode(factor(race),
                        "white" = "1",
                        "black" = "2",
                        "AIAN" = "3",
                        "asian" = "4",
                        "other" = "5",
                        "other" = "6",
                        "other" = "7",
                        "hispanic" = "8"),
      race = na_if(as.character(race), "9") %>%
        factor() %>% droplevels()
    )
  
  # Stop timing and store elapsed time
  elapsed_time <- toc(quiet = TRUE)
  cat("After running the code", elapsed_time$callback_msg, "before getting the result \n")
  
  # Check the updated data
  glimpse(recoded_data)
  
  return(recoded_data)
}


# Function to calculate ACE scores:
calculate_ACE_scores <- function(df) {
  # Start timing
  tic()
  
  # Calculate ACE variables and scores
  df <- df %>%
    mutate(
      # 1. Physical Abuse (ACEHURT1)
      abuse_physical = ifelse(ACEHURT1 %in% c(2, 3), 1, ifelse(ACEHURT1 == 1, 0, NA)),
      
      # 2. Sexual Abuse (ACETOUCH, ACETTHEM, ACEHVSEX)
      abuse_sexual = ifelse(ACETOUCH %in% c(2, 3) | ACETTHEM %in% c(2, 3) | ACEHVSEX %in% c(2, 3), 1, 
                            ifelse(ACETOUCH == 1 & ACETTHEM == 1 & ACEHVSEX == 1, 0, NA)),
      
      # 3. Emotional Abuse (ACESWEAR)
      abuse_emotional = ifelse(ACESWEAR %in% c(2, 3), 1, ifelse(ACESWEAR == 1, 0, NA)),
      
      # 4. Household Mental Health Dysfunction (ACEDEPRS)
      household_mental = ifelse(ACEDEPRS == 1, 1, ifelse(ACEDEPRS == 2, 0, NA)),
      
      # 5. Substance Abuse in Household (ACEDRINK, ACEDRUGS)
      household_substance = ifelse(ACEDRINK == 1 | ACEDRUGS == 1, 1, 
                                   ifelse(ACEDRINK == 2 & ACEDRUGS == 2, 0, NA)),
      
      # 6. Incarcerated Household Member (ACEPRISN)
      household_incarcerated = ifelse(ACEPRISN == 1, 1, ifelse(ACEPRISN == 2, 0, NA)),
      
      # 7. Violence Between Adults in Household (ACEPUNCH)
      household_violence = ifelse(ACEPUNCH %in% c(2, 3), 1, ifelse(ACEPUNCH == 1, 0, NA)),
      
      # 8. Parental Separation or Divorce (ACEDIVRC)
      parental_separation = ifelse(ACEDIVRC == 1, 1, ifelse(ACEDIVRC == 2, 0, NA))
    ) %>%
    
    # Remove rows with NA values across specified ACE categories
    filter(
      !is.na(abuse_physical) &
        !is.na(abuse_sexual) &
        !is.na(abuse_emotional) &
        !is.na(household_mental) &
        !is.na(household_substance) &
        !is.na(household_incarcerated) &
        !is.na(household_violence) &
        !is.na(parental_separation)
    ) %>%
    
    # Calculate ACE score (scaled to adjust for range variation)
    rowwise() %>%
    mutate(ACE_score = sum(c_across(c("abuse_physical", "abuse_sexual", "abuse_emotional", 
                                      "household_mental", "household_substance", 
                                      "household_incarcerated", "household_violence", 
                                      "parental_separation")), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Stop timing and store elapsed time
  elapsed_time <- toc(quiet = TRUE)
  cat("After running the code", elapsed_time$callback_msg, "before getting the result \n")
  
  # Return the entire dataset with the new columns appended
  return(df)
}

# Function to calculate prevalence of at least one ACE or none
calculate_ACE_experience_counts <- function(df) {
  ace_category_columns <- c("abuse_physical", "abuse_sexual", "abuse_emotional", 
                            "household_mental", "household_substance", 
                            "household_incarcerated", "household_violence", 
                            "parental_separation")
  
  at_least_one_experience <- sum(rowSums(df[ace_category_columns] == 1, na.rm = TRUE) > 0)
  all_no_experience <- sum(rowSums(df[ace_category_columns] == 1, na.rm = TRUE) == 0)
  
  return(list(at_least_one_experience = at_least_one_experience, all_no_experience = all_no_experience))
}

# Function to group ACE score
group_ACE_score <- function(df) {
  df <- df %>%
    mutate(
      ACE_group = case_when(
        ACE_score == 0 ~ "None",
        ACE_score >= 1 & ACE_score <= 2 ~ "Low ACE (1-2)",
        ACE_score >= 3 ~ "High ACE (3+)"
      )
    )
  return(df)
}

# Function to calculate prevalence of individual ACEs
calculate_individual_ACE_prevalence <- function(df) {
  prevalence_list <- list(
    emotional_abuse = mean(df$abuse_emotional == 1, na.rm = TRUE) * 100,
    physical_abuse = mean(df$abuse_physical == 1, na.rm = TRUE) * 100,
    substance_abuse = mean(df$household_substance == 1, na.rm = TRUE) * 100,
    sexual_abuse = mean(df$abuse_sexual == 1, na.rm = TRUE) * 100
  )
  return(prevalence_list)
}

# Function to calculate overall ACE prevalence
calculate_overall_ACE_prevalence <- function(df) {
  list(
    one_or_more_ACE = mean(df$ACE_score >= 1, na.rm = TRUE) * 100,
    two_or_more_ACE = mean(df$ACE_score >= 2, na.rm = TRUE) * 100,
    four_or_more_ACE = mean(df$ACE_score >= 4, na.rm = TRUE) * 100
  )
}

# Function to analyze gender and race prevalence of 4+ ACEs
analyze_gender_race_four_or_more_ACE <- function(df) {
  df <- df %>%
    mutate(four_or_more_aces = ifelse(ACE_score >= 4, "Yes", "No"))
  
  gender_summary <- df %>%
    group_by(gender) %>%
    summarise(
      total = n(),
      four_or_more_aces_count = sum(four_or_more_aces == "Yes", na.rm = TRUE),
      four_or_more_aces_prop = mean(four_or_more_aces == "Yes", na.rm = TRUE)
    )
  
  race_summary <- df %>%
    group_by(race) %>%
    summarise(
      total = n(),
      four_or_more_aces_count = sum(four_or_more_aces == "Yes", na.rm = TRUE),
      four_or_more_aces_prop = mean(four_or_more_aces == "Yes", na.rm = TRUE)
    )
  
  return(list(gender_summary = gender_summary, race_summary = race_summary))
}

# Function to create single column for sexual orientation
create_sexual_orientation <- function(df) {
  df <- df %>%
    mutate(
      sexual_orientation = case_when(
        sex_orient_m == "straight" | sex_orient_f == "straight" ~ "straight",
        sex_orient_m == "gay" | sex_orient_f == "lesbian" ~ "gay/lesbian",
        sex_orient_m == "bisexual" | sex_orient_f == "bisexual" ~ "bisexual",
        transgender == 1 | transgender == 2 | transgender == 3 ~ "transgender",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-sex_orient_m, -sex_orient_f, -transgender)
  return(df)
}

# Function to adjust weight and height variables
adjust_weight_height <- function(df) {
  df <- df %>%
    mutate(
      weight = as.numeric(as.character(weight)),
      weight = case_when(
        weight >= 50 & weight < 1000 ~ round(weight / 2.2),
        weight >= 9000 & weight < 9999 ~ weight - 9000,
        TRUE ~ as.numeric(NA)
      ),
      height = case_when(
        height >= 50 & height < 1000 ~ round(((height %/% 100) * 12 + (height %% 100)) * 2.54),
        height >= 9000 & height < 9999 ~ height - 9000,
        TRUE ~ as.numeric(NA)
      )
    )
  return(df)
}

# Function to calculate weighted prevalence and confidence intervals for ACE variables
calculate_weighted_prevalence <- function(df, ace_variables, id_col, weight_col) {
  # Set up survey design with weights
  design <- svydesign(ids = ~get(id_col), data = df, weights = ~get(weight_col))
  
  # Define a helper function for individual ACE prevalence calculation
  calc_prevalence <- function(var) {
    result <- svymean(~get(var), design = design, na.rm = TRUE)
    data.frame(
      Variable = var,
      Prevalence = round(100 * coef(result), 1),
      CI_low = round(100 * confint(result)[, 1], 1),
      CI_high = round(100 * confint(result)[, 2], 1)
    )
  }
  
  # Calculate prevalence for each ACE variable
  prevalence_results <- bind_rows(lapply(ace_variables, calc_prevalence))
  
  return(prevalence_results)
}

# Function to create socio-demographic tables by ACE group
create_socio_demographic_table <- function(df, demographic_vars, ace_group_col) {
  # Ensure ACE group is a factor with proper levels
  df <- df %>%
    mutate(
      !!ace_group_col := factor(!!sym(ace_group_col), levels = c("None", "Low ACE (1-2)", "High ACE (3+)"))
    )
  
  # Initialize an empty list to store tables
  tables_list <- list()
  
  # Loop through demographic variables and calculate counts by ACE group
  for (var in demographic_vars) {
    # Dynamically calculate the table and convert to data frame
    table_df <- as.data.frame(table(df[[var]], df[[ace_group_col]]))
    
    # Add a column for the variable name and rename columns
    table_df$Variable <- var
    colnames(table_df) <- c("Category", "ACE Group", "Count", "Variable")
    
    # Append the table to the list
    tables_list[[var]] <- table_df
  }
  
  # Combine all tables and pivot to get ACE groups as columns
  combined_table <- bind_rows(tables_list) %>%
    pivot_wider(names_from = `ACE Group`, values_from = Count) %>%
    arrange(Variable, Category)
  
  return(combined_table)
}


library(ggplot2)
# Function to plot some demographic variables in the data:

plot_distributions <- function(df, variables) {
  # Initialize an empty list to store plots
  plots <- list()
  
  # Loop through each variable
  for (var in variables) {
    # Check if the variable is numeric
    if (is.numeric(df[[var]])) {
      # Filter out NA values for numeric variables
      filtered_df <- df %>% filter(!is.na(.data[[var]]))
      
      # Create a histogram
      p <- ggplot(filtered_df, aes(x = .data[[var]])) +
        geom_histogram(fill = "blue", color = "black", bins = 30) +
        labs(
          title = paste("Distribution of", var),
          x = var,
          y = "Count"
        ) +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "white", color = NA),  # White background
          plot.background = element_rect(fill = "white", color = NA),  # White plot area
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      # Check if the variable is categorical (factor or character)
    } else if (is.factor(df[[var]]) || is.character(df[[var]])) {
      # Filter out NA values for categorical variables
      filtered_df <- df %>% filter(!is.na(.data[[var]]))
      
      # Create a bar plot
      p <- ggplot(filtered_df, aes(x = .data[[var]])) +
        geom_bar(fill = "blue", color = "black") +
        labs(
          title = paste("Distribution of", var),
          x = var,
          y = "Count"
        ) +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "white", color = NA),  # White background
          plot.background = element_rect(fill = "white", color = NA),  # White plot area
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    } else {
      next  # Skip variables that are not numeric or categorical
    }
    
    # Add the plot to the list
    plots[[var]] <- p
  }
  
  return(plots)
}



# Function to visualize ACE scores by a specified grouping variable
plot_ace_distribution <- function(df, group_var, save_name, comparisons = NULL) {
  # Ensure group_var exists in the dataset
  if (!group_var %in% names(df)) {
    stop(paste("Variable", group_var, "is not in the dataset."))
  }
  
  # Remove rows with NA in the grouping variable
  df <- df %>%
    filter(!is.na(!!sym(group_var)))
  
  # Get unique levels of the grouping variable
  group_levels <- unique(df[[group_var]])
  
  # Generate a palette with enough colors
  color_palette <- RColorBrewer::brewer.pal(n = length(group_levels), name = "Set3")
  
  # Generate the ggplot
  p <- ggplot(df, aes(x = !!sym(group_var), y = ACE_score, fill = !!sym(group_var))) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.4) +
    scale_fill_manual(values = color_palette) +
    labs(
      title = paste("Distribution of ACE Score by", group_var),
      x = group_var,
      y = "ACE Score"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),  # White background
      plot.background = element_rect(fill = "white", color = NA),  # White plot area
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.title = element_text(face = "bold")
    )
  
  # Add comparisons if specified
  if (!is.null(comparisons)) {
    p <- p + stat_compare_means(comparisons = comparisons, label = "p.signif")
  }
  
  # Save the plot
  ggsave(filename = paste0(save_name, ".png"), plot = p, width = 8, height = 6)
  
  return(p)
}

# Function that generates the map for a given race and then loop over all unique races
# Define a color palette for each race
race_colors <- list(
  "AIAN" = c("lightyellow", "coral"),
  "asian" = c("lightblue", "darkblue"),
  "black" = c("lightgreen", "darkgreen"),
  "hispanic" = c("lavender", "purple"),
  "other" = c("peachpuff", "darkorange"),
  "white" = c("lightpink", "red")
)

# Function to adjustr survey weights after calculating the ACE scores
adjust_survey_weights <- function(df, weight_col = "survey_weights", year_col = "IYEAR") {
  # Ensure the weight and year columns exist
  if (!all(c(weight_col, year_col) %in% colnames(df))) {
    stop("Required columns are missing from the dataset.")
  }
  
  # Total number of complete cases after ACE filtering
  total_n <- nrow(df)
  
  # Calculate number of cases per year
  year_counts <- df %>%
    group_by(.data[[year_col]]) %>%
    summarise(complete_n = n(), .groups = "drop")
  
  # Merge year-wise counts back to main data
  df_adjusted <- df %>%
    left_join(year_counts, by = setNames("IYEAR", year_col)) %>%
    mutate(
      !!weight_col := .data[[weight_col]] * (total_n / complete_n)
    ) %>%
    select(-complete_n)
  
  return(df_adjusted)
}


#########################################################
## Read in the data 
years <- c("2019", "2020", "2021", "2022", "2023")
# data_list <- lapply(years, function(year) {
#   file_name <- paste0(BRFSSdata_dir, "LLCP", year, ".XPT")
#   read.xport(file_name)
# })

# This updated version of the code will handle both XPT and ZIP files
data_list <- lapply(years, function(year) {
  file_name <- paste0(BRFSSdata_dir, "LLCP", year, ".XPT")
  
  if (file.exists(file_name)) {
    tryCatch({
      data <- read.xport(file_name)
      return(data)
    }, error = function(e) {
      warning(paste("Error reading XPT file for year", year, ":", e$message))
      return(NULL)
    })
  } else {
    zip_file_name <- paste0(BRFSSdata_dir, "LLCP", year, "XPT.zip")
    xpt_file_name <- paste0("LLCP", year, ".XPT")
    
    if (file.exists(zip_file_name)) {
      # Extract to a temp file
      temp_dir <- tempdir()
      extracted_file <- file.path(temp_dir, xpt_file_name)
      
      tryCatch({
        unzip(zip_file_name, exdir = temp_dir, overwrite = TRUE)
        data <- read.xport(extracted_file)
        file.remove(extracted_file) # Delete temp file immediately
        return(data)
      }, error = function(e) {
        warning(paste("Error processing XPT from zip for year", year, ":", e$message))
        if(file.exists(extracted_file)){
          file.remove(extracted_file) # Clean up temp file on error
        }
        return(NULL)
      })
    } else {
      warning(paste("XPT or ZIP file not found for year", year))
      return(NULL)
    }
  }
})


library(foreign)

data_list <- lapply(years, function(year) {
  file_name <- paste0(BRFSSdata_dir, "LLCP", year, ".XPT")
  
  if (file.exists(file_name)) {
    tryCatch({
      data <- read.xport(file_name)
      return(data)
    }, error = function(e) {
      warning(paste("Error reading XPT file for year", year, ":", e$message))
      return(NULL)
    })
  } else {
    zip_file_name <- paste0(BRFSSdata_dir, "LLCP", year, "XPT.zip")
    xpt_file_name <- paste0("LLCP", year, ".XPT")
    
    if (file.exists(zip_file_name)) {
      # Extract to a temp file
      temp_dir <- tempdir()
      extracted_file <- file.path(temp_dir, xpt_file_name) #Construct full path
      
      tryCatch({
        unzip(zip_file_name, files = xpt_file_name, exdir = temp_dir, overwrite = TRUE)
        data <- read.xport(extracted_file)
        file.remove(extracted_file) # Clean up temp file
        return(data)
      }, error = function(e) {
        warning(paste("Error processing XPT from zip for year", year, ":", e$message))
        if(file.exists(extracted_file)){
          file.remove(extracted_file) # Clean up temp file on error
        }
        return(NULL)
      })
    } else {
      warning(paste("XPT or ZIP file not found for year", year))
      return(NULL)
    }
  }
})


data_list <- lapply(years, function(year) {
  file_name <- paste0(BRFSSdata_dir, "LLCP", year, ".XPT")
  
  if (file.exists(file_name)) {
    tryCatch({
      data <- read.xport(file_name)
      return(data)
    }, error = function(e) {
      warning(paste("Error reading XPT file for year", year, ":", e$message))
      return(NULL)
    })
  } else {
    zip_file_name <- paste0(BRFSSdata_dir, "LLCP", year, "XPT.zip")
    xpt_file_name <- paste0("LLCP", year, ".XPT")
    
    if (file.exists(zip_file_name)) {
      con <- unz(zip_file_name, xpt_file_name)
      tryCatch({
        data <- read.xport(con)
        close(con)
        return(data)
      }, error = function(e) {
        warning(paste("Error reading XPT from zip for year", year, ":", e$message))
        close(con)
        return(NULL)
      })
    } else {
      warning(paste("XPT or ZIP file not found for year", year))
      return(NULL)
    }
  }
})



# Assign names to the list for easier identification
names(data_list) <- paste0("df_", years)

# Get column names for each dataset
lapply(data_list, colnames)


# Subset variables of interest from the data
variables_of_interest <- c("X_STATE","FMONTH","IDATE","IMONTH","IDAY","IYEAR","SEQNO","X_PSU","GENHLTH","PHYSHLTH","MENTHLTH",
                           "CVDINFR4","CVDCRHD4","CVDSTRK3","ASTHMA3","CHCSCNCR","CHCSCNC1","CHCOCNCR","CHCOCNC1","CHCCOPD2",
                           "ADDEPEV3","CHCKDNY2","DIABETE4","HAVARTH4","HAVARTH5","MARITAL","EMPLOY1","X_INCOMG1","WEIGHT2",
                           "HEIGHT3","SMOKDAY2","USENOW3","AVEDRNK3","ACEDEPRS","ACEDRINK","ACEDRUGS","ACEPRISN","ACEDIVRC",
                           "ACEPUNCH","ACEHURT1","ACESWEAR","ACETOUCH","ACETTHEM","ACEHVSEX","X_MENT14D","X_SEX","X_BMI5CAT",
                           "X_EDUCAG","X_INCOMG","X_SMOKER3","X_AGE_G","CHCCOPD3","X_RACE","X_RACE1","X_LLCPWT","SOMALE","SOFEMALE",
                           "X_RFBING5","X_RFBING6","TRNSGNDR","X_HCVU651","X_HCVU652","X_HCVU653","VETERAN3", "X_RFBMI5", "X_AGE80")
# Tell me the variables that are absent in each data-set
# Loop over each data-set in the list and check for missing variables
for (i in seq_along(data_list)) {
  df <- data_list[[i]]
  missing_vars <- variables_of_interest[!variables_of_interest %in% colnames(df)]
  
  if (length(missing_vars) > 0) {
    cat(paste("Missing variables in dataset", i, ":\n"))
    print(missing_vars)
  } else {
    cat(paste("All variables present in dataset", i, "\n"))
  }
}

# Subset each data set now
# Subset each dataset in the list by the variables of interest
data_list_subset <- lapply(data_list, function(df) {
  # Only keep the variables that are present in both 'variables_of_interest' and the dataset
  common_vars <- intersect(variables_of_interest, colnames(df))
  df_subset <- df[, common_vars, drop = FALSE]
  return(df_subset)
})

##########################################
## Check for missing values

# Function to calculate the percentage of missing values for each variable and round to 1 decimal place
missing_summary <- function(df) {
  missing_perc <- sapply(df, function(x) round((sum(is.na(x)) / length(x)) * 100, 1))
  return(data.frame(Variable = names(df), MissingPercentage = missing_perc))
}

# Apply the missing summary function to each data-set in the list
missing_data_list <- lapply(data_list_subset, missing_summary)

# Display missing value summaries for each data-set, rounded to 1 decimal place
for (i in seq_along(missing_data_list)) {
  cat("Missing Data Summary for Year:", years[i], "\n")
  print(missing_data_list[[i]])
  cat("\n")
}

##################################################
## Rename the variables to be the same in all the datasets.

# Rename columns in the respective datasets
data_list_renamed <- data_list_subset

# Rename columns in 2022 and 2023 datasets
data_list_renamed[[4]] <- data_list_renamed[[4]] %>%
  rename(
    CHCSCNCR = CHCSCNC1,
    CHCOCNCR = CHCOCNC1,
    X_INCOMG = X_INCOMG1,
    CHCCOPD2 = CHCCOPD3,
    X_RACE = X_RACE1,
    X_RFBING5 = X_RFBING6,
    X_HCVU651 = X_HCVU652
  )

# Rename columns in 2023 dataset
data_list_renamed[[5]] <- data_list_renamed[[5]] %>%
  rename(
    CHCSCNCR = CHCSCNC1,
    CHCOCNCR = CHCOCNC1,
    X_INCOMG = X_INCOMG1,
    CHCCOPD2 = CHCCOPD3,
    X_RFBING5 = X_RFBING6,
    X_HCVU651 = X_HCVU653
  )

# Rename columns in 2021 dataset
data_list_renamed[[3]] <- data_list_renamed[[3]] %>%
  rename(
    HAVARTH4 = HAVARTH5,
    X_INCOMG = X_INCOMG1,
    CHCCOPD2 = CHCCOPD3,
    X_HCVU651 = X_HCVU652
  )

# Verify that the columns were renamed correctly
lapply(data_list_renamed, colnames)


## Rename the variables to be the same in all the datasets.
# Rename columns in the respective datasets
data_list_renamed <- data_list_subset

# Rename columns in 2022 and 2023 datasets
data_list_renamed[[4]] <- data_list_renamed[[4]] %>%
  rename(
    CHCSCNCR = CHCSCNC1,
    CHCOCNCR = CHCOCNC1,
    X_INCOMG = X_INCOMG1,
    CHCCOPD2 = CHCCOPD3,
    X_RACE = X_RACE1,
    X_RFBING5 = X_RFBING6,
    X_HCVU651 = X_HCVU652
  )

# Rename columns in 2023 dataset
data_list_renamed[[5]] <- data_list_renamed[[5]] %>%
  rename(
    CHCSCNCR = CHCSCNC1,
    CHCOCNCR = CHCOCNC1,
    X_INCOMG = X_INCOMG1,
    CHCCOPD2 = CHCCOPD3,
    X_RFBING5 = X_RFBING6,
    X_HCVU651 = X_HCVU653
  )

# Rename columns in 2021 dataset
data_list_renamed[[3]] <- data_list_renamed[[3]] %>%
  rename(
    HAVARTH4 = HAVARTH5,
    X_INCOMG = X_INCOMG1,
    CHCCOPD2 = CHCCOPD3,
    X_HCVU651 = X_HCVU652
  )

# Verify that the columns were renamed correctly
lapply(data_list_renamed, colnames)


## Since we are pooling 5 years data, it is necessary to adjust the weight variable across the datasets before combining them. 
# Combine the adjusted datasets into one data frame
combined_data <- do.call(rbind, data_list_renamed)

# Check the dimensions of the combined dataset
dim(combined_data)
glimpse(combined_data)


## The data has about 25,000 rows for 2024 data. We would like to exclude this from our analysis;
# Remove rows where IYEAR is 2024
combined_data <- combined_data[combined_data$IYEAR != "2024", ]

# Confirm that 2024 has been removed
unique(combined_data$IYEAR)

# Rename `combined_data` as `data`:
data <- combined_data


## Only use complete cases data for ACE:
# List of ACE variable names (replace these with the actual ACE variable names in your dataset)
ace_variables <- c("ACEDEPRS", "ACEDRINK", "ACEDRUGS", "ACEPRISN", "ACEDIVRC", "ACEPUNCH", "ACEHURT1", 
                   "ACESWEAR", "ACETOUCH", "ACETTHEM", "ACEHVSEX")

# Subset combined_data to keep only rows with complete cases for the all variables except ACE variables
# The complete cases for ACE variables will be done in during the ACE scores calculation as expressed in the function.
# The function recodes the "Refused" and "Don't know" as NA, and these were excluded further on in the analysis
complete_cases_data <- data[complete.cases(data[ace_variables]), ]
########################################################################################

# Check the number of rows after filtering
cat("Number of rows with complete ACE cases:", nrow(complete_cases_data), "\n")

# Count the number of rows with any missing data in the ACE variables
rows_with_missing_ACE <- data[!complete.cases(data[ace_variables]), ]

# Display the number of rows with any missing ACE data
cat("Number of rows with any missing data in ACE variables:", nrow(rows_with_missing_ACE), "\n")

########################################
# Define a lookup table for FIPS codes and state names
fips_to_state <- data.frame(
  FIPS_code = c(1, 4, 5, 10, 11, 12, 13, 15, 16, 18, 19, 21, 26, 28, 29, 30, 32, 33, 34, 35, 38, 41, 42, 44, 
                45, 46, 47, 48, 49, 51, 54, 55, 56),
  State = c("ALABAMA", "ARIZONA", "ARKANSAS", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", 
            "GEORGIA", "HAWAII", "IDAHO", "INDIANA", "IOWA", "KENTUCKY", "MICHIGAN", 
            "MISSISSIPPI", "MISSOURI", "MONTANA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", 
            "NEW MEXICO", "NORTH DAKOTA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", 
            "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VIRGINIA", 
            "WEST VIRGINIA", "WISCONSIN", "WYOMING")
)

# Merge the lookup table with the data to get state names
complete_cases_data <- complete_cases_data %>%
  left_join(fips_to_state, by = c("X_STATE" = "FIPS_code")) %>%
  arrange(X_STATE)  # Sort by FIPS code

###########################################
# Apply ACE scoring to full dataset
# Here, we have the complete cases for all variables including the ACE variables as well.
complete_ACE_data_scored <- calculate_ACE_scores(complete_cases_data)

# Define the ACE-related variables
ace_variables <- c("abuse_physical", "abuse_sexual", "abuse_emotional",
                   "household_mental", "household_substance", "household_incarcerated",
                   "household_violence", "parental_separation")

# Calculate total respondents per year
total_respondents <- complete_ACE_data_scored %>%
  group_by(IYEAR) %>%
  summarise(Total_Respondents = n())

# Calculate number of complete ACE cases per year
complete_ACE_cases <- complete_ACE_data_scored %>%
  mutate(ACE_complete = complete.cases(across(all_of(ace_variables)))) %>%
  group_by(IYEAR) %>%
  summarise(
    Included_ACE_Cases = sum(ACE_complete),
    Missing_ACE_Cases = sum(!ACE_complete)
  )

# Join total respondents with ACE completeness
ace_year_summary <- total_respondents %>%
  left_join(complete_ACE_cases, by = "IYEAR") %>%
  mutate(
    Missing_Percentage = round((Missing_ACE_Cases / Total_Respondents) * 100, 2),
    weight_factor =  nrow(complete_ACE_data_scored)/Included_ACE_Cases
  )

# View the summary table
print(ace_year_summary)

###################################
# Add the weight to the data
complete_ACE_dt <- complete_ACE_data_scored %>%
  left_join(ace_year_summary %>% dplyr::select(IYEAR, weight_factor), by = "IYEAR") %>%
  mutate(X_LLCPWT = X_LLCPWT * weight_factor)
####################################

####################################
# Now, let's go ahead and make the correlation plots for each state, and also to do the regression analysis.

# List of unique states
unique_states <- unique(complete_ACE_dt$State)

# Loop through each state
for (state in unique_states) {
  tryCatch({
    cat("\nProcessing state:", state, "\n")
    
    # Create a directory for the state
    state_dir <- file.path("results", state)
    if (!dir.exists(state_dir)) {
      dir.create(state_dir, recursive = TRUE)
    }
    
    # Filter data for the state
    state_data <- complete_ACE_dt %>% filter(State == state)
    if (nrow(state_data) == 0) {
      cat("No data found for state:", state, "\n")
      next
    }
    
    # Rename variables
    renamed_data <- rename_columns(state_data)
    
    # Recode variables
    recoded_data <- recode_variables(renamed_data)
    
    # Calculate ACE scores
    ACE_operated <- calculate_ACE_scores(recoded_data)
    
    #Let us see the number of rows with missing variables for different category of ACE
    ACE_category <- c("abuse_physical", "abuse_sexual", "abuse_emotional", "household_mental",
                      "household_substance", "household_incarcerated",
                      "household_violence", "parental_separation")
    
    # Filter rows with complete cases for ACE_category
    ACE_operated <- ACE_operated[complete.cases(ACE_operated[, ACE_category]), ]
    
    # Calculate ACE experience counts
    ACE_experience_counts <- calculate_ACE_experience_counts(ACE_operated)
    cat("ACE Experience Counts for", state, ":\n")
    print(ACE_experience_counts)
    
    # Save ACE experience counts
    write.csv(ACE_experience_counts, file.path(state_dir, paste0(state, "_ACE_experience_counts.csv")), row.names = FALSE)
    
    # Group ACE score
    ACE_operated <- group_ACE_score(ACE_operated)
    cat("ACE Groups for", state, ":\n")
    print(table(ACE_operated$ACE_group))
    
    
    # Calculate individual ACE prevalence
    individual_ace_prevalence <- calculate_individual_ACE_prevalence(ACE_operated)
    cat("Individual ACE Prevalence for", state, ":\n")
    print(individual_ace_prevalence)
    # Save the overall ACE prevalence in the state's directory with the state name in the filename
    write.csv(individual_ace_prevalence, file.path(state_dir, paste0(state, "_individual_ACE_prevalence.csv")), row.names = FALSE)
    
    # Calculate overall ACE prevalence
    overall_ace_prevalence <- calculate_overall_ACE_prevalence(ACE_operated)
    cat("Overall ACE Prevalence for", state, ":\n")
    print(overall_ace_prevalence)
    # Save the overall ACE prevalence in the state's directory with the state name in the filename
    write.csv(overall_ace_prevalence, file.path(state_dir, paste0(state, "_overall_ACE_prevalence.csv")), row.names = FALSE)
    
    
    # Analyze gender and race for 4+ ACEs
    gender_race_summary <- analyze_gender_race_four_or_more_ACE(ACE_operated)
    cat("Gender and Race Summary for", state, ":\n")
    print(gender_race_summary)
    
    
    # Filter out NA values for gender and race plots
    gender_summary_filtered <- gender_race_summary$gender_summary %>%
      filter(!is.na(gender))
    race_summary_filtered <- gender_race_summary$race_summary %>%
      filter(!is.na(race))
    
    # Plot and save gender proportions
    gender_plot <- ggplot(gender_summary_filtered, aes(x = gender, y = four_or_more_aces_prop)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black") +
      labs(
        title = paste("Proportion of 4+ ACEs by Gender in", state),
        x = "Gender",
        y = "Proportion with 4+ ACEs"
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal()+
      theme(
        panel.background = element_rect(fill = "white", color = NA),  # White background
        plot.background = element_rect(fill = "white", color = NA),  # White plot area
        axis.text.x = element_text(angle = 45, hjust = 1))
    #save plot
    ggsave(file.path(state_dir, paste0(state, "_gender_proportion_plot.png")), gender_plot)
    
    # Plot and save race proportions
    race_plot <- ggplot(race_summary_filtered, aes(x = race, y = four_or_more_aces_prop)) +
      geom_bar(stat = "identity", fill = "coral", color = "black") +
      labs(
        title = paste("Proportion of 4+ ACEs by Race in", state),
        x = "Race",
        y = "Proportion with 4+ ACEs"
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),  # White background
        plot.background = element_rect(fill = "white", color = NA),  # White plot area
        axis.text.x = element_text(angle = 45, hjust = 1))
    #save plot
    ggsave(file.path(state_dir, paste0(state, "_race_proportion_plot.png")), race_plot)
    
    
    # Adjust weight and height
    ACE_operated <- adjust_weight_height(ACE_operated)
    
    # Calculate weighted variables
    ace_category_columns <- c("abuse_physical", "abuse_sexual", "abuse_emotional",
                              "household_mental", "household_substance",
                              "household_incarcerated", "household_violence",
                              "parental_separation")
    prevalence_results <- calculate_weighted_prevalence(
      df = ACE_operated,
      ace_variables = ace_category_columns,
      id_col = "X_PSU",
      weight_col = "survey_weights"
    )
    write.csv(prevalence_results, file.path(state_dir, paste0(state, "_weighted_prevalence.csv")), row.names = FALSE)
    
    # Reorder income levels
    ACE_operated <- ACE_operated %>%
      mutate(
        income_level = factor(
          income_level,
          levels = c("< 15k", "15k – <25k", "25k – <35k", "35k – <50k", "≥ 50k")
        )
      )
    
    # Generate socio-demographic table
    demographic_vars <- c("gender", "age_group", "race", "education_level", "income_level", "health_coverage")
    formatted_table <- create_socio_demographic_table(
      df = ACE_operated,
      demographic_vars = demographic_vars,
      ace_group_col = "ACE_group"
    )
    write.csv(formatted_table, file.path(state_dir, paste0(state, "_socio_demographics.csv")), row.names = FALSE)
    
    # Plot distributions
    variables_to_plot <- c("age", "gender", "employment_status", "income_level", "marital_status")
    plots <- plot_distributions(df = ACE_operated, variables = variables_to_plot)
    
    #Update the values in the ACE_group column before converting them into a factor. 
    #Match the existing values to the intended factor levels.
    ACE_operated <- ACE_operated %>%
      mutate(
        ACE_group = case_when(
          ACE_group == "None" ~ "None",
          ACE_group == "Low ACE (1-2)" ~ "1 - 2 ACEs",
          ACE_group == "High ACE (3+)" ~ "3 or more ACEs",
          TRUE ~ NA_character_  # Handle unexpected values
        )
      )
    
    # Standardize factor levels
    ACE_operated <- ACE_operated %>%
      mutate(
        ACE_group = factor(ACE_group, levels = c("None", "1 - 2 ACEs", "3 or more ACEs")),
        income_level = factor(income_level, levels = c("< 15k", "15k – <25k", "25k – <35k", "35k – <50k", "≥ 50k")),
        gender = factor(gender, levels = c("male", "female"))
      )
    
    # Convert specified variables to binary format with "no" as the reference level
    ACE_operated <- ACE_operated %>%
      # Recode diabetes to binary with "no" as the reference
      mutate(diabetes_bin = factor(ifelse(diabetes == "yes", "yes", "no"), levels = c("no", "yes"))) %>%
      
      # Recode general_health to binary with "no" as the reference
      mutate(fair_or_poor_general_health = factor(ifelse(general_health %in% c("fair", "poor"), "yes", "no"), levels = c("no", "yes"))) %>%
      
      # Recode smoking_status to binary with "no" as the reference
      mutate(current_smoker = factor(ifelse(smoking_status %in% c("former smoker", "cr smoker - evryday", "cr smoker - somedays"), "yes", "no"), levels = c("no", "yes"))) %>%
      
      # Recode physical_health to binary with "no" as the reference
      mutate(i4_days_poor_physical_health = factor(ifelse(physical_health >= 14, "yes", "no"), levels = c("no", "yes"))) %>%
      
      # Recode mental_health to binary with "no" as the reference
      mutate(i4_days_poor_mental_health = factor(ifelse(mental_health >= 14, "yes", "no"), levels = c("no", "yes"))) 
    
    # Log-binomial regression for outcomes
    outcome_vars <- c("heart_attack", "coronary_heart_disease", "stroke", "asthma", 
                      "skin_cancer", "other_cancer", "copd", "depressive_disorder",
                      "kidney_disease", "diabetes_bin", "arthritis", "bmi_above_25",
                      "fair_or_poor_general_health","i4_days_poor_physical_health",
                      "i4_days_poor_mental_health","current_smoker", "heavy_drinker")
    
    for (var in outcome_vars) {
      if (var %in% colnames(ACE_operated)) {
        # Convert character/factor to numeric 0/1
        ACE_operated[[var]] <- ifelse(ACE_operated[[var]] == "yes", 1, 0)
        ACE_operated[[var]] <- as.numeric(as.character(ACE_operated[[var]]))
      }
    }
    
    
    # Prepare survey design
    brfss_design <- svydesign(id = ~X_PSU, weights = ~survey_weights, data = ACE_operated, nest = TRUE)
    
    # Relevel ACE_group
    brfss_design$variables$ACE_group <- relevel(factor(brfss_design$variables$ACE_group), ref = "None")
    
    # Save the results
    results_summary <- data.frame(Outcome = character(), ACE_Group = character(), Risk_Ratio = numeric(), P_value = numeric())
    
    for (outcome in outcome_vars) {
      formula <- as.formula(paste(outcome, "~ ACE_group + income_level + education_level + gender + age_group + race"))
      model <- svyglm(formula, design = brfss_design, family = quasipoisson(link = "log"))
      model_coef <- tidy(model) %>% filter(grepl("ACE_group", term))
      results_summary <- rbind(
        results_summary,
        model_coef %>%
          mutate(
            Outcome = outcome,
            Risk_Ratio = round(exp(estimate), 2),
            P_value = round(p.value, 4)
          ) %>%
          dplyr::select(Outcome, ACE_Group = term, Risk_Ratio, P_value)
      )
    }
    # Adjust P-values using False Discovery Rate (FDR)
    results_summary <- results_summary %>%
      mutate(FDR = round(p.adjust(P_value, method = "fdr"),4)) %>%  # Apply FDR correction
      dplyr::select(Outcome, ACE_Group, Risk_Ratio, FDR)  # Remove P_value before writing CSV
    
    #######################
    # Reshape the table to wide format (one row per outcome)
    results_summary_wide <- results_summary %>%
      mutate(
        ACE_Group = case_when(
          ACE_Group == "ACE_group1 - 2 ACEs" ~ "1_2_ACEs",
          ACE_Group == "ACE_group3 or more ACEs" ~ "3_plus_ACEs"
        )
      ) %>%
      pivot_wider(
        names_from = ACE_Group, 
        values_from = c(Risk_Ratio, FDR),
        names_glue = "{ACE_Group}_{.value}"
      ) %>%
      dplyr::select(Outcome, `1_2_ACEs_Risk_Ratio`, `1_2_ACEs_FDR`, `3_plus_ACEs_Risk_Ratio`, `3_plus_ACEs_FDR`) %>%
      rename(
        `1 - 2 ACEs` = `1_2_ACEs_Risk_Ratio`,  # Rename Risk Ratio column for 1-2 ACEs
        `FDR` = `1_2_ACEs_FDR`,                # Rename FDR for 1-2 ACEs
        `3 or more ACEs` = `3_plus_ACEs_Risk_Ratio`,  # Rename Risk Ratio column for 3+ ACEs
        `FDR_3plus` = `3_plus_ACEs_FDR`        # Rename FDR for 3+ ACEs
      )
    ######################
    
    write.csv(results_summary_wide, file.path(state_dir, paste0(state, "_multiple_regression_result.csv")), row.names = FALSE)
    
    # Reshape the datta
    # Customize outcome names and reshape the data
    table_data <- results_summary %>%
      mutate(
        Outcome = case_when(
          Outcome == "heart_attack" ~ "Heart Attack",
          Outcome == "coronary_heart_disease" ~ "Coronary Heart Disease",
          Outcome == "stroke" ~ "Stroke",
          Outcome == "asthma" ~ "Asthma",
          Outcome == "skin_cancer" ~ "Skin Cancer",
          Outcome == "other_cancer" ~ "Other Cancer",
          Outcome == "copd" ~ "COPD",
          Outcome == "depressive_disorder" ~ "Depressive Disorder",
          Outcome == "kidney_disease" ~ "Kidney Disease",
          Outcome == "diabetes_bin" ~ "Diabetes",
          Outcome == "arthritis" ~ "Arthritis",
          Outcome == "bmi_above_25" ~ "BMI Above 25",
          Outcome == "fair_or_poor_general_health" ~ "Fair or Poor General Health",
          Outcome == "i4_days_poor_physical_health" ~ "14+ Days Poor Physical Health",
          Outcome == "i4_days_poor_mental_health" ~ "14+ Days Poor Mental Health",
          Outcome == "current_smoker" ~ "Current Smoker",
          Outcome == "heavy_drinker" ~ "Heavy Drinker",
          TRUE ~ Outcome  # Keep any other outcomes unchanged
        ),
        ACE_Group = case_when(
          ACE_Group == "ACE_group1 - 2 ACEs" ~ "1_2_ACEs",
          ACE_Group == "ACE_group3 or more ACEs" ~ "3_plus_ACEs"
        ),
        Risk_Ratio = paste0(Risk_Ratio),
        FDR = ifelse(FDR < 0.001, "<0.001", round(FDR, 3))  # Format FDR-adjusted p-values
      ) %>%
      dplyr::select(Outcome, ACE_Group, Risk_Ratio, FDR) %>%
      pivot_wider(
        names_from = ACE_Group, 
        values_from = c(Risk_Ratio, FDR),
        names_glue = "{ACE_Group}_{.value}"
      ) %>%
      dplyr::select(Outcome, `1_2_ACEs_Risk_Ratio`, `1_2_ACEs_FDR`, `3_plus_ACEs_Risk_Ratio`, `3_plus_ACEs_FDR`)
    
    
    
    # # Display the formatted table with customized headers
    table_data %>%
      kable("html", col.names = c("Outcome", "1 - 2 ACEs", "FDR", "3 or more ACEs", "FDR"),
            align = c("l", "c", "c", "c", "c")) %>%
      kable_styling(full_width = FALSE, position = "center") %>%
      add_header_above(c(" " = 1, "Risk ratio (95% CI)" = 2, " " = 2)) %>%
      row_spec(
        which(table_data$`1_2_ACEs_FDR` < 0.05 &
                table_data$`3_plus_ACEs_FDR` < 0.05),
        bold = TRUE,
        color = "darkblue"
      )%>%
      save_kable(file.path(state_dir, paste0(state, "_multiple_regrsn_table_data_output.html")))
    #Save the reshaped table as a CSV
    #write.csv(table_data, file.path(state_dir, paste0(state, "_multiple_regression_result.csv")), row.names = FALSE)
    
    
  }, error = function(e) {
    cat("Error encountered for state:", state, "\n")
    print(e)
  })
  
}

cat("\nProcessing complete! All results saved in respective state directories.\n")

###############################################################################################################
# SECTION 2
# Create a visualization map for the ACE scores by states:
###############################################################################################################
  
#library(usmap)

# Rename and recode the whole US data:
renamed_data <- rename_columns(complete_ACE_dt)

# Recode variables
recoded_data <- recode_variables(renamed_data)

# Calculate ACE scores
ACE_calculated <- calculate_ACE_scores(recoded_data)

# Calculate the Mean ACE scores by State
state_ace_data <- ACE_calculated %>%
  group_by(State) %>%
  summarise(mean_ace_score = mean(ACE_score, na.rm = TRUE))


# Prepare the data
state_ace_data <- state_ace_data %>%
  mutate(state = tolower(State))  # Convert to lowercase for compatibility

# Merge FIP code
state_ace_data <- state_ace_data %>%
  left_join(fips_to_state, by = "State")


# Plot the map
us_map <- plot_usmap(data = state_ace_data, values = "mean_ace_score", regions = "state", labels = TRUE) +
  scale_fill_continuous(name = "Mean ACE Score", low = "lightblue", high = "darkblue", na.value = "gray",
                        limits = c(1, 3)) + # Set limits for the color scale
  theme_minimal() +
  labs(title = "State-Level Distribution of Mean ACE Scores") +
  theme(
    panel.grid = element_blank(), #this line removes the latitude and longitude lines
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank()   # Remove axis ticks
  )

us_map

# Add state abbreviations to state_ace_data
state_ace_data <- state_ace_data %>%
  mutate(abbreviation = case_when(
    State == "ALABAMA" ~ "AL",
    State == "ARIZONA" ~ "AZ",
    State == "ARKANSAS" ~ "AR",
    State == "DELAWARE" ~ "DE",
    State == "DISTRICT OF COLUMBIA" ~ "DC",
    State == "FLORIDA" ~ "FL",
    State == "GEORGIA" ~ "GA",
    State == "HAWAII" ~ "HI",
    State == "IDAHO" ~ "ID",
    State == "INDIANA" ~ "IN",
    State == "IOWA" ~ "IA",
    State == "KANSAS" ~ "KS",
    State == "KENTUCKY" ~ "KY",
    State == "LOUISIANA" ~ "LA",
    State == "MAINE" ~ "ME",
    State == "MARYLAND" ~ "MD",
    State == "MASSACHUSETTS" ~ "MA",
    State == "MICHIGAN" ~ "MI",
    State == "MINNESOTA" ~ "MN",
    State == "MISSISSIPPI" ~ "MS",
    State == "MISSOURI" ~ "MO",
    State == "MONTANA" ~ "MT",
    State == "NEBRASKA" ~ "NE",
    State == "NEVADA" ~ "NV",
    State == "NEW HAMPSHIRE" ~ "NH",
    State == "NEW JERSEY" ~ "NJ",
    State == "NEW MEXICO" ~ "NM",
    State == "NEW YORK" ~ "NY",
    State == "NORTH CAROLINA" ~ "NC",
    State == "NORTH DAKOTA" ~ "ND",
    State == "OHIO" ~ "OH",
    State == "OKLAHOMA" ~ "OK",
    State == "OREGON" ~ "OR"
  ))
# Save the plot as a PNG file
ggsave("state_level_ACE_distribution_map.png", plot = us_map,  bg = "white", width = 10, height = 7, dpi = 300)

###########################################
# State-based data availability map:
  
# Count data availability
state_data_availability <- ACE_calculated %>%
  group_by(State) %>%
  summarise(data_points = n())

# Prepare the data
state_data_availability <- state_data_availability %>%
  mutate(state = tolower(State))  # Convert to lowercase for compatibility


# Map visualization
data_availability_map <- plot_usmap(data = state_data_availability, values = "data_points", regions = "states", labels = TRUE) +
  scale_fill_continuous(name = "Data Points", low = "lightgreen", high = "darkgreen", na.value = "gray") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank() 
  ) +
  labs(title = "State-Based Data Availability")

# Save the plot as a PNG file
ggsave("state_based_data_availability_map.png", plot = data_availability_map,  bg = "white", width = 10, height = 7, dpi = 300)

############################################
# Race-specific zoom in:

# # Filter and summarize for AIAN
aian_data <- ACE_calculated %>%
  filter(race == "AIAN") %>%
  group_by(State) %>%
  summarise(aian_count = n())

# I want to see the proportion of AIAN per state
aian_data_plus_prop <- aian_data %>% 
  mutate(prop =  round(prop.table(aian_count),3))

# Save the grouped data to an Excel file
# Define the folder path
output_folder <- "state_race_analysis_results"

# Check if the folder exists, and create it if it doesn't
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Define the output file path
output_file <- file.path(output_folder, "Counts_AIAN_in_ND_SD.csv")
#output_file <- file.path("state_race_analysis_results_new", "Counts_AIAN_in_ND_SD.csv")
write.csv(aian_data_plus_prop, output_file, row.names = FALSE)

# Group ACE categories for AIAN across all states with specified categories
aian_ACE_data <- ACE_calculated %>%
  filter(race == "AIAN") %>%               # Filter for AIAN population
  mutate(ACE_category = case_when(         # Create categories
    ACE_score == 0 ~ "None",
    ACE_score %in% 1:2 ~ "Low ACE",
    ACE_score >= 3 ~ "High ACE"
  )) %>%
  mutate(ACE_category = factor(ACE_category, levels = c("None", "Low ACE", "High ACE"))) %>%
  group_by(State, ACE_category) %>%        # Group by state and ACE category
  summarise(aian_count = n(), .groups = "drop") %>%  # Summarise counts
  pivot_wider(names_from = ACE_category,  # Reshape to one row per state
              values_from = aian_count, 
              values_fill = 0)   
output_file <- file.path("state_race_analysis_results", "ACE_categories_AIAN_in_ND_SD.csv")
write.csv(aian_ACE_data, output_file, row.names = FALSE)

# Plot the US maps using the highest response proportion from all the states. Overall, the highest is 30%
# sO WE SET THE LIMIT TO 30% (0 to 0.3)

# Create folder for output
folder_path <- "state_race_response_by_prop"
if (!dir.exists(folder_path)) {
  dir.create(folder_path)
}

# AIAN Plot
colnames(aian_data_plus_prop)[1] <- "state"
aian_plot <- plot_usmap(data = aian_data_plus_prop, values = "prop", regions = "state", labels = FALSE) +
  scale_fill_gradientn(name = "Proportion",
                       colors = c("lightpink", "orange", "darkred"),
                       limits = c(0, 0.3)) +
  theme_minimal() +
  labs(title = "AIAN response proportions") +
  theme(
    panel.grid = element_blank(), #this line removes the latitude and longitude lines
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank()   # Remove axis ticks
  )
aian_output_path <- file.path(folder_path, "AIAN.pdf")
ggsave(aian_output_path, plot = aian_plot, width = 10, height = 6, dpi = 300, bg = "white")
cat("AIAN plot saved at:", aian_output_path, "\n")

# Black Plot
black_data <- ACE_calculated %>%
  filter(race == "black") %>%
  group_by(State) %>%
  summarise(black_count = sum(race == "black")) %>%
  mutate(prop = round(prop.table(black_count), 3))
colnames(black_data)[1] <- "state"
black_plot <- plot_usmap(data = black_data, values = "prop", regions = "state", labels = FALSE) +
  scale_fill_gradientn(name = "Proportion",
                       colors = c("lightpink", "orange", "darkred"),
                       limits = c(0, 0.3)) +
  theme_minimal() +
  labs(title = "Black response proportions")+
  theme(
    panel.grid = element_blank(), #this line removes the latitude and longitude lines
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank()   # Remove axis ticks
  )
black_output_path <- file.path(folder_path, "Black.pdf")
ggsave(black_output_path, plot = black_plot, width = 10, height = 6, dpi = 300, bg = "white")
cat("Black plot saved at:", black_output_path, "\n")

# Asian Plot
asian_data <- ACE_calculated %>%
  filter(race == "asian") %>%
  group_by(State) %>%
  summarise(asian_count = n()) %>%
  mutate(prop = round(prop.table(asian_count), 3))
colnames(asian_data)[1] <- "state"
asian_plot <- plot_usmap(data = asian_data, values = "prop", regions = "state", labels = FALSE) +
  scale_fill_gradientn(name = "Proportion",
                       colors = c("lightpink", "orange", "darkred"),
                       limits = c(0, 0.3)) +
  theme_minimal() +
  labs(title = "Asian response proportions")+
  theme(
    panel.grid = element_blank(), # this line removes the latitude and longitude lines
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank()   # Remove axis ticks
  )
asian_output_path <- file.path(folder_path, "Asian.pdf")
ggsave(asian_output_path, plot = asian_plot, width = 10, height = 6, dpi = 300, bg = "white")
cat("Asian plot saved at:", asian_output_path, "\n")

# Hispanic Plot
hispanic_data <- ACE_calculated %>%
  filter(race == "hispanic") %>%
  group_by(State) %>%
  summarise(hispanic_count = n()) %>%
  mutate(prop = round(prop.table(hispanic_count), 3))
colnames(hispanic_data)[1] <- "state"
hispanic_plot <- plot_usmap(data = hispanic_data, values = "prop", regions = "state", labels = FALSE) +
  scale_fill_gradientn(name = "Proportion",
                       colors = c("lightpink", "orange", "darkred"),
                       limits = c(0, 0.3)) +
  theme_minimal() +
  labs(title = "Hispanic response proportions")+
  theme(
    panel.grid = element_blank(), # this line removes the latitude and longitude lines
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank()   # Remove axis ticks
  )
hispanic_output_path <- file.path(folder_path, "Hispanic.pdf")
ggsave(hispanic_output_path, plot = hispanic_plot, width = 10, height = 6, dpi = 300, bg = "white")
cat("Hispanic plot saved at:", hispanic_output_path, "\n")

# Other Plot
other_data <- ACE_calculated %>%
  filter(race == "other") %>%
  group_by(State) %>%
  summarise(other_count = n()) %>%
  mutate(prop = round(prop.table(other_count), 3))
colnames(other_data)[1] <- "state"
other_plot <- plot_usmap(data = other_data, values = "prop", regions = "state", labels = FALSE) +
  scale_fill_gradientn(name = "Proportion",
                       colors = c("lightpink", "orange", "darkred"),
                       limits = c(0, 0.3)) +
  theme_minimal() +
  labs(title = "Other response proportions")+
  theme(
    panel.grid = element_blank(), #this line removes the latitude and longitude lines
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank()   # Remove axis ticks
  )
other_output_path <- file.path(folder_path, "Other.pdf")
ggsave(other_output_path, plot = other_plot, width = 10, height = 6, dpi = 300, bg = "white")
cat("Other plot saved at:", other_output_path, "\n")

# White Plot
white_data <- ACE_calculated %>%
  filter(race == "white") %>%
  group_by(State) %>%
  summarise(white_count = n()) %>%
  mutate(prop = round(prop.table(white_count), 3))
colnames(white_data)[1] <- "state"
white_plot <- plot_usmap(data = white_data, values = "prop", regions = "state", labels = FALSE) +
  scale_fill_gradientn(name = "Proportion",
                       colors = c("lightpink", "orange", "darkred"),
                       limits = c(0, 0.3)) +
  theme_minimal() +
  labs(title = "White response proportions")+
  theme(
    panel.grid = element_blank(), #this line removes the latitude and longitude lines
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank()   # Remove axis ticks
  )
white_output_path <- file.path(folder_path, "White.pdf")
ggsave(white_output_path, plot = white_plot, width = 10, height = 6, dpi = 300, bg = "white")
cat("White plot saved at:", white_output_path, "\n")

###############################################################################################################
# SECTION 3
## To carry out log-binomial regression for races in just the states of North Dakota and South Dakota:
###############################################################################################################

#rename ACE_calculated as ACE_operated outside the loop
ACE_operated <- ACE_calculated
# Group ACE score
ACE_operated <- group_ACE_score(ACE_operated)

ACE_operated <- ACE_operated %>%
  mutate(
    ACE_group = case_when(
      ACE_group == "None" ~ "None",
      ACE_group == "Low ACE (1-2)" ~ "1 - 2 ACEs",
      ACE_group == "High ACE (3+)" ~ "3 or more ACEs",
      TRUE ~ NA_character_  # Handle unexpected values
    )
  )

# Standardize factor levels
ACE_operated <- ACE_operated %>%
  mutate(
    ACE_group = factor(ACE_group, levels = c("None", "1 - 2 ACEs", "3 or more ACEs")),
    income_level = factor(income_level, levels = c("< 15k", "15k – <25k", "25k – <35k", "35k – <50k", "≥ 50k")),
    gender = factor(gender, levels = c("male", "female"))
    )

# Convert specified variables to binary format with "no" as the reference level
ACE_operated <- ACE_operated %>%
  # Recode diabetes to binary with "no" as the reference
  mutate(diabetes_bin = factor(ifelse(diabetes == "yes", "yes", "no"), levels = c("no", "yes"))) %>%
  
  # Recode general_health to binary with "no" as the reference
  mutate(fair_or_poor_general_health = factor(ifelse(general_health %in% c("fair", "poor"), "yes", "no"), levels = c("no", "yes"))) %>%
  
  # Recode smoking_status to binary with "no" as the reference
  mutate(current_smoker = factor(ifelse(smoking_status %in% c("former smoker", "cr smoker - evryday", "cr smoker - somedays"), "yes", "no"), levels = c("no", "yes"))) %>%
  
  # Recode physical_health to binary with "no" as the reference
  mutate(i4_days_poor_physical_health = factor(ifelse(physical_health >= 14, "yes", "no"), levels = c("no", "yes"))) %>%
  
  # Recode mental_health to binary with "no" as the reference
  mutate(i4_days_poor_mental_health = factor(ifelse(mental_health >= 14, "yes", "no"), levels = c("no", "yes"))) 


# Prepare survey design
brfss_design <- svydesign(id = ~X_PSU, weights = ~survey_weights, data = ACE_operated, nest = TRUE)

# Relevel ACE_group
brfss_design$variables$ACE_group <- relevel(factor(brfss_design$variables$ACE_group), ref = "None")

# Log-binomial regression for outcomes
outcome_vars <- c("heart_attack", "coronary_heart_disease", "stroke", "asthma", 
                  "skin_cancer", "other_cancer", "copd", "depressive_disorder",
                  "kidney_disease", "diabetes_bin", "arthritis", "bmi_above_25",
                  "fair_or_poor_general_health","i4_days_poor_physical_health",
                  "i4_days_poor_mental_health","current_smoker", "heavy_drinker")


## To carry out log-binomial regression for races in just the states of North Dakota and South Dakota:
# Filter data for North Dakota and South Dakota
nd_sd_data <- ACE_operated %>%
  filter(State %in% c("NORTH DAKOTA", "SOUTH DAKOTA")) %>%
  filter(!is.na(race))  # Remove rows with missing race information

#Group ACE by category
nd_sd_data <- nd_sd_data %>%
  mutate(
    ACE_group = case_when(
      ACE_score == 0 ~ "None",
      ACE_score %in% 1:2 ~ "1 - 2 ACEs",
      ACE_score >= 3 ~ "3 or more ACEs",
      TRUE ~ NA_character_
    ),
    ACE_group = factor(ACE_group, levels = c("None", "1 - 2 ACEs", "3 or more ACEs"))
  )

# North Dakota data
nd_data <- ACE_operated %>%
  filter(State %in% c("NORTH DAKOTA")) %>%
  filter(!is.na(race))  # Remove rows with missing race information

# South Dakota data
sd_data <- ACE_operated %>%
  filter(State %in% c("SOUTH DAKOTA")) %>%
  filter(!is.na(race))  # Remove rows with missing race information

prop.table(table(sd_data$race))


# Run regression analysis
## Log-Binomial Regression Analysis subsetted by ND & SD Racially:
perform_race_analysis_by_state <- function(data, state_name, output_dir = "state_race_analysis") {
  # Create the state-specific output directory
  state_dir <- file.path(output_dir, state_name)
  if (!dir.exists(state_dir)) {
    dir.create(state_dir, recursive = TRUE)
  }
  
  # Filter data for the specified state
  state_data <- data %>%
    filter(State == state_name) %>%
    filter(!is.na(race))
  
  # List of unique racial groups
  unique_races <- unique(state_data$race)
  
  # Loop through each racial group
  for (race_group in unique_races) {
    tryCatch({
      cat("\nProcessing race:", race_group, "in state:", state_name, "\n")
      
      # Filter data for the current racial group
      race_data <- state_data %>% filter(race == race_group)
      race_label <- gsub(" ", "_", tolower(race_group))
      
      if (nrow(race_data) == 0) {
        cat("No data found for race:", race_group, "in state:", state_name, "\n")
        next
      }
      
      
      # Log-binomial regression for outcomes
      outcome_vars <- c("heart_attack", "coronary_heart_disease", "stroke", "asthma", 
                        "skin_cancer", "other_cancer", "copd", "depressive_disorder",
                        "kidney_disease", "diabetes_bin", "arthritis", "bmi_above_25",
                        "fair_or_poor_general_health", "i4_days_poor_physical_health",
                        "i4_days_poor_mental_health", "current_smoker", "heavy_drinker")
      
      # Convert outcome variables to binary 0/1 safely
      convert_to_binary <- function(x) {
        #x <- as.character(x)
        x <- tolower(as.character(x))
        ifelse(x %in% c("yes", "1"), 1,
               ifelse(x %in% c("no", "0"), 0, NA))
      }
      
      # Convert to numeric (0/1) if not already
      race_data[outcome_vars] <- lapply(race_data[outcome_vars], convert_to_binary)
      
      # Prepare survey design
      brfss_design <- svydesign(
        id = ~X_PSU,
        weights = ~survey_weights,
        data = race_data,
        nest = TRUE
      )
      
      # Relevel ACE group
      brfss_design$variables$ACE_group <- relevel(factor(brfss_design$variables$ACE_group), ref = "None")
      
      results_summary <- data.frame(Outcome = character(), ACE_Group = character(), Risk_Ratio = numeric(), P_value = numeric())
      
      
      for (outcome in outcome_vars) {
        formula <- as.formula(paste(outcome, "~ ACE_group + income_level + education_level + gender + age_group"))
        
        # Check for single-level categorical predictors
        model_vars <- all.vars(formula)
        vars_with_single_level <- model_vars[sapply(race_data[model_vars], function(x) length(unique(na.omit(x))) < 2)]
        
        if (length(vars_with_single_level) > 0) {
          cat("Skipping outcome:", outcome, "for race:", race_group, "in state:", state_name, 
              "due to single-level predictors:", paste(vars_with_single_level, collapse = ", "), "\n")
          next
        }
        
        model <- svyglm(formula, design = brfss_design, family = quasipoisson(link = "log"))
        model_coef <- tidy(model) %>% filter(grepl("ACE_group", term))
        
        results_summary <- rbind(
          results_summary,
          model_coef %>%
            mutate(
              Outcome = outcome,
              Risk_Ratio = round(exp(estimate), 2),
              CI_Lower = round(exp(estimate - 1.96 * std.error), 2),
              CI_Upper = round(exp(estimate + 1.96 * std.error), 2),
              P_value = round(p.value, 4)
            ) %>%
            dplyr::select(Outcome, ACE_Group = term, Risk_Ratio, P_value)
        )
      }
      # Adjust P-values using False Discovery Rate (FDR)
      results_summary <- results_summary %>%
        mutate(FDR = round(p.adjust(P_value, method = "fdr"),4)) 
      
      
      # Reshape the table to wide format (one row per outcome)
      results_summary_wide <- results_summary %>%
        mutate(
          ACE_Group = case_when(
            ACE_Group == "ACE_group1 - 2 ACEs" ~ "1_2_ACEs",
            ACE_Group == "ACE_group3 or more ACEs" ~ "3_plus_ACEs"
          )
        ) %>%
        pivot_wider(
          id_cols = Outcome,
          names_from = ACE_Group, 
          values_from = c(Risk_Ratio, FDR),
          names_glue = "{ACE_Group}_{.value}"
        ) %>%
        dplyr::select(Outcome, `1_2_ACEs_Risk_Ratio`, `1_2_ACEs_FDR`, `3_plus_ACEs_Risk_Ratio`, `3_plus_ACEs_FDR`) %>%
        rename(
          `1 - 2 ACEs` = `1_2_ACEs_Risk_Ratio`,  # Rename Risk Ratio column for 1-2 ACEs
          `FDR` = `1_2_ACEs_FDR`,                # Rename FDR for 1-2 ACEs
          `3 or more ACEs` = `3_plus_ACEs_Risk_Ratio`,  # Rename Risk Ratio column for 3+ ACEs
          `FDR_3plus` = `3_plus_ACEs_FDR`        # Rename FDR for 3+ ACEs
        )
      ######################
      
      # Create dynamic header using setNames
      header_label <- paste0("Risk Ratio - Race: ", race_group)
      dynamic_header <- setNames(c(1, 4), c(" ", header_label))
      
      # Save HTML Table
      html_path <- file.path(state_dir, paste0(race_label, "_", state_name, "_race_analysis_table.html"))
      
      
      # Reshape the datta
      # Customize outcome names and reshape the data
      table_data <- results_summary_wide %>%
        mutate(
          Outcome = case_when(
            Outcome == "heart_attack" ~ "Heart Attack",
            Outcome == "coronary_heart_disease" ~ "Coronary Heart Disease",
            Outcome == "stroke" ~ "Stroke",
            Outcome == "asthma" ~ "Asthma",
            Outcome == "skin_cancer" ~ "Skin Cancer",
            Outcome == "other_cancer" ~ "Other Cancer",
            Outcome == "copd" ~ "COPD",
            Outcome == "depressive_disorder" ~ "Depressive Disorder",
            Outcome == "kidney_disease" ~ "Kidney Disease",
            Outcome == "diabetes_bin" ~ "Diabetes",
            Outcome == "arthritis" ~ "Arthritis",
            Outcome == "bmi_above_25" ~ "BMI Above 25",
            Outcome == "fair_or_poor_general_health" ~ "Fair or Poor General Health",
            Outcome == "i4_days_poor_physical_health" ~ "14+ Days Poor Physical Health",
            Outcome == "i4_days_poor_mental_health" ~ "14+ Days Poor Mental Health",
            Outcome == "current_smoker" ~ "Current Smoker",
            Outcome == "heavy_drinker" ~ "Heavy Drinker",
            TRUE ~ Outcome  # Keep any other outcomes unchanged
          ),
          `FDR` = ifelse(FDR < 0.001, "<0.001", round(FDR, 3)),
          `FDR_3plus` = ifelse(FDR_3plus < 0.001, "<0.001", round(FDR_3plus, 3))
        )
      
      # Save HTML Table (only if data is not empty)
      if (nrow(table_data) > 0) {
        html_path <- file.path(state_dir, paste0(race_label, "_", state_name, "_race_analysis_table.html"))
        
        table_data %>%
          kable(
            "html",
            col.names = c("Outcome", "1 - 2 ACEs", "FDR", "3 or more ACEs", "FDR"),
            align = c("l", "c", "c", "c", "c")
          ) %>%
          kable_styling(full_width = FALSE, position = "center") %>%
          add_header_above(dynamic_header) %>%
          row_spec(
            which(as.numeric(results_summary_wide$`FDR`) < 0.05 |
                    as.numeric(results_summary_wide$`FDR_3plus`) < 0.05),
            bold = TRUE
          ) %>%
          save_kable(html_path, self_contained = FALSE)
        race_label <- gsub(" ", "_", tolower(race_group)) # Replace spaces with underscores
        write.csv(table_data, file.path(state_dir, paste0(race_label, "_race_analysis_results.csv")), 
                  row.names = FALSE)
        
      }
    }, error = function(e) {
      cat("Error encountered for race:", race_group, "in state:", state_name, "\n")
      print(e)
    })
  }
  
  cat("\nProcessing complete for state:", state_name, "\n")
}

# Run the function for ND and SD
perform_race_analysis_by_state(nd_sd_data, "NORTH DAKOTA")
perform_race_analysis_by_state(nd_sd_data, "SOUTH DAKOTA")


# Plot the mean ACE score by State sub-setting by Race 

# Calculate mean ACE score by state and race
mean_ace_by_state <- ACE_calculated %>%
  group_by(State, race) %>%
  summarise(mean_ace_score = mean(ACE_score, na.rm = TRUE), .groups = "drop") %>%
  mutate(state = tolower(State)) # Convert state names to lowercase

###############################################################################################################
# SECTION 4 - PLOT THE RISK RATIOS OF EACH OUTCOME VARIABLE PER STATE
# Define the folder path where state files are stored
###############################################################################################################

#folder_path <- getwd() # the files are in the same working directory as this script 
folder_path <- file.path(getwd(), "results")

# List all `_multiple_regression.csv` files in the 'results' subfolders
file_paths <- list.files(
  path = folder_path,
  pattern = "_multiple_regression_result\\.csv$",  # Match files ending with _multiple_regression.csv
  recursive = TRUE,                        # Search in subdirectories
  full.names = TRUE                        # Return full file paths
)

# Debugging: Check if files were found
if (length(file_paths) == 0) {
  cat("No matching files were found. Here's a list of all files in subfolders:\n")
  print(list.files(path = folder_path, recursive = TRUE, full.names = TRUE))
  stop("Please verify file names or structure.")
}

# Read and combine all state data
# Read all files into a list of data frames
state_files <- lapply(file_paths, function(path) read.csv(path))


# # Combine all files into one data frame
all_data <- do.call(rbind, lapply(seq_along(state_files), function(i) {
  state_data <- state_files[[i]]  # Extract the data frame
  state_name <- str_extract(basename(file_paths[i]), "^[^_]+")  # Extract state name from file name
  state_data$State <- state_name  # Add state name as a column
  return(state_data)
}))


# Reshape back to long format with correct column names
all_data <- all_data %>%
  pivot_longer(cols = c("X1...2.ACEs", "X3.or.more.ACEs"),
               names_to = "ACE_Group",
               values_to = "Risk_Ratio") %>%
   mutate(
     ACE_Group = case_when(
       ACE_Group == "X1...2.ACEs" ~ "1 - 2 ACEs",
       ACE_Group == "X3.or.more.ACEs" ~ "3 or more ACEs"
     )
  ) %>%
  dplyr::select(Outcome, ACE_Group, Risk_Ratio, FDR, State)  # keep only required columns


# Filter for significant results
significant_data <- all_data %>%
  filter(FDR < 0.05) %>%
  mutate(
    Risk_Ratio_Label = round(Risk_Ratio, 2),  # Round odds ratio for display
    state = tolower(State)  # Convert state names to lowercase
  )


# We need to differentiate between the non-significant states (i.e. we have valueS for it, but it's not just significant), 
# and the ones we do not have data for (Missing values). To do this, we must populate our data with the missing states and NA values.


# Create a data frame with US states
US_Countries <- data.frame(
  US_Countries = c(
    "alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut",
    "delaware", "district of columbia","florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa",
    "kansas", "kentucky", "louisiana", "maine", "Maryland", "massachusetts", "michigan",
    "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire",
    "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma",
    "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee",
    "texas", "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming"
  )
)

# View the data frame
print(US_Countries)

## Create new data that consists all of the other states as well:
# Ensure that state names in US_Countries and all_data match in capitalization
US_Countries$US_Countries <- tolower(US_Countries$US_Countries)
all_data$State <- tolower(all_data$State)

# Find the states present in US_Countries but missing in all_data
missing_states <- setdiff(US_Countries$US_Countries, unique(all_data$State))

# Create a data frame for the missing states, filling all columns with NA except State
missing_data <- data.frame(
  Outcome = rep(NA, length(missing_states)),
  ACE_Group = rep(NA, length(missing_states)),
  Risk_Ratio = rep(NA, length(missing_states)),
  FDR = rep(NA, length(missing_states)),
  State = missing_states
)


# List of all possible Outcome variables and ACE groups
outcome_variables <- unique(all_data$Outcome)
ace_groups <- unique(all_data$ACE_Group)

# Expand the missing data to include all combinations of Outcome, ACE_Group, and missing states
expanded_missing_data <- expand.grid(
  Outcome = outcome_variables,
  ACE_Group = ace_groups,
  State = missing_states,
  Risk_Ratio = NA,
  FDR = NA
)


# Combine the original all_data with the missing data
all_data_with_others <- rbind(all_data, expanded_missing_data)

# Ensure the column order matches `all_data`
all_data_with_others <- all_data_with_others[, colnames(all_data)]

# View the resulting data frame
head(all_data_with_others)
dim(all_data_with_others)

#################################################
#Let us visualize the risk ratio geographically:

# Load required libraries
library(dplyr)
library(ggplot2)
library(usmap)
library(usmapdata)
library(sf)
library(stringr)

# Ensure output directory exists
output_dir <- "RR_by_states"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Convert state names in `all_data` to title case for correct merging
all_data2 <- all_data %>%
  mutate(state = str_to_title(State))  # "alabama" → "Alabama"

# Get centroids for state labels
state_centroids <- usmapdata::centroid_labels("states") %>%
  rename(state = full) %>%
  mutate(
    x = st_coordinates(geom)[,1],
    y = st_coordinates(geom)[,2]
  ) %>%
  dplyr::select(state, x, y)

# Merge centroid positions with `all_data2`
all_data2 <- all_data2 %>%
  left_join(state_centroids, by = "state")

# Function to Create Maps with Risk Ratio Labels
create_categorical_maps <- function(disease, ace_group, data) {
  # Filter data for the specific disease and ACE group
  map_data <- data %>%
    filter(Outcome == disease, ACE_Group == ace_group) %>%
    mutate(state = tolower(state))
  
  # Skip if no data is found
  if (nrow(map_data) == 0) {
    cat("\nSkipping", disease, "-", ace_group, "due to missing data.\n")
    return()
  }
  
  # Create base list of all states
  all_states <- data.frame(state = tolower(state.name))
  
  # Join with your map_data to ensure all states appear
  map_data <- all_states %>%
    left_join(map_data, by = "state")
  
  # Assign categories for coloring
  map_data <- map_data %>%
    mutate(
      fill_value = case_when(
        is.na(FDR) ~ "Missing Data",
        FDR >= 0.05 ~ "Non-Significant",
        Risk_Ratio > 0.5 & Risk_Ratio <= 1 ~ "0.5 - 1",
        Risk_Ratio > 1 & Risk_Ratio <= 1.25 ~ "1 - 1.25",
        Risk_Ratio > 1.25 & Risk_Ratio <= 1.5 ~ "1.25 - 1.5",
        Risk_Ratio > 1.5 & Risk_Ratio <= 2 ~ "1.5 - 2",
        Risk_Ratio > 2 & Risk_Ratio <= 3 ~ "2 - 3",
        Risk_Ratio > 3 & Risk_Ratio <= 4 ~ "3 - 4",
        Risk_Ratio > 4 & Risk_Ratio <= 6 ~ "4 - 6",
        TRUE ~ "Missing Data"
      )
    )
  
  # Define categorical colors
  category_colors <- c(
    "Missing Data" = "grey",
    "Non-Significant" = "white",
    "0.5 - 1" = "lightblue",
    "1 - 1.25" = "#FFA07A",
    "1.25 - 1.5" = "orange",
    "1.5 - 2" = "darkorange",
    "2 - 3" = "red",
    "3 - 4" = "darkred",
    "4 - 6" = "brown"
  )
  
  # Better disease name formatting
  disease_label <- case_when(
    disease == "heart_attack" ~ "Heart Attack",
    disease == "coronary_heart_disease" ~ "Coronary Heart Disease",
    disease == "stroke" ~ "Stroke",
    disease == "asthma" ~ "Asthma",
    disease == "skin_cancer" ~ "Skin Cancer",
    disease == "other_cancer" ~ "Other Cancer",
    disease == "copd" ~ "COPD",
    disease == "depressive_disorder" ~ "Depressive Disorder",
    disease == "kidney_disease" ~ "Kidney Disease",
    disease == "diabetes_bin" ~ "Diabetes",
    disease == "arthritis" ~ "Arthritis",
    disease == "bmi_above_25" ~ "BMI Above 25",
    disease == "fair_or_poor_general_health" ~ "Fair or Poor General Health",
    disease == "i4_days_poor_physical_health" ~ "14+ Days Poor Physical Health",
    disease == "i4_days_poor_mental_health" ~ "14+ Days Poor Mental Health",
    disease == "current_smoker" ~ "Current Smoker",
    disease == "heavy_drinker" ~ "Heavy Drinker",
    TRUE ~ disease
  )
  
  # Better ACE group formatting
  ace_group_label <- case_when(
    ace_group %in% c("1 - 2 ACEs", "ACE_group1 - 2 ACEs") ~ "Low ACE",
    ace_group %in% c("3 or more ACEs", "ACE_group3 or more ACEs") ~ "High ACE",
    TRUE ~ ace_group
  )
  
  # Title of plots
  map_title <- paste("Risk Ratios of", disease_label, "Among Adults with", ace_group_label, "Exposure")
  
  # Create map
  categorical_map <- plot_usmap(data = map_data, values = "fill_value", regions = "states") +
    scale_fill_manual(name = "Risk Ratio (Categorical)", values = category_colors, na.translate = FALSE) +
    labs(title = map_title, subtitle = "Based on FDR-adjusted significance and categorized risk ratios",
         x = NULL, y = NULL) +
    geom_text(data = map_data %>% filter(!is.na(Risk_Ratio)), 
              aes(x = x, y = y, label = round(Risk_Ratio, 2)),
              color = "black", size = 4, fontface = "bold") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  # Replace all characters that are not letters or numbers with an underscore
  safe_disease <- gsub("[^A-Za-z0-9]", "_", disease_label)
  safe_ace <- gsub("[^A-Za-z0-9]", "_", ace_group_label)
  
  # Save plot
  ggsave(filename = file.path(output_dir, paste0(safe_disease, "_", safe_ace, "_categorical_map.png")),
         plot = categorical_map, width = 10, height = 7, dpi = 300, bg = "white")
  ggsave(filename = file.path(output_dir, paste0(safe_disease, "_", safe_ace, "_categorical_map.pdf")),
         plot = categorical_map, width = 10, height = 7, dpi = 300, bg = "white")
}

# Loop through and generate maps
unique_diseases <- unique(all_data2$Outcome)
ace_groups <- unique(all_data2$ACE_Group)

for (disease in unique_diseases) {
  for (ace_group in ace_groups) {
    create_categorical_maps(disease, ace_group, all_data2)
  }
}

cat("Categorical maps created and saved in the RR_by_states directory. \n")


#################################################################################################################
# SECTION 5 - SUBGROUP ANALYSIS BY RACE
# This cannot run alone, you have to run the script from section 1 up till the point where "complete_ACE_dt" 
# was defined before you run this. 
# Please note that "complete_ACE_data" was defined before "complete_ACE_dt", they are different.
#################################################################################################################

# Rename variables
renamed_data <- rename_columns(complete_ACE_dt)

# Recode variables
recoded_data <- recode_variables(renamed_data)


# Calculate ACE scores
ACE_operated <- calculate_ACE_scores(recoded_data)

# Adjust weight and height
ACE_operated <- adjust_weight_height(ACE_operated)


# Group ACE score
ACE_operated <- group_ACE_score(ACE_operated)
print(table(ACE_operated$ACE_group))

#Update the values in the ACE_group column before converting them into a factor. 
#Match the existing values to the intended factor levels.
ACE_operated <- ACE_operated %>%
  mutate(
    ACE_group = case_when(
      ACE_group == "None" ~ "None",
      ACE_group == "Low ACE (1-2)" ~ "1 - 2 ACEs",
      ACE_group == "High ACE (3+)" ~ "3 or more ACEs",
      TRUE ~ NA_character_  # Handle unexpected values
    )
  )

# Standardize factor levels
ACE_operated <- ACE_operated %>%
  mutate(
    ACE_group = factor(ACE_group, levels = c("None", "1 - 2 ACEs", "3 or more ACEs")),
    income_level = factor(income_level, levels = c("< 15k", "15k – <25k", "25k – <35k", "35k – <50k", "≥ 50k")),
    gender = factor(gender, levels = c("male", "female"))
  )

# Convert specified variables to binary format with "no" as the reference level
ACE_operated <- ACE_operated %>%
  # Recode diabetes to binary with "no" as the reference
  mutate(diabetes_bin = factor(ifelse(diabetes == "yes", "yes", "no"), levels = c("no", "yes"))) %>%
  
  # Recode general_health to binary with "no" as the reference
  mutate(fair_or_poor_general_health = factor(ifelse(general_health %in% c("fair", "poor"), "yes", "no"), levels = c("no", "yes"))) %>%
  
  # Recode smoking_status to binary with "no" as the reference
  mutate(current_smoker = factor(ifelse(smoking_status %in% c("former smoker", "cr smoker - evryday", "cr smoker - somedays"), "yes", "no"), levels = c("no", "yes"))) %>%
  
  # Recode physical_health to binary with "no" as the reference
  mutate(i4_days_poor_physical_health = factor(ifelse(physical_health >= 14, "yes", "no"), levels = c("no", "yes"))) %>%
  
  # Recode mental_health to binary with "no" as the reference
  mutate(i4_days_poor_mental_health = factor(ifelse(mental_health >= 14, "yes", "no"), levels = c("no", "yes"))) 

########################################################
## Log-Binomial Regression Analysis subsetted by Race:

# Define the analysis function
perform_subset_analysis_by_race <- function(data, output_dir = "race_analysis_results_new") {
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  data <- data %>% filter(!is.na(race))
  
  # List of unique racial groups
  unique_races <- unique(data$race)
  
  # Loop through each racial group
  for (race_group in unique_races) {
    tryCatch({
      cat("\nProcessing race:", race_group, "\n")
      
      # Filter data for the current racial group
      race_data <- data %>% filter(race == race_group)
      race_label <- gsub(" ", "_", tolower(race_group))  # define early
      
      if (nrow(race_data) == 0) {
        cat("No data found for race:", race_group, "\n")
        next
      }
      
      # Prepare survey design
      brfss_design <- svydesign(
        id = ~X_PSU,
        weights = ~survey_weights,
        data = race_data,
        nest = TRUE
      )
      
      # Relevel ACE group
      brfss_design$variables$ACE_group <- relevel(factor(brfss_design$variables$ACE_group), ref = "None")
      
      # Logistic regression for outcomes
      outcome_vars <- c("heart_attack", "coronary_heart_disease", "stroke", "asthma", 
                        "skin_cancer", "other_cancer", "copd", "depressive_disorder",
                        "kidney_disease", "diabetes_bin", "arthritis", "bmi_above_25",
                        "fair_or_poor_general_health", "i4_days_poor_physical_health",
                        "i4_days_poor_mental_health", "current_smoker", "heavy_drinker")
      
      results_summary <- data.frame(Outcome = character(), ACE_Group = character(), Risk_Ratio = numeric(), P_value = numeric())
      
      for (outcome in outcome_vars) {
        formula <- as.formula(paste(outcome, "~ ACE_group + income_level + education_level + gender + age_group"))
        model <- svyglm(formula, design = brfss_design, family = quasibinomial())
        model_coef <- tidy(model) %>% filter(grepl("ACE_group", term))
        
        results_summary <- rbind(
          results_summary,
          model_coef %>%
            mutate(
              Outcome = outcome,
              Risk_Ratio = round(exp(estimate), 2),
              P_value = round(p.value, 4)
            ) %>%
            dplyr::select(Outcome, ACE_Group = term, Risk_Ratio, P_value)
        )
      }
      
      # Adjust P-values using False Discovery Rate (FDR)
      results_summary <- results_summary %>%
        mutate(FDR = round(p.adjust(P_value, method = "fdr"),4)) %>%  # Apply FDR correction
        dplyr::select(Outcome, ACE_Group, Risk_Ratio, FDR)  # Remove P_value before writing CSV
      
      
      #######################
      # Reshape the table to wide format (one row per outcome)
      results_summary_wide <- results_summary %>%
        mutate(
          ACE_Group = case_when(
            ACE_Group == "ACE_group1 - 2 ACEs" ~ "1_2_ACEs",
            ACE_Group == "ACE_group3 or more ACEs" ~ "3_plus_ACEs"
          )
        ) %>%
        pivot_wider(
          names_from = ACE_Group, 
          values_from = c(Risk_Ratio, FDR),
          names_glue = "{ACE_Group}_{.value}"
        ) %>%
        dplyr::select(Outcome, `1_2_ACEs_Risk_Ratio`, `1_2_ACEs_FDR`, `3_plus_ACEs_Risk_Ratio`, `3_plus_ACEs_FDR`) %>%
        rename(
          `1 - 2 ACEs` = `1_2_ACEs_Risk_Ratio`,  # Rename Risk Ratio column for 1-2 ACEs
          `FDR` = `1_2_ACEs_FDR`,                # Rename FDR for 1-2 ACEs
          `3 or more ACEs` = `3_plus_ACEs_Risk_Ratio`,  # Rename Risk Ratio column for 3+ ACEs
          `FDR_3plus` = `3_plus_ACEs_FDR`        # Rename FDR for 3+ ACEs
        )
      ######################
      
      # Create dynamic header using setNames
      header_label <- paste0("Risk Ratio - Race: ", race_group)
      dynamic_header <- setNames(c(1, 4), c(" ", header_label))
      
      # Save HTML Table
      html_path <- file.path(output_dir, paste0(race_label,  "_race_analysis_table.html"))
      
      
      # Customize outcome names and reshape the data
      table_data <- results_summary_wide %>%
        mutate(
          Outcome = case_when(
            Outcome == "heart_attack" ~ "Heart Attack",
            Outcome == "coronary_heart_disease" ~ "Coronary Heart Disease",
            Outcome == "stroke" ~ "Stroke",
            Outcome == "asthma" ~ "Asthma",
            Outcome == "skin_cancer" ~ "Skin Cancer",
            Outcome == "other_cancer" ~ "Other Cancer",
            Outcome == "copd" ~ "COPD",
            Outcome == "depressive_disorder" ~ "Depressive Disorder",
            Outcome == "kidney_disease" ~ "Kidney Disease",
            Outcome == "diabetes_bin" ~ "Diabetes",
            Outcome == "arthritis" ~ "Arthritis",
            Outcome == "bmi_above_25" ~ "BMI Above 25",
            Outcome == "fair_or_poor_general_health" ~ "Fair or Poor General Health",
            Outcome == "i4_days_poor_physical_health" ~ "14+ Days Poor Physical Health",
            Outcome == "i4_days_poor_mental_health" ~ "14+ Days Poor Mental Health",
            Outcome == "current_smoker" ~ "Current Smoker",
            Outcome == "heavy_drinker" ~ "Heavy Drinker",
            TRUE ~ Outcome  # Keep any other outcomes unchanged
          ),
          `FDR` = ifelse(FDR < 0.001, "<0.001", round(FDR, 3)),
          `FDR_3plus` = ifelse(FDR_3plus < 0.001, "<0.001", round(FDR_3plus, 3))
        )
      
      
      # Save HTML Table (only if data is not empty)
      if (nrow(table_data) > 0) {
        # Save HTML Table with Race Name Included in Header
        html_path <- file.path(output_dir, paste0(race_label, "_subset_analysis_table_output.html"))
        
        table_data %>%
          kable(
            "html",
            col.names = c("Outcome", "1 - 2 ACEs", "FDR", "3 or more ACEs", "FDR"),
            align = c("l", "c", "c", "c", "c")
          ) %>%
          kable_styling(full_width = FALSE, position = "center") %>%
          add_header_above(dynamic_header) %>%
          row_spec(
            which(as.numeric(results_summary_wide$`FDR`) < 0.05 |
                    as.numeric(results_summary_wide$`FDR_3plus`) < 0.05),
            bold = TRUE
          ) %>%
          save_kable(html_path, self_contained = FALSE)
        # # Save results as CSV
        write.csv(table_data, file.path(output_dir, paste0(race_label, "_race_analysis_results.csv")), 
                  row.names = FALSE)
        
        # Save HTML Table with Formatted Output
        # Save HTML Table with Race Name Included in Header
      }      
    }, error = function(e) {
      cat("Error encountered for race:", race_group, "\n")
      print(e)
    })
  }
  
  cat("\nProcessing complete! Results saved in", output_dir, "\n")
}


# Run the function and obtain results for the subgroup analysis by race
perform_subset_analysis_by_race(ACE_operated)
