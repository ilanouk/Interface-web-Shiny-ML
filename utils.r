replace_by_NA <- function(df, value_to_replace) {
  print("in replace by NA")
    # Replace NA values with 0 using dplyr pipe operator
      df_value <- df %>% mutate_all(~ ifelse(. == value_to_replace, NA, .))
      #data <- reactiveVal({df_value})
    return(df_value)
}
replace_missing_values <- function(df, numericMethod, categoricalMethod) {
#Fonction qui remplace tout les NA par la méthode choisi par l'utilisateur
  df_value <- df
  for (var in colnames(df_value)) {
  # Replace missing values based on user's choice
    if (is.numeric(df_value[[var]])) {
      if (numericMethod == "Mean") {
        df_value[[var]][is.na(df_value[[var]])] <- mean(df_value[[var]], na.rm = TRUE)
      
        } else if (numericMethod == "Median") {
        df_value[[var]][is.na(df_value[[var]])] <- median(df_value[[var]], na.rm = TRUE)
      }
    } else {
      if (categoricalMethod == "Most Frequent") {
        most_frequent_value <- names(sort(table(df_value[[var]], useNA = "no"), decreasing = TRUE))[1]
        df_value[[var]][is.na(df_value[[var]])] <- most_frequent_value
        
      } else if (categoricalMethod == "Least Frequent") {
        frequencies <- table(df_value[[var]], useNA = "always")
        least_frequent_value <- names(sort(table(df_value[[var]], useNA = "always"), decreasing = FALSE))[1]


        if (is.na(least_frequent_value)) {
          # Replace NAs with the most frequent non-NA value
          most_frequent_non_na <- names(sort(frequencies, decreasing = TRUE))[2]  # Assumes there is at least one non-NA value
          df_value[[var]][is.na(df_value[[var]])] <- most_frequent_non_na
        } else {
          # Replace NAs with the least frequent non-NA value
          df_value[[var]][is.na(df_value[[var]])] <- least_frequent_value
        }
      }
    }
  }
#  data <- reactiveVal({df_value})
  return(df_value)
}

#Pour la normalisation : iris_norm <- as.data.frame(lapply(iris[1:4], min_max_norm))

class_diff <- function(df,classe){
 # df <- data()
  class_counts <- table(df$classe)
  print(df$classe)
  class_proportions <- prop.table(class_counts)
  return (class_proportions)
}
