replace_by_NA <- function(data, value_to_replace) {
  print(colnames(data()))
  
  for (col in colnames(data())) {
    data()[[col]][data()[[col]] == value_to_replace] <- NA
  }
  
  return(data())
}

replace_missing_values <- function(df, var, numericMethod, categoricalMethod) {
  # Create a copy of the dataframe to modify
  df_modified <- df
  print("in")
  # Replace missing values based on user's choice
  if (is.numeric(df[[var]])) {
    print("numeric")
    if (numericMethod == "Mean") {
      print("mean")
      df_modified[[var]][is.na(df[[var]])] <- mean(df[[var]], na.rm = TRUE)
      print('uwu')
    } else if (numericMethod == "Median") {
      print("median")
      df_modified[[var]][is.na(df[[var]])] <- median(df[[var]], na.rm = TRUE)
    }
  } else {
    print("else")
    if (categoricalMethod == "Most Frequent") {
      print("most")
      df_modified[[var]][is.na(df[[var]])] <- names(sort(table(df[[var]], useNA = "always"), decreasing = TRUE))[1]
    } else if (categoricalMethod == "Least Frequent") {
      print("least")
      df_modified[[var]][is.na(df[[var]])] <- names(sort(table(df[[var]], useNA = "always"), decreasing = FALSE))[1]
    }
  }
  
  return(df_modified)
}

replace_missing_values_all <- function(df, numericMethod, categoricalMethod) {
  #observe({
    for (col in names(df())) {
      print(replace_missing_values(df(), col, numericMethod, categoricalMethod) )
      df()[[col]] <- replace_missing_values(df(), col, numericMethod, categoricalMethod)
    }
  #})
  print(df())
  return(df())
}


