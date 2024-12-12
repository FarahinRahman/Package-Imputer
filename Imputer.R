#' Handle Missing Values
#'
#' @param df A data frame containing columns "age", "salary", and "experience".
#' @param fill_value_age
#' @param fill_value_salary
#' @param fill_value_experience
#'
#' @return A data frame with missing values in the specified columns handled.
#'
#'
#'
#' @examples
#' df<- data.frame(
#'age = c(25, NA, 35, 40, NA),
#'salary = c(50000, 55000, NA, 60000, 65000),
#'experience = c(2, 5, NA, 8, NA)
#')
#' @export
handle_missing_specific <- function(df,
                                    fill_value_age = NULL,
                                    fill_value_salary = NULL,
                                    fill_value_experience = NULL) {


  # Fill age column
  if ("age" %in% colnames(df) && !is.null(fill_value_age)) {
    df$age <- replace(df$age, is.na(df$age), fill_value_age)
  }

  # Fill salary column
  if ("salary" %in% colnames(df) && !is.null(fill_value_salary)) {
    df$salary <- replace(df$salary, is.na(df$salary), fill_value_salary)
  }

  # Fill experience column
  if ("experience" %in% colnames(df) && !is.null(fill_value_experience)) {
    df$experience <- replace(df$experience, is.na(df$experience), fill_value_experience)
  }

  return(df)
}

# Sample data frame
df <- data.frame(
  age = c(25, NA, 35, 40, NA),
  salary = c(50000, 55000, NA, 60000, 65000),
  experience = c(2, 5, NA, 8, NA)
)

# View original data frame
print("Original Data Frame:")
print(df)

# Handle missing values using fixed values
df_handled <- handle_missing_specific(
  df,
  fill_value_age = 30,         # Fixed value for age
  fill_value_salary = 50000,  # Fixed value for salary
  fill_value_experience = 2  # Fixed value for experience
)

# View updated data frame
print("Data Frame with Missing Values Handled:")
print(df_handled)
devtools::check()

