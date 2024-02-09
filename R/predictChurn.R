predictChurn <- function(data, cId){

  if(!any(data$CustomerId == cId)) {
    stop("The specified CustomerId does not exist.")
  } else {

    print("The specified CustomerId has been located.")

    # Logistic Regression Model for Churn Probability using glm function
    churn_model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
                       data = data,
                       family = "binomial")

    print("Logistic Regression Model for churn probability for this dataset has been calculated successfully.")

    # Select specific customer
    customer_data <- data[data$CustomerId == cId, ]

    print("Now calculating the churn prediction for the provided CustomerId ...")

    # Predicting Churn Probability
    customer_data$ChurnProbability <- predict(churn_model, customer_data, type = "response")

    # Return Churn Probability for
    return(customer_data$ChurnProbability)

  }
}

usethis::use_testthat
