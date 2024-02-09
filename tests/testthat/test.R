context("correct probabilities")

# Read in initial data
customers <- fread("data_customer.csv", sep = "auto", header = TRUE)
personal <- fread("data_personal.csv", sep = "auto", header = TRUE)

# Merge data based on "CustomerId" --> Outer Join
data <- merge(customers, personal, by="CustomerId", all = TRUE)

# Following columns to factors: Exited & Gender
data$Exited <- as.factor(data$Exited)
data$Gender <- as.factor(data$Gender)

test_that("Ensure that probabilities are correct", {
  expect_lt(
    predictChurn(data, 15662641),
    predictChurn(data, 15653251)
    )
})
