# Assignment 2
# Ning Xu

# Prompt the user to enter a three digit positive number (get user input)
num <- readline("Please enter a three digit positive number: ")

# Convert user input into numeric.
num <- as.numeric(num)

# Check if the user input is numeric (valid). If not, print an error message and quit.
if (is.na(num) | (num <= 0) | !(100 <= num & num <= 999) | (num != floor(num))) {
  print("Invalid input. Please retry. Quitting program...")
} else {
  # Get the individual digits representing the hundreds, tens, and ones of num
  hundreds <- num %/% 100
  tens <- (num %% 100) %/% 10
  ones <- num %% (hundreds * 100 + tens * 10)
  # Calculate the sum of the cubes of num's own digits
  sum_cube <- sum(c(hundreds, tens, ones) ^ 3)
  # Check if the number is narcissistic by checking if the sum of the cubes of num's own digits is equal to num
  if (sum_cube == num) {
    # Display the result with an appropriate message
    print(paste(num, "is a narcissistic number."))
  } else {
    print(paste(num, "is not a narcissistic number."))
  }
}