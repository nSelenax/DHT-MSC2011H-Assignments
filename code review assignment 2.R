# MSC2011H A2
# Ning Xu

# Prompt the user to enter a three digit positive number (get user input) #Bre - Your comments make it clear what your code is doing 
num <- readline("Please enter a three digit positive number: ")

# Convert user input into numeric.
num <- as.numeric(num)

# Check if the user input is numeric (valid). If not, print an error message and quit. #maybe mention here in your comments what case causes an invalid input (ie what does your code translate to)
if (is.na(num) | (num <= 0) | !(100 <= num & num <= 999) | (num != floor(num))) {
  print("Invalid input. Please retry. Quitting program...") #tried putting in a 4 digit number and print function worked, 
} else {
  # Get the individual digits representing the hundreds, tens, and ones of num
  hundreds <- num %/% 100
  tens <- (num %% 100) %/% 10
  ones <- num %% (hundreds * 100 + tens * 10)
  # Calculate the sum of the cubes of num's own digits #smart way to do this (never thought of it this but using a vector makes sense as the cubed function will apply to all the elements in the vector)
  sum_cube <- sum(c(hundreds, tens, ones) ^ 3)
  # Check if the number is narcissistic by checking if the sum of the cubes of num's own digits is equal to num
  if (sum_cube == num) {
    # Display the result with an appropriate message
    print(paste(num, "is a narcissistic number."))
  } else {
    print(paste(num, "is not a narcissistic number."))
  }
} #I ran the code and everything functions as it should and all requirements of the assignment were achieved 