library(dplyr)

hello_world <- function(name) {
  # Object assignment
  if (nchar(name) > 8) {
    greeting <- paste("Hello, Dr.", name, "!")
  } else {
    greeting <- paste("Hi,", name, "!")
  }
  
  # Return both greetings
  return(greeting)
}

#more than 8 characters
result <- hello_world("Kaila Scott-Charles")

print(result)

#less than 8 characters
result <- hello_world("Kaila")

print(result)
