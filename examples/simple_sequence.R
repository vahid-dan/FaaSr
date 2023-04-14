# here’s a test function sequence
# F1 computes the product of each element of two vectors stored as CSV files in S3_A
# F2 computes the square of each element of a vector stored as CSV file in S3_A

# The user will access FaaSr functions as a library
library("FaaSr")
        
F1 <- function(faasr) {
  # print("Hello from inside function F1")
  args <- faasr_get_user_function_args(faasr)
  # return arguments for this function (F1)
  # expects input1, input2: input file names; output: 
  faasr_get_file(faasr, "S3_A", "folder", args$input1, "local", "input1.csv")
  faasr_get_file(faasr, "S3_A", "folder", args$input2, "local", "input2.csv")
  # need to implement output <- input1 * input2
# if there's a header in each csv file, "header=T"
  input1 <- read.table("local/input1.csv", sep=",", header=F)
  input2 <- read.table("local/input2.csv", sep=",", header=F)
  output <- input1 * input2
  # if there's a header in csv file, "col.names=T"
  write.table(output, file="local/output.csv", sep=",", row.names=F, col.names=F)
  faasr_put_file(faasr, "S3_A", "local", "output.csv", "folder", args$output)
  faasr_log(faasr, "User function F1 finished")
}	


F2 <- function(faasr) {
  # print("Hello from inside function Fs")
  args <- faasr_get_user_function_args(faasr)
  # return arguments for this function (F2)
  # expects input1, input2: input file names; output: 
  faasr_get_file(faasr, "S3_A", "folder", args$input, "local", "input.csv")
  # need to implement output <- input * input
  # if there's a header in each csv file, "header=T"
  input <- read.table("local/input.csv", sep=",", header=F)
  output <- input^2
  # if there's a header in csv file, "col.names=T"
  write.table(output, file="local/output.csv", sep=",", row.names=F, col.names=F)   
  faasr_put_file(faasr, "S3_A", "local", "output.csv", "folder", args$output)
  faasr_log(faasr, "User function F2 finished")
}	
