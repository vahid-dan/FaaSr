# here’s a test function sequence
# F1 computes the product of each element of two vectors stored as CSV files in S3_A
# F2 computes the square of each element of a vector stored as CSV file in S3_A

F1 <- function(faasr) {
  # print(“Hello from inside function F1”)
  args <- faasr_get_user_function_args(faasr)
  # return arguments for this function (F1)
  # expects input1, input2: input file names; output: 
  faasr_get_file(faasr, “S3_A”, “folder”, args$input1, “local”, “input1.csv”)
  faasr_get_file(FaaSr, “S3_A”, “folder”, args$input2, “local”, “input2.csv”)
  # need to implement output <- input1 * input2
  faasr_put_file(faasr, “S3_A”, “folder”, args$output, “local”, “output.csv”)
  faasr_log(faasr, “User function F1 finished”)
}	


F2 <- function(faasr) {
  # print(“Hello from inside function Fs”)
  args <- faasr_get_user_function_args(faasr)
  # return arguments for this function (F2)
  # expects input1, input2: input file names; output: 
  faasr_get_file(faasr, “S3_A”, “folder”, args$input1, “local”, “input.csv”)
  # need to implement output <- input * input
  faasr_put_file(faasr, “S3_A”, “folder”, args$output, “local”, “output.csv”)
  faasr_log(faasr, “User function F2 finished”)
}	
