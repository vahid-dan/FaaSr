# using Tidyverse style guide - https://style.tidyverse.org/index.html

# The package implements the following functions:
# faasr_start - start the execution of the function; takes a JSON string as argument
# faasr_parse - parse the JSON payload, returns a parsed list if payload validation is successful
# faasr_get_user_function_args - extract user function arguments from parsed list
# faasr_put_file - put a file from local storage to S3
# faasr_get_file - get a file from S3 to local storage
# faasr_log - append to a log file stored in S3
# faasr_trigger - generate trigger(s) for any additional user-specified actions

library("jsonlite")
library("aws.s3")

# faasr_start is the function that starts execution of the user-supplied function
# faasr_start is the entry point invoked by the FaaS platform (e.g. OpenWhisk, Lambda, GH Actions) when a container starts
# faasr_payload is a JSON file payload containing all configuration key/value pairs for this invocation
faasr_start <- function(faasr_payload) {
  # First, call faasr_parse to validate the JSON payload, return parsed list
  faasr <- faasr_parse(faasr_payload)

  # TBD need to check for error and return
  
  # Now extract the name of the user-provided function to invoke
  user_function = get(faasr$FunctionInvoke)
  
  # Invoke the user function, passing the parsed list as argument
  faasr_result <- user)function_invoke(faasr)
  
  # Now trigger the next actions(s) if any
 faaSr_trigger(faaSr)
}

# faasr_parse is the function that parses and validates the JSON payload containing all configuration key/value pairs for this invocation
faasr_parse <- function(faasr_payload) {
  # First, attempt to read JSON
  faasr <- read_json(faasr_payload)

  # TBD need to perform all validations here
  # return an error if validation fails
}

faasr_get_user_function_args <- function(faasr) {
  # faasr is the list parsed/validated from JSON payload
  # First extract the name of the user function to invoke
  user_function = faasr$FunctionInvoke
  
  # Now extract the arguments for this function
  args = faasr$FunctionList[[user_function]]$Arguments
}

faasr_put_file <- function(faasr, server_name, local_folder, local_file, remote_folder, remote_file) {
  # This should put a file into S3
  # faasr is the list parsed/validated from JSON payload
  # The name of the S3 server is server_name, a string that references an entry in the list stored in faasr with S3 configuration
  # local and remote folder file names arer strings
  
  # TBD validate server_name exists
  
  # TBD prepare env variables for S3 access
  
  # TBD use aws.s3 to put data into the server
  
  # TBD log any errors
}

faasr_get_file <- function(faasr, server_name, remote_folder, remote_file, local_folder, local_file) {
  # This should get a file from S3
  # faasr is the list parsed/validated from JSON payload
  # The name of the S3 server is server_name, a string that references an entry in the list stored in faasr with S3 configuration
  # local and remote folder file names arer strings
  
  # TBD validate server_name exists
  
  # TBD prepare env variables for S3 access
  
  # TBD use aws.s3 to get data from the server
  
  # TBD log any errors
}

  

