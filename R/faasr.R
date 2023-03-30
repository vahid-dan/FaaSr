# using Tidyverse style guide - https://style.tidyverse.org/index.html

# The package implements the following functions:
# faasr_start - start the execution of the function; takes a JSON string as argument
# faasr_parse - parse the JSON payload, returns a parsed list if payload validation is successful
# faasr_get_user_function_args - extract user function arguments from parsed list
# faasr_put_file - put a file from local storage to S3
# faasr_get_file - get a file from S3 to local storage
# faasr_log - append to a log file stored in S3
# faasr_trigger - generate trigger(s) for any additional user-specified actions


# faasr_start is the function that starts execution of the user-supplied function
# faasr_start is the entry point invoked by the FaaS platform (e.g. OpenWhisk, Lambda, GH Actions) when a container starts
# faasr_payload is a JSON file payload containing all configuration key/value pairs for this invocation
faasr_start <- function(faasr_payload) {
  # First, call faasr_parse to validate the JSON payload, return parsed list
  faasr <- faasr_parse(faasr_payload)

  # TBD need to check for error and return
  
  # Now extract the name of the user-provided function to invoke
  user_function = get(faasr$user_function)
  
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


  

