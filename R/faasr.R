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
library("jsonvalidate")
library("aws.s3")
library("RCurl")
library("httr")

# faasr_start is the function that starts execution of the user-supplied function
# faasr_start is the entry point invoked by the FaaS platform (e.g. OpenWhisk, Lambda, GH Actions) when a container starts
# faasr_payload is a JSON file payload containing all configuration key/value pairs for this invocation
faasr_start <- function(faasr_payload) {
  # First, call faasr_parse to validate the JSON payload, return parsed list
  faasr <- faasr_parse(faasr_payload)
 
  # TBD first, need to check for parsing error and schema compliance and return if there's an error parsing/validating the JSON file
  
  # TBD second, need to check if the log server is correctly configured, otherwise return an error
  
  # TBD third, need to check if the rest of the JSON payload is correctly configured - for anything incorrect, use faasr_log to log to S3, and then return an error
  
  # TBD fourth, need to check if there are no invocation cycles
  
  # TBD if there is an empty InvocationID in the JSON, generate a UUID at random and add to faasr
  # i.e. the first function in the invocation generates a UUID that is carried over to any others it triggers
  
  # Now extract the name of the user-provided function to invoke
  user_function = get(faasr$FunctionInvoke)
  
  # TBD let's do this later - need to come up with a strategy for dealing with a function that is a dependence "sink", 
  # i.e. it depends/is triggered by multiple other functions and should only execute when the last trigger has been received
  
  # Invoke the user function, passing the parsed list as argument
  faasr_result <- user_function(faasr)
  
  # Now trigger the next actions(s) if any
 faaSr_trigger(faasr)
}

# faasr_parse is the function that parses and validates the JSON payload containing all configuration key/value pairs for this invocation
faasr_parse <- function(faasr_payload) {
  # First, attempt to read JSON
  faasr <- read_json(faasr_payload)
  return(faasr)
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
  target_s3 <- faasr$DataStores[[server_name]]
  Sys.setenv("AWS_ACCESS_KEY_ID"=target_s3$AccessKey, "AWS_SECRET_ACCESS_KEY"=target_s3$SecretKey, "AWS_DEFAULT_REGION"=target_s3$Region)
  put_file <- paste0(local_folder,"/",local_file)
  put_file_s3 <- paste0(remote_folder, "/", remote_file)
  put_object(file=put_file, object=put_file_s3, bucket=target_s3$Bucket)
  
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
  target_s3 <- faasr$DataStores[[server_name]]
  Sys.setenv("AWS_ACCESS_KEY_ID"=target_s3$AccessKey, "AWS_SECRET_ACCESS_KEY"=target_s3$SecretKey, "AWS_DEFAULT_REGION"=target_s3$Region)
  get_file <- paste0(local_folder,"/",local_file)
  get_file_s3 <- paste0(remote_folder, "/", remote_file)
  save_object(get_file_s3, file=get_file, bucket=target_s3$Bucket)
  # TBD validate server_name exists
  
  # TBD prepare env variables for S3 access
  
  # TBD use aws.s3 to get data from the server
  
  # TBD log any errors
}

faasr_log <- function(faasr,log_message) {
  # Logs a message to the S3 log server
  # faasr is the list parsed/validated from JSON payload
  # the name of the S3 server is implicit from the validated JSON payload, key LoggingServer
  # the name of the log file should be folder "logs" and file name "faasr_log_" + InvocationID + ".txt"
  
  # extract name of logging server
  log_server_name = faasr$LoggingServer
  
  # TBD validate server_name exists
  
  # TBD prepare env variables for S3 access
  
  # TBD set file name to be "faasr_log_" + faasr$InvocationID + ".txt"
  
  # TBD use aws.s3 to get log file from the server
  
  # TBD append message to the local file
  
  # TBD use aws.s3 to put log file back into server
}

faasr_trigger <- function(faasr) {
  # Sends triggers to functions that the current function should invoke
  # faasr is the list parsed/validated from JSON payload
  
  # First extract the name of the user function
  user_function = faasr$FunctionInvoke

  # Now get the list of InvokeNext
  invoke_next = faasr$FunctionList[[user_function]]$InvokeNext
 
  # check if the list is empty or not
  if (length(invoke_next) == 0){cat('{\"msg\":\"success_',user_function,'\"}')} else {
    
    # TBD iterate through invoke_next and use FaaS-specific mechanisms to send trigger
    # use "for" loop to iteratively check functions in invoke_next list
    for (invoke_next_function in invoke_next){
		  
       #Change the FunctionInvoke to next function name
       faasr$FunctionInvoke <- invoke_next_function
		   
       #TBD generate unique InvocationID for next functions : [function name]-[random number+characters]-[time]
       #faasr$InvocationID <- paste0("F1-",paste(sample(c(1:9, letters), 6, replace = TRUE), collapse=""),"-",as.character(Sys.time()))
		   
       # determine FaaS server name via faasr$FunctionList[[invoke_next_function]]$FaaSServer
       next_server <- faasr$FunctionList[[invoke_next_function]]$FaaSServer
		   
       # validate that FaaS server name exists in faasr$ComputeServers list
       if (next_server %in% names(faasr$ComputeServers)){NULL
       } else{cat('{\"msg\":\"invalid server name\"}')
               break}
       
       # check FaaSType from the named compute server
       next_server_type <- faasr$ComputeServers[[next_server]]$FaaSType
      
       # if OpenWhisk - use OpenWhisk API to send trigger
       if (next_server_type=="OpenWhisk"){ 
        api_key <- faasr$ComputeServers[[next_server]]$API.key
		    region <- faasr$ComputeServers[[next_server]]$Region
		    namespace <- faasr$ComputeServers[[next_server]]$Namespace
		    actionname <- faasr$FunctionList[[invoke_next_function]]$Actionname
		
		    #Openwhisk - Get a token by using the API key
		    url <- "https://iam.cloud.ibm.com/identity/token"
		    body <- list(grant_type = "urn:ibm:params:oauth:grant-type:apikey",apikey=api_key)
		    headers <- c("Content-Type" = "application/x-www-form-urlencoded")
		    response <- POST(url = url,body = body,encode = "form",add_headers(.headers = headers))
		    result <- content(response, as = "parsed")
		    token <- paste("Bearer",result$access_token)

		    #Openwhisk - Invoke next action - action name should be described.
		    url_2<- paste0("https://",region,".functions.cloud.ibm.com/api/v1/namespaces/",namespace,"/actions/faasr","?blocking=true&result=true")
		    headers_2 <- c("accept"="application/json", "authorization"=token, "content-type"="application/json")
		    data_2<-toJSON(faasr)
		    curl_opts_2 <- list(post=TRUE, httpheader=headers_2, postfields=data_2)
		    response_2 <- curlPerform(url=url_2, .opts=curl_opts_2)
        } else {cat('{\"msg\":\"success_',user_function,'_next_action_',func,'will_be_executed by_',next_server_type,'\"}')}
      
        # if Lambda - use Lambda API
		    if (next_server_type=="Lambda"){
        
        } else {}
        
        # if GitHub Actions - use GH Actions
        if (next_server_type=="GitHubActions"){
        
        } else {}
	   }
    
}

  

