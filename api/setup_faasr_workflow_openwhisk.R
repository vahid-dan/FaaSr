#!bin/local/usr/Rscript
# create an action-server list

# setup_resource_group <- function(){}
# create_resource_group <- function(){}

action_lists <- function(faasr){
	# empty list
	action_list <- list()
	# for each function, iteratively collect server names and action names
	for (fn in names(faasr$FunctionList)){
		server_name <- faasr$FunctionList[[fn]]$FaaSServer
		# if FaaStype is Openwhisk, add it to the list
		if (faasr$ComputeServers[[server_name]]$FaaSType == "OpenWhisk"){
			action_name <- faasr$FunctionList[[fn]]$Actionname
			action_list[[server_name]] <- unique(c(action_list[[server_name]],action_name))
		}	
	}
	return(action_list)
}

# create an api key
create_api_key <- function(){
	cat("Create a new api-key?[y/n]")
	# Create an API key
	while(TRUE){			
				check <- readLines(con="stdin", 1)
				if (check=="y"){
					cat("type api-key name: ")
					# receive the user's input for the namespace name
					name <- readLines(con="stdin", 1) 
					# create a new api key
					command <- paste0("ibmcloud iam api-key-create ",name)
					cat("creating a new api-key\n")
					response<-system(command, intern=TRUE,ignore.stdout=TRUE, ignore.stderr=TRUE)
					# save the result to the json file
					cat("successful", "\n")
					# parse only api key from the response
					api_key <- strsplit(response[10], "\\s+")[[1]][3]
					return(api_key)
					break
				}else if(check=="n"){
					return(FALSE)
					break
				}else{cat("Enter \"y\" or \"n\": ")} 
			}

 
}

# create a namespace
create_namespace <- function(name){
	command <- paste0("ibmcloud fn namespace create ",name)
	cat("creating a new namespace\n")
	system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
	# get the id of the namespace
	command <- paste0("ibmcloud fn namespace get ",name," --properties")
	response <- system(command, intern=TRUE)		
	# parse only ID from the response
	name_id <- sub("^ID:\\s*", "", grep("^ID:", response, value = TRUE))
	return(name_id)		
}

# target a namespace
target_namespace <- function(server,faasr){
	namespace<-faasr$ComputeServers[[server]]$Namespace
	command <- paste("ibmcloud fn namespace target",namespace)
	cat("targetting a new namespace\n")
	check <- system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
	# if check == 0, i.e., no errors, return TRUE, retrieve ID in case that a user only doesn't provide ID
	if (check==0){
		command <- paste0("ibmcloud fn namespace get ",namespace," --properties")
		response <- system(command, intern=TRUE)		
		# parse only ID from the response
		name_id <- sub("^ID:\\s*", "", grep("^ID:", response, value = TRUE))
		return(name_id)
	# if check != 0, i.e., errors, ask the user to create a new one
	}else{
		cat("Invalid Namespace\n")
		cat("Create a new Namespace?[y/n]")
		while(TRUE){			
				check <- readLines(con="stdin", 1)
				if (check=="y"){
					cat("type Namespace name: ")
					# receive the user's input for the namespace name
					name <- readLines(con="stdin", 1) 
					# create a new namespace
					namespace <- create_namespace(name)
					# save the result to the json file
					faasr$ComputeServers[[server]]$Namespace <- namespace
					cat("successful", "\n")
					break
				}else if(check=="n"){
					stop() 
				}else{cat("Enter \"y\" or \"n\": ")} 
			}
		# recursively target a namespace
		target_namespace(server,faasr)
	}
}

# XXXXXXX This is for later usage. XXXXXXXXXXXXXXXXX
# login to the ibm cloud by using api keys
login_ibm <- function(server,faasr){
	# retrieve api key and try login by using it
	api_key <- faasr$ComputeServers[[server]]$API.key
	command <- paste("ibmcloud login --apikey",api_key)
	check <- system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
	# if check == 0, i.e., no errors, return TRUE
	if (check==0){return(TRUE)
	# if check != 0, i.e., errors, ask the user to create a new one
	}else{
		cat("Invalid API key\n")
		stop()
		#cat("Create a new API.key?[y/n]")
		#while(TRUE){			
		#		check <- readLines(con="stdin", 1)
		#		if (check=="y"){
		#			cat("type API key name: ")
		#			name <- readLines(con="stdin", 1) 
		#			# create a new api key with user-provided name
		#			api_key <- create_api_key(name)
		#			faasr$ComputeServers[[server]]$API.key <- api_key
		#			cat("successful", "\n")
		#			break
		#		}else if(check=="n"){
		#			stop() 
		#		}else{cat("Enter \"y\" or \"n\": ")}
		#	} 
		}
	# recursively try login
	#login_ibm(server, faasr)
}


# create an action
create_action <- function(actionname, faasr){
	# actioncontainer can be either default or user-customized
	if (length(faasr$Actioncontainer[[actionname]])==0){
		actioncontainer <- "faasr/base-tidyverse-openwhisk"
	}else{
		actioncontainer <- faasr$Actioncontainer[[actionname]]
	}
	# create a function with maximum timeout and 512MB memory space
	command <- paste("ibmcloud fn action create",actionname,"--docker",actioncontainer,"--timeout 600000 --memory 512")
	cat("creating a new action\n")
	check <- system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
	# if action already exists, ask the user to update the action
	if (check[1]==153){
		print("error: action name already exists")
		cat("Do you want to update the action?[y/n]")
		while(TRUE){
			check <- readLines(con="stdin", 1)
			if (check=="y"){
				# update the action
				command <- paste("ibmcloud fn action update",actionname,"--docker",actioncontainer,"--timeout 600000 --memory 512")
				cat("updating an action\n")
				system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
				cat("successful", "\n")
				break
			}else if(check=="n"){
				stop() 
			}else{cat("Enter \"y\" or \"n\": ")} 
			}
	}
}

# update the payload: there could be new API keys, Namespaces
update_payload <- function(faasr){
	# Update the payload
	cat("updating a payload\n")
	payload<-jsonlite::toJSON(faasr, auto_unbox=TRUE)
	payload<-jsonlite::prettify(payload)
	writeLines(payload, args[1])
}

# Body
# receive the user's json file as an argument
args<-commandArgs(trailingOnly = TRUE)
faasr<-jsonlite::fromJSON(args[1])

# create a server-action set
action_list <- action_lists(faasr)

# check servers and actions, create actions
for (server in names(action_list)){
	#login_ibm(server, faasr)
	api_key <- create_api_key()
	if (api_key!=FALSE){faasr$ComputeServers[[server]]$API.key <- api_key}
	name_id <- target_namespace(server, faasr)
	faasr$ComputeServers[[server]]$Namespace <- name_id	
	for (act in action_list[[server]]){
		create_action(act, faasr)
	}
}
# update payload
update_payload(faasr)