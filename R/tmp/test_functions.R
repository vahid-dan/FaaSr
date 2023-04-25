#test functions - work in progress

#lock implementation - acquire
faasr_acquire<-function(faasr){
	Lock<-faasr_rsm(faasr)
	#if function acquires a lock, it gets out of the loop
	while(TRUE){
		if (Lock){return(TRUE)}else
		{
		#if it doesn't, keep trying to get the flag&lock
		Lock<-faasr_rsm(faasr)
		}
	}
}


# lock implementation - release
faasr_release<-function(faasr){
	lock_name <- paste0(faasr$InvocationID,"/",faasr$FunctionInvoke,"./lock")
	target_s3 <- faasr$LoggingServer
	target_s3 <- faasr$DataStores[[target_s3]]
	Sys.setenv("AWS_ACCESS_KEY_ID"=target_s3$AccessKey, "AWS_SECRET_ACCESS_KEY"=target_s3$SecretKey, "AWS_DEFAULT_REGION"=target_s3$Region)
	# delete the file named ".lock"
	delete_object(lock_name, target_s3$Bucket)
}



# "waiting" implementation
faasr_check<-function(faasr, pre){
	#if predecessors are more 2, it gets through codes below, if not, just pass this
	if (length(pre)>1){
		log_server_name = faasr$LoggingServer
		log_server <- faasr$DataStores[[log_server_name]]
		Sys.setenv("AWS_ACCESS_KEY_ID"=log_server$AccessKey, "AWS_SECRET_ACCESS_KEY"=log_server$SecretKey, "AWS_DEFAULT_REGION"=log_server$Region)
		
		#check all "predecessorname.done" exists. If TRUE, it passes, elif FALSE, it stops
		for (func in pre){
			file_names <- paste0(faasr$InvocationID,"/",func,".done") 
			if (object_exists(file_names, log_server$Bucket)){
				NULL
			} else{
				faasr_log(faasr, "error:function should wait")
				stop()
			}
		}
		
		# put random number into the file named "function.candidate"
		random_number <- sample(1:10000, 1)
		if (!dir.exists(faasr$InvocationID)){dir.create(faasr$InvocationID)}
		file_names <- paste0(faasr$InvocationID,"/",faasr$FunctionInvoke,".candidate")
		
		# acquire a Lock
		faasr_acquire(faasr)
		
		# if file named "function.candidate" exists, save it to the local
		if (object_exists(file_names, log_server$Bucket)){
			save_object(file_names, file=file_names, bucket=log_server$Bucket)
		}
		# append random number to the file / put it back to the s3 bucket 
		write.table(random_number, file_names, col.names=FALSE, row.names = FALSE, append=TRUE, quote=FALSE)
		put_object(file=file_names, object=file_names, bucket=log_server$Bucket)
		# save it to the local, again
		save_object(file_names, file=file_names, bucket=log_server$Bucket)

		# release the Lock
		faasr_release(faasr)
		
		# if the first line of the file matches the random number, it will process codes behind it, else, it stops.
		if (as.character(random_number) == readLines(file_names,1)){
			NULL
			}else{
			stop()
			}

	}
}



# Read-Set Memory implementation
faasr_rsm <- function(faasr){
	flag_content <- as.character(sample(1:1000,1))
	flag_path <- paste0(faasr$InvocationID,"/",faasr$FunctionInvoke,"/flag/")
	flag_name <- paste0(flag_path,flag_content)
	lock_name <- paste0(faasr$InvocationID,"/",faasr$FunctionInvoke,"./lock")

	target_s3 <- faasr$LoggingServer
	target_s3 <- faasr$DataStores[[target_s3]]
	Sys.setenv("AWS_ACCESS_KEY_ID"=target_s3$AccessKey, "AWS_SECRET_ACCESS_KEY"=target_s3$SecretKey, "AWS_DEFAULT_REGION"=target_s3$Region)

	while(TRUE){
		put_object("T", flag_name, target_s3$Bucket)
		if(faasr_ANYONE_ELSE_INTERESTED(faasr, target_s3, flag_path, flag_name)){
			delete_object(flag_name, target_s3$Bucket)
		}else{ 
			if (object_exists(lock_name, target_s3$Bucket)){
				return(FALSE)
			}else{
				put_object(flag_content, lock_name, target_s3$Bucket)
				delete_object(flag_name, target_s3$Bucket)
				return(TRUE)	
			}
		}
		
	}
}




# Anyone_else_interested implementation
faasr_ANYONE_ELSE_INTERESTED <- function(faasr, target_s3, flag_path, flag_name){
	pool <- get_bucket_df(target_s3$Bucket,prefix=flag_path)
	if (flag_name %in% pool$Key && length(pool$Key)==1){
		return(FALSE)
	}else{
		return(TRUE) 
	}
}




# workflow implementation - check loop iteratively, predecessors.
# TBD check unreachable
faasr_workflow <- function(faasr){
	
	# build empty lists for the graph and predecessors.
	graph <- list()
	pre <- list()
	
	# build the graph indicating adjacent nodes, e.g., "F1":["F2","F3"], so on.
	for (func in names(faasr$FunctionList)){
		graph[[func]] <- faasr$FunctionList[[func]]$InvokeNext
	}
	
	# build an empty list of stacks - this will prevent the infinite loop
	stack <- list()
	# implement dfs - recursive function
	dfs <- function(start, target){
		# find target in the graph's successor. If it matches, there's a loop
		if (target %in% graph[[start]]){
			cat('{\"msg\":\"function loop found\"}')
			stop()
		}
		# add start, marking as "visited"
		stack <<- c(stack, start)
		
		# set one of the successors as another "start"
		for (func in graph[[start]]){
			# if new "start" has been visited, do nothing
			if (func %in% stack){
				NULL
			} else {
				dfs(func, target)
			}
		}
	}
	# do dfs starting with function invoke.
	dfs(faasr$FunctionInvoke, faasr$FunctionInvoke)
	
	# find the predecessors and add them to the list "pre" 
	for (func in names(faasr$FunctionList)){
		if (faasr$FunctionInvoke %in% graph[[func]]){
		pre <- c(pre, func)
		} 
	}

	return(pre) 
}
