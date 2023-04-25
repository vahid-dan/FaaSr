faasr_acquire<-function(faasr){
	Lock<-faasr_rsm(faasr)
	while(TRUE){
		if (Lock){return(TRUE)}else
		{
		Lock<-faasr_rsm(faasr)
		}
}
}

faasr_check<-function(faasr, pre){

if (length(pre)>1){
log_server_name = faasr$LoggingServer
log_server <- faasr$DataStores[[log_server_name]]
Sys.setenv("AWS_ACCESS_KEY_ID"=log_server$AccessKey, "AWS_SECRET_ACCESS_KEY"=log_server$SecretKey, "AWS_DEFAULT_REGION"=log_server$Region)

for (func in pre){
	file_names <- paste0(faasr$InvocationID,"/",func,".done") 
	if (object_exists(file_names, log_server$Bucket)){NULL}else{
		faasr_log(faasr, "error:function should wait")
		stop()
	}
}

random_number <- sample(1:10000, 1)
if (!dir.exists(faasr$InvocationID)){dir.create(faasr$InvocationID)}
file_names <- paste0(faasr$InvocationID,"/",faasr$FunctionInvoke,".candidate")

faasr_acquire(faasr)

if (object_exists(file_names, log_server$Bucket)){
	save_object(file_names, file=file_names, bucket=log_server$Bucket)
	}
write.table(random_number, file_names, col.names=FALSE, row.names = FALSE, append=TRUE, quote=FALSE)
put_object(file=file_names, object=file_names, bucket=log_server$Bucket)
save_object(file_names, file=file_names, bucket=log_server$Bucket)

faasr_release(faasr)

if (as.character(random_number) == readLines(file_names,1)){
	NULL
	}else{
	stop()
	}

}
}

faasr_release<-function(faasr){
	lock_name <- paste0(faasr$InvocationID,"/",faasr$FunctionInvoke,"./lock")
	target_s3 <- faasr$LoggingServer
	target_s3 <- faasr$DataStores[[target_s3]]
	Sys.setenv("AWS_ACCESS_KEY_ID"=target_s3$AccessKey, "AWS_SECRET_ACCESS_KEY"=target_s3$SecretKey, "AWS_DEFAULT_REGION"=target_s3$Region)
	delete_object(lock_name, target_s3$Bucket)
}

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
	if(ANYONE_ELSE_INTERESTED(faasr, target_s3, flag_path, flag_name)){
		delete_object(flag_name, target_s3$Bucket)
	}else{ 
		if (object_exists(lock_name, target_s3$Bucket)){
			return(FALSE)}else{
			put_object(flag_content, lock_name, target_s3$Bucket)
			delete_object(flag_name, target_s3$Bucket)
			return(TRUE)	
			}
		}
		
}
}

ANYONE_ELSE_INTERESTED <- function(faasr, target_s3, flag_path, flag_name){
	pool <- get_bucket_df(target_s3$Bucket,prefix=flag_path)
	if (flag_name %in% pool$Key && length(pool$Key)==1){
		return(FALSE)
		}else{
		return(TRUE) 
	}
}

faasr_workflow <- function(faasr){
graph <- list()
pre <- list()

for (func in names(faasr$FunctionList)){
	graph[[func]] <- faasr$FunctionList[[func]]$InvokeNext
}

stack <- list()
dfs <- function(start, target){
	if (target %in% graph[[start]]){
		cat('{\"msg\":\"function loop found\"}')
		stop()
	}
	stack <<- c(stack, start)
	for (func in graph[[start]]){
		if (func %in% stack){
			NULL
		}else {dfs(func, target)}
	}
}
dfs(faasr$FunctionInvoke, faasr$FunctionInvoke)

for (func in names(faasr$FunctionList)){
	if (faasr$FunctionInvoke %in% graph[[func]]){
	pre <- c(pre, func)
	} 
}

return(pre) 
}
