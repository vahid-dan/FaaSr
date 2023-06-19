library("httr")
library("jsonlite")
library("githubinstall")

# Recursive function to replace values
replace_values <- function(user_info, secrets) {
  
  for (name in names(user_info)) {
    if (name == "FunctionList") {
      next
    }
    # If the value is a list, call this function recursively
    if (is.list(user_info[[name]])) {
      user_info[[name]] <- replace_values(user_info[[name]], secrets)
    } else {
      # If the value exists in the secrets, replace it
      if (user_info[[name]] %in% names(secrets)) {
        user_info[[name]] <- secrets[[user_info[[name]]]]
      }
    }
  }
  
  return(user_info)
}

# REST API get faasr payload json file from repo

get_payload <- function(secrets) {
  # GitHub username and repo
  #cat("exec.R: will get payload from another repo\n")
  github_repo <- Sys.getenv("PAYLOAD_REPO")
  parts <- strsplit(github_repo, "/")[[1]]
  if (length(parts) < 3) {
    stop("PAYLOAD_REPO should contains at least three parts.")
  }
  username <- parts[1]
  repo <- parts[2]
  
  path <- paste(parts[3: length(parts)], collapse = "/")
  pat <- secrets[["PAYLOAD_GITHUB_TOKEN"]]
  url <- paste0("https://api.github.com/repos/", username, "/", repo, "/contents/", path)

  # Send the POST request
  response1 <- GET(
    url = url,
    encode = "json",
    add_headers(
      Authorization = paste("token", pat),
      Accept = "application/vnd.github.v3+json",
      "X-GitHub-Api-Version" = "2022-11-28"
    )
  )


  # Check if the request was successful
  if (status_code(response1) == "200") {
    cat("exec.R: success get payload from github repo\n")
    # Parse the response content
    content <- content(response1, "parsed")
    
    # The content of the file is in the 'content' field and is base64 encoded
    file_content <- rawToChar(base64enc::base64decode(content$content))
    
    faasr <- fromJSON(file_content)
    return (faasr)
    
    
  } else {
    print(paste("Error:", http_status(response1)$message))
    stop()
  }
}


secrets <- fromJSON(Sys.getenv("SECRET_PAYLOAD"))


faasr <- get_payload(secrets)

faasr$InvocationID <- Sys.getenv("INPUT_ID")
faasr$FunctionInvoke <- Sys.getenv("INPUT_INVOKENAME")
#cat("exec.R: faasr-invocationID is: ", faasr$InvocationID, "\n")
#cat("exec.R: faasr-FunctionInvoke is: ", faasr$FunctionInvoke, "\n")

# Replace secrets to faasr
#cat("exec.R: will update user payload\n")
faasr_source <- replace_values(faasr, secrets)

# back to json formate
faasr <- toJSON(faasr_source, auto_unbox = TRUE)

gits <- faasr_source$FunctionGitRepo[[faasr_source$FunctionInvoke]]
if (length(gits)==0){NULL} else{
for (file in gits){
	command <- paste("git clone --depth=1",file)
	system(command)
	}
}
	
packages <- faasr_source$FunctionCRANPackage[[faasr_source$FunctionInvoke]]
if (length(packages)==0){NULL} else{
for (package in packages){
	install.packages(package)
	}
}

ghpackages <- faasr_source$FunctionGitHubPackage[[faasr_source$FunctionInvoke]]
if (length(ghpackages)==0){NULL} else{
for (ghpackage in ghpackages){
	githubinstall("ghpackage")
	}
}

r_files <- list.files(pattern="\\.R$", recursive=TRUE, full.names=TRUE)
for (rfile in r_files){
    if (rfile != "./exec_owld.R" && rfile != "./exec_gh.R") {
	  source(rfile)
	}
}



faasr_start(faasr)
