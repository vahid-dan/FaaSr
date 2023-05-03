
r_files <- list.files(pattern="\\.R$")
for (file in r_files){
    if (file != "exec.R") {
	  source(file)
	}
}

faasr <- commandArgs(TRUE)
faasr_start(faasr)