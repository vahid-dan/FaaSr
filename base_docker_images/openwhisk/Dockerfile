FROM rocker/tidyverse

#Install Dependencies
RUN apt-get -yq update && \
    apt-get -yq install \
		python3 \
		python3-pip
RUN update-ca-certificates \
    && pip install --upgrade pip setuptools six \
    && pip install --no-cache-dir gevent==21.12.0 flask==2.1.2


#Install required R packages
RUN R -e "install.packages('aws.s3', repos='https://cloud.R-project.org')" && \
    R -e "install.packages('jsonlite', repos='https://cloud.R-project.org')" && \
    R -e "install.packages('devtools', repos='https://cloud.R-project.org')" && \
    R -e "install.packages('jsonvalidate', repos='https://cloud.R-project.org')" && \
    R -e "install.packages('RCurl', repos='https://cloud.R-project.org')" && \
    R -e "install.packages('httr', repos='https://cloud.R-project.org')"
# \&&   R -e "devtools::install_git('renatof/FaaSr')"

#Setup Port
ENV FLASK_PROXY_PORT 8080

#Copy and make files & directories for actionProxy
RUN mkdir -p /actionProxy/owplatform
ADD actionproxy.py /actionProxy/
ADD owplatform/__init__.py /actionProxy/owplatform/
ADD owplatform/knative.py /actionProxy/owplatform/
ADD owplatform/openwhisk.py /actionProxy/owplatform/

#Setup basic executable 
RUN mkdir -p /action
ADD exec.R /action/exec
RUN chmod +x /action/exec

#Add source R file - this will be removed once the library generated.
ADD faasr.R /action/

CMD ["/bin/bash", "-c", "cd actionProxy && python3 actionproxy.py"]
