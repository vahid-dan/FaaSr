#pre_faasr_image
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
    R -e "install.packages('httr', repos='https://cloud.R-project.org')" && \
    R -e "install.packages('uuid', repos='https://cloud.R-project.org')" && \
    R -e "install.packages('paws', repos='https://cloud.R-project.org')"

#below is for Github Action
# Define the function directory

ARG ACTION_DIR="/action"

# Create function directory

RUN mkdir -p ${ACTION_DIR}

# Copy function code
COPY app/* ${ACTION_DIR}

WORKDIR ${ACTION_DIR}

CMD ["Rscript", "exec.R"]