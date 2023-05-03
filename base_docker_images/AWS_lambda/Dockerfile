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

#below is for Lambda
# Define the function directory
ARG FUNCTION_DIR="/function"
ARG ACTION_DIR="/action"
RUN apt-get update && \
    apt-get install -y \
        curl \
        unzip \
        g++ \
        make \
        cmake \
        libcurl4-openssl-dev

# Create function directory
RUN mkdir -p ${FUNCTION_DIR}
RUN mkdir -p ${ACTION_DIR}
# Install the AWS Lambda Runtime Interface Client for Python
RUN pip install \
    --target ${FUNCTION_DIR} \
        awslambdaric


# Copy function code
COPY app/* ${ACTION_DIR}
# Copy Python wrapper
COPY lambda_function.py  ${FUNCTION_DIR}

WORKDIR ${FUNCTION_DIR}

# Install the AWS Lambda Runtime Interface Emulator
RUN curl -Lo /usr/bin/aws-lambda-rie https://github.com/aws/aws-lambda-runtime-interface-emulator/releases/latest/download/aws-lambda-rie && \
    chmod +x /usr/bin/aws-lambda-rie
COPY entry.sh /
RUN chmod 755 /entry.sh


ENTRYPOINT ["/entry.sh"]

CMD [ "lambda_function.handler" ]





