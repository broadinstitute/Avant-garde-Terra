FROM debian:buster

ARG TERM=linux
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update                                          \
    && apt-get install --assume-yes                         \
      autoconf                                              \
      vim                                                   \
      r-base                                                \
      build-essential                                       \
      bison                                                 \
      ca-certificates                                       \
      cmake                                                 \
      flex                                                  \
      g++                                                   \
      git                                                   \
      libboost-dev                                          \
      libboost-filesystem-dev                               \
      libboost-program-options-dev                          \
      libboost-regex-dev                                    \
      libboost-system-dev                                   \
      libboost-test-dev                                     \
      libssl-dev                                            \
      libtool                                               \
      make                                                  \
      pkg-config                                            \
      python3                                               \
      python3-pip                                           \
    && rm -rf /var/lib/apt/lists/*
RUN pip3 install \
      cython    \
      pandas    \
      setuptools


ENV ARROW_SOURCE_PATH=/usr/local/src/arrow  
ENV ARROW_HOME=/opt/arrow         

##
## arrow
##
WORKDIR /usr/local/src
RUN git clone https://github.com/apache/arrow.git
WORKDIR $ARROW_SOURCE_PATH/cpp
RUN cmake                                \
      -DCMAKE_INSTALL_PREFIX=$ARROW_HOME \
      -DCMAKE_BUILD_TYPE=Release         \
      -DARROW_BUILD_TESTS=Off            \
      -DARROW_PYTHON=On                  \
      -DARROW_PARQUET=ON                 \
      .
RUN make -j4 
RUN make install


##
## pyarrow
##
WORKDIR /usr/local/src
RUN pip3 install pyarrow

##
## Install R Packages
##
RUN R -e "install.packages(c('doSNOW', 'snow', 'iterators', 'foreach', 'sqldf','RSQLite','gsubfn',\
          'proto','data.table','stringr','tidyr','dplyr','reticulate', 'GA'), \
            dependencies=TRUE,                   \
            repos='http://cran.rstudio.com/')"

##
## Copy over AvG utils python module
##

#WORKDIR /usr/local/src
COPY avg_utils-0.0.0.tar.gz \
	/usr/local/src/

## Install AvG utils python module
RUN pip3 install avg_utils-0.0.0.tar.gz

#WORKDIR /root



