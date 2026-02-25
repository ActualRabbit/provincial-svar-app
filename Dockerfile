# Rocker Shiny base image
FROM rocker/shiny:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'bslib', 'vars', 'cansim', 'tidyverse', 'plotly', 'markdown'), repos='https://cloud.r-project.org/')"

# Create app directory
RUN mkdir -p /srv/shiny-server/provincial-svar
WORKDIR /srv/shiny-server/provincial-svar

# Copy app files
COPY app.R .
COPY about.md .

# Create cache directory with write permissions
RUN mkdir -p cache && chmod 777 cache

# Expose port
EXPOSE 3838

# Configure shiny-server
RUN echo "run_as shiny;\n\
server {\n\
  listen 3838;\n\
  location / {\n\
    site_dir /srv/shiny-server/provincial-svar;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}" > /etc/shiny-server/shiny-server.conf

# Run shiny server
CMD ["/usr/bin/shiny-server"]
