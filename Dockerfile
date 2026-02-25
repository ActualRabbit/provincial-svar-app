# Rocker Shiny base image
FROM rocker/shiny-verse:4.3.2

# Install additional system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages (tidyverse already in shiny-verse, add the rest)
RUN R -e "install.packages(c('bslib', 'vars', 'cansim', 'plotly', 'markdown'), repos='https://cloud.r-project.org/')"

# Create app directory
RUN mkdir -p /srv/shiny-server/provincial-svar
WORKDIR /srv/shiny-server/provincial-svar

# Copy app files
COPY app.R .
COPY about.md .
COPY data/ data/

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
