# Base image: match this to your local R version (R.version.string)
FROM rocker/shiny:4.6.1
LABEL description="QuaNTA docker build for SciLifeLab Serve"

# Setting up shiny-server pathing
RUN echo "R_BOX_PATH=/srv/shiny-server" >> /usr/local/lib/R/etc/Renviron.site

# Set by Docker buildx for each target platform (e.g. amd64, arm64);
# matches Quarto's linux-<arch>.deb naming. Defaults to amd64 (what Serve runs on)
# in case the build doesn't set it.
ARG TARGETARCH=amd64

# Set the working directory
WORKDIR /proj
COPY /app/envs/SumExp_0.2.1.tar.gz /tmp/SumExp_0.2.1.tar.gz
COPY /app/envs/Lato/ /usr/local/share/fonts/Lato/

# General updates (curl and cmake added: curl for renv downloads, cmake for nloptr)
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git curl cmake libxml2-dev libmagick++-dev libssl-dev libharfbuzz-dev libfribidi-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Refresh the font cache so the Lato fonts are found
RUN fc-cache -f

# Install Quarto (apt-get instead of gdebi, in case gdebi isn't in the base image)
RUN wget -O quarto-linux.deb https://quarto.org/download/latest/quarto-linux-${TARGETARCH}.deb && \
    apt-get update && \
    apt-get install -y ./quarto-linux.deb && \
    rm quarto-linux.deb && \
    rm -rf /var/lib/apt/lists/*

RUN quarto install tinytex

# Install the required packages
# Recreate the R environment using renv package, skipping the local SumExp package
RUN Rscript -e 'install.packages(c("renv"))'
COPY /renv.lock /srv/shiny-server/renv.lock
RUN Rscript -e 'setwd("/srv/shiny-server/"); renv::restore(exclude = "SumExp")'

# Install the local SumExp package after its dependencies are in place
RUN Rscript -e 'install.packages("/tmp/SumExp_0.2.1.tar.gz", repos = NULL, type = "source")'

# Copy the app files (scripts, data, etc.)
RUN rm -rf /srv/shiny-server/*
COPY /app/ /srv/shiny-server/

# Ensure that the expected user is present in the container
RUN if id shiny &>/dev/null && [ "$(id -u shiny)" -ne 999 ]; then \
        userdel -r shiny; \
        id -u 999 &>/dev/null && userdel -r "$(id -un 999)"; \
    fi; \
    useradd -u 999 -m -s /bin/bash shiny; \
    chown -R shiny:shiny /srv/shiny-server/ /var/lib/shiny-server/ /var/log/shiny-server/

# Other settings
USER shiny
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]