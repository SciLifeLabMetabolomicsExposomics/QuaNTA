# Base image
FROM rocker/shiny:4.5
LABEL description="QuaNTA docker build for SciLifeLab Serve"

# Set by Docker buildx for each target platform (e.g. amd64, arm64);
# matches Quarto's linux-<arch>.deb naming
ARG TARGETARCH

# Set the working directory
WORKDIR /proj
COPY /app/envs/SumExp_0.2.1.tar.gz /tmp/SumExp_0.2.1.tar.gz
COPY /app/envs/Lato/ /usr/local/share/fonts/Lato/

# General updates
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git libxml2-dev libmagick++-dev libssl-dev libharfbuzz-dev libfribidi-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install Quarto
RUN wget -O quarto-linux.deb https://quarto.org/download/latest/quarto-linux-${TARGETARCH}.deb && \
    gdebi --non-interactive quarto-linux.deb && \
    rm quarto-linux.deb

RUN quarto install tinytex

# Install the required packages
# Recreate the R environment using renv package
RUN Rscript -e 'install.packages(c("renv"))'
COPY /renv.lock /srv/shiny-server/renv.lock
RUN Rscript -e 'setwd("/srv/shiny-server/");renv::restore();'

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