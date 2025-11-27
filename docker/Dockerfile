# -----------------------------
# Base R image
# -----------------------------
FROM r-base:4.3.1

# -----------------------------
# Install system dependencies
# -----------------------------
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# -----------------------------
# Install R packages required
# -----------------------------
RUN R -e "install.packages(c( \
 'ggplot2', 'dplyr', 'ggthemes', \
    'gridExtra'), repos='https://cloud.r-project.org')"

# -----------------------------
# Create working directory
# -----------------------------
WORKDIR /app

# -----------------------------
# Copy your R script into container
# -----------------------------
COPY bin/cvd_risk_analysis.R /app/cvd_risk_analysis.R

# -----------------------------
# Set entrypoint to Rscript
# -----------------------------
ENTRYPOINT ["Rscript"]
