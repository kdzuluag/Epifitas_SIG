# Plantilla NO probada en este entorno. Construir desde la raíz: docker build -f deploy/Dockerfile -t epig .
# Ejecutar detrás de nginx + oauth2-proxy (deploy/nginx.conf). Pasar el secreto en runtime:
#   docker run --rm -e EPIG_PROXY_TOKEN=... -v epig_private:/srv/epig/data/private epig
FROM rocker/geospatial:4.5.3

RUN install2.r --error --skipinstalled shiny bslib bsicons DT leaflet RSQLite digest withr jsonlite stringi \
    && rm -rf /tmp/downloaded_packages

WORKDIR /srv/epig
COPY app.R ./
COPY R ./R
COPY www ./www
COPY data/processed ./data/processed

RUN useradd -r -u 10001 epig && mkdir -p data/private && chown -R epig data/private
USER epig

ENV EPIG_ENV=production EPIG_AUTH_MODE=proxy
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/srv/epig', host='0.0.0.0', port=3838)"]
