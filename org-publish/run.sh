
docker run \
    -i \
    --rm \
    -v "${WORKSPACE}/public/notes:/publish/input" \
    -v "/var/www/mainsite/public/public-notes:/publish/output" \
    -v "${PWD}/cache:/root/.org-timestamps" \
    -v "${PWD}/config:/publish/config" \
    org-publish
