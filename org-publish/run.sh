
docker run \
    -i \
    --rm \
    -v "${WORKSPACE}/public/notes:/publish/input" \
    -v "/var/www/mainsite/public/public-notes:/publish/output" \
    -v "${WORKSPACE}/org-publish/cache:/root/.org-timestamps" \
    -v "${WORKSPACE}/org-publish/config:/publish/config" \
    org-publish
