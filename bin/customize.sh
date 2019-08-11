#!/bin/bash

# check if customizations exist
if [[ -f /customizations/variables.scss ]]; then
    # regenerate styles
    cat /customizations/variables.scss > /srv/web/scss/_variables.new.scss
    echo '$fa-font-path: "";' >> /srv/web/scss/_variables.new.scss
    cat /srv/web/scss/_variables.scss >> /srv/web/scss/_variables.new.scss
    rm /srv/web/scss/_variables.scss
    mv /srv/web/scss/_variables.new.scss /srv/web/scss/_variables.scss
    find /srv/dist/static -name "*.css" -exec sassc -I /srv/web -t compressed /srv/web/scss/main.scss {} \;
fi
