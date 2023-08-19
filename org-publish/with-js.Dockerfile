FROM ubuntu:latest

RUN apt-get update && \
    apt-get install -y curl wget unzip emacs git

RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs

RUN npm install --global tailwindcss postcss autoprefixer

WORKDIR /publish

RUN wget https://registry.npmjs.org/mermaid/-/mermaid-10.1.0.tgz -O mermaid.tgz
RUN wget https://github.com/mathjax/MathJax/archive/master.zip -O mathjax.zip
RUN wget https://github.com/be5invis/Iosevka/releases/download/v23.0.0/webfont-iosevka-23.0.0.zip -O iosevka.zip
RUN wget https://use.fontawesome.com/releases/v6.4.2/fontawesome-free-6.4.2-web.zip -O fontawesome.zip

RUN git clone https://github.com/highlightjs/highlight.js.git && \
    cd highlight.js && \
    npm install

RUN cd highlight.js && \
    node tools/build.js -n python shell haskell c

COPY packages.el packages.el
RUN emacs --batch -l /publish/packages.el
