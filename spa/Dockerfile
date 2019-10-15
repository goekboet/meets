FROM node:12

WORKDIR /home/elm
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.0/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin
RUN npm install browserify@16.5.0 -g
RUN npm install uglify-js@3.6.1 -g

WORKDIR /home/spa
COPY package.json package-lock.json ./
RUN npm install date-fns@2.4.1

COPY . .
ENTRYPOINT [ "./build.sh" ]


