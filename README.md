## R Shiny App Test : Deploying a Shiny Application

This Shiny Dashboard visualizes a basic anomaly exploration analysis on a set of variables in time (Time Series dataset).
It is a simple example to demonstrate how to deploy a shiny app from URL, or dockerized for better compatibility handling.

## Dockerized version

* Get Docker running on your environment first!

* Git Clone the Repository with the Dockerfile and the application:
```bash
git clone https://github.com/mtripiana/shiny-tsboard.git
```

* Build the Docker Image
```bash
cd shiny-tsboard
docker build -t shiny-app-tsboard .
```
Note: if you are running on Windows (via WLS) you might need to configure the docker port (+ expose the daemon, as discussed [here](https://medium.com/@sebagomez/installing-the-docker-client-on-ubuntus-windows-subsystem-for-linux-612b392a44c4) )
```bash
EXPORT DOCKER_HOST=localhost:2375
```

* Run the resulting image as a container
```bash
docker run -p 3838:3838 --name shiny-app shiny-app-tsboard
```

* You are done! Visit ```localhost:3838``` to see the Shiny application on your browser.

### Run from Gitlab

To run the application directly from the gitlab host, e.g. in your RStudio IDE, you can just run these lines:

```R
library(shiny)
runUrl("https://gitlab.com/mtripiana/shiny-tsboard/raw/master/master.zip")
```

