
# rco - Docker file

The rco Docker image will optimize any CRAN package, using the latest
GitHub `rco` stable version. If the package to optimize has a `testthat`
suite, then it will test the original and optimized versions, and show
the obtained speed-up.

## Installation

First, you need to install [Docker](https://www.docker.com/) on your
computer. And then, from a terminal, the `rco` Docker image can be
pulled from [Docker Hub](https://hub.docker.com/r/jcrodriguez1989/rco)
with:

``` bash
docker pull jcrodriguez1989/rco
```

## Usage

The docker image basic usage will optimize a random CRAN package, this
is done with:

``` bash
docker run rco
```

We can also specify a package to optimize, using the `RCO_PKG`
environment variable, for example, if we want to optimize the `rflights`
package:

``` bash
docker run -e RCO_PKG=rflights rco
```

Also, if we want to get the resulting optimized files we can set a
docker shared folder, with:

``` bash
# Replace DEST_FOLDER path, with your desired output path
DEST_FOLDER=/tmp/rco_dock_res
docker run -v $DEST_FOLDER:/rco_results rco
```

In summary, if we want to optimize the `rflights` package and save its
results, we can do:

``` bash
docker run -e RCO_PKG=rflights -v $DEST_FOLDER:/rco_results rco
```

And in the `DEST_FOLDER` we will have files as:

``` bash
ls $DEST_FOLDER
## rflights  rflights_0.1.0.tar.gz  rflights_opt
```
