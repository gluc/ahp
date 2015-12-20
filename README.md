# Badges

## Master

[![Build Status master](https://travis-ci.org/gluc/ahp.svg?branch=master)](https://travis-ci.org/gluc/ahp)
[![Windows Build status]( https://ci.appveyor.com/api/projects/status/github/gluc/ahp?branch=master&svg=true)](https://ci.appveyor.com/project/gluc/ahp)
[![codecov.io](http://codecov.io/github/gluc/ahp/coverage.svg?branch=master)](http://codecov.io/github/gluc/ahp?branch=master)
[![CRAN Version](http://www.r-pkg.org/badges/version/ahp)](http://cran.rstudio.com/web/packages/ahp)

## Dev

[![Build Status dev](https://travis-ci.org/gluc/ahp.svg?branch=dev)](https://travis-ci.org/gluc/ahp)
[![Windows Build status]( https://ci.appveyor.com/api/projects/status/github/gluc/ahp?branch=dev&svg=true)](https://ci.appveyor.com/project/gluc/ahp)
[![codecov.io](http://codecov.io/github/gluc/ahp/coverage.svg?branch=dev)](http://codecov.io/github/gluc/ahp?branch=dev)

# About this Package

An R package to model complex decision making problems using AHP (Analytic Hierarchy Process).

The basic workflow with this package is:
1. specify your ahp problem in an ahp file
2. load ahp file, using \code{\link{LoadFile}}
3. calculate model, using \code{\link{Calculate}}
4. output model analysis, either using \code{\link{GetDataFrame}} or using \code{\link{ShowTable}}
 
For more information, see the package vignette using \code{vignette("AHP car example")}

# Example

```{code = R} 
library(ahp)
ahpFile <- system.file("extdata", "car.ahp", package="ahp")
carAhp <- LoadFile(ahpFile)
Calculate(carAhp)
GetDataFrame(carAhp)
ShowTable(carAhp)
```

# NOTE:
The latest from github dev branch may have some breaking changes compared to CRAN. See [NEWS](https://github.com/gluc/ahp/blob/dev/NEWS) for details.

# Getting Started

# Conventions

Coding Conventions: Google Style Guide, see https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml

Versioning Conventions: SemanticVersioning. See http://semver.org/ for details

Branching Conventions: GitFlow. See https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow

Pull Requests: Very welcome. Please branch from dev.
