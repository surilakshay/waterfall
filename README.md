---
output: github_document
bibliography: "`r here::here('README-references.bib')`"
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, echo = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
library(exampleRPackage)
```

# exampleRPackage

exampleRPackage is an example R package [available on GitHub](https://github.com/mvuorre/exampleRPackage).

This is the Git(Hub) repository of an example R package. In our manuscript (not yet available), we describe why and how researchers might choose to share their research products[^1] as R packages. This repository is the example described in the manuscript, and can be viewed online for details of the implementation (that is, the R package's source code). The exampleRPackage can also be installed from github (although as an example package it does not contain anything useful):

```{r, eval = F}
# install.packages("devtools")
devtools::install_github("mvuorre/exampleRPackage")
```

[^1]: By "product", we mean any combination of text (manuscripts), code, data, stimuli, and other research materials.

The file you are reading now is the package's README, which describes how to create R packages with functions, data, and appropriate documentation. In writing this online tutorial, we relied heavily on Hadley Wickham's "R Packages", which is an excellent source of information on creating R packages [@wickham_r_2015].

---

# Motivation

Lack of reproducibility has been identified as a key limiting factor to the reliability of scientific research, and researchers are now urged to focus on factors in their workflow that could improve the reproducibility of their results [@Munafomanifestoreproduciblescience2017]. 

>"A research project is computationally *reproducible* if a second investigator (including you in the future) can recreate the final reported results of the project, including key quantitative findings, tables, and figures, given only a set of files and written instructions." [@KitzesPracticeReproducibleResearch2017]
The role of standards and common practices has always been important in ensuring the continuity of one's work, especially in areas where results depend on computational work. For example, the BIDS (Brain Imaging Data Structure) has been introduced as a standardized organization for brain imaging to facilitate collaborative work on very large data sets and computationally demanding projects [@gorgolewski_brain_2016]. However, similar standards have not been established for less computationally intense areas of behavioral science (although there have been some attempts, for example the Tier protocol[^tier]).
[^tier]: <http://www.projecttier.org/tier-protocol/specifications/>


## Why adopt a common standard?

Quoting @gorgolewski_brain_2016:

* Minimized curation: Common standards make it possible for researchers who were not directly involved in data collection to understand and work with the data. This is particularly important to ensure that data remain accessible and usable by different researchers over time in the following instances:
    * within a laboratory over time
    * between labs facilitating collaboration and making combining data in multi-center studies easier
and less ambiguous
    * between public databases (i.e., OpenfMRI) allowing for the quick ingestion of big data organized
according to a common scheme.
* Error reduction: Errors attributed to the misunderstanding of the meaning of a given datum (e.g., when variable names are not explicitly stated in the data file and standardized across files).
* Optimized usage of data analysis software is made possible when the metadata necessary for analysis (i.e., details of the task or imaging protocol) are easily accessible in a standardized and machine- readable way. This enables the application of completely automated analysis workflows, which greatly enhances reproducibility and efficiency.
* Development of automated tools for verifying the consistency and completeness of datasets is realized. Such tools make it easier to spot missing metadata that limit how the data could be analyzed in the future.

## Which standard to adopt?

Instead of suggesting yet another arbitrary standard, we propose that behavioral scientists could adopt a well-established standard from statistical software development for their project organizing principles. Specifically, in this tutorial we describe how to organize and share one's data sets, functions, and related materials including analyses, as packages for the R programming language [@RCoreTeamLanguageEnvironmentStatistical2017].

Why R? R is an easily accessible programming language for statistical computing and graphics, and is rapidly increasing in popularity in the behavioral sciences. For example, the APS Observer recently ran a series of articles promoting R within the psychological science community [@yee_why_2017]. Importantly, users can create R packages with no or minimal coding, because many of the procedures have been implemented in the RStudio [@RStudioTeamRStudioIntegratedDevelopment2016] graphical interface.

# How to Create an R Package

The outline of the tutorial is as follows:

1. [Create a new R package with R Studio](#create-a-new-r-package-with-r-studio)
    - With a few button clicks, this automatically sets up the underlying software infrastructure
2. [Describe the package](#describe-the-package)
    - DESCRIPTION and README files
3. [Add data to package](#add-data)
    - Raw data, preprocessing scripts, R data object
4. [Create and add functions](#create-functions)
    - [todo]
5. [Document the package](#document-the-package)
    - Describe the package, its functions, and data, in a machine- and human-readable format

After these simple steps, you will have a functional R package on your computer. We will also go through advanced (optional) steps.

- [Sharing the R package](#sharing-the-R-package)
    - Upload it to GitHub so it is easily available to anyone (R user or otherwise)
    - Mint a DOI for citeability and longevity (todo)
    - Connect to Open Science Framework (todo)
- [Document data analysis as package vignette](#documenting-analysis-as-package-vignette)
    - Creates a readable file showing how to use the package. For example the vignette can describe how the data was (or could be) analyzed
- [Create a website for the package](#creating-a-website-for-the-data-package)
    - Showcase your R package online with a website

You will need one R package (R developer tools) to follow these instructions:

```{r, eval = FALSE}
install.packages("devtools")
```

The **devtools** package [@wickham_devtools:_2017] contains helpful functions for creating R packages.

## Create a new R package with R Studio

First, use R Studio to create a new R Project. While creating the project, make sure to create the project as an R Package:

```{r rstudio-create-pkg, echo = F}
knitr::include_graphics("README-rstudio-create-project.png", dpi = 360)
```

Creating an R (Package) Project with R Studio sets up the necessary infrastructure leaving little work for the user. After creating the package, the project's files and folders look like this (`exampleRPackage` is the project's root folder):

```bash
exampleRPackage/
├── man/
|   └── hello.Rd
├── R/
|   └── hello.R
├── DESCRIPTION
├── NAMESPACE
├── exampleRPackage.Rproj
├── .gitignore
└── .Rbuildignore
```

`man/` is the "manuals" folder which will have files documenting the package. `R/` is a folder for R functions. `DESCRIPTION` is a file describing the package, and `NAMESPACE` its functions. `exampleRPackage.Rproj` identifies the folder as an R package project. `.gitignore` and `.Rbuildignore` are hidden files, and specify which files should be ignored by Git [@vuorre_curating_2017], and R package building operations, respectively. These last three files can be ignored for now.

At this point, you can delete `man/hello.Rd` and `R/hello.R`. These two files are examples of R function files and R documentation files.

This is already a fully functional R package (but it contains nothing so it's useless.) We now need to introduce content, and change some of the included files, to turn it into an R package. 

## Describe the package

The `DESCRIPTION` file includes necessary information about the package in standard format. When you create an R package with R Studio, the process automatically creates this file with example content:

```bash
Package: exampleRPackage
Type: Package
Title: What the Package Does (Title Case)
Version: 0.1.0
Author: Who wrote it
Maintainer: The package maintainer <yourself@somewhere.net>
Description: More about what it does (maybe more than one line)
    Use four spaces when indenting paragraphs within the Description.
License: What license is it under?
Encoding: UTF-8
LazyData: true
```

Modify this file to reflect the details of your package, but make sure you don't change the formatting: This file is read by the R package creating process, and the file must therefore remain machine-readable. Here's an example:

```bash
Package: exampleRPackage
Type: Package
Title: An example R package
Version: 0.1.0
Authors@R: person("Matti", "Vuorre", email = "mv2521@columbia.edu",
                  role = c("aut", "cre"))
Maintainer: Matti Vuorre <mv2521@columbia.edu>
Description: This package is an example R package.
License: What license is it under?
Depends: R (>= 3.1.0)
Encoding: UTF-8
LazyData: true
```

The important changes to the above were the package's name, title, authors, maintainer, description, and depends. Note the odd formatting for the `Authors@R` field; this ensures that the author's information is correctly recorded within the package. Then, you can go ahead and delete the `NAMESPACE` file. To add a license, it is easiest to use a helper function from the devtools package:

```{r, eval = FALSE}
library(devtools)
use_mit_license()
```

The above function will automatically add the MIT license file and update the license field in the `DESCRIPTION` file. Alternatively, you can use the GPL3 license with `use_gpl3_license()`. The appropriate licensing of scientific data (and software) is an important topic but outside the scope of this tutorial [todo].

Then, you should add a `README` file which describes the package in some detail. We recommend writing the file in Markdown[^markdown] or R Markdown [@allaire_rmarkdown:_2016]. Here, we choose to create a `README.Rmd` R Markdown file, which produces a nice looking `README.md` Markdown file. We use devtools to create a template README.rmd file, which we can then change to suit our needs:

[^markdown]: <https://daringfireball.net/projects/markdown/>

```{r, eval = FALSE}
use_readme_rmd()
```

You can then write a description of the package (what is it, why does it exist, who created it, who to contact, etc.) Make changes to `README.Rmd` with R Studio's text editor. When you are done, click Knit in R Studio.

```{r, echo = F}
knitr::include_graphics("README-rstudio-knit.png", dpi = 360)
```

The package is now described, and includes a readme file that gives additional details about it to other potential users. The first content we add to this example R package is some (simulated) data.

## Add data

First, we will add the raw data to its appropriate location (a `data-raw/` folder inside the project). You should use this helper function from devtools to create the folder, so that it is also appropriately handled when R builds the package:

```{r, eval = FALSE}
use_data_raw()
```

You can then put all the raw data files to `data-raw/`. For this example R package, we simulated some data, and therefore also put the R script that simulated the data and created the data files in the same folder.

Then, move (or create) any pre-processing scripts or instructions to the same `data-raw/` folder. Doing so will allow for exact reproduction of the final data set. The pre-processing should output two files: One an easily downloadable .csv file for non-R users, and an R data object for the R package. To create the R data object, include the following at the end of your pre-processing script (or run in the console once you have completed pre-processing):

```{r, eval = FALSE}
use_data(exampleData)
```

The above command assumes that your preprocessing script creates an R data object called exampleData; you can change it to whatever you'd like. The `use_data()` function will save the R data object into `data/`. This means that your R package now includes a data set called `exampleData`, which can be easily accessed within R:

```{r}
library(exampleRPackage)  # Activate package in current R session
head(exampleData)  # Display first 6 rows of example data
```

### How to download the raw data manually

Although it is not part of the formal R package, we stress that the raw data, and possible some form of pre-processed data, should be made available to others in a format that does not require R. We therefore recommend also including the data in some common format, such as comma separated values (`.csv`), in the `data-raw/` folder.

For exampleRPackage, you will find all the raw data as .csv files in the [`data-raw/`](https://github.com/mvuorre/exampleRPackage/tree/master/data-raw) folder, from where they can be downloaded. That folder also contains full pre-processing instructions in the form of an R script.

## Document the package

R users are familiar with reading function documentation by typing `?mean` in the R console. That function reveals the documentation page for the `mean()` function. By adding a documentation file, your data object will also have a documentation page, which is easily accessible from within R.

To document your data set, create a file called `data.R` in the `R` folder. Then, use R's (roxygen2 [@wickham_roxygen2:_2017]) documentation syntax to write your data object's documentation in the `R/data.R` file. It will look something like the following for our `exampleData` object:

```{r, eval = F}
#' Scores of Group A and Group B.
#'
#' @description A data set with the scores of two groups, Group A and Group B.
#'     The data were simulated to illustrate how to create R data packages.
#'
#' @format A data.frame with 60 rows and 2 variables:
#' \describe{
#'   \item{group}{Participant's group, A or B.}
#'   \item{score}{Participant's score in hypothetical task Z.}
#' }
#' @source \url{https://www.github.com/mvuorre/exampleRPackage}
"exampleData"
```

The key features of this documentation file are (from top to bottom in the above code listing):

Each line begins with a `#'`. The first line is a short description of the data. The `@description` field is an optional longer description of the data (with indentation if it spans multiple lines). The `@format` field describes the object's type (e.g. an R data.frame), its dimensions, and then describes all the variables (e.g. `group` and `score`). The `@source` field includes the source of the data, which could be a citation to an academic article, for example. Finally, the last line should be the name of the data object in quotation marks. You can document multiple data files in the same `R/data.R` file; simply leave one blank line between them.

Once you are done writing your description, you can use the `document()` helper function:

```{r, eval = FALSE}
document()
```

This function, from the devtools package, will take what you have written and creates machine-readable files in the `man/` folder.

Then go to R Studio's **Build** tab, and click "Build & Reload". If everything went well, this should build the R package you have just created, and loads it into your current R workspace. 

It is important to document data well and precisely so there is never any ambiguity in the meaning of variables, where the data is from, etc. You should therefore spend some time writing documentation files for data objects. Once the package has been built, this documentation (for `exampleData`) can be viewed in R by calling `?exampleData`:

```{r, echo = F}
knitr::include_graphics("README-exampleData-rdocumentation.png")
```

## Create functions

[todo]

# Advanced (optional) steps

## Sharing the R package

The easiest way to share the data product is to create the R package as a Git repository (see <https://github.com/mvuorre/reproguide-curate> and [@vuorre_curating_2017]) and share it on GitHub. We therefore recommend enabling Git (check the "Create a Git repository" button, Figure \@ref(fig:rstudio-create-pkg)) when creating the project. Once the R package's source code is pushed to GitHub, authorized users (by default, anyone) can browse it on GitHub and manually download any files they'd like (e.g. the raw data files.) Importantly, they can obtain the data with R by installing the R package you have just created:

```{r, eval = FALSE}
devtools::install_github("mvuorre/exampleRPackage")
```

The above command, when executed in R, downloads and installs the `exampleRPackage` from GitHub user `mvuorre`. You can view this example R package's source code on GitHub: <https://github.com/mvuorre/exampleRPackage>.

## Documenting analysis as package vignette

It is also helpful to share the full analysis code in which the data was used. We recommend writing analyses with R Markdown. R Markdown files can easily be turned into an *R package vignette*. To initiate a vignette, use

```{r, eval = FALSE}
devtools::use_vignette("Example-Analysis")
```

This creates a vignette template into the `vignettes/` folder. Write your analysis into the `.Rmd` file. See `vignettes/Example-Analysis.Rmd` for an example. Writing your analysis into a vignette ensures that users of the data also have access to the original analysis of the data. Furthermore, vignettes can be built into the package's website.

## Creating a website for the R package

[todo pkgdown is currently broken]

You can even create a website for the R package. For this, you need the [pkgdown](https://hadley.github.io/pkgdown/) R package [@wickham_pkgdown:_2017]:

```{r, eval = FALSE}
# install.packages("devtools")
devtools::install_github("hadley/pkgdown")
```

Then, to build your package's website, you will need to edit the first few lines of the `README.Rmd` file, from

```
output:
  md_document:
    variant: markdown_github
```

to

```
output: rmarkdown::github_document
```

We expect that in future releases of devtools will eliminate the need for manually changing this.  To create the website, run:

```{r, eval = FALSE}
pkgdown::build_site()
```

The website is now available at `docs/index.html`. You can open it and view it locally. However, you will certainly want to upload the website somewhere so that others can access it too.

To make the website available to others, you can host it somewhere in the internet. The easiest option, again, is to host it on GitHub. 

Assuming you have created the package in a local Git repository and have synced the repository to GitHub, this is easy. Push all the current changes to GitHub, and then go to the package's GitHub website, click "Settings", and scroll down to "GitHub Pages". There, click on the "Source" pull-down menu that currently says "None", and choose the "master branch /docs folder". Save the changes. After a little while, the page will be visible at https://username.github.io/packagename. For example, `exampleRPackage`'s website is at <https://mvuorre.github.io/exampleRPackage>.

Thank you for reading.

# Further Reading

## Online Resources

- <http://r-pkgs.had.co.nz/>: Website of Hadley Wickham's R Packages book
- [Writing an R package from scratch](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/): A short and good blog post on how to create minimal R packages
- [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html): The official R documentation on writing R packages. This is the complete and definitive set of instructions on how to write R packages. It is almost unreadable in it's comprehensiveness, and unnecessary for small R packages such as the data package described here.

## References
