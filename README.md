# Medinet Parser

Parses data from the Medinet Scheduling system, developed by [Medical Networks Scandinavia AB](https://www.medinetworks.se), for analysis in R, aiding scheduling and follow up. This project is in no way connected to the developers of Medinet.

## Installing the *medinetparser* Package

The following steps describe how to install `medinetparser` from GitHub.

1. Install *devtools*
```r
install.packages('devtools')
```
2. Load *devtools*
```r
library(devtools)
```
3. Install *medinetparser*
```r
install_github('JonasEngstrom/medinetparser')
```
4. Load *medinetparser*
```r
library(medinetparser)
```

## Get Started Using the Medinet Parser

Medinet Parser currently does not support scraping data directly from Medinet, as this would require handling login credentials. Therefore data must first be downloaded manually, by following the subsequent steps.

1. Go to the [Medinet Website](https://medinet.se).

2. Log in.

3. If you work in several departments, you will be prompted to choose one at this point. If not, skip ahead to the next step.

4. Click *Schema*.

5. Make sure *Schemavy* is set to *Vecka vs användare*.

6. Choose the weeks you are interested in analyzing.

7. Use the web brower’s save command.

8. Save the page in HTML format. This will be your input file to Medinet Parser.
