This repository contains an R package called `finnsurveytext`. 
For further details on how to use the package, please see the [package website](https://dariah-fi-survey-concept-network.github.io/finnsurveytext/index.html) which contains tutorials covering all the functions available in `finnsurveytext`. 

A video demonstrating use of the first version of the package is available [here](https://www.helsinki.fi/fi/unitube/video/307d2df5-1a2b-4440-9562-d7d915addc35)
 
## Background ##
DARIAH-FI is one of two components of FIN-CLARIAH which is a research infrastructure project for Social Sciences and Humanities (SSH) in Finland. DARIAH-FI involves all Finnish universities with research in SSH. 
 
The first version of our package, `finnsurveytext`, was the output of WP3.3 of DARIAH-FI. This is a joint work package with Tampere University, University of Eastern Finland, University of Jyvaskyla and University of Helsinki with the objective of "better use of unstructured textual data in the context of Finnish surveys." 

The second release is output from WP4.1.6. The main updates in this release are:

* integration with the `survey` package by allowing `svydesign` objects as inputs
* the inclusion of survey response weights within tables and plots
* simplification of splitting data into groups within the 'comparison functions'
* enable use of package for multiple languages (not just Finnish!)
 
## Motivation ##
Open-ended questions are an important but challenging way to obtain informative data in surveys. Open-ended question data usually requires extra time investment (Fielding et al., 2013), but open-ended questions are particularly useful if researchers do not want to constrain respondents’ answers to pre-specified selections. Open-ended questions allow respondents to provide diverse answers based on their experience, and some answers are probably never thought of by researchers. (He & Schonlau, 2021.)
 
There's limited support for conducting qualitative analysis on Finnish open-ended survey responses and many researchers are more confident analysing responses to closed questions within surveys.
 
This package aims to provide a useful and user friendly set of tools for social science researchers to be able to analyse and understand responses to open-ended questions within their surveys. 
 
## Components ##
There are 5 sets of functions included in the `finnsurveytext` package. These are: 
 
1. Preparation functions (R/01_prepare.R) and (R/01b_prepare_svydesign.R)
    * These are functions to annotate survey data into a useful format (CoNLL-U) for analysis. There is a 'main' function within this set, `fst_prepare()` which combines the other preparation functions and can be run as a single function to prepare data for analysis. 
    * The second set of preparation functions enables the use of a `svydesign` object as input. 
2. Data exploration functions (R/02_data_exploration.R)
    * This file contains a number of functions which can be used for exploratory data analysis such as summary tables, plotting frequently occurring words and phrases, and creating wordclouds.
3. Concept Network functions (R/03_concept_network.R)
    *	All our concept network functions for a single network are in this file. Our concept network is one way of visualising the data that allows for interpretation. Our concept network function uses the TextRank algorithm which is a graph-based ranking model for text processing. Vertices represent words and co-occurrence between words is shown through edges. Word importance is determined recursively where words get more weight based on how many words co-occur and the weight of these co-occurring words. 
4. Comparison functions (R/04_comparison_functions.R)
    * We have created  partner functions for all the data exploration functions which compare different sets of data. These comparison functions can be used to compare different cohorts of survey respondents based on responses to closed questions such as gender, education level, location, age, etc. 
5.	Comparison concept network functions (R/05_comparison_concept_network.R)
    * Similarly, in this script we have functions for comparing respondent cohort responses in concept networks.
 
 
## Function Demos and Tutorials ##
Tutorials accompanying each of these R scripts can be found in the 'Articles' tab within the website. These tutorials use the sample data outlined below.  

A **BETA** demo of the package can also be launched by running the function finnsurveytext::runDemo() within R.

 
## Sample Data ##
Our repository also contains sample data which can be used to demonstrate and learn the functionality of `finnsurveytext`. 
 
The sample data comes from 2 surveys and can be found in the 'data' folder. The raw data (just from the relevant open-ended questions) is in data/bullying_data.rda and data/dev_data.rda. The data folder also contains examples of this data after the preparation functions have been applied and split by sample cohort groups. 
 
The raw data can also be downloaded from the Finnish Social Science Data Archive.  
 
1. Child Barometer 2016 Data
    * Source: FSD3134 Lapsibarometri 2016
    * Open-ended questions: q3 'Missä asioissa olet hyvä? (Avokysymys)', q7 ‘Kertoisitko, mitä sinun mielestäsi kiusaaminen on? (Avokysymys)’, q11 'Mikä tekee sinut iloiseksi? (Avokysymys)'
    *	Licence: (A) openly available for all users without registration (CC BY 4.0).
    *	Link to Data: https://urn.fi/urn:nbn:fi:fsd:T-FSD3134
 
2. Young Peoples' Views on Development Cooperation 2012 Data
    *	Source: FSD2821 Nuorten ajatuksia kehitysyhteistyöstä 2012
    *	Open-ended questions: q11_1 ‘Jatka lausetta: Kehitysmaa on maa, jossa… (Avokysymys)’, q11_2 ‘Jatka lausetta: Kehitysyhteistyö on toimintaa, jossa… (Avokysymys)’, q11_3’ Jatka lausetta: Maailman kolme suurinta	ongelmaa ovat… (Avokysymys)’
    *	Licence: (A) openly available for all users without registration (CC BY 4.0).
    *	Link to Data: https://urn.fi/urn:nbn:fi:fsd:T-FSD2821
 
 
## Installation and License ##
The package is available under the MIT license.
The released version of `finnsurveytext` can be installed from the CRAN: install.packages("finnsurveytext")
 
## References ##
Fielding, J., Fielding, N., & Hughes, G. (2013). Opening up open-ended survey data using qualitative software. Quality & Quantity, 47(6), 3261–3276. https://doi.org/10.1007/s11135-012-9716-1.
 
Finnish Children and Youth Foundation: Young People’s Views on Development Cooperation 2012 [dataset].
Version 2.0 (2019-01-22). Finnish Social Science Data Archive [distributor]. https://urn.fi/urn:nbn:fi:fsd:T-FSD2821
 
He, Z., & Schonlau, M. (2021). Coding Text Answers to Open-ended Questions: Human Coders and Statistical Learning Algorithms Make Similar Mistakes. Methods, Data, Analyses, 15(1), Article 1. https://doi.org/10.12758/mda.2020.10.
 
The Office of Ombudsman for Children: Child Barometer 2016 [dataset]. Version 1.0 (2016-12-09). Finnish
Social Science Data Archive [distributor]. https://urn.fi/urn:nbn:fi:fsd:T-FSD3134
 
 
 
 
 

