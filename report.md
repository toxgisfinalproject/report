Report
================
James Dalgleish, Joy Hsu, Rachel Tsong, Yishan Wang, Adina Zhang
6 December 2018

Motivation
----------

It is well known that exposure to certain chemicals can be a causal factor for many cancers, and many of the chemicals released from industry are carcinogenic. For this project, we wanted to know if we could use datasets that are publicly available to see if there is a geographic association between incidences of cancer and amount of toxic waste dumping. We hope our project results can help people understand the importance of reducing the amount of toxic waste dumping and create a cleaner, healthier living environment.

Related Work
------------

Extensive research has been done regarding the carcinogenicity of many toxic chemicals. A research article “Association between Six Environmental Chemicals and Lung Cancer Incidence in the United States” was particularly inspiring because it used publicly available datasets (Toxic Release Inventory and Surveillance, Epidemiology, and End Results) to link environmental waste to cancer. It explained what chemicals that are important determinants in lung cancer development. Based on this research article, we wanted to conduct further research to find out the relationships between major chemical waste exposures and common cancers (including lung cancer).

Initial Questions
-----------------

Initially, in our proposal we suggested exploring several lines of inquiry:

-   Toxic waste data
    -   What chemicals are being released? Which ones are carcinogens?
    -   What is the geographic distribution (by county, state) of chemical waste releases?
    -   What are the temporal trends of chemical waste release?
-   Census data
    -   Is there a geographic and/or temporal relationship between socio-economic status and chemical waste releases?
-   Disease data
    -   Is there a geographic and/or temporal relationship between health outcomes (including cancer, asthma, birth outcomes, etc.) and chemical waste releases?

We wanted to begin our project with a broad general idea and then narrow our focus as time went on. One of the first things we focused on was selecting cancer as our main disease outcome. This was in part due to the availability of cancer data and also based on group member interest. Additionally, we looked into scientific literature to help us decide what cancers and chemicals were already shown to be linked, so that we had some guidelines for our analysis. Based on a couple of papers, we decided that we could look at lung cancer and determine which (if any chemicals) were geographically correlated to lung cancer incidence.

Over time, our research questions changed a lot based on the limitations in the datasets that we chose. For example, there is a strong connection between asbestos exposure and mesothelioma, so we thought that it would be interesting to investigate if mesothelioma rates are correlated to places where asbestos is released into the environment. However, mesothelioma was such a rare cancer that there were not enough counties that reported both asbestos release and cases of mesothelioma. Additionally, although we initially wanted to investigate national trends regarding cancer and toxic waste, we were limited by the SEER dataset because there is not data for every state.

Data Sources
------------

### The Toxic Releases Inventory (TRI)

The Toxic Releases Inventory (TRI) is a dataset compiled by the U.S. Environmental Protection Agency (EPA) and contains comprehensive information describing estimated waste release and transfers of toxic chemicals in the United States between 1987 to present. Following policy passed in 1986, it became required for manufacturing facilities to report their releases of toxic chemicals to the EPA. By making these data public, industries are held accountable for their waste and environmental impact.

### The Surveillance, Epidemiology, and End Results (SEER) Program

The Surveillance, Epidemiology, and End Results (SEER) Program is a dataset compiled by the National Cancer Institute (NCI) and contains information that describes cancer statistics in the United States. They compile information from 17 cancer registries across ten states from 1987 to 2009.

Data Collection and Cleaning
----------------------------

-   SEER data (Surveillance, Epidemiology, and End Results) was from [SEER data](https://seer.cancer.gov/data/)

SEER data was obtained by applying for permission for the data (<https://seer.cancer.gov/seertrack/data/request/>) and after a period, we were sent links to self-extracting archives containing textual and binary data of relatively large size. The SEERaBOMB package (from CRAN: <https://cran.r-project.org/web/packages/SEERaBomb/SEERaBomb.pdf>) was used to take the original ASCII text files into RData format. We modified the pickFields function such that it works on current SEER data to make it function.

Following this, we used the maps package to convert the state and county FIPS codes to state and county names and saved the result into an RDS file. Following this, we selected the state name, county name, cancer, state-county fips code, and the year of diagnosis and then summarized the flat initial SEER data frame where every row represented an individual cancer case and converted it into county based sums of patients with a specific cancer in that county. We converted state names to abbreviations.

-   TRI data (Toxics Release Inventory) was from [TRI Data](https://toxmap.nlm.nih.gov/toxmap/download.html)

TRI data that contained lines for individual chemicals released from a single facility on a certain year was pulled in from several large comma separated value files, combined by rows, and summarized similarly such that the total number of released pounds by chemical was obtained for every chemical in every county.

This tidy dataset was then formed by performing an inner join by county, state, and year for aggregated cancer data to aggregated TRI facility data.

-   Census data was from [Census Bureau](https://www.census.gov/support/USACdataDownloads.html)

Population estimates for each county by year were then obtained from the census bureau website (<https://www.census.gov/support/USACdataDownloads.html>) by the following process: Census codes were obtained from the master datasheet (“Mastdata.xls”) for every year from 1979 to 2009. Utilizing a custom function to read all the sheets from an excel file into a list of sheets, several individual excel files were read in after conversion to xlsx format (INC01 to INC03, PVY01 to PVY02, PST01 and PST02, and IPE01). Area name and fips were extracted along with the columns for income and population estimates for the respective year. These columns were converted from wide to long format and census codes naming the original columns were resolved to years. Two dataframes resulted from this, one for median household income by county and one for population estimates. Given time constraints, we aimed to use the population estimates and merged them to the aggregated seer and tri facility data. Population data was then joined to the joined cancer-tri data by state-county fips code and year.

Exploratory Analysis
--------------------

Based off of our outside research, we identified some combinations of chemicals and cancers we would be interested in investigating. Benzene and acute myeloid leukemia have been strongly linked in the literature. Lung cancers have also been linked with several carcinogens in our dataset include formaldehyde, acrylonitrile, 1,3-butadiene, and several others. Throughout our exploratory analysis, we provide visualizations and summaries including both general trends and specific trends to our cancers and chemicals of interest. In our summaries, the TRI dataset has been filtered to only include carcinogens.

### TRI data

The amount of waste released from industrial factories is categorized into the separate release routes in order to better evaluate the waste management system as well as gauge environmental and human impact. From the literature and TRI website, most analysis is categorized between onsite and offsite waste. Offsite waste is the waste transported away for processing and disposal at waste management facilities or reused for other industrial processes. Onsite waste is the waste released directly from the factory into the environment. This category is further divided into waste released into the air, water, or land. We are most interested in evaluating these routes as these are the paths that most directly impact the health of individuals living near industrial facilities.

#### Common carcinogens released

\[code for table\]

#### Amount of waste released over time

\[code for total waste in America over time\]\[code for benzene\]

In general, carcinogenic waste has decreased between 1987 to 2017, reflecting strongly on the improvements to waste management policy and practices. Most carcinogenic waste is released through the air.

#### Geographic distribution of waste

\[code\]

Based off of this barplot, the top five states that produce the most carcinogenic waste are Texas, Louisiana, Indiana, Ohio, and Pennsylvania.

#### Top industries that produce the most carcinogenic waste

\[code for US\]\[code for benzene\]

### SEER data

Due to limited information on cancer incidence rates in our dataset, we are only able to report the data from ten states.

#### Cancer incidence over time

\[combined\]

### Combined TRI and SEER geographic association

#### Geographic distribution of cancers relative to industrial waste sites. (James’ choropleth)

\[side by side combined-- AML and Benzene\]\[Pleura and Lung?\]

### Dashboard

In the exploratory dashboard, we present several interactive plots that allow for users to explore industrial waste release and cancer incidences across different states and counties. These plots are derived from our exploratory analysis and take advantage of the Shiny web app’s interactivity that gives the user control to choose specific chemicals and cancers they might be interested in.

An interactive map….. Two plots that show chemical waste release and cancer incidence over time allows the user to better visualize trends.

Additional Analysis
-------------------

We investigated the relationship between chemical release and lung cancer incidence, at the county level, for seven known carcinogens. Multiple linear regression models were fitted with lung cancer incidence (per 100,000 individuals per year) as the main response and chemical release (log-pounds, by year) as the main effect. We adjusted for median income\* as a proxy for socioeconomic status and percent of current/previous smokers\*\*.

\* median income (in thousands) was adjusted for at the county level, by year.

\*\* percent of current/previous smokers was adjusted for at the state level, by year.

**1. Formaldehyde**

**Model 1 (adjusted *R*<sup>2</sup>: 0.21, p-value: &lt; 0.05):** Lung Cancer Incidence = 17.81 + 1.40 \* ln(pounds formaldehyde release) + 1.15 \* (percent current/previous smokers) - 2.77e-7 \* (median income)

We conclude that formaldehyde release is a significant predictor of lung cancer incidence, adjusting for smoking rates and median income. For every ln(pound) increase in formaldehyde release, we expect 1.4 additional case of of lung cancer per 100,000 persons (p-value &lt; 0.01).

Discussion
----------

### Future Direction

There are two future directions that are generated by our project. First, exposing to certain chemicals can be a causal factor for lung cancer, however, certain gene mutations can also predict lung cancer. Future research could be conducted by analyzing lung cancer patients’ gene information and add it as a potential predictor in the regression model that we produced in the additional analysis. Second, waste management policy could be improved by taking advantage of the information provided in our project.

Extra: Slope is defined as the coefficient estimate abstracted from a simple linear regression with incidence (per 100,000 persons) as the response and year as the predictor.

Incidence: New cancer cases per 100,000 persons, at the county level.