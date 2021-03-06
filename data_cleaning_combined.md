Data cleaning, combined
================
James Dalgleish
December 5, 2018

A package with an odd name, SEERaBomb, was created specifically for reading this data in R, utilizing the dplyr framework (<http://rpubs.com/mascha/mergingSEERData>).

``` r
df <- SEERaBomb::getFields(seerHome = "./seer_data/ascii/SEER_1973_2015_TEXTDATA/")
```

We've now set the seer home. The old package generated errors due to a minimum set of seer fields that are no longer present in the current seer dataset (radiatn, chemo). For whatever reason, they don't exist in data any longer, so I've updated the function to reflect this. Pardon the appearance of "pickFields," it's a function that we have only slightly modified to fix a small bug that occurred due to updated SEER data having slightly different fields, specifically we removed "radiatn" and "chemo" from the musts vector, which "must" be present for the function to work. Radiation and Chemo data were removed from SEER data as of November 2016(<https://seer.cancer.gov/data/treatment.html>).

``` r
pickFields <- function(sas, picks = c("casenum", "reg", "race", "sex", "agedx", 
    "yrbrth", "seqnum", "modx", "yrdx", "histo3", "ICD9", "COD", 
    "surv", "radiatn", "chemo")) 
{
    notRows = setdiff(picks, sas$names)
    if (length(notRows) > 0) 
        stop(paste0("The following picks are not allowed: ", 
            paste(notRows, collapse = ", ")))
    ncols = dim(sas)[1]
    nBytesP1 = sum(sas[ncols, 1:2])
    rownames(sas) <- sas$names
    ord = 1:ncols
    names(ord) = sas$names
    ord = ord[picks]
    picks = names(sort(ord))
    musts = c("reg", "race", "sex", "agedx", "histo3", "ICD9")
    missing = setdiff(musts, picks)
    if (!all(musts %in% picks)) {
        cat("Picks must at least include:")
        print(musts)
        stop("\n  The following picks are missing: ", paste(missing, 
            collapse = ", "))
    }
    sas = sas[picks, ]
    N = length(picks)
    sas = cbind(sas, type = "integer", stringsAsFactors = FALSE)
    if ("siteo2" %in% sas$names) 
        sas["siteo2", "type"] = "string"
    if ("ICD10" %in% sas$names) 
        sas["ICD10", "type"] = "string"
    if ("eod13" %in% sas$names) 
        sas["eod13", "type"] = "string"
    if ("eod2" %in% sas$names) 
        sas["eod2", "type"] = "string"
    if ("primsite" %in% sas$names) 
        sas["primsite", "type"] = "string"
    if ("plcbrthcntry" %in% sas$names) 
        sas["plcbrthcntry", "type"] = "string"
    if ("plcbrthstate" %in% sas$names) 
        sas["plcbrthstate", "type"] = "string"
    if (picks[1] == "casenum") 
        outdf = sas[1, , drop = F]
    else outdf = data.frame(start = 1, width = sas$start[1] - 
        1, names = " ", desc = " ", type = "string")
    for (i in 2:N) if (sas$start[i] == (up <- sas$start[i - 1] + 
        sas$width[i - 1])) 
        outdf = rbind(outdf, sas[i, ])
    else {
        outdf = rbind(outdf, data.frame(start = up, width = (sas$start[i] - 
            up), sasnames = " ", names = " ", desc = " ", type = "string"))
        outdf = rbind(outdf, sas[i, ])
    }
    if ((up <- sas$start[i] + sas$width[i]) < nBytesP1) 
        outdf = rbind(outdf, data.frame(start = up, width = (nBytesP1 - 
            up), sasnames = " ", names = " ", desc = " ", type = "string"))
    outdf$type = as.character(outdf$type)
    if (any(outdf$width < 0)) 
        stop("Negative field width. One of your picks may be out of order!")
    outdf
}
df <- pickFields(sas = df, picks = df$names) 
mkSEER(df, seerHome = "./seer_data/ascii/SEER_1973_2015_TEXTDATA/")
```

The SEER data is now in a very large flatfile in compressed RDA format. We'll summarize this data shortly and combine it with the tox data, but first, we'll resolve the FIPS codes to state and county names using the maps package.

``` r
load("./seer_data/ascii/SEER_1973_2015_TEXTDATA/mrgd/cancDef.RData")
library(maps)
library(totalcensus)
Sys.setenv(PATH_TO_CENSUS = getwd())
canc_w_names <- canc %>% mutate(state_county_name = maps::county.fips[match(x = stcnty, maps::county.fips$fips),]$polyname,state_name = str_split(state_county_name, pattern = ",", simplify = T) %>% .[,1],
county_name = str_split(state_county_name, pattern = ",",
simplify = T) %>% .[,2])
#saveRDS(object = canc_w_names,file = "canc_w_names.rds")
```

TRI data processing used for main cancer-tox dataset simply involves pulling in all the CSV files (in the tri\_temporal\_fixed folder) and row binding them. All columns are converted to character, initially to avoid errors. Parallelization through the furrr package was used to speed up the process and the difference was quite noticable. Reading in this data correctly was a time consuming process otherwise.

``` r
library(furrr)
plan(multiprocess)
tri_temp_df_dplyr <- list.files("tri_temporal_fixed/",pattern = glob2rx("*.csv"),full.names = T) %>% 
furrr::future_map_dfr(.f = read_csv,
                      col_types = rep("c",110) %>% str_c(collapse = "")
                      )
saveRDS(object = tri_temp_df_dplyr, "tri_temp_df_dplyr.rds")
```

Summarization of cancer cases by county and year:

``` r
can_inc_by_coun <- canc_w_names %>% 
  select(state_name,county_name,cancer,stcnty,yrdx) %>% 
  group_by(state_name,county_name,cancer,stcnty,yrdx) %>% 
  summarise(n = n()) 
```

Here we change the states into FIPS codes for merging:

``` r
state2abb <- tolower(state.abb)
names(state2abb) <- tolower(state.name)
can_inc_by_coun_st <- can_inc_by_coun %>% 
  mutate(state_abb = state2abb[state_name])
```

TRI data that contained lines for individual chemicals released from a single facility on a certain year was pulled in from several large comma separated value files, combined by rows, and summarized similarly such that the total number of released pounds by chemical was obtained for every chemical in every county. Only carcinogens were summarized in this dataset.

``` r
rel_summary_county <- tri_temp_df_dplyr %>% 
  janitor::clean_names() %>% 
  mutate(total_releases = as.numeric(total_releases)) %>% 
  mutate(county = tolower(county)) %>%
  mutate(st = tolower(st)) %>%
  filter(carcinogen == "YES") %>% #https://stackoverflow.com/questions/5411979/state-name-to-abbreviation-in-r
  group_by(chemical,st,county,year) %>% 
  summarize(total_rel_summ = sum(total_releases)) %>% 
  arrange(-total_rel_summ)
```

Now, we'll merge the TRI summary data with the cancer (SEER) county-summarized data, using an inner join by county, state, and year, selecting only the merged chemical, state, county, total amount released in pounds, the type of cancer, the number of cases, and the FIPS code (stcnty as the original data called it).

``` r
county_cancer_chem <- rel_summary_county %>% 
  mutate(year = as.integer(year)) %>% 
  inner_join(can_inc_by_coun_st, by = c("county" = "county_name","st" = "state_abb", "year" = "yrdx")) %>% 
  select(chemical,st,county,year,total_rel_summ,cancer,n,stcnty)
map(county_cancer_chem, ~sum(is.na(.)))
```

Census codes were obtained from the master datasheet (“Mastdata.xls”) for every year from 1979 to 2009. Utilizing a custom function to read all the sheets from an excel file into a list of sheets, several individual excel files were read in after conversion to xlsx format (INC01 to INC03, PVY01 to PVY02, PST01 and PST02, and IPE01). Area name and fips were extracted along with the columns for income and population estimates for the respective year. These columns were converted from wide to long format and census codes naming the original columns were resolved to years. Two dataframes resulted from this, one for median household income by county and one for population estimates.

``` r
read_excel_allsheets_dfc <- function(fn) {
  sheets <- readxl::excel_sheets(fn)
  sheet_list <- map2_dfc(.x = fn,.y = sheets, .f = ~readxl::read_excel(path = .x, sheet = .y))
  return(sheet_list)
}
data_dict <- readxl::read_excel("census_income/Mastdata.xls") %>% janitor::clean_names()
income_cols_desc <- data_dict %>%
  filter(
    str_detect(string = item_description, pattern = "Median household income") &
    !str_detect(string = item_description, pattern = "in the past 12 months")  
    ) %>% 
  select("item_id","item_description") %>% 
  mutate(year = stringr::word(item_description,-1))
pop_cols_desc <- data_dict %>%
  filter(
    str_detect(string = item_description, pattern = "Resident total population estimate ") &
    !str_detect(string = item_description, pattern = "rank")  
    ) %>% 
  select("item_id","item_description") %>% 
  mutate(year = stringr::word(item_description,-1))

cencode2year <- c(income_cols_desc %>% pull(year), pop_cols_desc %>% pull(year)) 
names(cencode2year) <- c(income_cols_desc %>% pull(item_id), pop_cols_desc %>% pull(item_id))
  
census_income_pop_all <- list("./census_income/INC01.xlsx",
                          "./census_income/INC02.xlsx",
                          "./census_income/INC03.xlsx",
                          "./census_income/PVY01.xlsx",
                          "./census_income/PVY02.xlsx",
                          "./census_income/PST01.xlsx",
                          "./census_income/PST02.xlsx",
                          "./census_income/IPE01.xlsx") %>%
  map_dfc(read_excel_allsheets_dfc) %>% 
  select("Area_name",   "STCOU",
income_cols_desc %>% pull(item_id),pop_cols_desc %>% pull(item_id))
census_inc_tidy <-  census_income_pop_all %>% 
    gather(key = year_inc, value = med_income, INC110179D:IPE010209D) %>%
    mutate(code = year_inc,
         year_inc = cencode2year[year_inc]) %>% 
  select("Area_name",   "STCOU",year_inc,med_income,code)
census_pop_tidy <-  census_income_pop_all %>% 
  gather(key = year_pop, value = pop_est, PST015171D:PST045209D) %>%
  mutate(code = year_pop,
         year_pop = cencode2year[year_pop]) %>% 
  select("Area_name",   "STCOU",year_pop,pop_est,code)
census_income_pop_tidy <- (census_inc_tidy %>% 
                             select(-code)) %>% 
  inner_join((census_pop_tidy %>% select(-code)),
             by = c("year_inc" = "year_pop","Area_name","STCOU"))
# write_csv(census_income_pop_tidy,"census_income_pop_tidy.csv")
# write_csv(census_pop_tidy,"census_pop_tidy.csv")
# write_csv(census_inc_tidy,"census_inc_tidy.csv")
# saveRDS(census_income_pop_tidy,"census_income_pop_tidy.rds")
 saveRDS(census_pop_tidy,"census_pop_tidy.rds")
# saveRDS(census_inc_tidy,"census_inc_tidy.rds")
```

The population estimates are then combined with the merged aggregated toxic release inventory data and the aggregated cancer cases by county, cancer type, and year. String padding was used to make the FIPS code from SEER compatible with the FIPS code from the census data, and incidence was calculated by the number of diagnosed cases for a specific cancer in a given year divided by the population estimate for the county for that specific year. Cancer "Prevalence" was later termed cancer "incidence" in later data analyses. The final variables used in this dataset contain county information, total releases, the number of cancer patients diagnosed, the type of cancer, the population estimate, and the incidence.

``` r
census_pop_tidy <- readRDS("census_pop_tidy.rds") %>% 
  mutate(year_pop = as.integer(year_pop))

cancer_county_chem_pop <- county_cancer_chem %>% 
  mutate(stcnty = (as.character(stcnty) %>% 
           str_pad(string = .,width = 5,side = "left", pad = "0"))) %>% 

  inner_join(census_pop_tidy, by = c("stcnty" = "STCOU", "year" = "year_pop")) %>% 
  mutate(prevalence = n/pop_est) %>% 
  select(chemical,st,county,year,total_rel_summ,cancer,n,pop_est, prevalence)
saveRDS(cancer_county_chem_pop,"cancer_county_chem_pop.rds") 
```

The TRI dataset was further filtered down to only include carcinogens as our primary focus is cancer. Variables were converted to their appropriate type (numeric, factor). Based off of other research studies, we summarized total waste release between onsite and offsite release. Onsite waste release was further summarized into three categories: air, water, and land. The data was then simplified by removing variables that were not of interest to us in our analyses. Some examples include industrial codes (NAICS, SIC) and specific waste release routes (ex. wastewater treatment for non-metals, offsite incineration of non-metals):

``` r
tri_df = tri_temp_df_dplyr %>% 
  janitor::clean_names()
# Select only variables we are interested in
# Filter to only include chemicals that are classified as carcinogens
# Convert to factor carcinogen and metal variables
tri_df = tri_df %>% 
  select(year:facility_name, city:industry_sector, primary_naics, chemical, clear_air_act_chemical:carcinogen, unit_of_measure:x8_8_one_time_release, parent_company_name, -frs_id, -bia_code, -tribe) %>% 
  filter(carcinogen == "YES") %>% 
  mutate(state = st,
         carcinogen = as.factor(carcinogen),
         metal = as.factor(metal))
# Convert numeric variables from character type
# Specify column numbers that need to be converted
cols.num = c(1, 8, 9, 21:86)
tri_df[cols.num] = map_df(.x = tri_df[cols.num], ~as.numeric(.x))
# Simplify waste release and management data
# Sum together onsite release categories: air, water, land
# Remove repetitive waste release information
tri_df = tri_df %>% 
  mutate(air_onsite_release = x5_1_fugitive_air + x5_2_stack_air,
         water_onsite_release = x5_3_water,
         land_onsite_release = rowSums(select(.,x5_4_underground:x5_5_4_other_disposal)),
         potw_total_transfers = x6_1_potw_total_transfers,
         one_time_release = x8_8_one_time_release) %>% 
  select(year:county, -st, state, zip:unit_of_measure, air_onsite_release, 
         water_onsite_release, land_onsite_release, 
         on_site_release_total, potw_total_transfers, off_site_release_total, 
         off_site_recycled_total,off_site_recovery_total, off_site_treated_total, 
         total_releases, prod_waste_8_1_thru_8_7, parent_company_name)
```
