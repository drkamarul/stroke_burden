---
title: "Longitudinal Data Study"
author: "Kamarul Imran Musa"
date: "1/26/2022"
output: 
  html_document: 
    toc: yes
    toc_float:
      collapsed: no
      smooth_scroll: no
    keep_md: yes
---



# Prepare


```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.1.6     v dplyr   1.0.7
## v tidyr   1.1.4     v stringr 1.4.0
## v readr   2.1.0     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(gtsummary)
library(DT)
```

# Read data


```r
data1 <- read_rds('zbicafu.Rds')
summary(data1)
```

```
##    record_id     redcap_event_name    pt_name             pt_ic          
##  Min.   : 1.00   Length:204         Length:204         Length:204        
##  1st Qu.:18.00   Class :character   Class :character   Class :character  
##  Median :38.00   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :40.84                                                           
##  3rd Qu.:62.00                                                           
##  Max.   :94.00                                                           
##                                                                          
##   pt_gender       pt_age        cg_name           cg_gender  
##  Female: 38   Min.   :23.90   Length:204         Female: 61  
##  Male  : 56   1st Qu.:56.60   Class :character   Male  : 32  
##  NA's  :110   Median :62.90   Mode  :character   NA's  :111  
##               Mean   :61.96                                  
##               3rd Qu.:68.20                                  
##               Max.   :92.30                                  
##               NA's   :111                                    
##        cg_relation   zbi_dcdurday   zbi_dcdurweek    zbi_dcdurmonth  
##  Parent      : 44   Min.   : 3.00   Min.   : 0.400   Min.   :0.1000  
##  Spouse      : 27   1st Qu.: 7.25   1st Qu.: 1.025   1st Qu.:0.2250  
##  Child       : 12   Median :31.00   Median : 4.400   Median :1.0000  
##  Grandparents:  4   Mean   :28.81   Mean   : 4.109   Mean   :0.9436  
##  Siblings    :  3   3rd Qu.:34.75   3rd Qu.: 4.975   3rd Qu.:1.1000  
##  (Other)     :  3   Max.   :92.00   Max.   :13.100   Max.   :3.0000  
##  NA's        :111   NA's   :94      NA's   :94       NA's   :94      
##  zbi_total_score cafu_iadl_d_score cafu_iadl_u_score cafu_iadl_u_meanscore
##  Min.   : 0.00   Min.   : 8.00     Min.   : 0.000    Min.   :0.0000       
##  1st Qu.:11.00   1st Qu.:32.75     1st Qu.: 0.000    1st Qu.:0.0000       
##  Median :20.00   Median :46.50     Median : 2.000    Median :0.3650       
##  Mean   :20.97   Mean   :41.94     Mean   : 4.167    Mean   :0.6520       
##  3rd Qu.:27.00   3rd Qu.:55.25     3rd Qu.: 4.000    3rd Qu.:0.8375       
##  Max.   :62.00   Max.   :56.00     Max.   :28.000    Max.   :3.5000       
##  NA's   :94      NA's   :96        NA's   :96        NA's   :100          
##  cafu_adl_d_score cafu_adl_u_score cafu_adl_u_meanscore
##  Min.   : 7.00    Min.   : 0.000   Min.   :0.0000      
##  1st Qu.:26.50    1st Qu.: 0.000   1st Qu.:0.0000      
##  Median :40.50    Median : 3.000   Median :0.6000      
##  Mean   :34.18    Mean   : 4.324   Mean   :0.8175      
##  3rd Qu.:49.00    3rd Qu.: 6.000   3rd Qu.:1.0000      
##  Max.   :49.00    Max.   :27.000   Max.   :3.8600      
##  NA's   :96       NA's   :96       NA's   :115
```

There are

- 79 patients
- 93 caregivers


```r
sum(!is.na(unique(data1$pt_name)))
```

```
## [1] 79
```

```r
sum(!is.na(unique(data1$cg_name)))
```

```
## [1] 93
```





```r
data1id <- 
  data1 %>%
  select(record_id, pt_name, cg_name, 
         pt_gender, pt_age, cg_gender) %>%
  filter(!is.na(pt_name))
#data1id
data1id %>%
  select(cg_gender) %>%
  tbl_summary()
```

```{=html}
<div id="arezamcmxq" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#arezamcmxq .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#arezamcmxq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#arezamcmxq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#arezamcmxq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#arezamcmxq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#arezamcmxq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#arezamcmxq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#arezamcmxq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#arezamcmxq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#arezamcmxq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#arezamcmxq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#arezamcmxq .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#arezamcmxq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#arezamcmxq .gt_from_md > :first-child {
  margin-top: 0;
}

#arezamcmxq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#arezamcmxq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#arezamcmxq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#arezamcmxq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#arezamcmxq .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#arezamcmxq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#arezamcmxq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#arezamcmxq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#arezamcmxq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#arezamcmxq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#arezamcmxq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#arezamcmxq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#arezamcmxq .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#arezamcmxq .gt_left {
  text-align: left;
}

#arezamcmxq .gt_center {
  text-align: center;
}

#arezamcmxq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#arezamcmxq .gt_font_normal {
  font-weight: normal;
}

#arezamcmxq .gt_font_bold {
  font-weight: bold;
}

#arezamcmxq .gt_font_italic {
  font-style: italic;
}

#arezamcmxq .gt_super {
  font-size: 65%;
}

#arezamcmxq .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N = 94</strong><sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">cg_gender</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
<td class="gt_row gt_center">61 (66%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
<td class="gt_row gt_center">32 (34%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">1</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="2">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          n (%)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
```


# describe


```r
data2 <- 
  data1 %>%
  select(record_id, 
         #pt_name,
         #cg_name,
         redcap_event_name, 
         starts_with('zbi_total'), starts_with('cafu')) %>%
  mutate(redcap_event_name = factor(redcap_event_name)) 
#data2
```




```r
data3 <- data2 %>%
  left_join(data1id, by = 'record_id') %>%
  filter(redcap_event_name == 'baseline_1week_arm_1' | 
           redcap_event_name == '1month_arm_1') %>%
  relocate(pt_name, cg_name, .after = record_id)
#data3
```


# overall scores 


```r
data3 %>%
  select(redcap_event_name, 
         cg_gender, 
         starts_with('zbi'), starts_with('cafu')) %>%
  tbl_summary(by = 'redcap_event_name',
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = all_continuous() ~ 2,)
```

```{=html}
<div id="uqkmspcoeg" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uqkmspcoeg .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uqkmspcoeg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uqkmspcoeg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uqkmspcoeg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uqkmspcoeg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uqkmspcoeg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uqkmspcoeg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uqkmspcoeg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uqkmspcoeg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uqkmspcoeg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uqkmspcoeg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uqkmspcoeg .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#uqkmspcoeg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uqkmspcoeg .gt_from_md > :first-child {
  margin-top: 0;
}

#uqkmspcoeg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uqkmspcoeg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uqkmspcoeg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#uqkmspcoeg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uqkmspcoeg .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#uqkmspcoeg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uqkmspcoeg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uqkmspcoeg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uqkmspcoeg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uqkmspcoeg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uqkmspcoeg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#uqkmspcoeg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uqkmspcoeg .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#uqkmspcoeg .gt_left {
  text-align: left;
}

#uqkmspcoeg .gt_center {
  text-align: center;
}

#uqkmspcoeg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uqkmspcoeg .gt_font_normal {
  font-weight: normal;
}

#uqkmspcoeg .gt_font_bold {
  font-weight: bold;
}

#uqkmspcoeg .gt_font_italic {
  font-style: italic;
}

#uqkmspcoeg .gt_super {
  font-size: 65%;
}

#uqkmspcoeg .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>1month_arm_1</strong>, N = 40<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2month_arm_1</strong>, N = 0<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>baseline_1week_arm_1</strong>, N = 53<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>pre_discharge_arm_1</strong>, N = 0<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">cg_gender</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
<td class="gt_row gt_center">28 (70%)</td>
<td class="gt_row gt_center">0 (NA%)</td>
<td class="gt_row gt_center">33 (62%)</td>
<td class="gt_row gt_center">0 (NA%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
<td class="gt_row gt_center">12 (30%)</td>
<td class="gt_row gt_center">0 (NA%)</td>
<td class="gt_row gt_center">20 (38%)</td>
<td class="gt_row gt_center">0 (NA%)</td></tr>
    <tr><td class="gt_row gt_left">zbi_total_score</td>
<td class="gt_row gt_center">19.92 (12.67)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">23.74 (14.45)</td>
<td class="gt_row gt_center">NA (NA)</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_d_score</td>
<td class="gt_row gt_center">39.36 (15.19)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">43.49 (12.65)</td>
<td class="gt_row gt_center">NA (NA)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_score</td>
<td class="gt_row gt_center">3.92 (5.47)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">5.25 (7.69)</td>
<td class="gt_row gt_center">NA (NA)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_meanscore</td>
<td class="gt_row gt_center">0.64 (0.73)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.77 (0.97)</td>
<td class="gt_row gt_center">NA (NA)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">3</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_d_score</td>
<td class="gt_row gt_center">31.15 (15.43)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">36.19 (14.64)</td>
<td class="gt_row gt_center">NA (NA)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_score</td>
<td class="gt_row gt_center">3.82 (5.12)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">5.28 (7.13)</td>
<td class="gt_row gt_center">NA (NA)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">0</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_meanscore</td>
<td class="gt_row gt_center">0.76 (0.79)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.95 (1.03)</td>
<td class="gt_row gt_center">NA (NA)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">9</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">7</td>
<td class="gt_row gt_center">0</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="5">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          n (%); Mean (SD)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
```



# Pre-post ZBI


```r
data3_zbi <- data3 %>%
  select(record_id, pt_name, cg_name, 
         cg_gender, 
         redcap_event_name, zbi_total_score) %>%
  pivot_wider(names_from = redcap_event_name,
              values_from = zbi_total_score) %>%
  rename(at_1_mth = "1month_arm_1") %>%
  mutate(diff = at_1_mth - baseline_1week_arm_1,
         percent_diff = (diff/baseline_1week_arm_1)*100)
#data3_zbi
```

## overall


```r
data3_zbi %>%
  select(cg_gender, baseline_1week_arm_1:percent_diff) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 2,)
```

```{=html}
<div id="erehnnfjxt" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#erehnnfjxt .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#erehnnfjxt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#erehnnfjxt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#erehnnfjxt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#erehnnfjxt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#erehnnfjxt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#erehnnfjxt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#erehnnfjxt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#erehnnfjxt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#erehnnfjxt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#erehnnfjxt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#erehnnfjxt .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#erehnnfjxt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#erehnnfjxt .gt_from_md > :first-child {
  margin-top: 0;
}

#erehnnfjxt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#erehnnfjxt .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#erehnnfjxt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#erehnnfjxt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#erehnnfjxt .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#erehnnfjxt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#erehnnfjxt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#erehnnfjxt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#erehnnfjxt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#erehnnfjxt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#erehnnfjxt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#erehnnfjxt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#erehnnfjxt .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#erehnnfjxt .gt_left {
  text-align: left;
}

#erehnnfjxt .gt_center {
  text-align: center;
}

#erehnnfjxt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#erehnnfjxt .gt_font_normal {
  font-weight: normal;
}

#erehnnfjxt .gt_font_bold {
  font-weight: bold;
}

#erehnnfjxt .gt_font_italic {
  font-style: italic;
}

#erehnnfjxt .gt_super {
  font-size: 65%;
}

#erehnnfjxt .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N = 57</strong><sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">cg_gender</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
<td class="gt_row gt_center">36 (63%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
<td class="gt_row gt_center">21 (37%)</td></tr>
    <tr><td class="gt_row gt_left">baseline_1week_arm_1</td>
<td class="gt_row gt_center">23.74 (14.45)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">4</td></tr>
    <tr><td class="gt_row gt_left">at_1_mth</td>
<td class="gt_row gt_center">19.92 (12.67)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">17</td></tr>
    <tr><td class="gt_row gt_left">diff</td>
<td class="gt_row gt_center">-4.03 (6.73)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">21</td></tr>
    <tr><td class="gt_row gt_left">percent_diff</td>
<td class="gt_row gt_center">-16.22 (31.16)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">21</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="2">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          n (%); Mean (SD)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
```

## based on gender


```r
data3_zbi %>%
  select(cg_gender, baseline_1week_arm_1:percent_diff) %>%
  tbl_summary(by = cg_gender,
              statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 2,)
```

```{=html}
<div id="eavcbqvsrd" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#eavcbqvsrd .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#eavcbqvsrd .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eavcbqvsrd .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#eavcbqvsrd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#eavcbqvsrd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eavcbqvsrd .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eavcbqvsrd .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#eavcbqvsrd .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#eavcbqvsrd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#eavcbqvsrd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#eavcbqvsrd .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#eavcbqvsrd .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#eavcbqvsrd .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#eavcbqvsrd .gt_from_md > :first-child {
  margin-top: 0;
}

#eavcbqvsrd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#eavcbqvsrd .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#eavcbqvsrd .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#eavcbqvsrd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eavcbqvsrd .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#eavcbqvsrd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eavcbqvsrd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#eavcbqvsrd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#eavcbqvsrd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eavcbqvsrd .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eavcbqvsrd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#eavcbqvsrd .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eavcbqvsrd .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#eavcbqvsrd .gt_left {
  text-align: left;
}

#eavcbqvsrd .gt_center {
  text-align: center;
}

#eavcbqvsrd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#eavcbqvsrd .gt_font_normal {
  font-weight: normal;
}

#eavcbqvsrd .gt_font_bold {
  font-weight: bold;
}

#eavcbqvsrd .gt_font_italic {
  font-style: italic;
}

#eavcbqvsrd .gt_super {
  font-size: 65%;
}

#eavcbqvsrd .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Female</strong>, N = 36<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Male</strong>, N = 21<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">baseline_1week_arm_1</td>
<td class="gt_row gt_center">23.70 (15.35)</td>
<td class="gt_row gt_center">23.80 (13.21)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">3</td>
<td class="gt_row gt_center">1</td></tr>
    <tr><td class="gt_row gt_left">at_1_mth</td>
<td class="gt_row gt_center">20.79 (13.14)</td>
<td class="gt_row gt_center">17.92 (11.78)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">8</td>
<td class="gt_row gt_center">9</td></tr>
    <tr><td class="gt_row gt_left">diff</td>
<td class="gt_row gt_center">-4.04 (7.28)</td>
<td class="gt_row gt_center">-4.00 (5.60)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">11</td>
<td class="gt_row gt_center">10</td></tr>
    <tr><td class="gt_row gt_left">percent_diff</td>
<td class="gt_row gt_center">-13.18 (32.14)</td>
<td class="gt_row gt_center">-23.12 (29.06)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">11</td>
<td class="gt_row gt_center">10</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="3">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Mean (SD)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
```

# pre-post cafu


```r
data3_cafu <- data3 %>%
  select(record_id, pt_name, cg_name, 
         cg_gender, 
         redcap_event_name, 
         starts_with('cafu')) %>%
  pivot_wider(names_from = redcap_event_name,
              values_from = cafu_iadl_d_score:cafu_adl_u_score) %>%
  mutate(diff_iadl_d = 
           cafu_iadl_d_score_1month_arm_1 - cafu_iadl_d_score_baseline_1week_arm_1,
         diff_iadl_d_pct = (diff_iadl_d/cafu_iadl_d_score_baseline_1week_arm_1)*100,
         diff_iadl_u = 
           cafu_iadl_u_score_1month_arm_1 - cafu_iadl_u_score_baseline_1week_arm_1,
         diff_iadl_u_pct = (diff_iadl_u/cafu_iadl_u_score_baseline_1week_arm_1)*100,
         diff_adl_d = 
           cafu_adl_d_score_1month_arm_1 - cafu_adl_d_score_baseline_1week_arm_1, 
         diff_adl_d_pct = (diff_adl_d/cafu_adl_d_score_baseline_1week_arm_1)*100,
         diff_adl_u = 
           cafu_adl_u_score_1month_arm_1 - cafu_adl_u_score_baseline_1week_arm_1,
         diff_adl_u_pct = (diff_adl_u/cafu_adl_u_score_baseline_1week_arm_1)*100)

# data3_cafu %>%
#  datatable()
```

## Overall


```r
data3_cafu %>%
  select(cg_gender:diff_adl_u) %>%
  tbl_summary(type = list(contains('diff') ~ "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 2,)
```

```{=html}
<div id="blaftthmuh" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#blaftthmuh .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#blaftthmuh .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#blaftthmuh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#blaftthmuh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#blaftthmuh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#blaftthmuh .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#blaftthmuh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#blaftthmuh .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#blaftthmuh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#blaftthmuh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#blaftthmuh .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#blaftthmuh .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#blaftthmuh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#blaftthmuh .gt_from_md > :first-child {
  margin-top: 0;
}

#blaftthmuh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#blaftthmuh .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#blaftthmuh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#blaftthmuh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#blaftthmuh .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#blaftthmuh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#blaftthmuh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#blaftthmuh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#blaftthmuh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#blaftthmuh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#blaftthmuh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#blaftthmuh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#blaftthmuh .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#blaftthmuh .gt_left {
  text-align: left;
}

#blaftthmuh .gt_center {
  text-align: center;
}

#blaftthmuh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#blaftthmuh .gt_font_normal {
  font-weight: normal;
}

#blaftthmuh .gt_font_bold {
  font-weight: bold;
}

#blaftthmuh .gt_font_italic {
  font-style: italic;
}

#blaftthmuh .gt_super {
  font-size: 65%;
}

#blaftthmuh .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N = 79</strong><sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">cg_gender</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
<td class="gt_row gt_center">52 (66%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
<td class="gt_row gt_center">27 (34%)</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_meanscore</td>
<td class="gt_row gt_center">0.92 (0.96)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">9</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_d_score_baseline_1week_arm_1</td>
<td class="gt_row gt_center">43.49 (12.65)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">26</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_d_score_1month_arm_1</td>
<td class="gt_row gt_center">39.36 (15.19)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">40</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_score_baseline_1week_arm_1</td>
<td class="gt_row gt_center">5.25 (7.69)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">26</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_score_1month_arm_1</td>
<td class="gt_row gt_center">3.92 (5.47)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">40</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_meanscore_baseline_1week_arm_1</td>
<td class="gt_row gt_center">0.77 (0.97)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">26</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_meanscore_1month_arm_1</td>
<td class="gt_row gt_center">0.64 (0.73)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">42</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_d_score_baseline_1week_arm_1</td>
<td class="gt_row gt_center">36.19 (14.64)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">26</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_d_score_1month_arm_1</td>
<td class="gt_row gt_center">31.15 (15.43)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">40</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_score_baseline_1week_arm_1</td>
<td class="gt_row gt_center">5.28 (7.13)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">26</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_score_1month_arm_1</td>
<td class="gt_row gt_center">3.82 (5.12)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">40</td></tr>
    <tr><td class="gt_row gt_left">diff_iadl_d</td>
<td class="gt_row gt_center">-1.29 (4.75)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">65</td></tr>
    <tr><td class="gt_row gt_left">diff_iadl_d_pct</td>
<td class="gt_row gt_center">-8.54 (20.54)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">65</td></tr>
    <tr><td class="gt_row gt_left">diff_iadl_u</td>
<td class="gt_row gt_center">0.14 (1.70)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">65</td></tr>
    <tr><td class="gt_row gt_left">diff_iadl_u_pct</td>
<td class="gt_row gt_center">Inf (NA)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">70</td></tr>
    <tr><td class="gt_row gt_left">diff_adl_d</td>
<td class="gt_row gt_center">-0.21 (3.91)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">65</td></tr>
    <tr><td class="gt_row gt_left">diff_adl_d_pct</td>
<td class="gt_row gt_center">-0.80 (18.04)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">65</td></tr>
    <tr><td class="gt_row gt_left">diff_adl_u</td>
<td class="gt_row gt_center">0.00 (0.00)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">65</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="2">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          n (%); Mean (SD)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
```

## By gender



```r
data3_cafu %>%
  select(cg_gender:diff_adl_u) %>%
  tbl_summary(by = cg_gender,
              type = list(contains('diff') ~ "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 2,)
```

```{=html}
<div id="bbzzerrfta" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#bbzzerrfta .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#bbzzerrfta .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bbzzerrfta .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#bbzzerrfta .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#bbzzerrfta .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bbzzerrfta .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bbzzerrfta .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#bbzzerrfta .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#bbzzerrfta .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bbzzerrfta .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bbzzerrfta .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#bbzzerrfta .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#bbzzerrfta .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#bbzzerrfta .gt_from_md > :first-child {
  margin-top: 0;
}

#bbzzerrfta .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bbzzerrfta .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#bbzzerrfta .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#bbzzerrfta .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bbzzerrfta .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#bbzzerrfta .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bbzzerrfta .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bbzzerrfta .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bbzzerrfta .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bbzzerrfta .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bbzzerrfta .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#bbzzerrfta .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bbzzerrfta .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#bbzzerrfta .gt_left {
  text-align: left;
}

#bbzzerrfta .gt_center {
  text-align: center;
}

#bbzzerrfta .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bbzzerrfta .gt_font_normal {
  font-weight: normal;
}

#bbzzerrfta .gt_font_bold {
  font-weight: bold;
}

#bbzzerrfta .gt_font_italic {
  font-style: italic;
}

#bbzzerrfta .gt_super {
  font-size: 65%;
}

#bbzzerrfta .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Female</strong>, N = 52<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Male</strong>, N = 27<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">cafu_adl_u_meanscore</td>
<td class="gt_row gt_center">0.83 (0.78)</td>
<td class="gt_row gt_center">1.11 (1.23)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">6</td>
<td class="gt_row gt_center">3</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_d_score_baseline_1week_arm_1</td>
<td class="gt_row gt_center">41.73 (13.15)</td>
<td class="gt_row gt_center">46.40 (11.50)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">19</td>
<td class="gt_row gt_center">7</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_d_score_1month_arm_1</td>
<td class="gt_row gt_center">39.36 (15.50)</td>
<td class="gt_row gt_center">39.36 (15.11)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">24</td>
<td class="gt_row gt_center">16</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_score_baseline_1week_arm_1</td>
<td class="gt_row gt_center">4.27 (6.19)</td>
<td class="gt_row gt_center">6.85 (9.64)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">19</td>
<td class="gt_row gt_center">7</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_score_1month_arm_1</td>
<td class="gt_row gt_center">3.75 (4.94)</td>
<td class="gt_row gt_center">4.36 (6.90)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">24</td>
<td class="gt_row gt_center">16</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_meanscore_baseline_1week_arm_1</td>
<td class="gt_row gt_center">0.64 (0.81)</td>
<td class="gt_row gt_center">0.96 (1.19)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">19</td>
<td class="gt_row gt_center">7</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_meanscore_1month_arm_1</td>
<td class="gt_row gt_center">0.61 (0.64)</td>
<td class="gt_row gt_center">0.70 (0.95)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">25</td>
<td class="gt_row gt_center">17</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_d_score_baseline_1week_arm_1</td>
<td class="gt_row gt_center">34.18 (15.59)</td>
<td class="gt_row gt_center">39.50 (12.60)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">19</td>
<td class="gt_row gt_center">7</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_d_score_1month_arm_1</td>
<td class="gt_row gt_center">29.96 (16.00)</td>
<td class="gt_row gt_center">34.18 (14.14)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">24</td>
<td class="gt_row gt_center">16</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_score_baseline_1week_arm_1</td>
<td class="gt_row gt_center">4.61 (5.51)</td>
<td class="gt_row gt_center">6.40 (9.28)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">19</td>
<td class="gt_row gt_center">7</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_score_1month_arm_1</td>
<td class="gt_row gt_center">3.57 (5.00)</td>
<td class="gt_row gt_center">4.45 (5.63)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">24</td>
<td class="gt_row gt_center">16</td></tr>
    <tr><td class="gt_row gt_left">diff_iadl_d</td>
<td class="gt_row gt_center">-1.00 (4.15)</td>
<td class="gt_row gt_center">-1.80 (6.18)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">43</td>
<td class="gt_row gt_center">22</td></tr>
    <tr><td class="gt_row gt_left">diff_iadl_d_pct</td>
<td class="gt_row gt_center">-6.98 (18.15)</td>
<td class="gt_row gt_center">-11.35 (26.41)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">43</td>
<td class="gt_row gt_center">22</td></tr>
    <tr><td class="gt_row gt_left">diff_iadl_u</td>
<td class="gt_row gt_center">0.56 (1.81)</td>
<td class="gt_row gt_center">-0.60 (1.34)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">43</td>
<td class="gt_row gt_center">22</td></tr>
    <tr><td class="gt_row gt_left">diff_iadl_u_pct</td>
<td class="gt_row gt_center">Inf (NA)</td>
<td class="gt_row gt_center">-11.11 (101.84)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">46</td>
<td class="gt_row gt_center">24</td></tr>
    <tr><td class="gt_row gt_left">diff_adl_d</td>
<td class="gt_row gt_center">-1.00 (2.00)</td>
<td class="gt_row gt_center">1.20 (6.14)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">43</td>
<td class="gt_row gt_center">22</td></tr>
    <tr><td class="gt_row gt_left">diff_adl_d_pct</td>
<td class="gt_row gt_center">-4.52 (7.10)</td>
<td class="gt_row gt_center">5.89 (29.49)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">43</td>
<td class="gt_row gt_center">22</td></tr>
    <tr><td class="gt_row gt_left">diff_adl_u</td>
<td class="gt_row gt_center">0.00 (0.00)</td>
<td class="gt_row gt_center">0.00 (0.00)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">43</td>
<td class="gt_row gt_center">22</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="3">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Mean (SD)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
```




