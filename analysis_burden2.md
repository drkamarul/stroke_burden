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
  select(pt_gender, pt_age, cg_gender) %>%
  tbl_summary()
```

```{=html}
<div id="uyefvtdvvb" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uyefvtdvvb .gt_table {
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

#uyefvtdvvb .gt_heading {
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

#uyefvtdvvb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uyefvtdvvb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uyefvtdvvb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uyefvtdvvb .gt_col_headings {
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

#uyefvtdvvb .gt_col_heading {
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

#uyefvtdvvb .gt_column_spanner_outer {
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

#uyefvtdvvb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uyefvtdvvb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uyefvtdvvb .gt_column_spanner {
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

#uyefvtdvvb .gt_group_heading {
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

#uyefvtdvvb .gt_empty_group_heading {
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

#uyefvtdvvb .gt_from_md > :first-child {
  margin-top: 0;
}

#uyefvtdvvb .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uyefvtdvvb .gt_row {
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

#uyefvtdvvb .gt_stub {
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

#uyefvtdvvb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uyefvtdvvb .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#uyefvtdvvb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uyefvtdvvb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uyefvtdvvb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uyefvtdvvb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uyefvtdvvb .gt_footnotes {
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

#uyefvtdvvb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#uyefvtdvvb .gt_sourcenotes {
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

#uyefvtdvvb .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#uyefvtdvvb .gt_left {
  text-align: left;
}

#uyefvtdvvb .gt_center {
  text-align: center;
}

#uyefvtdvvb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uyefvtdvvb .gt_font_normal {
  font-weight: normal;
}

#uyefvtdvvb .gt_font_bold {
  font-weight: bold;
}

#uyefvtdvvb .gt_font_italic {
  font-style: italic;
}

#uyefvtdvvb .gt_super {
  font-size: 65%;
}

#uyefvtdvvb .gt_footnote_marks {
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
    <tr><td class="gt_row gt_left">pt_gender</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
<td class="gt_row gt_center">38 (40%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
<td class="gt_row gt_center">56 (60%)</td></tr>
    <tr><td class="gt_row gt_left">pt_age</td>
<td class="gt_row gt_center">63 (57, 68)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">1</td></tr>
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
           
          n (%); Median (IQR)
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
         pt_gender, pt_age, 
         cg_gender, 
         starts_with('zbi'), starts_with('cafu')) %>%
  tbl_summary(by = 'redcap_event_name',
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = all_continuous() ~ 2,)
```

```{=html}
<div id="mkxfxorbdh" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#mkxfxorbdh .gt_table {
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

#mkxfxorbdh .gt_heading {
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

#mkxfxorbdh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#mkxfxorbdh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#mkxfxorbdh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mkxfxorbdh .gt_col_headings {
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

#mkxfxorbdh .gt_col_heading {
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

#mkxfxorbdh .gt_column_spanner_outer {
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

#mkxfxorbdh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mkxfxorbdh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mkxfxorbdh .gt_column_spanner {
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

#mkxfxorbdh .gt_group_heading {
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

#mkxfxorbdh .gt_empty_group_heading {
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

#mkxfxorbdh .gt_from_md > :first-child {
  margin-top: 0;
}

#mkxfxorbdh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mkxfxorbdh .gt_row {
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

#mkxfxorbdh .gt_stub {
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

#mkxfxorbdh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mkxfxorbdh .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#mkxfxorbdh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mkxfxorbdh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mkxfxorbdh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mkxfxorbdh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mkxfxorbdh .gt_footnotes {
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

#mkxfxorbdh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#mkxfxorbdh .gt_sourcenotes {
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

#mkxfxorbdh .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#mkxfxorbdh .gt_left {
  text-align: left;
}

#mkxfxorbdh .gt_center {
  text-align: center;
}

#mkxfxorbdh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mkxfxorbdh .gt_font_normal {
  font-weight: normal;
}

#mkxfxorbdh .gt_font_bold {
  font-weight: bold;
}

#mkxfxorbdh .gt_font_italic {
  font-style: italic;
}

#mkxfxorbdh .gt_super {
  font-size: 65%;
}

#mkxfxorbdh .gt_footnote_marks {
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
    <tr><td class="gt_row gt_left">pt_gender</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
<td class="gt_row gt_center">12 (30%)</td>
<td class="gt_row gt_center">0 (NA%)</td>
<td class="gt_row gt_center">17 (32%)</td>
<td class="gt_row gt_center">0 (NA%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
<td class="gt_row gt_center">28 (70%)</td>
<td class="gt_row gt_center">0 (NA%)</td>
<td class="gt_row gt_center">36 (68%)</td>
<td class="gt_row gt_center">0 (NA%)</td></tr>
    <tr><td class="gt_row gt_left">pt_age</td>
<td class="gt_row gt_center">62.50 (11.40)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">63.23 (12.13)</td>
<td class="gt_row gt_center">NA (NA)</td></tr>
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



# At baseline 1 week by gender


```r
data3 %>%
  select(redcap_event_name, 
         pt_gender, pt_age, 
         cg_gender, 
         starts_with('zbi'), starts_with('cafu')) %>%
  filter(redcap_event_name == 'baseline_1week_arm_1') %>%
  tbl_summary(by = 'cg_gender',
              statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 2,)
```

```{=html}
<div id="wroepwaoqn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#wroepwaoqn .gt_table {
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

#wroepwaoqn .gt_heading {
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

#wroepwaoqn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wroepwaoqn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wroepwaoqn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wroepwaoqn .gt_col_headings {
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

#wroepwaoqn .gt_col_heading {
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

#wroepwaoqn .gt_column_spanner_outer {
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

#wroepwaoqn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wroepwaoqn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wroepwaoqn .gt_column_spanner {
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

#wroepwaoqn .gt_group_heading {
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

#wroepwaoqn .gt_empty_group_heading {
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

#wroepwaoqn .gt_from_md > :first-child {
  margin-top: 0;
}

#wroepwaoqn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wroepwaoqn .gt_row {
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

#wroepwaoqn .gt_stub {
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

#wroepwaoqn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wroepwaoqn .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#wroepwaoqn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wroepwaoqn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wroepwaoqn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wroepwaoqn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wroepwaoqn .gt_footnotes {
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

#wroepwaoqn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#wroepwaoqn .gt_sourcenotes {
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

#wroepwaoqn .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#wroepwaoqn .gt_left {
  text-align: left;
}

#wroepwaoqn .gt_center {
  text-align: center;
}

#wroepwaoqn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wroepwaoqn .gt_font_normal {
  font-weight: normal;
}

#wroepwaoqn .gt_font_bold {
  font-weight: bold;
}

#wroepwaoqn .gt_font_italic {
  font-style: italic;
}

#wroepwaoqn .gt_super {
  font-size: 65%;
}

#wroepwaoqn .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Female</strong>, N = 33<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Male</strong>, N = 20<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">redcap_event_name</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">1month_arm_1</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">2month_arm_1</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">baseline_1week_arm_1</td>
<td class="gt_row gt_center">33 (100%)</td>
<td class="gt_row gt_center">20 (100%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">pre_discharge_arm_1</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left">pt_gender</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
<td class="gt_row gt_center">8 (24%)</td>
<td class="gt_row gt_center">9 (45%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
<td class="gt_row gt_center">25 (76%)</td>
<td class="gt_row gt_center">11 (55%)</td></tr>
    <tr><td class="gt_row gt_left">pt_age</td>
<td class="gt_row gt_center">62.01 (13.46)</td>
<td class="gt_row gt_center">65.24 (9.54)</td></tr>
    <tr><td class="gt_row gt_left">zbi_total_score</td>
<td class="gt_row gt_center">23.70 (15.35)</td>
<td class="gt_row gt_center">23.80 (13.21)</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_d_score</td>
<td class="gt_row gt_center">41.73 (13.15)</td>
<td class="gt_row gt_center">46.40 (11.50)</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_score</td>
<td class="gt_row gt_center">4.27 (6.19)</td>
<td class="gt_row gt_center">6.85 (9.64)</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_meanscore</td>
<td class="gt_row gt_center">0.64 (0.81)</td>
<td class="gt_row gt_center">0.96 (1.19)</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_d_score</td>
<td class="gt_row gt_center">34.18 (15.59)</td>
<td class="gt_row gt_center">39.50 (12.60)</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_score</td>
<td class="gt_row gt_center">4.61 (5.51)</td>
<td class="gt_row gt_center">6.40 (9.28)</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_meanscore</td>
<td class="gt_row gt_center">0.86 (0.78)</td>
<td class="gt_row gt_center">1.08 (1.34)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">5</td>
<td class="gt_row gt_center">2</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="3">
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


# At 1 month by gender


```r
data3 %>%
  select(redcap_event_name, 
         pt_gender, pt_age, 
         cg_gender, 
         starts_with('zbi'), starts_with('cafu')) %>%
  filter(redcap_event_name == '1month_arm_1') %>%
  tbl_summary(by = 'cg_gender',
              statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 2,)
```

```{=html}
<div id="dlixuxddee" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#dlixuxddee .gt_table {
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

#dlixuxddee .gt_heading {
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

#dlixuxddee .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dlixuxddee .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dlixuxddee .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dlixuxddee .gt_col_headings {
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

#dlixuxddee .gt_col_heading {
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

#dlixuxddee .gt_column_spanner_outer {
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

#dlixuxddee .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dlixuxddee .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dlixuxddee .gt_column_spanner {
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

#dlixuxddee .gt_group_heading {
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

#dlixuxddee .gt_empty_group_heading {
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

#dlixuxddee .gt_from_md > :first-child {
  margin-top: 0;
}

#dlixuxddee .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dlixuxddee .gt_row {
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

#dlixuxddee .gt_stub {
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

#dlixuxddee .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dlixuxddee .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#dlixuxddee .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dlixuxddee .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dlixuxddee .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dlixuxddee .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dlixuxddee .gt_footnotes {
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

#dlixuxddee .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#dlixuxddee .gt_sourcenotes {
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

#dlixuxddee .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#dlixuxddee .gt_left {
  text-align: left;
}

#dlixuxddee .gt_center {
  text-align: center;
}

#dlixuxddee .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dlixuxddee .gt_font_normal {
  font-weight: normal;
}

#dlixuxddee .gt_font_bold {
  font-weight: bold;
}

#dlixuxddee .gt_font_italic {
  font-style: italic;
}

#dlixuxddee .gt_super {
  font-size: 65%;
}

#dlixuxddee .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Female</strong>, N = 28<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Male</strong>, N = 12<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">redcap_event_name</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">1month_arm_1</td>
<td class="gt_row gt_center">28 (100%)</td>
<td class="gt_row gt_center">12 (100%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">2month_arm_1</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">baseline_1week_arm_1</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">pre_discharge_arm_1</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left">pt_gender</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
<td class="gt_row gt_center">7 (25%)</td>
<td class="gt_row gt_center">5 (42%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
<td class="gt_row gt_center">21 (75%)</td>
<td class="gt_row gt_center">7 (58%)</td></tr>
    <tr><td class="gt_row gt_left">pt_age</td>
<td class="gt_row gt_center">62.17 (12.57)</td>
<td class="gt_row gt_center">63.28 (8.51)</td></tr>
    <tr><td class="gt_row gt_left">zbi_total_score</td>
<td class="gt_row gt_center">20.79 (13.14)</td>
<td class="gt_row gt_center">17.92 (11.78)</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_d_score</td>
<td class="gt_row gt_center">39.36 (15.50)</td>
<td class="gt_row gt_center">39.36 (15.11)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">1</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_score</td>
<td class="gt_row gt_center">3.75 (4.94)</td>
<td class="gt_row gt_center">4.36 (6.90)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">1</td></tr>
    <tr><td class="gt_row gt_left">cafu_iadl_u_meanscore</td>
<td class="gt_row gt_center">0.61 (0.64)</td>
<td class="gt_row gt_center">0.70 (0.95)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">2</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_d_score</td>
<td class="gt_row gt_center">29.96 (16.00)</td>
<td class="gt_row gt_center">34.18 (14.14)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">1</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_score</td>
<td class="gt_row gt_center">3.57 (5.00)</td>
<td class="gt_row gt_center">4.45 (5.63)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">1</td></tr>
    <tr><td class="gt_row gt_left">cafu_adl_u_meanscore</td>
<td class="gt_row gt_center">0.70 (0.75)</td>
<td class="gt_row gt_center">0.90 (0.89)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">6</td>
<td class="gt_row gt_center">3</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="3">
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




```r
data3_zbi %>%
  select(cg_gender, baseline_1week_arm_1:percent_diff) %>%
  tbl_summary(by = cg_gender,
              statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 2,)
```

```{=html}
<div id="fwejsogueo" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#fwejsogueo .gt_table {
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

#fwejsogueo .gt_heading {
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

#fwejsogueo .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fwejsogueo .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fwejsogueo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fwejsogueo .gt_col_headings {
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

#fwejsogueo .gt_col_heading {
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

#fwejsogueo .gt_column_spanner_outer {
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

#fwejsogueo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fwejsogueo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fwejsogueo .gt_column_spanner {
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

#fwejsogueo .gt_group_heading {
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

#fwejsogueo .gt_empty_group_heading {
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

#fwejsogueo .gt_from_md > :first-child {
  margin-top: 0;
}

#fwejsogueo .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fwejsogueo .gt_row {
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

#fwejsogueo .gt_stub {
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

#fwejsogueo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fwejsogueo .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#fwejsogueo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fwejsogueo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fwejsogueo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fwejsogueo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fwejsogueo .gt_footnotes {
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

#fwejsogueo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#fwejsogueo .gt_sourcenotes {
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

#fwejsogueo .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#fwejsogueo .gt_left {
  text-align: left;
}

#fwejsogueo .gt_center {
  text-align: center;
}

#fwejsogueo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fwejsogueo .gt_font_normal {
  font-weight: normal;
}

#fwejsogueo .gt_font_bold {
  font-weight: bold;
}

#fwejsogueo .gt_font_italic {
  font-style: italic;
}

#fwejsogueo .gt_super {
  font-size: 65%;
}

#fwejsogueo .gt_footnote_marks {
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
         diff_iadl_u = 
           cafu_iadl_u_score_1month_arm_1 - cafu_iadl_u_score_baseline_1week_arm_1,
         diff_iadl_u_mean =  
           cafu_iadl_u_meanscore_1month_arm_1 - cafu_iadl_u_meanscore_baseline_1week_arm_1,          diff_adl_d = 
           cafu_adl_d_score_1month_arm_1 - cafu_adl_d_score_baseline_1week_arm_1, 
         diff_adl_u = 
           cafu_adl_u_score_1month_arm_1 - cafu_adl_u_score_baseline_1week_arm_1)

# data3_cafu %>%
#  datatable()
```



```r
data3_cafu %>%
  select(cg_gender:diff_adl_u) %>%
  tbl_summary(by = cg_gender,
              statistic = list(all_continuous() ~ "{mean} ({sd})"), 
              digits = all_continuous() ~ 2,)
```

```{=html}
<div id="uevelsedot" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uevelsedot .gt_table {
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

#uevelsedot .gt_heading {
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

#uevelsedot .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uevelsedot .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uevelsedot .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uevelsedot .gt_col_headings {
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

#uevelsedot .gt_col_heading {
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

#uevelsedot .gt_column_spanner_outer {
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

#uevelsedot .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uevelsedot .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uevelsedot .gt_column_spanner {
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

#uevelsedot .gt_group_heading {
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

#uevelsedot .gt_empty_group_heading {
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

#uevelsedot .gt_from_md > :first-child {
  margin-top: 0;
}

#uevelsedot .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uevelsedot .gt_row {
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

#uevelsedot .gt_stub {
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

#uevelsedot .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uevelsedot .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#uevelsedot .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uevelsedot .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uevelsedot .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uevelsedot .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uevelsedot .gt_footnotes {
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

#uevelsedot .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#uevelsedot .gt_sourcenotes {
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

#uevelsedot .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#uevelsedot .gt_left {
  text-align: left;
}

#uevelsedot .gt_center {
  text-align: center;
}

#uevelsedot .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uevelsedot .gt_font_normal {
  font-weight: normal;
}

#uevelsedot .gt_font_bold {
  font-weight: bold;
}

#uevelsedot .gt_font_italic {
  font-style: italic;
}

#uevelsedot .gt_super {
  font-size: 65%;
}

#uevelsedot .gt_footnote_marks {
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
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-9</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">1 (20%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-8</td>
<td class="gt_row gt_center">1 (11%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-5</td>
<td class="gt_row gt_center">1 (11%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-4</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">1 (20%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-3</td>
<td class="gt_row gt_center">1 (11%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-2</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">2 (40%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">0</td>
<td class="gt_row gt_center">5 (56%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">7</td>
<td class="gt_row gt_center">1 (11%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">8</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">1 (20%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">43</td>
<td class="gt_row gt_center">22</td></tr>
    <tr><td class="gt_row gt_left">diff_iadl_u</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-2</td>
<td class="gt_row gt_center">1 (11%)</td>
<td class="gt_row gt_center">2 (40%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">0</td>
<td class="gt_row gt_center">6 (67%)</td>
<td class="gt_row gt_center">2 (40%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">1</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">1 (20%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">3</td>
<td class="gt_row gt_center">1 (11%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">4</td>
<td class="gt_row gt_center">1 (11%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">43</td>
<td class="gt_row gt_center">22</td></tr>
    <tr><td class="gt_row gt_left">diff_iadl_u_mean</td>
<td class="gt_row gt_center">0.14 (0.31)</td>
<td class="gt_row gt_center">-0.11 (0.24)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">44</td>
<td class="gt_row gt_center">23</td></tr>
    <tr><td class="gt_row gt_left">diff_adl_d</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-6</td>
<td class="gt_row gt_center">1 (11%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-3</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">1 (20%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-2</td>
<td class="gt_row gt_center">1 (11%)</td>
<td class="gt_row gt_center">1 (20%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-1</td>
<td class="gt_row gt_center">1 (11%)</td>
<td class="gt_row gt_center">1 (20%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">0</td>
<td class="gt_row gt_center">6 (67%)</td>
<td class="gt_row gt_center">1 (20%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">12</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">1 (20%)</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
<td class="gt_row gt_center">43</td>
<td class="gt_row gt_center">22</td></tr>
    <tr><td class="gt_row gt_left">diff_adl_u</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td></tr>
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
           
          Mean (SD); n (%)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
```

# Below - Not necessary


```r
res_by_fu2 <- 
  data3 %>%
  select(redcap_event_name, 
         pt_gender, pt_age, 
         cg_gender, 
         starts_with('zbi'), starts_with('cafu')) %>%
  group_by(redcap_event_name) %>%
  nest() %>%
  mutate(descri = map(.x = data, 
                      ~ tbl_summary(data = .x, by = 'cg_gender', 
                                    statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                                    digits = all_continuous() ~ 2,) %>%
                        add_p() %>%
                        as_hux_table()
                      )) %>%
  ungroup(descri)
```

```
## Warning for variable 'pt_age':
##  simpleWarning in wilcox.test.default(x = c(62.8, 69.6, 67.7, 36.8, 61.2, 62.6, : cannot compute exact p-value with ties
```

```
## Warning for variable 'zbi_total_score':
##  simpleWarning in wilcox.test.default(x = c(19, 4, 4, 37, 23, 5, 2, 1, 24, 17, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_iadl_d_score':
##  simpleWarning in wilcox.test.default(x = c(51, 31, 18, 54, 56, 47, 12, 8, 56, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_iadl_u_score':
##  simpleWarning in wilcox.test.default(x = c(3, 4, 0, 3, 1, 1, 0, 0, 1, 0, 3, 14, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_iadl_u_meanscore':
##  simpleWarning in wilcox.test.default(x = c(0.75, 0.67, 0, 0.5, 0.17, 0.25, 0, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_adl_d_score':
##  simpleWarning in wilcox.test.default(x = c(42, 9, 7, 43, 49, 11, 7, 7, 42, 42, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_adl_u_score':
##  simpleWarning in wilcox.test.default(x = c(4, 0, 0, 6, 6, 0, 0, 0, 1, 0, 0, 7, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_adl_u_meanscore':
##  simpleWarning in wilcox.test.default(x = c(0.57, 0.86, 0.86, 0.17, 0, 1, 1.57, : cannot compute exact p-value with ties
```

```
## Warning for variable 'pt_age':
##  simpleWarning in wilcox.test.default(x = c(62.8, 69.6, 30.9, 67.7, 36.8, 61.2, : cannot compute exact p-value with ties
```

```
## Warning for variable 'zbi_total_score':
##  simpleWarning in wilcox.test.default(x = c(22, 4, 55, 5, 41, 30, 10, 7, 3, 17, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_iadl_d_score':
##  simpleWarning in wilcox.test.default(x = c(51, 31, 47, 21, 55, 56, 36, 12, 16, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_iadl_u_score':
##  simpleWarning in wilcox.test.default(x = c(3, 0, 7, 0, 3, 0, 0, 0, 0, 0, 1, 5, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_iadl_u_meanscore':
##  simpleWarning in wilcox.test.default(x = c(0.5, 0, 1, 0, 0.5, 0, 0, 0, 0, 0, 0.14, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_adl_d_score':
##  simpleWarning in wilcox.test.default(x = c(48, 10, 49, 7, 43, 49, 9, 7, 7, 46, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_adl_u_score':
##  simpleWarning in wilcox.test.default(x = c(4, 0, 5, 0, 9, 7, 0, 0, 0, 0, 1, 0, : cannot compute exact p-value with ties
```

```
## Warning for variable 'cafu_adl_u_meanscore':
##  simpleWarning in wilcox.test.default(x = c(0.57, 0.71, 1.29, 1, 0, 0, 0.5, 0.5, : cannot compute exact p-value with ties
```

```r
res_by_fu2$descri
```

```
## [[1]]
##           Characteristic     Female, N = 33   Male, N = 20    p-value  
##         ---------------------------------------------------------------
##           pt_gender                                            0.12    
##           Female                8 (24%)          9 (45%)               
##           Male                  25 (76%)        11 (55%)               
##           pt_age             62.01 (13.46)    65.24 (9.54)      0.5    
##           zbi_total_score    23.70 (15.35)    23.80 (13.21)     0.7    
##           cafu_iadl_d_scor   41.73 (13.15)    46.40 (11.50)     0.2    
##           e                                                            
##           cafu_iadl_u_scor    4.27 (6.19)      6.85 (9.64)      0.5    
##           e                                                            
##           cafu_iadl_u_mean    0.64 (0.81)      0.96 (1.19)      0.5    
##           score                                                        
##           cafu_adl_d_score   34.18 (15.59)    39.50 (12.60)     0.4    
##           cafu_adl_u_score    4.61 (5.51)      6.40 (9.28)      0.9    
##           cafu_adl_u_means    0.86 (0.78)      1.08 (1.34)      0.7    
##           core                                                         
##           Unknown                  5                2                  
##         ---------------------------------------------------------------
##           n (%); Mean (SD)                                             
##           Pearson's Chi-squared test; Wilcoxon rank sum test           
## 
## Column names: label, stat_1, stat_2, p.value
## 
## [[2]]
##           Characteristic     Female, N = 28   Male, N = 12    p-value  
##         ---------------------------------------------------------------
##           pt_gender                                             0.5    
##           Female                7 (25%)          5 (42%)               
##           Male                  21 (75%)         7 (58%)               
##           pt_age             62.17 (12.57)    63.28 (8.51)     >0.9    
##           zbi_total_score    20.79 (13.14)    17.92 (11.78)     0.5    
##           cafu_iadl_d_scor   39.36 (15.50)    39.36 (15.11)     0.9    
##           e                                                            
##           Unknown                  0                1                  
##           cafu_iadl_u_scor    3.75 (4.94)      4.36 (6.90)      0.7    
##           e                                                            
##           Unknown                  0                1                  
##           cafu_iadl_u_mean    0.61 (0.64)      0.70 (0.95)      0.7    
##           score                                                        
##           Unknown                  1                2                  
##           cafu_adl_d_score   29.96 (16.00)    34.18 (14.14)     0.7    
##           Unknown                  0                1                  
##           cafu_adl_u_score    3.57 (5.00)      4.45 (5.63)      0.7    
##           Unknown                  0                1                  
##           cafu_adl_u_means    0.70 (0.75)      0.90 (0.89)      0.6    
##           core                                                         
##           Unknown                  6                3                  
##         ---------------------------------------------------------------
##           n (%); Mean (SD)                                             
##           Fisher's exact test; Wilcoxon rank sum test                  
## 
## Column names: label, stat_1, stat_2, p.value
```


