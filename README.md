# **threadr**

[![Build Status](https://travis-ci.org/skgrange/threadr.svg?branch=master)](https://travis-ci.org/skgrange/threadr)

**threadr** is a collection of R utility functions to link pieces of analysis together. **threadr** is not a focused, single-unit package, rather a collection of functions which I use to glue many processes together.

## Installation

```
# Development version
devtools::install_github("skgrange/threadr")
```

## To-do

  1. Do some more unit testing
  2. Work on documentation
  3. Get package on CRAN
  
## Some infomation

  - Utilities:
    - Write JSON files with `write_json`. 
    - FTP/SFTP functions. 
      - `list_files_ftp`, `download_ftp_file`, and `upload_to_ftp`.
    - `str_*` functions. Do things with strings which **stringr** does not. 
      - `str_proper_case`, `str_trim_length`, `str_sentence_case`, `str_trim_many_spaces`, `str_underscore`, `str_chop`, `str_drop_xml_tags`, `str_rm_brackets_and_contents`, `str_extract_digits`, `str_sql_quote`, `str_unique`, `str_nth_character`.
    - A number of unit conversion functions.
      - `miles_to_km`, `knots_to_km_h`, `kw_to_hp`, `fahrenheit_to_celsius`, `psi_to_bar`, `newton_metre_to_foot_pound`, `mpg_to_l_100_km`, `mpg_to_km_l`. 
    - Test if a vector is within one or many ranges `within_range`.
    - Enhanced downloading functions (`download_file` and `download_to_temporary`). 
    - Create UUIDs with `uuid`. 

  - Date functions
    - Pad time-series to different intervals with `time_pad`. 
    - Make an irregular time-series regular with `time_pad_irregular`. 
    - Round dates to arbitrary time intervals such as 5-seconds, 5-minutes, 15-minutes, 2-minutes, 30-minutes, etc. with `round_date_interval`.
    - Get Monday-based weeks of the year with `week_monday`. 
    - Get time-zone from date vector with `time_zone`.
    - Parse numerical date formats easily with `parse_unix_time` and `parse_excel_date`.
    
  - Data frame functions: 
    - `add_row_numbers`. Very similar to `dplyr::add_rownames` but the variable is an integer, not a character so it can be arranged, joined, and plotted easier. 
    - `arrange_left`. Move variables/columns to the left of a data frame. 
    - `base_df`. Remove **dplyr**'s `data.frame` extension (`tbl_df` and others) from a data frame. I have encountered issues with some functions when piping data frames within the **dplyr**'s grammar; especially those interacting with SQL databases and using data frame indexing. 
    - `drop_na_columns` and `rm_na_rows`.
    - `grepl_all`
    
  - Wrappers for Microsoft Excel reading functions (from **readxl**)
    - `excel_read`, `excel_sheets`, and `excel_read_all`.

  - **threadr** originally contained many database functions. These function were pulled from the package from version 0.4.0 onwards and can now be found in [**databaser**](https://github.com/skgrange/databaser).  

