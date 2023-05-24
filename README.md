# **threadr** <a href='https://github.com/skgrange/threadr'><img src='man/figures/logo.png' align="right" height="131.5" /></a>

[![Lifecycle Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/)

**threadr** is a collection of R utility functions to link pieces of analysis together. **threadr** is not a focused, single-unit package, rather a collection of functions which I use to glue many processes together.

## Installation

```
# Development version
install.packages("remotes")
remotes::install_github("skgrange/threadr")
```

## Some infomation

  - Utilities:
    - Write JSON files with `write_json`. 
    - FTP/SFTP functions. 
      - `list_files_ftp`, `download_ftp_file`, and `upload_to_ftp`.
    - scp functions
      - `list_files_scp` and `download_with_scp`. 
    - `str_*` functions. Do things with strings which **stringr** does not. 
      - `str_trim_length`, `str_sentence_case`, `str_to_underscore`, `str_chop`, `str_drop_xml_tags`, `str_extract_digits`, `str_sql_quote`, `str_nth_character`.
    - A number of unit conversion functions.
      - `miles_to_km`, `knots_to_km_h`, `kw_to_hp`, `fahrenheit_to_celsius`, `psi_to_bar`, `newton_metre_to_foot_pound`, `mpg_to_l_100_km`, `mpg_to_km_l`. 
    - Test if a vector is within one or many ranges `within_range`.
    - Create UUIDs with `uuid`. 

  - Date functions
    - Pad time-series to different intervals with `time_pad`. 
    - Make an irregular time-series regular with `time_pad_irregular`. 
    - Round dates to arbitrary time intervals such as 5-seconds, 5-minutes, 15-minutes, 2-minutes, 30-minutes, etc. with `round_date_interval`.
    - Get Monday-based weeks of the year with `week_monday`. 
    - Get time-zone from date vector with `time_zone`.
    - Parse numerical date formats easily with `parse_unix_time` and `parse_excel_date`.
    - Aggregate by dates with `aggregate_by_date`. 

  - **threadr** originally contained many database functions. These function were pulled from the package from version 0.4.0 onwards and can now be found in [**databaser**](https://github.com/skgrange/databaser).

