# benchmark <- microbenchmark(
#   
#   do = time_pad(data_uk_sampled, by = c("site", "country", "site_type"),
#                 interval = "min"),
#   
#   do_merge = time_pad(data_uk_sampled, by = c("site", "country", "site_type"), 
#                       merge = TRUE, interval = "min"),
#   
#   plyr = time_pad(data_uk_sampled, by = c("site", "country", "site_type"), 
#                   do = FALSE, interval = "min"),
#   
#   plyr_merge = time_pad(data_uk_sampled, by = c("site", "country", "site_type"), 
#                         merge = TRUE, do = FALSE, interval = "min"),
#   
#   times = 10
# )
# 
# autoplot(benchmark)