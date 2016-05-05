r_squared <- tryCatch({
  
  broom::glance(fit)$r.squared
  
}, error = function(e) {
  
  NA
  
})

