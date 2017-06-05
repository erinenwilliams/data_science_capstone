dropouts <- left_join(dropouts_May_2017, Drop_out_analysis2, by = "id")
demo_drop <- left_join(dropouts, demographics, by = "id")
