# Fill scale for prevalences

# DOUBLE BURDEN
fill_scale_double_burden <- scale_fill_manual(values = c("prev_bmi_30" = "#EB0358", "prev_bmi_l185" = "#00CED1"), 
                                              labels = c("prev_bmi_30" = "Obesity","prev_bmi_l185" = "Underweight"))

col_scale_double_burden <- scale_colour_manual(values = c("prev_bmi_30" = "#EB0358", "prev_bmi_l185" = "#00CED1"), 
                                                labels = c("prev_bmi_30" = "Obesity","prev_bmi_l185" = "Underweight"))

fill_scale_double_burden_ado <- scale_fill_manual(values = c("prev_bmi_2sd" = "#EB0358", "prev_bmi_neg2sd" = "#00CED1"), 
                                              labels = c("prev_bmi_2sd" = "Obesity","prev_bmi_neg2sd" = "Thinness"))

col_scale_double_burden_ado <-scale_colour_manual(values = c("prev_bmi_2sd" = "#EB0358", "prev_bmi_neg2sd" = "#00CED1"), 
                                              labels = c("prev_bmi_2sd" = "Obesity","prev_bmi_neg2sd" = "Thinness"))

fill_scale_proportion_double_burden <- scale_fill_gradient2(high = "#e3160b", mid = "white", low = "#619edb", 
                                                            midpoint = 50,
                                                            limits = c(0, 100),
                                                            breaks = c(0, 50, 100),
                                                            labels = c("Underweight\ndominance", "Equal\nprevalence" ,"Obesity\ndominance"),
                                                            name =  "")

fill_scale_proportion_double_burden_ado <- scale_fill_gradient2(high = "#e3160b", mid = "white", low = "#619edb", 
                                                            midpoint = 50,
                                                            limits = c(0, 100),
                                                            breaks = c(0, 50, 100),
                                                            labels = c("Thinness\ndominance", "Equal\nprevalence" ,"Obesity\ndominance"),
                                                            name =  "")

col_scale_proportion_double_burden <- scale_color_gradient2(high = "#e3160b", mid = "white", low = "#619edb", midpoint = 50,limits = c(0, 100),
                                                            name =  "(%)")
fill_col_scale_proportion_double_burden <- scale_fill_gradient2(high = "#e3160b", mid = "white", low = "#619edb", midpoint = 50,limits = c(0, 100),
                                                            name =  "(%)")


## ALL PREVALENCES
# fill_scale_all <- scale_fill_manual(values = c("prev_bmi_l185"   = "#00CED1",
#                                                "prev_bmi_185_20" = "#3288BD",
#                                                "prev_bmi_20_25"  = "#66C2A5",
#                                                "prev_bmi_25_30"  = "#FFFFBF",
#                                                "prev_bmi_30_35"  = "#FDAE61",
#                                                "prev_bmi_35_40"  = "#D53E4F",
#                                                "prev_bmi_40"     = "#9E0142"),
#                                     labels = c("prev_bmi_l185"   = "<18.5",
#                                                "prev_bmi_185_20" = "18.5 to <20",
#                                                "prev_bmi_20_25" = "20 to <25",
#                                                "prev_bmi_25_30" = "25 to <30",
#                                                "prev_bmi_30_35" = "Class I (30 to < 35 kg/m\u00B2)",
#                                                "prev_bmi_35_40" = "Class II (35 \u2264 BMI < 40 kg/m\u00B2)",
#                                                "prev_bmi_40" = "Class III (BMI \u2265 40 kg/m\u00B2)"))

fill_scale_all <- scale_fill_manual(values = c("prev_bmi_l185"   = "#00CED1",
                                               "prev_bmi_185_20" = "#3288BD",
                                               "prev_bmi_20_25"  = "#66C2A5",
                                               "prev_bmi_25_30"  = "#FFFFBF",
                                               "prev_bmi_30_35"  = "#FDAE61",
                                               "prev_bmi_35_40"  = "#D53E4F",
                                               "prev_bmi_40"     = "#9E0142"),
                                    labels = c("prev_bmi_l185"   = "< 18.5",
                                               "prev_bmi_185_20" = "18.5 to < 20",
                                               "prev_bmi_20_25" = "20 to < 25",
                                               "prev_bmi_25_30" = "25 to < 30",
                                               "prev_bmi_30_35" = "30 to < 35",
                                               "prev_bmi_35_40" = "35 to < 40",
                                               "prev_bmi_40" = "\u2265 40"))

fill_scale_obesity_classes <- scale_fill_manual(values = c(
                                               "prev_bmi_30_35"  = "#FDAE61",
                                               "prev_bmi_35_40"  = "#D53E4F",
                                               "prev_bmi_40"     = "#9E0142"),
                                    labels = c(
                                               "prev_bmi_30_35" = "Class I",
                                               "prev_bmi_35_40" = "Class II",
                                               "prev_bmi_40" = "Class III"))

col_scale_all <- scale_colour_manual(values = c("prev_bmi_l185"  = "#00CED1", 
                                               "prev_bmi_185_20" = "#3288BD", 
                                               "prev_bmi_20_25"  = "#66C2A5",
                                               "prev_bmi_25_30"  = "#FFFFBF",
                                               "prev_bmi_30_35"  = "#FDAE61",
                                               "prev_bmi_35_40"  = "#D53E4F",
                                               "prev_bmi_40"     = "#9E0142"), 
                                    labels = c("prev_bmi_l185" = "<18.5",
                                               "prev_bmi_185_20" = "18.5 to <20",
                                               "prev_bmi_20_25" = "20 to <25",
                                               "prev_bmi_25_30" = "25 to <30",
                                               "prev_bmi_30_35" = "30 to <35",
                                               "prev_bmi_35_40" = "35 to <40)",
                                               "prev_bmi_40" = "\u2265 40"))

fill_scale_all_ado <- scale_fill_manual(values = c("prev_bmi_neg2sd"        = "#00CED1", 
                                                   "prev_bmi_neg2sd_neg1sd" = "#3288BD", 
                                                   "prev_bmi_neg1sd_1sd"    = "#66C2A5",
                                                   "prev_bmi_1sd_2sd"       = "#FDAE61",
                                                   "prev_bmi_2sd"           = "#EB0358"), 
                                        labels = c("prev_bmi_neg2sd"        = "< -2 SD", 
                                                   "prev_bmi_neg2sd_neg1sd" = "-2 SD to < -1 SD", 
                                                   "prev_bmi_neg1sd_1sd"    = "-1 SD to 1 SD",
                                                   "prev_bmi_1sd_2sd"       = "> 1 SD to 2 SD",
                                                   "prev_bmi_2sd"           = "> 2SD"))

col_scale_all_ado <- scale_colour_manual(values = c("prev_bmi_neg2sd"        = "#00CED1", 
                                                   "prev_bmi_neg2sd_neg1sd" = "#3288BD", 
                                                   "prev_bmi_neg1sd_1sd"    = "#66C2A5",
                                                   "prev_bmi_1sd_2sd"       = "#FDAE61",
                                                   "prev_bmi_2sd"           = "#EB0358"), 
                                         labels = c("prev_bmi_neg2sd"        = "BMI < -2 SD", 
                                                    "prev_bmi_neg2sd_neg1sd" = "-2 SD \u2264 BMI < -1 SD", 
                                                    "prev_bmi_neg1sd_1sd"    = "-1 SD \u2264 BMI \u2264 1 SD",
                                                    "prev_bmi_1sd_2sd"       = "1 SD < BMI \u2264 2 SD",
                                                    "prev_bmi_2sd"           = "BMI > 2SD"))

fill_scale_obesity <- scale_fill_manual(values = c("prev_bmi_30" = "#C80815", "prev_bmi_35" = "#cd362d", "prev_bmi_40" = "#9E0142"), 
                                        labels = c("prev_bmi_30" = "Total obesity","prev_bmi_35" = "Severe obesity", "prev_bmi_40" = "Morbid obesity"))



col_scale_obesity <- scale_colour_manual(values = c("prev_bmi_30" = "#C80815", "prev_bmi_35" = "#cd362d", "prev_bmi_40" = "#9E0142"), 
                                        labels = c("prev_bmi_30" = "Total obesity","prev_bmi_35" = "Severe obesity", "prev_bmi_40" = "Morbid obesity"))

