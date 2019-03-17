# what if we don't trim scales?

# study 1a -----
temp_scales_d1a_ad <- scale_fun(efa = efa_wdm_d1a_ad, factor_names = factor_names_efa_wdm_d1a_ad, trim = F)

temp_scores_d1a_ad <- score_fun(df = d1a_ad, scales = temp_scales_d1a_ad)
temp_diffscores_d1a_ad <- diff_fun(temp_scores_d1a_ad)

relviz_fun(d_scored = temp_scores_d1a_ad, colors = colors02)
diffplot_fun(df_diff = temp_diffscores_d1a_ad, colors = colors02)

# study 1b -----
temp_scales_d1b_ad <- scale_fun(efa = efa_wdm_d1b_ad, factor_names = factor_names_efa_wdm_d1b_ad, trim = F)

temp_scores_d1b_ad <- score_fun(df = d1b_ad, scales = temp_scales_d1b_ad)
temp_diffscores_d1b_ad <- diff_fun(temp_scores_d1b_ad)

relviz_fun(d_scored = temp_scores_d1b_ad, colors = colors02)
diffplot_fun(df_diff = temp_diffscores_d1b_ad, colors = colors02)

# study 1c -----
temp_scales_d1c_ad <- scale_fun(efa = efa_wdm_d1c_ad, factor_names = factor_names_efa_wdm_d1c_ad, trim = F)

temp_scores_d1c_ad <- score_fun(df = d1c_ad, scales = temp_scales_d1c_ad)
temp_diffscores_d1c_ad <- diff_fun(temp_scores_d1c_ad)

relviz_fun(d_scored = temp_scores_d1c_ad, colors = colors02)
diffplot_fun(df_diff = temp_diffscores_d1c_ad, colors = colors02)

# study 1d -----
temp_scales_d1d_ad <- scale_fun(efa = efa_wdm_d1d_ad, factor_names = factor_names_efa_wdm_d1d_ad, trim = F)

temp_scores_d1d_ad <- score_fun(df = d1d_ad, scales = temp_scales_d1d_ad)
temp_diffscores_d1d_ad <- diff_fun(temp_scores_d1d_ad)

relviz_fun(d_scored = temp_scores_d1d_ad, colors = colors21)
diffplot_fun(df_diff = temp_diffscores_d1d_ad, colors = colors21)
