library(shiny)
library(bslib)
library(readxl)
library(showtext)
library(zip)
library(ggplot2)
library(nlme)

# システムの zip コマンドおよび bsdtar の使用を無効化し、zip パッケージを使わせる
options("openxlsx2.no_utils_zip" = TRUE)
options("openxlsx2.no_bsdtar" = TRUE)
options("openxlsx2.no_maybe_zip" = TRUE)

font_paths("./")
font_add("noto", "NotoSansJP-VariableFont_wght.ttf")
showtext_auto()

downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

data_pk <- read_excel("./example_excel_file.xlsx", sheet = "PK")
data_time <- read_excel("./example_excel_file.xlsx", sheet = "time")

# 読み込んだデータからPKパラメータを計算する関数
pkParam <- function(pk, time){
  if(sum(pk$subject != time$subject) > 0 | sum(pk$period != time$period) > 0 | sum(pk$treatment != time$treatment) > 0){return(NULL)}
  
  # pkと時間をデータフレームに成形
  pk_d <- data.frame(
    s_t = paste(pk$subject, pk$treatment),
    subject = pk$subject,
    period = pk$period,
    treatment = pk$treatment,
    group = pk$subject |> substr(1, 1),
    value = pk[, 4:ncol(pk)] |> unlist(),
    time = time[, 4:ncol(time)] |> unlist()
  ) |> na.omit()
  
  if(!is.numeric(pk_d$value) | !is.numeric(pk_d$time)){return(NULL)}
  
  # PKパラメータの計算（NonCompart::tblNCAを利用）
  pkparam <- NonCompart::tblNCA(pk_d, key="s_t", colTime="time", colConc="value", excludeDelta = 0.3)
  
  pkparam <- 
    data.frame(
      subject = pk$subject,
      period = pk$period,
      treatment = pk$treatment,
      group = pk$subject |> substr(1, 1),
      AUC = pkparam$AUCLST,
      AUCinf = pkparam$AUCIFO |> round(4),
      Cmax = pkparam$CMAX,
      tmax = pkparam$TMAX,
      kel = pkparam$LAMZ |> round(4),
      RApoint = pkparam$LAMZNPT |> round(4),
      CorrCoef = pkparam$R2 |> round(4),
      thalf = pkparam$LAMZHL |> round(4),
      MRT = pkparam$MRTEVLST |> round(4),
      MRTinf = pkparam$MRTEVIFO |> round(4),
      AUCratio = (pkparam$AUCLST / pkparam$AUCIFO) |> round(4)
    )

  return(pkparam)
}

# PKパラメータの要約を作成するための関数
pk_summary <- function(pkparam, group = "all"){
  
  groups <- pkparam$group |> unique()
  
  if(group == 1){
    pkparam <- pkparam[pkparam$group == groups[1], ]
  } else if(group == 2){
    pkparam <- pkparam[pkparam$group == groups[2], ]
  }
  
  mean_value <- apply(pkparam[, 5:15], 2, mean) |> round(4)
  sd_value <- apply(pkparam[, 5:15], 2, sd) |> round(4)
  max_value <- apply(pkparam[, 5:15], 2, max) |> round(4)
  min_value <- apply(pkparam[, 5:15], 2, min) |> round(4)
  n_value <- apply(pkparam[, 5:15], 2, length)
  
  temp_ms <- cbind(mean_value, sd_value, max_value, min_value, n_value) |> t() |> as.data.frame()
  temp_ms <- cbind(label = c("平均値", "標準偏差", "最小値", "最大値", "例数"), temp_ms)
  
  colnames(temp_ms) <- c(" ", colnames(pkparam)[5:15])
  rownames(temp_ms) <- NULL
  
  return(temp_ms)
}

# PKの平均値・標準偏差をグラフにする関数
pk_summary_plot <- function(pk, time, grouping = "治験薬"){
  # pkと時間をデータフレームに成形
  pk_d <- data.frame(
    s_t = paste(pk$subject, pk$treatment),
    subject = pk$subject,
    period = pk$period,
    treatment = pk$treatment,
    group = pk$subject |> substr(1, 1),
    value = pk[, 4:ncol(pk)] |> unlist(),
    time = time[, 4:ncol(time)] |> unlist()
  ) |> na.omit()
  
  if(grouping == "治験薬"){
    pk_d_fc <- factor(paste(pk_d$time, pk_d$treatment, sep = ","))
    
    pk_d_m <- tapply(pk_d$value, pk_d_fc, mean)
    pk_d_s <- tapply(pk_d$value, pk_d_fc, sd)
    
    tags <- names(pk_d_m) |> strsplit(",") |> as.data.frame() |> t() |> as.data.frame()
    colnames(tags) <- c("time", "treat")
    rownames(tags) <- NULL
    tags$time <- tags$time |> as.numeric()
    tags <- cbind(tags, m = pk_d_m, s = pk_d_s)
    ggplot(tags, aes(x = time, y = m, ymax = m + s, ymin = m - s, color = treat)) +
      geom_point(size = 4) +
      geom_linerange(linewidth = 1) +
      geom_line(linewidth = 1) +
      labs(x = "時間（h）", y = "血漿中薬物濃度", color = NULL, caption = paste0("平均±標準偏差，n = ", pk$subject |> unique() |> length())) +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 15))
    
  } else if (grouping == "時期"){
    pk_d_fc <- factor(paste(pk_d$time, pk_d$period, sep = ","))
    
    pk_d_m <- tapply(pk_d$value, pk_d_fc, mean)
    pk_d_s <- tapply(pk_d$value, pk_d_fc, sd)
    
    tags <- names(pk_d_m) |> strsplit(",") |> as.data.frame() |> t() |> as.data.frame()
    colnames(tags) <- c("time", "period")
    rownames(tags) <- NULL
    tags$time <- tags$time |> as.numeric()
    tags <- cbind(tags, m = pk_d_m, s = pk_d_s)
    ggplot(tags, aes(x = time, y = m, ymax = m + s, ymin = m - s, color = period)) +
      geom_point(size = 4) +
      geom_linerange(linewidth = 1) +
      geom_line(linewidth = 1) +
      labs(x = "時間（h）", y = "血漿中薬物濃度", color = NULL, caption = paste0("平均±標準偏差，n = ", pk$subject |> unique() |> length())) +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 15))    
    
  } else if (grouping == "群"){
    pk_d_fc <- factor(paste(pk_d$time, pk_d$group, sep = ","))
    
    pk_d_m <- tapply(pk_d$value, pk_d_fc, mean)
    pk_d_s <- tapply(pk_d$value, pk_d_fc, sd)
    
    tags <- names(pk_d_m) |> strsplit(",") |> as.data.frame() |> t() |> as.data.frame()
    colnames(tags) <- c("time", "group")
    rownames(tags) <- NULL
    tags$time <- tags$time |> as.numeric()
    tags <- cbind(tags, m = pk_d_m, s = pk_d_s)
    ggplot(tags, aes(x = time, y = m, ymax = m + s, ymin = m - s, color = group)) +
      geom_point(size = 4) +
      geom_linerange(linewidth = 1) +
      geom_line(linewidth = 1) +
      labs(x = "時間（h）", y = "血漿中薬物濃度", color = NULL, caption = paste0("平均±標準偏差，n = ", pk$subject |> unique() |> length())) +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 15))    
  }
}

# 個々の被験者のPKをグラフにする関数
pk_each_plot <- function(pk, time, grouping = "治験薬"){
  # pkと時間をデータフレームに成形
  pk_d <- data.frame(
    s_t = paste(pk$subject, pk$treatment),
    subject = pk$subject,
    period = pk$period,
    treatment = pk$treatment,
    group = pk$subject |> substr(1, 1),
    value = pk[, 4:ncol(pk)] |> unlist(),
    time = time[, 4:ncol(time)] |> unlist()
  ) |> na.omit()
  
  if(grouping == "治験薬"){
    ggplot(pk_d, aes(x = time, y = value, color = treatment)) +
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      labs(x = "時間（h）", y = "血漿中薬物濃度", color = NULL) +
      facet_wrap(~subject) +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15))
  } else if (grouping == "時期"){
    ggplot(pk_d, aes(x = time, y = value, color = factor(period))) +
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      labs(x = "時間（h）", y = "血漿中薬物濃度", color = NULL) +
      facet_wrap(~subject) +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15))
  }
}

# PKパラメータの個別の列をMMRMで解析し，結果と信頼区間を返す関数
mmrm_f <- function(pkparam, col_name, grouping, logarithm = TRUE){
  if(grouping == "治験薬"){l <- 4} else if(grouping == "時期"){l <- 3} else if(grouping == "群"){l <- 2}
  
  formulaLme <- 
    ifelse(logarithm, 
      paste("log(", col_name, ")", "~ group + period + treatment"),
      paste(col_name, "~ group + period + treatment")) |> as.formula()
  
  lmm_out = lme(formulaLme, random=~1|subject, data=pkparam)
  ci_out = intervals(lmm_out, 0.9) |> _$fixed |> _[l,] |> exp()
  
  return(list(lmm_out, ci_out))
}

# lmeによるMMRMの結果をデータフレームにする関数
conv_lme_df <- function(lme_obj){
  temp <- summary(lme_obj)$tTable |> as.data.frame()
  temp <- cbind(term = c("交互作用", "群", "時期", "薬剤"), temp)
  return(temp)
}

# lmeによるMMRMの結果をデータフレームにする関数
conv_lme_rand_df <- function(lme_obj){
  temp <- ranef(lme_obj)
  temp <- cbind(tag = rownames(temp), temp)
  return(temp)
}

# 信頼区間をデータフレームにする関数
conv_ci_df <- function(ci_obj){
  temp <- cbind(label = c("上側90%", "中央値", "下側90%"), (1/ci_obj) |> as.data.frame())
  colnames(temp) <- c("", "値")
  temp
}

# MMRMを一度に演算する関数
mmrm_params_f <- function(pkparam, grouping){
  calc_cols <- c("AUC", "AUCinf", "Cmax", "tmax", "kel", "thalf", "MRT", "MRTinf")
  out <- list()
  for(i in 1:8){
    if(calc_cols[i] == "tmax"){
      out[[i]] <- mmrm_f(pkparam, calc_cols[i], grouping, logarithm = FALSE)
    } else {
      out[[i]] <- mmrm_f(pkparam, calc_cols[i], grouping)
    }
  }
  return(out)
}

# PKパラメータの箱ひげ図・ジッタープロットを作成するための関数
pkparam_bj_plot <- function(pkparam, grouping, type){
  if(grouping == "治験薬"){
    p <- 
      pkparam[,c(1:9, 12:14)] |> 
        tidyr::pivot_longer(5:12) |> 
        ggplot(aes(x = treatment, y = value, color = treatment)) +
        facet_wrap(~name, scales = "free_y") +
        labs(x = NULL, y = NULL) +
        theme(
          axis.text.x = element_text(size = 15), 
          axis.text.y = element_text(size = 15), 
          legend.position = "none",
          legend.text = element_text(size = 15),
          strip.text = element_text(size = 15))
  } else if(grouping == "時期"){
    p <- 
      pkparam[,c(1:9, 12:14)] |> 
      tidyr::pivot_longer(5:12) |> 
      ggplot(aes(x = factor(period), y = value, color = factor(period))) +
      facet_wrap(~name, scales = "free_y") +
      labs(x = NULL, y = NULL) +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        legend.position = "none",
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15))
  } else if(grouping == "群"){
    p <- 
      pkparam[,c(1:9, 12:14)] |> 
      tidyr::pivot_longer(5:12) |> 
      ggplot(aes(x = factor(group), y = value, color = factor(group))) +
      facet_wrap(~name, scales = "free_y") +
      labs(x = NULL, y = NULL) +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        legend.position = "none",
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15))
  }
  
  if(type == "boxplot"){
    p + geom_boxplot()
  } else if(type == "jitter"){
    p + geom_jitter(size = 2, width=0.2, height = 0)
  }
}

normality_test <- function(pkparam){
  names_param <- c("AUC", "AUCinf", "Cmax", "tmax", "kel", "thalf", "MRT", "MRTinf")
  out <- list()
  for(i in 1:8){
    tname <- names_param[i]
    if(tname == "tmax"){
      out[[i]] <- pkparam[, tname] |> shapiro.test() |> broom::tidy() |> as.data.frame() |> _[, 1:2]
    } else {
      out[[i]] <- pkparam[, tname] |> log() |> shapiro.test() |> broom::tidy() |> as.data.frame() |> _[, 1:2]
    }
  }
  return(out)
}

pk_ratio_calc <- function(pkparam){
  temp_auc <- 
    pkparam[, c("subject", "treatment", "AUC")] |> 
    tidyr::pivot_wider(names_from = treatment, values_from = AUC)
  
  temp_cmax <- 
    pkparam[, c("subject", "treatment", "Cmax")] |> 
    tidyr::pivot_wider(names_from = treatment, values_from = Cmax)
  
  out <- data.frame(
    subject = temp_auc$subject,
    AUC = temp_auc[, 2] / temp_auc[, 3],
    Cmax = temp_cmax[, 2] / temp_cmax[, 3])
  
  colnames(out) <- c("被験者", "AUC", "Cmax")
  
  out_s <- 
    data.frame(
      `平均値` = out[, 2:3] |> apply(2, mean) |> round(4),
      `標準偏差` = out[, 2:3] |> apply(2, sd) |> round(4),
      `最大値` = out[, 2:3] |> apply(2, max) |> round(4),
      `最小値` = out[, 2:3] |> apply(2, min) |> round(4)
    )
  
  out_s <- cbind(`パラメータ` = c("AUC", "Cmax"), out_s)
  
  return(list(out, out_s))
}
