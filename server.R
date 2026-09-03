function(input, output, session) {

  pk <- reactiveVal(data_pk)
  time <- reactiveVal(data_time)

  # Excelファイルの読み込み
  observeEvent(
    input$excelfile,{ 
      req(input$excelfile)
      pk_read <- readxl::read_excel(input$excelfile$datapath, sheet = "PK")
      time_read <- readxl::read_excel(input$excelfile$datapath, sheet = "time")
      
      if(sum(pk_read$subject != time_read$subject) > 0 | sum(pk_read$period != time_read$period) > 0 | sum(pk_read$treatment != time_read$treatment) > 0){
        showNotification("PKと時間のテーブルのsubject、period、treatmentが一致していません。", duration = 5, type = "error")
        return(0)
      }
      
      if(!is.numeric(pk_read[, 4:ncol(pk_read)]) | !is.numeric(time_read[, 4:ncol(time_read)])){
        showNotification("PKと時間のテーブルの値に文字列が含まれています。", duration = 5, type = "error")
        return(0)
      }
      
      if(pk_read$treatment |> unique() %in% c("試験製剤", "標準製剤") |> sum() != 2){
        showNotification("製剤は2つとし，「試験製剤」，「標準製剤」の名前で設定して下さい。", duration = 5, type = "error")
        return(0)
      }
      
      pk(pk_read)
      time(time_read)
  })
  
  # テンプレートのExcelファイルを保存する
  output$downloadData <- downloadHandler(
    filename = "テンプレートExcelファイル.xlsx",
    content = function(file){
      file.copy(file.path("./", "example_excel_file.xlsx"), file)
    }
  )
  
  pkparam_temp <- reactiveVal(NULL)
  
  # PKパラメータの計算
  observeEvent(input$calc_pkparam, {
    pkparam_temp(pkParam(pk(), time()))
    output$pkparam_table <- DT::renderDataTable(pkparam_temp(), options = list(pageLength = 50, dom = "t"))
    
    output$pkparam_test <- DT::renderDataTable(pk_summary_treat(pkparam_temp(), "試験製剤"), options = list(pageLength = 50, dom = "t"))
    output$pkparam_ref <- DT::renderDataTable(pk_summary_treat(pkparam_temp(), "標準製剤"), options = list(pageLength = 50, dom = "t"))
        
    output$pkparam_group1 <- DT::renderDataTable(pk_summary(pkparam_temp(), group = 1), options = list(pageLength = 50, dom = "t"))
    output$pkparam_group2 <- DT::renderDataTable(pk_summary(pkparam_temp(), group = 2), options = list(pageLength = 50, dom = "t"))

    output$pkparam_period1 <- DT::renderDataTable(pk_summary_period(pkparam_temp(), 1), options = list(pageLength = 50, dom = "t"))
    output$pkparam_period2 <- DT::renderDataTable(pk_summary_period(pkparam_temp(), 2), options = list(pageLength = 50, dom = "t"))
    
    output$pkparam_ms <- DT::renderDataTable(pk_summary(pkparam_temp()), options = list(pageLength = 50, dom = "t"))
    
    groupnames <- pkparam_temp()$group |> unique()
    
    output$group1_name <- renderText(paste0(groupnames[1], "群"))
    output$group2_name <- renderText(paste0(groupnames[2], "群"))
    
    nav_select(id = "switcher", selected = "PKパラメータ")
  })
  
  # PKパラメータの演算後にボタンを表示する
  output$mmrmcalcButton <-
    renderUI({
      req(pkparam_temp())
      actionButton("mmrm_calc", "分散分析・信頼区間を計算する")
    })

  output$PKparamAnalysisButton <-
    renderUI({
      req(pkparam_temp())
      actionButton("PKparam_analysis", "PKパラメータのグラフ・正規性等を計算する")
    })  

  output$samplesizeNinput <-
    renderUI({
      req(pkparam_temp())
      sliderInput("sample_size_nmax", "サンプルサイズ演算の最大被験者数（1群）", min = 2, max = 50, value = 15, step = 1)
    })  
   
  output$Excelfilename <-
    renderUI({
      req(pkparam_temp())
      textInput("filename_excel", "Excelファイル名", value = "BE解析結果")
    })  
  
  
  output$downloadExcelButton <-
    renderUI({
      req(pkparam_temp())
      downloadButton("downloadExcel", "ダウンロード")
    })
  
  
  output$pk_table <- DT::renderDataTable(pk(), options = list(pageLength = 50, dom = "t"))
  output$time_table <- DT::renderDataTable(time(), options = list(pageLength = 50, dom = "t"))
  
  # PKの平均値をグラフにする
  observeEvent(input$plot_summary_show, {
    output$pk_summary_plot_out <- renderPlot(
      if(input$grouping == "時期"){
        pk_summary_plot(pk(), time(), "時期")
      } else if(input$grouping == "群"){
        pk_summary_plot(pk(), time(), "群")
      } else{
        pk_summary_plot(pk(), time())
      }
    )
    nav_select(id = "switcher", selected = "血漿中薬物濃度グラフ（平均±標準偏差）")
  })
  
  # PK（個々の被験者）をグラフにする
  observeEvent(input$plot_each_show, {
    output$pk_each_plot_out <- renderPlot(
      if(input$grouping == "時期"){
        pk_each_plot(pk(), time(), "時期")
      } else {
        pk_each_plot(pk(), time())
      }
    )
    nav_select(id = "switcher", selected = "血漿中薬物濃度グラフ（被験者ごと）")
  })
  
  # 分散分析結果・信頼区間を表示する
  observeEvent(input$mmrm_calc, {
    if(is.null(pkparam_temp())){
      showNotification("先にPKパラメータを計算して下さい。", duration = 5, type = "error")
      return(0)
    }
      
    mmrm_out <- mmrm_params_f(pkparam_temp(), input$grouping)
    
    output$ci_AUC <-    renderTable(conv_ci_df(mmrm_out[[1]][[2]]))
    output$ci_AUCinf <- renderTable(conv_ci_df(mmrm_out[[2]][[2]]))
    output$ci_Cmax <-   renderTable(conv_ci_df(mmrm_out[[3]][[2]]))
    output$ci_tmax <-   renderTable(conv_ci_df_tmax(mmrm_out[[4]][[2]]))
    output$ci_kel <-    renderTable(conv_ci_df(mmrm_out[[5]][[2]]))
    output$ci_thalf <-  renderTable(conv_ci_df(mmrm_out[[6]][[2]]))
    output$ci_MRT <-    renderTable(conv_ci_df(mmrm_out[[7]][[2]]))
    output$ci_MRTinf <- renderTable(conv_ci_df(mmrm_out[[8]][[2]]))
    
    output$lme_AUC <-    renderTable(conv_lme_df(mmrm_out[[1]][[1]]))
    output$lme_AUCinf <- renderTable(conv_lme_df(mmrm_out[[2]][[1]]))
    output$lme_Cmax <-   renderTable(conv_lme_df(mmrm_out[[3]][[1]]))
    output$lme_tmax <-   renderTable(conv_lme_df(mmrm_out[[4]][[1]]))
    output$lme_kel <-    renderTable(conv_lme_df(mmrm_out[[5]][[1]]))
    output$lme_thalf <-  renderTable(conv_lme_df(mmrm_out[[6]][[1]]))
    output$lme_MRT <-    renderTable(conv_lme_df(mmrm_out[[7]][[1]]))
    output$lme_MRTinf <- renderTable(conv_lme_df(mmrm_out[[8]][[1]]))
    
    output$lme_rand_AUC <-    renderTable(conv_lme_rand_df(mmrm_out[[1]][[1]]))
    output$lme_rand_AUCinf <- renderTable(conv_lme_rand_df(mmrm_out[[2]][[1]]))
    output$lme_rand_Cmax <-   renderTable(conv_lme_rand_df(mmrm_out[[3]][[1]]))
    output$lme_rand_tmax <-   renderTable(conv_lme_rand_df(mmrm_out[[4]][[1]]))
    output$lme_rand_kel <-    renderTable(conv_lme_rand_df(mmrm_out[[5]][[1]]))
    output$lme_rand_thalf <-  renderTable(conv_lme_rand_df(mmrm_out[[6]][[1]]))
    output$lme_rand_MRT <-    renderTable(conv_lme_rand_df(mmrm_out[[7]][[1]]))
    output$lme_rand_MRTinf <- renderTable(conv_lme_rand_df(mmrm_out[[8]][[1]]))
    
    output$grouping_text <- renderText(paste0("グラフ/統計のグループ：", input$grouping))
    
    sample_size_results <- calc_cv(mmrm_out, input$sample_size_nmax)
    
    output$AUC_ci_ss <- renderTable(sample_size_results[[1]][[4]])
    output$AUC_power_ss <- renderTable(sample_size_results[[1]][[3]])
    output$AUC_ep <- renderText(sample_size_results[[1]][[1]])
    output$AUC_CVw <- renderText(sample_size_results[[1]][[2]])
    
    output$Cmax_ci_ss <- renderTable(sample_size_results[[2]][[4]])
    output$Cmax_power_ss <- renderTable(sample_size_results[[2]][[3]])
    output$Cmax_ep <- renderText(sample_size_results[[2]][[1]])
    output$Cmax_CVw <- renderText(sample_size_results[[2]][[2]])
    
    nav_select(id = "switcher", selected = "統計解析")
    nav_select(id = "switcher_stat", selected = "信頼区間の計算結果")
  })
  
  # PKパラメータのグラフ・正規性評価・試験製剤/標準製剤比を示す
  observeEvent(input$PKparam_analysis, {
    if(is.null(pkparam_temp())){
      showNotification("先にPKパラメータを計算して下さい。", duration = 5, type = "error")
      return(0)
    }
    
    output$pkparam_boxplot <- 
      renderPlot(
        pkparam_bj_plot(pkparam_temp(), input$grouping, type = "boxplot")
      )
    
    output$pkparam_jitterplot <- 
      renderPlot(
        pkparam_bj_plot(pkparam_temp(), input$grouping, type = "jitter")
      )
    
    nav_select(id = "switcher", selected = "統計解析")
    nav_select(id = "switcher_stat", selected = "PKパラメータのグラフ")
    
    lst_norm_test <- normality_test(pkparam_temp())
    
    output$normality_test_AUC <- renderTable(lst_norm_test[[1]])
    output$normality_test_AUCinf <- renderTable(lst_norm_test[[2]])
    output$normality_test_Cmax <- renderTable(lst_norm_test[[3]])
    output$normality_test_tmax <- renderTable(lst_norm_test[[4]])
    output$normality_test_kel <- renderTable(lst_norm_test[[5]])
    output$normality_test_thalf <- renderTable(lst_norm_test[[6]])
    output$normality_test_MRT <- renderTable(lst_norm_test[[7]])
    output$normality_test_MRTinf <- renderTable(lst_norm_test[[8]])
    
    ratio_c <- pk_ratio_calc(pkparam_temp())
    
    output$ratio_table <- renderTable(ratio_c[[1]])
    output$ratio_table_s <- renderTable(ratio_c[[2]])
  })
  
  # Excelファイルのダウンロードを処理
  output$downloadExcel <- downloadHandler(
    filename = function(){
      paste0(
        Sys.Date(), " ", input$filename_excel, ".xlsx")
    },
    content = function(file){
      out_excel(pk(), time(), pkparam_temp(), n_sbj = input$sample_size_nmax)$save(file)
    }
  )
  
  
}
