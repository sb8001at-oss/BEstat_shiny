ui <- page_sidebar(
  title = "BEの統計解析",
  theme = bs_theme(bootswatch = "united"),
  sidebar = sidebar(
    accordion(
      open = FALSE,
      multiple = FALSE,
      accordion_panel(
        "Excelファイルを読み込む",
        fileInput("excelfile", NULL, accept=".xlsx")
      ),
      
      accordion_panel(
        "テンプレートのExcelファイル",
        downloadButton("downloadData", "保存")
      )
    ),
    actionButton("calc_pkparam", "パラメータを計算", icon = icon("star"), style = "color: #fff; background-color: #007bff; border-color: #007bff; font-weight: bold;"),
    actionButton("plot_summary_show", "血漿中薬物濃度グラフ（平均）を表示"),
    actionButton("plot_each_show", "血漿中薬物濃度グラフ（被験者ごと）を表示"),
    uiOutput("mmrmcalcButton"),
    uiOutput("samplesizeNinput"),
    selectInput("grouping", "グラフ/統計のグループ", choices = c("治験薬", "時期", "群"), selected = "治験薬"),
    card(
      card_header("結果をExcelに出力"),
      uiOutput("Excelfilename"),
      uiOutput("downloadExcelButton")
    )
  ),
  navset_tab(
    id = "switcher",
    nav_panel(
      "血漿中薬物濃度",
      DT::dataTableOutput("pk_table", fill = TRUE)
    ),
    nav_panel(
      "採血時間",
      DT::dataTableOutput("time_table", fill = TRUE)
    ),
    nav_panel(
      "PKパラメータ",
      accordion(
        open = "各被験者のPKパラメータ",
        accordion_panel(
          "各被験者のPKパラメータ",
          DT::dataTableOutput("pkparam_table", fill = TRUE)
        ),
        
        accordion_panel(
          "試験製剤の要約",
          DT::dataTableOutput("pkparam_test", fill = TRUE)
        ),
        accordion_panel(
          "標準製剤の要約",
          textOutput("group2_name"),
          DT::dataTableOutput("pkparam_ref", fill = TRUE)
        ),
        
        accordion_panel(
          "全被験者の要約",
          DT::dataTableOutput("pkparam_ms", fill = TRUE)
        ),
        
        accordion_panel(
          "グループ1の要約",
          textOutput("group1_name"),
          DT::dataTableOutput("pkparam_group1", fill = TRUE)
        ),
        accordion_panel(
          "グループ2の要約",
          textOutput("group2_name"),
          DT::dataTableOutput("pkparam_group2", fill = TRUE)
        ),
        
        accordion_panel(
          "時期1の要約",
          DT::dataTableOutput("pkparam_period1", fill = TRUE)
        ),
        accordion_panel(
          "時期2の要約",
          DT::dataTableOutput("pkparam_period2", fill = TRUE)
        ),
      ),
      div(p("＊AUC：AUClast、RApoint：遡及点、CorrCoef：相関係数、thalf：t1/2、AUCratio：AUC/AUCinf"), style = "font-size: 0.9rem;")
    ),
    nav_panel(
      "血漿中薬物濃度グラフ（平均±標準偏差）",
      card(
        plotOutput("pk_summary_plot_out", fill = TRUE, height = "850px")
      )
    ),
    nav_panel(
      "血漿中薬物濃度グラフ（被験者ごと）",
      card(
        plotOutput("pk_each_plot_out", fill = TRUE, height = "850px")
      )
    ),
    nav_panel(
      "統計解析",
      navset_card_underline(
        id = "switcher_stat",
        nav_panel(
          "信頼区間の計算結果",
          card(textOutput("grouping_text")),
          layout_column_wrap(
            width = 1/4,
            card(
              card_header("AUC"),
              tableOutput("ci_AUC")
            ),
            card(
              card_header("AUCinf"),
              tableOutput("ci_AUCinf")
            ),
            card(
              card_header("Cmax"),
              tableOutput("ci_Cmax")
            ),
            card(
              card_header("tmax（差の信頼区間）"),
              tableOutput("ci_tmax")
            ),
            card(
              card_header("kel"),
              tableOutput("ci_kel")
            ),
            card(
              card_header("t1/2"),
              tableOutput("ci_thalf")
            ),
            card(
              card_header("MRT"),
              tableOutput("ci_MRT")
            ),
            card(
              card_header("MRTinf"),
              tableOutput("ci_MRTinf")
            )
          )
        ),
        nav_panel(
          "分散分析結果",
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("AUC：分散分析"),
              tableOutput("lme_AUC")
            ),
            card(
              card_header("AUC：ランダム効果"),
              tableOutput("lme_rand_AUC")
            )
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("AUCinf：分散分析"),
              tableOutput("lme_AUCinf")
            ),
            card(
              card_header("AUCinf：ランダム効果"),
              tableOutput("lme_rand_AUCinf")
            )
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("Cmax：分散分析"),
              tableOutput("lme_Cmax")
            ),
            card(
              card_header("Cmax：ランダム効果"),
              tableOutput("lme_rand_Cmax")
            )
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("tmax：分散分析"),
              tableOutput("lme_tmax")
            ),
            card(
              card_header("tmax：ランダム効果"),
              tableOutput("lme_rand_tmax")
            )
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("kel：分散分析"),
              tableOutput("lme_kel")
            ),
            card(
              card_header("kel：ランダム効果"),
              tableOutput("lme_rand_kel")
            )
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("t1/2：分散分析"),
              tableOutput("lme_thalf")
            ),
            card(
              card_header("t1/2：ランダム効果"),
              tableOutput("lme_rand_thalf")
            )
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("MRT：分散分析"),
              tableOutput("lme_MRT")
            ),
            card(
              card_header("MRT：ランダム効果"),
              tableOutput("lme_rand_MRT")
            )
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("MRTinf：分散分析"),
              tableOutput("lme_MRTinf")
            ),
            card(
              card_header("MRTinf：ランダム効果"),
              tableOutput("lme_rand_MRTinf")
            )
          )
        ),
        nav_panel(
          "PKパラメータのグラフ",
          layout_columns(
            card(
              card_header("箱ひげ図"),
              plotOutput("pkparam_boxplot", fill = TRUE, height = "750px")
            ),
            card(
              card_header("ジッタープロット"),
              plotOutput("pkparam_jitterplot", fill = TRUE, height = "750px")
            )
            
          )
        ),
        nav_panel(
          "正規性の確認",
          layout_column_wrap(
            width = 1/3, 
            card(
              card_header("AUC"),
              tableOutput("normality_test_AUC")
            ),
            card(
              card_header("AUCinf"),
              tableOutput("normality_test_AUCinf")
            ),
            card(
              card_header("Cmax"),
              tableOutput("normality_test_Cmax")
            ),
            card(
              card_header("tmax"),
              tableOutput("normality_test_tmax")
            ),
            card(
              card_header("kel"),
              tableOutput("normality_test_kel")
            ),
            card(
              card_header("t1/2"),
              tableOutput("normality_test_thalf")
            ),
            card(
              card_header("MRT"),
              tableOutput("normality_test_MRT")
            ),
            card(
              card_header("MRTinf"),
              tableOutput("normality_test_MRTinf")
            )
          ),
          div(p("＊Shapiro-Wilk検定の結果。p < 0.05の時は正規分布ではない"), style = "font-size: 0.9rem;")
        ),
        nav_panel(
          "試験製剤/標準製剤の比",
          card(
            card_header("各被験者の比"),
            tableOutput("ratio_table")
          ),
          card(
            card_header("被験者の比の要約"),
            tableOutput("ratio_table_s")
          ),
          div(p("＊値はグラフ/統計のグループの影響を受けず、常に試験製剤/標準製剤の比を返します。"), style = "font-size: 0.9rem;")
        ),
        nav_panel(
          "例数設計",
          accordion(
            multiple = FALSE,
            open = "AUC",
            accordion_panel(
              "AUC",
              layout_column_wrap(
                card(
                  card_header("AUC：信頼区間"),
                  tableOutput("AUC_ci_ss")
                ),
                card(
                  card_header("AUC：検出力"),
                  tableOutput("AUC_power_ss")
                )
              )              
            ),
            accordion_panel(
              "Cmax",
              layout_column_wrap(
                card(
                  card_header("Cmax：信頼区間"),
                  tableOutput("Cmax_ci_ss")
                ),
                card(
                  card_header("Cmax：検出力"),
                  tableOutput("Cmax_power_ss")
                )
              )
            )
          ),

          layout_column_wrap(
            value_box("AUC：epsilon（AUCの個体内標準偏差）", value = textOutput("AUC_ep"), theme = "primary"),
            value_box("AUC：CVw（AUCの個体内分散）", value = textOutput("AUC_CVw"), theme = "secondary"),
            value_box("Cmax：epsilon（Cmaxの個体内標準偏差）", value = textOutput("Cmax_ep"), theme = "primary"),
            value_box("Cmax：CVw（Cmaxの個体内分散）", value = textOutput("Cmax_CVw"), theme = "secondary")
          ),
          div(p("＊被験者数は2群。値はグラフ/統計のグループの影響を受けます。epsilonは線形混合モデルの演算結果として取得。CVwはexp(epsilon^2) - 1の平方根として計算。どちらもBEの難易度を反映します。"), style = "font-size: 0.9rem;")
        )
      )
    )
  )
)