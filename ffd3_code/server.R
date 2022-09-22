library(shiny)
library(tidyverse)

calculator = function(df, qb_now, rb_now, wr_now, te_now, dst_now, k_now, subsequent_pick) {
  position = c("QB", "RB", "WR", "TE", "DST", "K")
  best_available = c(qb_now, rb_now, wr_now, te_now, dst_now, k_now)
  
  df_calc = tibble(position, best_available)
  
  df_calc$fp_now = rep(NA, times = 6)
  predict_qb = df[df$pos == "QB" & df$rank_adp == qb_now, "prediction"]
  if (nrow(predict_qb) == 1) {
    df_calc$fp_now[1] = predict_qb
  } else {df_calc$fp_now[1] = NA}
  
  predict_rb = df[df$pos == "RB" & df$rank_adp == rb_now, "prediction"]
  if (nrow(predict_rb) == 1) {
    df_calc$fp_now[2] = predict_rb
  } else {df_calc$fp_now[2] = NA}
  
  predict_wr = df[df$pos == "WR" & df$rank_adp == wr_now, "prediction"]
  if (nrow(predict_wr) == 1) {
    df_calc$fp_now[3] = predict_wr
  } else {df_calc$fp_now[3] = NA}
  
  
  predict_te = df[df$pos == "TE" & df$rank_adp == te_now, "prediction"]
  if (nrow(predict_te) == 1) {
    df_calc$fp_now[4] = predict_te
  } else {df_calc$fp_now[4] = NA}
  
  predict_dst = df[df$pos == "DST" & df$rank_adp == dst_now, "prediction"]
  if (nrow(predict_dst) == 1) {
    df_calc$fp_now[5] = predict_dst
  } else {df_calc$fp_now[5] = NA}
  
  predict_k = df[df$pos == "K" & df$rank_adp == k_now, "prediction"]
  if (nrow(predict_k) == 1) {
    df_calc$fp_now[6] = predict_k
  } else {df_calc$fp_now[6] = NA}
  
  df_calc$fp_later = rep(NA, times = 6)
  later_qb = df[df$pos == "QB" & df$rank_adp == subsequent_pick, "prediction"]
  if (nrow(later_qb) == 1) {
    df_calc$fp_later[1] = later_qb
  } else {df_calc$fp_later[1] = NA}
  
  later_rb = df[df$pos == "RB" & df$rank_adp == subsequent_pick, "prediction"]
  if (nrow(later_rb) == 1) {
    df_calc$fp_later[2] = later_rb
  } else {df_calc$fp_later[2] = NA}
  
  later_wr = df[df$pos == "WR" & df$rank_adp == subsequent_pick, "prediction"]
  if (nrow(later_wr) == 1) {
    df_calc$fp_later[3] = later_wr
  } else {df_calc$fp_later[3] = NA}
  
  
  later_te = df[df$pos == "TE" & df$rank_adp == subsequent_pick, "prediction"]
  if (nrow(later_te) == 1) {
    df_calc$fp_later[4] = later_te
  } else {df_calc$fp_later[4] = NA}
  
  later_dst = df[df$pos == "DST" & df$rank_adp == subsequent_pick, "prediction"]
  if (nrow(later_dst) == 1) {
    df_calc$fp_later[5] = later_dst
  } else {df_calc$fp_later[5] = NA}
  
  later_k = df[df$pos == "K" & df$rank_adp == subsequent_pick, "prediction"]
  if (nrow(later_k) == 1) {
    df_calc$fp_later[6] = later_k
  } else {df_calc$fp_later[6] = NA}
  
  df_calc$fp_now = as.numeric(df_calc$fp_now)
  df_calc$fp_later = as.numeric(df_calc$fp_later)
  
  df_calc$Value = df_calc$fp_now - df_calc$fp_later
  
  colnames(df_calc) = c("Position", "Best Available",
                        "Expected Points This Round", 
                        "Expected Points Next Round",
                        "Value")
  
  df_calc = df_calc %>% arrange(desc(Value))
  
  return(df_calc)
}

get_picks = function(teams, draft_order, round) {
  
  draft_order = ifelse(draft_order > teams, teams, draft_order)
  
  now_pick = as.integer(ifelse(round %% 2 == 0, teams*round+1-draft_order, teams*(round-1)+draft_order))
  next_round = round + 1
  later_pick = as.integer(ifelse(next_round %% 2 == 0, teams*next_round+1-draft_order, teams*(next_round-1)+draft_order))
  
  return(c(now_pick, later_pick))
}

function(input, output) {
  
  df = read_csv("./shiny_data/df_value.csv", show_col_types = FALSE)
  df_real = read_csv("./shiny_data/fantasypros_2012_to_2021.csv", show_col_types = FALSE)
  
  output$text = renderText({
    
    teams = input$teams
    draft_order = input$draft_order
    round = input$round
    
    picks = get_picks(teams, draft_order, round)
    
    if (input$mode == 'Simple') {
      qb_now = picks[1]
      rb_now = picks[1]
      wr_now = picks[1]
      te_now = picks[1]
      dst_now = picks[1]
      k_now = picks[1]
    } else {
      qb_now = input$qb
      rb_now = input$rb
      wr_now = input$wr
      te_now = input$te
      dst_now = input$dst
      k_now = input$k
    }
    
    subsequent_pick = picks[2]
    
    df_calc = calculator(df, qb_now, rb_now, wr_now, te_now, dst_now, k_now, subsequent_pick)
    
    paste0("Suggestion: Pick the best ", pull(df_calc[1, "Position"]))
    
  })
  
  output$table = renderTable({
    
    teams = input$teams
    draft_order = input$draft_order
    round = input$round
    
    picks = get_picks(teams, draft_order, round)
    
    if (input$mode == 'Simple') {
      qb_now = picks[1]
      rb_now = picks[1]
      wr_now = picks[1]
      te_now = picks[1]
      dst_now = picks[1]
      k_now = picks[1]
    } else {
      qb_now = input$qb
      rb_now = input$rb
      wr_now = input$wr
      te_now = input$te
      dst_now = input$dst
      k_now = input$k
    }
    
    subsequent_pick = picks[2]
    
    df_calc = calculator(df, qb_now, rb_now, wr_now, te_now, dst_now, k_now, subsequent_pick)
    
    df_calc
    
  }, align='c', width="300%")
  
  output$plot1 = renderPlot({
    
    teams = input$teams
    draft_order = input$draft_order
    round = input$round
    
    picks = get_picks(teams, draft_order, round)
    
    if (input$mode == 'Simple') {
      qb_now = picks[1]
      rb_now = picks[1]
      wr_now = picks[1]
      te_now = picks[1]
      dst_now = picks[1]
      k_now = picks[1]
    } else {
      qb_now = input$qb
      rb_now = input$rb
      wr_now = input$wr
      te_now = input$te
      dst_now = input$dst
      k_now = input$k
    }
    
    subsequent_pick = picks[2]
    
    df_plot = df[(df$pos == "QB" & df$rank_adp %in% 
                    seq(qb_now, subsequent_pick, length.out=(abs(qb_now - subsequent_pick) + 1))) |
                   (df$pos == "RB" & df$rank_adp %in% 
                      seq(rb_now, subsequent_pick, length.out=(abs(rb_now - subsequent_pick) + 1))) |
                   (df$pos == "WR" & df$rank_adp %in% 
                      seq(wr_now, subsequent_pick, length.out=(abs(wr_now - subsequent_pick) + 1))) |
                   (df$pos == "TE" & df$rank_adp %in% 
                      seq(te_now, subsequent_pick, length.out=(abs(te_now - subsequent_pick) + 1))) |
                   (df$pos == "DST" & df$rank_adp %in% 
                      seq(dst_now, subsequent_pick, length.out=(abs(dst_now - subsequent_pick) + 1))) |
                   (df$pos == "K" & df$rank_adp %in% 
                      seq(k_now, subsequent_pick, length.out=(abs(k_now - subsequent_pick) + 1))), ]
    df_plot = df_plot[df_plot$rank_adp <= subsequent_pick, ]
    
    df_plot$pos = factor(df_plot$pos, levels=c("QB", "RB", "WR", "TE", "DST", "K"))
    
    df_real = df_real[(df_real$pos == "QB" & df_real$rank_adp %in% 
                    seq(qb_now, subsequent_pick, length.out=(abs(qb_now - subsequent_pick) + 1))) |
                   (df_real$pos == "RB" & df_real$rank_adp %in% 
                      seq(rb_now, subsequent_pick, length.out=(abs(rb_now - subsequent_pick) + 1))) |
                   (df_real$pos == "WR" & df_real$rank_adp %in% 
                      seq(wr_now, subsequent_pick, length.out=(abs(wr_now - subsequent_pick) + 1))) |
                   (df_real$pos == "TE" & df_real$rank_adp %in% 
                      seq(te_now, subsequent_pick, length.out=(abs(te_now - subsequent_pick) + 1))) |
                   (df_real$pos == "DST" & df_real$rank_adp %in% 
                      seq(dst_now, subsequent_pick, length.out=(abs(dst_now - subsequent_pick) + 1))) |
                   (df_real$pos == "K" & df_real$rank_adp %in% 
                      seq(k_now, subsequent_pick, length.out=(abs(k_now - subsequent_pick) + 1))), ]
    df_real = df_real[df_real$rank_adp <= subsequent_pick, ]
    if (min(df_real$rank_adp) == max(df_real$rank_adp)) {
      df_real = df_real[df_real$rank_adp < subsequent_pick, ]
    }
    
    df_real$pos = factor(df_real$pos, levels=c("QB", "RB", "WR", "TE", "DST", "K"))
    
    ggplot(df_plot, aes(rank_adp, prediction, col=pos))+
      geom_point(data=df_real, mapping=aes(y=avg_scoring), alpha=0.15)+
      geom_line(alpha=0.75, size=2)+
      labs(title="Expected Fantasy Points by ADP and Position",  
           x="Average Draft Position", y="Expected Fantasy Points", color="Position")+
      scale_color_manual(values = c("#f8766d", "#b79f00", "#00ba38", "#00bfc4", "#619cff", "#f564e3"),
                         drop = FALSE)+
      theme_minimal()+
      theme(plot.background = element_rect(fill = "#2b3e50", color = "#2b3e50"),
            panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
            title = element_text(color = "#FFFFFF"),
            text = element_text(color = "#FFFFFF"),
            axis.text = element_text(color = "#FFFFFF"))
  })
  
  output$plot2 = renderPlot({
    
    teams = input$teams
    draft_order = input$draft_order
    round = input$round
    
    picks = get_picks(teams, draft_order, round)
    
    if (input$mode == 'Simple') {
      qb_now = picks[1]
      rb_now = picks[1]
      wr_now = picks[1]
      te_now = picks[1]
      dst_now = picks[1]
      k_now = picks[1]
    } else {
      qb_now = input$qb
      rb_now = input$rb
      wr_now = input$wr
      te_now = input$te
      dst_now = input$dst
      k_now = input$k
    }
    
    subsequent_pick = picks[2]
    
    df_plot = df[(df$pos == "QB" & df$rank_adp %in% 
                    seq(qb_now, subsequent_pick, length.out=(abs(qb_now - subsequent_pick) + 1))) |
                   (df$pos == "RB" & df$rank_adp %in% 
                      seq(rb_now, subsequent_pick, length.out=(abs(rb_now - subsequent_pick) + 1))) |
                   (df$pos == "WR" & df$rank_adp %in% 
                      seq(wr_now, subsequent_pick, length.out=(abs(wr_now - subsequent_pick) + 1))) |
                   (df$pos == "TE" & df$rank_adp %in% 
                      seq(te_now, subsequent_pick, length.out=(abs(te_now - subsequent_pick) + 1))) |
                   (df$pos == "DST" & df$rank_adp %in% 
                      seq(dst_now, subsequent_pick, length.out=(abs(dst_now - subsequent_pick) + 1))) |
                   (df$pos == "K" & df$rank_adp %in% 
                      seq(k_now, subsequent_pick, length.out=(abs(k_now - subsequent_pick) + 1))), ]
    df_plot = df_plot[df_plot$rank_adp <= subsequent_pick, ]
    
    df_plot$pos = factor(df_plot$pos, levels=c("QB", "RB", "WR", "TE", "DST", "K"))
    
    ggplot(df_plot, aes(rank_adp, value, col=pos))+ 
      geom_line(alpha=0.75, size=2)+
      labs(title="Position Value by ADP",  
           x="Average Draft Position", y="Value Versus Subsequent Pick", color="Position")+
      scale_color_manual(values = c("#f8766d", "#b79f00", "#00ba38", "#00bfc4", "#619cff", "#f564e3"), 
                         drop = FALSE)+
      theme_minimal()+
      theme(plot.background = element_rect(fill = "#2b3e50", color = "#2b3e50"),
            panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
            title = element_text(color = "#FFFFFF"),
            text = element_text(color = "#FFFFFF"),
            axis.text = element_text(color = "#FFFFFF"))
  })
  
}