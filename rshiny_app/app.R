library(shiny)
library(rvest)
library(dplyr)

title_lvl<- list('GM'=1, 'IM'=2, 'WGM'=3, 'WIM'=4, 'FM'=5, 'WFM'=6, 'Others'=7)
d_pd_table<- read.csv('d_to_pd.csv')
p_dp_table<- read.csv('p_to_dp.csv')
norm_cond<- read.csv('norm_cond.csv')
cond_name<- c('Num. games', 'Num. MO', 'Num. TH', 'Opp. Avg. ELO', 'Points')

# ui ----------------------------------------------------------------------
col_width<- c(40, 100, 200, 100, 80, 180)
col_style<- function(col_num){
  css_style<- paste0(
    "display:inline-block; vertical-align:top; width:", 
    toString(col_width[col_num]), "px; padding-right:15px"
  )
  
  return(css_style)
}

summ_style<- function(side){
  if (side == 'Label'){
    w<-300
  } else if(side=='Value'){
    w<-60
  } else (
    w<-100
  )
  
  css_style<- paste0(
    "display:inline-block; vertical-align:bottom; width:", 
    toString(w), "px"
  )
  
  return(css_style)
}

total_width<- sum(col_width)

rd_num_block<- function(rd){
  rd_block<- div(style = col_style(1), h4(toString(rd)))
}

title_input<- function(rd){
  in_id<- paste0('title_', toString(rd))
  
  dd_input<- div(
    style = col_style(2),
    selectInput(
      inputId = in_id,
      label = NULL,
      choices = names(title_lvl),
      selected = 'Others',
    )
  )
  
  return(dd_input)
}

name_input<- function(rd){
  in_id<- paste0('name_', toString(rd))
  
  t_input<- div(
    style = col_style(3),
    textInput(
      inputId = in_id,
      label = NULL,
      value = "",
      placeholder = paste0('Round ', toString(rd), ' Opp')
    )
  )
  
  return(t_input)
}

elo_input<- function(rd){
  in_id<- paste0('elo_', toString(rd))
  
  fourd_input<- div(
    style = col_style(4),
    numericInput(
      inputId = in_id,
      label = NULL,
      value = 0,
      min = 0,  max = 4000,
      step = 1,
    )
  )
  
  return(fourd_input)
}

res_input<- function(rd){
  in_id<- paste0('res_', toString(rd))
  
  dd_input<- div(
    style = col_style(5),
    selectInput(
      inputId = in_id,
      label = NULL,
      choices = list('0.0' = 0, '0.5' = 0.5, '1.0' = 1),
      selected = 0.5,
    )
  )
  
  return(dd_input)
}

elo_output<- function(rd){
  out_id<- paste0('elo_', toString(rd))
  t_output<- div(
    style = col_style(6),
    verbatimTextOutput(
      outputId = out_id
    )
  )
  
  return(t_output)
}

opp_row<- function(rd){
  rd_row<- fluidRow(
    column(12, 
      rd_num_block(rd), title_input(rd), name_input(rd), 
      elo_input(rd), res_input(rd), elo_output(rd)
    )
  )
  
  return(rd_row)
}

label_row<- fluidRow(
  column(12,
     div(style = col_style(1), h3('Rd')),
     div(style = col_style(2), h3('Title')),
     div(style = col_style(3), h3('Name')),
     div(style = col_style(4), h3('Rating')),
     div(style = col_style(5), h3('Result')),
     div(style = col_style(6), h3('Rating Change')),
  ),
  hr()
)

player_row<- fluidRow(
  column(12,
    div(
       style = "display:inline-block; vertical-align:bottom; padding-right:20px",
       h2('Current Rating: '),
    ),
    div(
      style = "display:inline-block; vertical-align:bottom; padding-right:100px",
      numericInput(
        inputId = 'player_elo',
        label = NULL,
        value = 2000,
        min = 0,
        max = 4000,
        step = 1,
        width = '100px'
      )
    ),
    div(
      style = "display:inline-block; vertical-align:bottom; padding-right:20px",
      h2('K-factor: '),
    ),
    div(
      style = "display:inline-block; vertical-align:bottom; padding-right:20px",
      numericInput(
        inputId = 'player_k',
        label = NULL,
        value = 20,
        min = 10,
        max = 40,
        step = 1,
        width = '80px'
      )
    ),
  )
)

imp_res_row<- fluidRow(
  column(12,
         div(
           style = "text-align:left",
           h2('Tournament')
         ),
         div(
           style = "display:inline-block; vertical-align:center; width:300px; padding-right:15px",
           textInput(
             inputId = 'res_url',
             label = NULL,
             value = '',
             placeholder = 'chess-results link'
           )
         ),
         div(
           style = "display:inline-block; vertical-align:center; width:80px",
           actionButton(
             inputId = 'imp_res_but',
             label = 'Import',
             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
           )
         ),
         div(
           style = "display:inline-block; vertical-align:bottom; width:320px",
           verbatimTextOutput('imp_status')
         )
  )
)


elo_chg_row<- fluidRow(
  column(12,
         div(
           style = "display:inline-block; vertical-align:top; width:200px; padding-right:15px",
           actionButton(
             inputId = 'reset_tour',
             label = 'Reset all',
             style = "color: #fff; background-color: #f44336; border-color: #2e6da4"
           )
         ),
         div(
           style = "display:inline-block; vertical-align:bottom; text-align:right; width:335px; padding-right:15px",
           h3('Total Rating Change: ')
         ),
         div(
           style = "display:inline-block; vertical-align:bottom; width:180px; padding-right:15px",
           verbatimTextOutput('elo_chg')
         )
  )
)


tour_div<- fluidRow(
  column(12,
         div(
           imp_res_row,
           label_row,
           opp_row(1),
           opp_row(2),
           opp_row(3),
           opp_row(4),
           opp_row(5),
           opp_row(6),
           opp_row(7),
           opp_row(8),
           opp_row(9),
           elo_chg_row
         )
  )
)


summary_row<- fluidRow(
  column(12,
      div(
        style = summ_style('Label'),
        h4('Number of Rated Games: '),
      ),
      div(
        style = summ_style('Value'),
        verbatimTextOutput('num_games')
      ),
      br(),
      div(
        style = summ_style('Label'),
        h4("Opponent's Avg. Rating: "),
      ),
      div(
        style = summ_style('Value'),
        verbatimTextOutput('elo_avg')
      ),
      br(),
      div(
        style = summ_style('Label'),
        h4('Points: '),
      ),
      div(
        style = summ_style('Value'),
        verbatimTextOutput('res_sum')
      ),
      br(),
      div(
        style = summ_style('Label'),
        h4('Performance Rating: ')
      ),
      div(
        style = summ_style('Value'),
        verbatimTextOutput('tpr')
      )

  )
)

tar_norm_row<- fluidRow(
  column(12,
     div(
       style = "display:inline-block; vertical-align:center; width:200px",
       h3('Targetted Norm: ')
     ),
     div(
       style = "display:inline-block; vertical-align:center; width:100px",
       selectInput(
         inputId = 'norm',
         label = NULL,
         choices = colnames(norm_cond),
         selected = 'IM'
       )
     ),
     br(),
     div(
       style = "display:inline-block; vertical-align:bottom; width:200px",
       h4('Adj. Rating Floor: ')
     ),
     div(
       style = "display:inline-block; vertical-align:bottom; width:100px",
       verbatimTextOutput('adj_elo')
     ),
     br(),
     div(
       h4('Conditions:'),
       tableOutput('cond_table'),
       h5('Other conditions:'),
       tags$ul(
         tags$li('Min 2 foreign opponents.'),
         tags$li('Max 2 unrated opponents.'),
       ),
       h5('For more detailed conditions, check ', 
          tags$a(href="https://handbook.fide.com/chapter/B012022", 'FIDE website','.')
       )
     )
  )
)

ui<-fluidPage(
  div(
    style = 'text-align:center',
    h1("FIDE Rating, Performance and Norm Calculator")
  ),
  br(),
  fluidRow(
    column(8,
       player_row,
       br(),
       tour_div
    ),
    column(4,
       h2('Summary'),
       summary_row,
       hr(),
       tar_norm_row
    )
  )
  

)

# server ------------------------------------------------------------------


d_to_PD<- function(d){
  
  if (d>0){
    D<- (d)
    H<- TRUE
  } else {
    D<- (-d)
    H<- FALSE
  }
  
  tab_len<- dim(d_pd_table)[1]
  
  PD<- d_pd_table[tab_len, 2]
  
  for (i in tab_len:1){
    if (D >= d_pd_table$d[i]){
      PD<- d_pd_table$pd[i]
      break
    }
  }
  
  if (!H) {PD<- 1-PD}
  
  return (PD)
  
}

elo_change<- function(p_elo, o_elo, res, k){
  
  if (o_elo == 0){
    elo_chg<- 0
  } else {
    d<- p_elo - o_elo
    
    PD<- d_to_PD(d)
    score_diff<- res - PD
    
    elo_chg<- round(score_diff*k, digits = 2)
  }
  
  return(elo_chg)
  
}


read_url<- function(res_url){
  res_html<- tryCatch(
    {
      read_html(res_url)
    },
    error = function(cond){
      status_msg<- 'Error'
      return(NULL)
    },
    warning = function(cond){
      status_msg<- 'Error'
      return(NULL)
    }
  )
    
  if (!is.null(res_html)) {
    
    main_tables<- html_nodes(res_html, ".CRs1")
    
    if (length(main_tables) != 2){
      return(list('msg' = 'Error'))
      
    } else {
      player_info<- data.frame(html_table(main_tables[[1]], header=FALSE), row.names = 1) %>% setNames('Value')
      full_res_table<- data.frame(html_table(main_tables[[2]], header=TRUE)) %>% filter(!is.na(`Rd.`))
      
      title_col<- names(full_res_table)[which(names(full_res_table)=='Name')-1][1]
      
      res_table<- full_res_table %>% select(c('Rd.', all_of(title_col), 'Name', 'Rtg', 'Res.')) %>% 
        setNames(c('rd', 'title', 'name', 'elo', 'res')) %>% arrange(rd)
      
      res_table$title<- res_table$title %>% replace(!res_table$title %in% names(title_lvl[1:6]), values = 'Others') 
      res_table$elo<- as.numeric(res_table$elo) %>% replace(!res_table$elo %in% 0:4000, values = 0)
      res_table$res<- res_table$res %>% replace(!res_table$res %in% c('1','0'), values = '0.5')
      
      #find player rating
      if ('Rating international' %in% rownames(player_info)){
        player_elo<- player_info['Rating international',][1]
      } else if ('Rating' %in% rownames(player_info)){
        player_elo<- player_info['Rating',][1]
      } else if ('Rating national' %in% rownames(player_info)){
        player_elo<- player_info['Rating national',][1]
      } else{
        player_elo<- 0
      }
      
      if (player_elo >=2400){
        player_k<- 10
      } else{
        player_k<- 20
      }
      
      status_msg<- 'Done importing, please check.'
      return(list('res_table'=res_table, 'player_elo'=player_elo, 'player_k'=player_k, 'msg'=status_msg))
    }
  } else{
    return(list('msg' = 'Error'))
  }
  
  
}


max_games<- 9

server<- function(input, output){

  # save input in vector, calculate change
  title_vec<- reactive({
    c(
      title_lvl[[input$title_1]],
      title_lvl[[input$title_2]],
      title_lvl[[input$title_3]],
      title_lvl[[input$title_4]],
      title_lvl[[input$title_5]],
      title_lvl[[input$title_6]],
      title_lvl[[input$title_7]],
      title_lvl[[input$title_8]],
      title_lvl[[input$title_9]]
    )
  })

  elo_vec<- reactive({
    c(
      as.numeric(input$elo_1),
      as.numeric(input$elo_2),
      as.numeric(input$elo_3),
      as.numeric(input$elo_4),
      as.numeric(input$elo_5),
      as.numeric(input$elo_6),
      as.numeric(input$elo_7),
      as.numeric(input$elo_8),
      as.numeric(input$elo_9)
    )
  })

  res_vec<- reactive({
    c(
      as.numeric(input$res_1),
      as.numeric(input$res_2),
      as.numeric(input$res_3),
      as.numeric(input$res_4),
      as.numeric(input$res_5),
      as.numeric(input$res_6),
      as.numeric(input$res_7),
      as.numeric(input$res_8),
      as.numeric(input$res_9)
    )
  })
  
  elo_chg_vec<- reactive({
    
    p_elo<- input$player_elo
    k<- input$player_k
    
    c(
      elo_change(p_elo, elo_vec()[1], res_vec()[1], k),
      elo_change(p_elo, elo_vec()[2], res_vec()[2], k),
      elo_change(p_elo, elo_vec()[3], res_vec()[3], k),
      elo_change(p_elo, elo_vec()[4], res_vec()[4], k),
      elo_change(p_elo, elo_vec()[5], res_vec()[5], k),
      elo_change(p_elo, elo_vec()[6], res_vec()[6], k),
      elo_change(p_elo, elo_vec()[7], res_vec()[7], k),
      elo_change(p_elo, elo_vec()[8], res_vec()[8], k),
      elo_change(p_elo, elo_vec()[9], res_vec()[9], k)
    )
  })
  
  
  #show elo change per game
  output$elo_1<- renderText(elo_chg_vec()[1])
  output$elo_2<- renderText(elo_chg_vec()[2])
  output$elo_3<- renderText(elo_chg_vec()[3])
  output$elo_4<- renderText(elo_chg_vec()[4])
  output$elo_5<- renderText(elo_chg_vec()[5])
  output$elo_6<- renderText(elo_chg_vec()[6])
  output$elo_7<- renderText(elo_chg_vec()[7])
  output$elo_8<- renderText(elo_chg_vec()[8])
  output$elo_9<- renderText(elo_chg_vec()[9])
  
  #Calculate Summary
  active_rd<- reactive(which(elo_vec() > 0))
  num_games<- reactive(length(active_rd()))
  elo_avg<- reactive({
    if (num_games()==0){
      return(0)
    } else{
      prec_ra<- mean(elo_vec()[active_rd()])
      int_ra<- floor(prec_ra+0.5)
      return(int_ra)  
    }
  })
  res_sum<- reactive(sum(res_vec()[active_rd()]))
  elo_chg<- reactive(sum(elo_chg_vec()[active_rd()]))
  
  tpr<- reactive({
    if (num_games()==0){
      dp<- 0
    } else{
      perc<- floor(res_sum()*100/num_games()+0.5)/100
      dp_idx<- which(p_dp_table$p == perc)
      dp<- p_dp_table$dp[dp_idx]
    }

    return(elo_avg()+dp)
  })
  
  output$num_games<- renderText(toString(num_games()))
  output$elo_avg<- renderText(toString(elo_avg()))
  output$res_sum<- renderText(toString(res_sum()))
  output$elo_chg<- renderText(toString(elo_chg()))
  output$tpr<- renderText(toString(tpr()))
  
  # Calculate Norm
  adj_elo_avg<- reactive({
    
    floor_elo<- norm_cond[[input$norm]][1]
    
    if (num_games()!=0){
      min_opp_elo<- min(elo_vec()[active_rd()])
      
      if (min_opp_elo < floor_elo){
        new_sum<- sum(elo_vec()[active_rd()]) - min_opp_elo + floor_elo
        new_avg<- new_sum/num_games()
        new_avg<- floor(new_avg+0.5)
        ret_msg<- paste0(min_opp_elo, '->', floor_elo)
        
      } else {
        new_avg<- elo_avg()
        ret_msg<- 'None'
      }
      
    } else{
      new_avg<- 0
      ret_msg<- 'None'
    }
    
    return(list('avg'=new_avg, 'msg'=ret_msg))
  })
  
  cond_table<- reactive({
    max_points<- 7
    elo_points_vec<- norm_cond[[input$norm]][9:2]
    
    min_ra<- tail(elo_points_vec, 1)
    
    
    req_points<- 99
    for (i in 1:length(elo_points_vec)){
      if (adj_elo_avg()$avg >= elo_points_vec[i]){
        req_points<- max_points - (8-i)/2
        break
      }
    }

    req_cond<- c(9, 3, 5, min_ra, req_points)
    
    norm_lvl<- title_lvl[[input$norm]]
    act_MO<- sum(title_vec()[active_rd()] <= norm_lvl)
    act_TH<- sum(title_vec()[active_rd()] <= 6)
    act_cond<- c(num_games(), act_MO, act_TH, adj_elo_avg()$avg, res_sum())
  
    final_table<- data.frame('Required' = req_cond, 'Actual' = act_cond)
    rownames(final_table)<- cond_name
    
    return(final_table)
    
  })

  output$cond_table<- renderTable(cond_table(), rownames = TRUE)
  
  output$adj_elo<- renderText({adj_elo_avg()$msg})
  
  # for chess results import
  observeEvent(input$imp_res_but, 
  {
    url_info<- read_url(input$res_url)
    status_msg<- url_info$msg
    
    if (status_msg != 'Error'){
      
      player_elo<- url_info$player_elo
      player_k<- url_info$player_k
      
      updateNumericInput(
        inputId = 'player_elo',
        value = player_elo
      )
      
      updateNumericInput(
        inputId = 'player_k',
        value = player_k
      )
      
      res_table<- url_info$res_table
      
      for (i in 1:9){
        curr_rd<- toString(i)
        t_id<- paste0('title_', curr_rd)
        name_id<- paste0('name_', curr_rd)
        elo_id<- paste0('elo_', curr_rd)
        res_id<- paste0('res_', curr_rd)
        
        if (i<=nrow(res_table)){
          
          updateSelectInput(
            inputId = t_id,
            selected = res_table$title[i]
          )
          
          updateTextInput(
            inputId = name_id,
            value = res_table$name[i]
          )
          
          updateNumericInput(
            inputId = elo_id,
            value = res_table$elo[i]
          )
          
          updateSelectInput(
            inputId = res_id,
            selected = as.numeric(res_table$res[i])
          )
        } else{
          updateSelectInput(
            inputId = t_id,
            selected = 'Others'
          )
          
          updateTextInput(
            inputId = name_id,
            value = ""
          )
          
          updateNumericInput(
            inputId = elo_id,
            value = 0
          )
          
          updateSelectInput(
            inputId = res_id,
            selected = 0.5
          )
        }
        
        
      }
    }
    
    output$imp_status<- renderText({
      status_msg
    })
    
  })
  
  
  
  
  
  observeEvent(input$reset_tour,
  {
    updateNumericInput(
      inputId = 'player_elo',
      value = 2000
    )
    
    updateNumericInput(
      inputId = 'player_k',
      value = 20
    )
    
    output$imp_status<- renderText({NULL})
    
    
    for (i in 1:9){
      curr_rd<- toString(i)
      t_id<- paste0('title_', curr_rd)
      name_id<- paste0('name_', curr_rd)
      elo_id<- paste0('elo_', curr_rd)
      res_id<- paste0('res_', curr_rd)
      
      updateSelectInput(
        inputId = t_id,
        selected = 'Others'
      )
      
      updateTextInput(
        inputId = name_id,
        value = NULL
      )
      
      updateNumericInput(
        inputId = elo_id,
        value = 0
      )
      
      updateSelectInput(
        inputId = res_id,
        selected = 0.5
      )
    }
  })
  
}


# Run App -----------------------------------------------------------------


shinyApp(ui=ui, server=server)