grade_stocks <- function(df) {
  
  df_result <- df
  
  # --- 1. Market cap ---
  df_result$grade_market_cap <- ifelse(
    is.na(df[,1]), NA,
    ifelse(df[,1] < 100, 1,
           ifelse(df[,1] > 1000, 0.33, 0.66))
  )
  
  # --- 2. P/E ---
  df_result$grade_pe <- ifelse(
    is.na(df[,2]), NA,
    ifelse(df[,2] < 0, 0.33,
           ifelse(df[,2] < 20, 1, 0.66))
  )
  
  # --- 3. P/BV ---
  df_result$grade_pbv <- ifelse(
    is.na(df[,3]), NA,
    ifelse(df[,3] < 0, 0.33,
           ifelse(df[,3] < 3, 1, 0.66))
  )
  
  # --- 4. P/S ---
  df_result$grade_ps <- ifelse(
    is.na(df[,4]), NA,
    ifelse(df[,4] < 0, 0.33,
           ifelse(df[,4] < 2, 1, 0.66))
  )
  
  # --- 5. P/FCF ---
  df_result$grade_pfcf <- ifelse(
    is.na(df[,5]), NA,
    ifelse(df[,5] < 0, 0.33,
           ifelse(df[,5] < 20, 1, 0.66))
  )
  
  # --- 6. EV/EBITDA (depends on P/E) ---
  pe <- df[,2]
  ev <- df[,6]
  
  df_result$grade_ev_ebitda <- ifelse(
    is.na(pe) | is.na(ev), NA,
    ifelse(pe < 0,
           ifelse(ev < 0, 0.33,
                  ifelse(ev < 10, 1, 0.66)),
           1)
  )
  
  # --- 7. Debt/EBITDA (depends on P/E) ---
  debt <- df[,7]
  
  df_result$grade_debt_ebitda <- ifelse(
    is.na(pe) | is.na(debt), NA,
    ifelse(pe < 0,
           ifelse(debt < 0, 0.33,
                  ifelse(debt < 4, 1, 0.66)),
           ifelse(debt < 4, 1, 0.66))  # negative debt also gives 1 here
  )
  
  # --- Aggregations ---
  
  # 1) market cap grade (already exists)
  
  # 2) valuation product
  df_result$grade_valuation <- 
    df_result$grade_pe *
    df_result$grade_pbv *
    df_result$grade_ps *
    df_result$grade_pfcf *
    df_result$grade_ev_ebitda
  
  # 3) debt grade
  df_result$grade_debt <- df_result$grade_debt_ebitda
  
  # Final grade
  df_result$final_grade <- 
    df_result$grade_market_cap *
    df_result$grade_valuation *
    df_result$grade_debt
  
  # Multiply by 10 (as required)
  df_result$final_grade <- df_result$final_grade * 10
  
  # Sort descending (row names preserved automatically)
  df_result <- df_result[order(-df_result$final_grade, na.last = TRUE), ]
  
  return(df_result)
}
grade_stocks(sm_data5)
