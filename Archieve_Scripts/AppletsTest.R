# ============================================
# === STATISTICAL DISTRIBUTION APPLETS =====
# ============================================
# 
# Simplified Shiny app with 6 key distributions
# Features:
# - Minimal, focused inputs
# - Clear visualizations
# - Tooltips for student understanding
# ============================================

# Load required packages
library(shiny)
library(shinydashboard)
library(ggplot2)

# ============================================
# === UI DEFINITION ==========================
# ============================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Distribution Calculators"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Normal", tabName = "tab_normal"),
      menuItem("t-Distribution", tabName = "tab_t"),
      menuItem("Chi-Square", tabName = "tab_chisq"),
      menuItem("F-Distribution", tabName = "tab_f"),
      menuItem("Binomial", tabName = "tab_binomial"),
      menuItem("Poisson", tabName = "tab_poisson")
    )
  ),
  
  dashboardBody(
    
    # Add custom CSS for tooltips
    tags$head(
      tags$style(HTML("
        .tooltip-text {
          font-size: 12px;
          color: #666;
          font-style: italic;
          margin-top: -5px;
          margin-bottom: 10px;
        }
        .result-box {
          background-color: #f4f4f4;
          padding: 15px;
          border-radius: 5px;
          border-left: 4px solid #3c8dbc;
          margin-top: 10px;
        }
      "))
    ),
    
    tabItems(
      
      # =====================================
      # === NORMAL DISTRIBUTION TAB =========
      # =====================================
      tabItem(tabName = "tab_normal",
              fluidRow(
                box(
                  title = "Normal Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(4,
                           h4("Distribution Parameters"),
                           numericInput("norm_mean", "Mean (μ):", value = 0, step = 1),
                           div(class = "tooltip-text", "The center of the distribution"),
                           
                           numericInput("norm_sd", "Standard Deviation (σ):", 
                                        value = 1, min = 0.01, step = 0.1),
                           div(class = "tooltip-text", "Spread of the distribution (must be positive)"),
                           
                           hr(),
                           h4("Find Probability"),
                           
                           numericInput("norm_x", "Value (x):", value = 1.96, step = 0.1),
                           div(class = "tooltip-text", "The value you want to find probability for"),
                           
                           radioButtons("norm_tail", "Calculate:",
                                        choices = c("Left tail: P(X ≤ x)" = "left",
                                                    "Right tail: P(X > x)" = "right",
                                                    "Both tails: P(|X - μ| > |x - μ|)" = "both"),
                                        selected = "right"),
                           div(class = "tooltip-text", "Choose which area under the curve to calculate")
                    ),
                    
                    column(8,
                           plotOutput("norm_plot", height = "400px"),
                           div(class = "result-box",
                               h4("Result"),
                               verbatimTextOutput("norm_result")
                           )
                    )
                  )
                )
              )
      ),
      
      # =====================================
      # === t-DISTRIBUTION TAB ==============
      # =====================================
      tabItem(tabName = "tab_t",
              fluidRow(
                box(
                  title = "Student's t-Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(4,
                           h4("Distribution Parameters"),
                           numericInput("t_df", "Degrees of Freedom:", 
                                        value = 10, min = 1, step = 1),
                           div(class = "tooltip-text", "Usually n - 1 for one sample, or combined df for two samples"),
                           
                           hr(),
                           h4("Find Probability"),
                           
                           numericInput("t_x", "t-value:", value = 2.228, step = 0.1),
                           div(class = "tooltip-text", "The t-statistic from your analysis"),
                           
                           radioButtons("t_tail", "Calculate:",
                                        choices = c("Left tail: P(T ≤ t)" = "left",
                                                    "Right tail: P(T > t)" = "right",
                                                    "Both tails: P(|T| > |t|)" = "both"),
                                        selected = "both"),
                           div(class = "tooltip-text", "For two-sided tests, use 'Both tails'")
                    ),
                    
                    column(8,
                           plotOutput("t_plot", height = "400px"),
                           div(class = "result-box",
                               h4("Result"),
                               verbatimTextOutput("t_result")
                           )
                    )
                  )
                )
              )
      ),
      
      # =====================================
      # === CHI-SQUARE DISTRIBUTION TAB =====
      # =====================================
      tabItem(tabName = "tab_chisq",
              fluidRow(
                box(
                  title = "Chi-Square Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(4,
                           h4("Distribution Parameters"),
                           numericInput("chisq_df", "Degrees of Freedom:", 
                                        value = 5, min = 1, step = 1),
                           div(class = "tooltip-text", "Depends on your test (e.g., k - 1 for goodness of fit)"),
                           
                           hr(),
                           h4("Find Probability"),
                           
                           numericInput("chisq_x", "χ² value:", value = 11.07, min = 0, step = 0.1),
                           div(class = "tooltip-text", "The chi-square statistic from your test"),
                           
                           radioButtons("chisq_tail", "Calculate:",
                                        choices = c("Left tail: P(χ² ≤ x)" = "left",
                                                    "Right tail: P(χ² > x)" = "right"),
                                        selected = "right"),
                           div(class = "tooltip-text", "Chi-square tests typically use right tail")
                    ),
                    
                    column(8,
                           plotOutput("chisq_plot", height = "400px"),
                           div(class = "result-box",
                               h4("Result"),
                               verbatimTextOutput("chisq_result")
                           )
                    )
                  )
                )
              )
      ),
      
      # =====================================
      # === F-DISTRIBUTION TAB ==============
      # =====================================
      tabItem(tabName = "tab_f",
              fluidRow(
                box(
                  title = "F-Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(4,
                           h4("Distribution Parameters"),
                           numericInput("f_df1", "Numerator df:", 
                                        value = 3, min = 1, step = 1),
                           div(class = "tooltip-text", "Between-group degrees of freedom (k - 1)"),
                           
                           numericInput("f_df2", "Denominator df:", 
                                        value = 20, min = 1, step = 1),
                           div(class = "tooltip-text", "Within-group degrees of freedom (N - k)"),
                           
                           hr(),
                           h4("Find Probability"),
                           
                           numericInput("f_x", "F-value:", value = 3.10, min = 0, step = 0.1),
                           div(class = "tooltip-text", "The F-statistic from your ANOVA"),
                           
                           radioButtons("f_tail", "Calculate:",
                                        choices = c("Left tail: P(F ≤ x)" = "left",
                                                    "Right tail: P(F > x)" = "right"),
                                        selected = "right"),
                           div(class = "tooltip-text", "F-tests typically use right tail")
                    ),
                    
                    column(8,
                           plotOutput("f_plot", height = "400px"),
                           div(class = "result-box",
                               h4("Result"),
                               verbatimTextOutput("f_result")
                           )
                    )
                  )
                )
              )
      ),
      
      # =====================================
      # === BINOMIAL DISTRIBUTION TAB =======
      # =====================================
      tabItem(tabName = "tab_binomial",
              fluidRow(
                box(
                  title = "Binomial Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(4,
                           h4("Distribution Parameters"),
                           numericInput("binom_n", "Number of trials (n):", 
                                        value = 20, min = 1, step = 1),
                           div(class = "tooltip-text", "Total number of independent trials"),
                           
                           numericInput("binom_p", "Success probability (p):", 
                                        value = 0.5, min = 0, max = 1, step = 0.01),
                           div(class = "tooltip-text", "Probability of success on each trial (0 to 1)"),
                           
                           hr(),
                           h4("Find Probability"),
                           
                           numericInput("binom_k", "Number of successes (k):", 
                                        value = 12, min = 0, step = 1),
                           div(class = "tooltip-text", "The count you're interested in"),
                           
                           radioButtons("binom_type", "Calculate:",
                                        choices = c("Exactly k: P(X = k)" = "exact",
                                                    "At most k: P(X ≤ k)" = "left",
                                                    "At least k: P(X ≥ k)" = "right"),
                                        selected = "exact"),
                           div(class = "tooltip-text", "Type of probability to calculate")
                    ),
                    
                    column(8,
                           plotOutput("binom_plot", height = "400px"),
                           div(class = "result-box",
                               h4("Result"),
                               verbatimTextOutput("binom_result"),
                               hr(),
                               verbatimTextOutput("binom_stats")
                           )
                    )
                  )
                )
              )
      ),
      
      # =====================================
      # === POISSON DISTRIBUTION TAB ========
      # =====================================
      tabItem(tabName = "tab_poisson",
              fluidRow(
                box(
                  title = "Poisson Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(4,
                           h4("Distribution Parameters"),
                           numericInput("pois_lambda", "Rate (λ):", 
                                        value = 5, min = 0.01, step = 0.5),
                           div(class = "tooltip-text", "Average number of events per time period"),
                           
                           hr(),
                           h4("Find Probability"),
                           
                           numericInput("pois_k", "Number of events (k):", 
                                        value = 7, min = 0, step = 1),
                           div(class = "tooltip-text", "The count you're interested in"),
                           
                           radioButtons("pois_type", "Calculate:",
                                        choices = c("Exactly k: P(X = k)" = "exact",
                                                    "At most k: P(X ≤ k)" = "left",
                                                    "At least k: P(X ≥ k)" = "right"),
                                        selected = "exact"),
                           div(class = "tooltip-text", "Type of probability to calculate")
                    ),
                    
                    column(8,
                           plotOutput("pois_plot", height = "400px"),
                           div(class = "result-box",
                               h4("Result"),
                               verbatimTextOutput("pois_result"),
                               hr(),
                               verbatimTextOutput("pois_stats")
                           )
                    )
                  )
                )
              )
      )
    )
  )
)

# ============================================
# === SERVER LOGIC ===========================
# ============================================

server <- function(input, output, session) {
  
  # =====================================
  # === NORMAL DISTRIBUTION =============
  # =====================================
  
  # Generate plot for Normal distribution
  output$norm_plot <- renderPlot({
    
    # Get parameters
    mean_val <- input$norm_mean
    sd_val <- input$norm_sd
    x_val <- input$norm_x
    tail_type <- input$norm_tail
    
    # Create x-axis range: mean ± 4 standard deviations
    x_range <- c(mean_val - 4*sd_val, mean_val + 4*sd_val)
    x_seq <- seq(x_range[1], x_range[2], length.out = 500)
    y_seq <- dnorm(x_seq, mean_val, sd_val)
    
    # Create base data frame
    df_plot <- data.frame(x = x_seq, y = y_seq)
    
    # Determine shaded region based on tail type
    if (tail_type == "left") {
      df_shade <- df_plot[df_plot$x <= x_val, ]
    } else if (tail_type == "right") {
      df_shade <- df_plot[df_plot$x >= x_val, ]
    } else if (tail_type == "both") {
      # For two-tailed, shade both extremes
      distance <- abs(x_val - mean_val)
      df_shade <- df_plot[abs(df_plot$x - mean_val) >= distance, ]
    }
    
    # Create the plot
    ggplot() +
      geom_line(data = df_plot, aes(x = x, y = y), 
                color = "black", linewidth = 1.2) +
      geom_area(data = df_shade, aes(x = x, y = y), 
                fill = "steelblue", alpha = 0.6) +
      geom_vline(xintercept = x_val, color = "red", 
                 linetype = "dashed", linewidth = 1) +
      labs(title = paste0("Normal Distribution (μ = ", mean_val, ", σ = ", sd_val, ")"),
           x = "Value",
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", linewidth = 16))
    
  })
  
  # Generate result text for Normal distribution
  output$norm_result <- renderText({
    
    mean_val <- input$norm_mean
    sd_val <- input$norm_sd
    x_val <- input$norm_x
    tail_type <- input$norm_tail
    
    # Calculate probability based on tail type
    if (tail_type == "left") {
      prob <- pnorm(x_val, mean_val, sd_val)
      paste0("P(X ≤ ", round(x_val, 4), ") = ", round(prob, 6))
    } else if (tail_type == "right") {
      prob <- 1 - pnorm(x_val, mean_val, sd_val)
      paste0("P(X > ", round(x_val, 4), ") = ", round(prob, 6))
    } else if (tail_type == "both") {
      distance <- abs(x_val - mean_val)
      prob <- 2 * (1 - pnorm(mean_val + distance, mean_val, sd_val))
      paste0("P(|X - ", mean_val, "| > ", round(distance, 4), ") = ", round(prob, 6),
             "\n\nThis is the probability of being more than ", round(distance, 4),
             " units away from the mean in either direction.")
    }
  })
  
  # =====================================
  # === t-DISTRIBUTION ==================
  # =====================================
  
  # Generate plot for t-distribution
  output$t_plot <- renderPlot({
    
    df_val <- input$t_df
    t_val <- input$t_x
    tail_type <- input$t_tail
    
    # Create x-axis range based on df
    x_range <- qt(c(0.0001, 0.9999), df_val)
    x_seq <- seq(x_range[1], x_range[2], length.out = 500)
    y_seq <- dt(x_seq, df_val)
    
    df_plot <- data.frame(x = x_seq, y = y_seq)
    
    # Determine shaded region
    if (tail_type == "left") {
      df_shade <- df_plot[df_plot$x <= t_val, ]
    } else if (tail_type == "right") {
      df_shade <- df_plot[df_plot$x >= t_val, ]
    } else if (tail_type == "both") {
      df_shade <- df_plot[abs(df_plot$x) >= abs(t_val), ]
    }
    
    ggplot() +
      geom_line(data = df_plot, aes(x = x, y = y), 
                color = "black", linewidth = 1.2) +
      geom_area(data = df_shade, aes(x = x, y = y), 
                fill = "steelblue", alpha = 0.6) +
      geom_vline(xintercept = t_val, color = "red", 
                 linetype = "dashed", linewidth = 1) +
      labs(title = paste0("t-Distribution (df = ", df_val, ")"),
           x = "t-value",
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", linewidth = 16))
  })
  
  # Generate result text for t-distribution
  output$t_result <- renderText({
    
    df_val <- input$t_df
    t_val <- input$t_x
    tail_type <- input$t_tail
    
    if (tail_type == "left") {
      prob <- pt(t_val, df_val)
      paste0("P(T ≤ ", round(t_val, 4), ") = ", round(prob, 6))
    } else if (tail_type == "right") {
      prob <- 1 - pt(t_val, df_val)
      paste0("P(T > ", round(t_val, 4), ") = ", round(prob, 6),
             "\n\nThis is your p-value for a one-tailed test.")
    } else if (tail_type == "both") {
      prob <- 2 * (1 - pt(abs(t_val), df_val))
      paste0("P(|T| > ", round(abs(t_val), 4), ") = ", round(prob, 6),
             "\n\nThis is your p-value for a two-tailed test.")
    }
  })
  
  # =====================================
  # === CHI-SQUARE DISTRIBUTION =========
  # =====================================
  
  # Generate plot for chi-square distribution
  output$chisq_plot <- renderPlot({
    
    df_val <- input$chisq_df
    x_val <- input$chisq_x
    tail_type <- input$chisq_tail
    
    # Create x-axis range
    x_max <- max(x_val * 1.5, qchisq(0.995, df_val))
    x_seq <- seq(0, x_max, length.out = 500)
    y_seq <- dchisq(x_seq, df_val)
    
    df_plot <- data.frame(x = x_seq, y = y_seq)
    
    # Determine shaded region
    if (tail_type == "left") {
      df_shade <- df_plot[df_plot$x <= x_val, ]
    } else if (tail_type == "right") {
      df_shade <- df_plot[df_plot$x >= x_val, ]
    }
    
    ggplot() +
      geom_line(data = df_plot, aes(x = x, y = y), 
                color = "black", linewidth = 1.2) +
      geom_area(data = df_shade, aes(x = x, y = y), 
                fill = "steelblue", alpha = 0.6) +
      geom_vline(xintercept = x_val, color = "red", 
                 linetype = "dashed", linewidth = 1) +
      labs(title = paste0("Chi-Square Distribution (df = ", df_val, ")"),
           x = "χ² value",
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", linewidth = 16))
  })
  
  # Generate result text for chi-square distribution
  output$chisq_result <- renderText({
    
    df_val <- input$chisq_df
    x_val <- input$chisq_x
    tail_type <- input$chisq_tail
    
    if (tail_type == "left") {
      prob <- pchisq(x_val, df_val)
      paste0("P(χ² ≤ ", round(x_val, 4), ") = ", round(prob, 6))
    } else if (tail_type == "right") {
      prob <- 1 - pchisq(x_val, df_val)
      paste0("P(χ² > ", round(x_val, 4), ") = ", round(prob, 6),
             "\n\nThis is your p-value for the chi-square test.")
    }
  })
  
  # =====================================
  # === F-DISTRIBUTION ==================
  # =====================================
  
  # Generate plot for F-distribution
  output$f_plot <- renderPlot({
    
    df1_val <- input$f_df1
    df2_val <- input$f_df2
    x_val <- input$f_x
    tail_type <- input$f_tail
    
    # Create x-axis range
    x_max <- max(x_val * 1.5, qf(0.995, df1_val, df2_val))
    x_seq <- seq(0.01, x_max, length.out = 500)
    y_seq <- df(x_seq, df1_val, df2_val)
    
    df_plot <- data.frame(x = x_seq, y = y_seq)
    
    # Determine shaded region
    if (tail_type == "left") {
      df_shade <- df_plot[df_plot$x <= x_val, ]
    } else if (tail_type == "right") {
      df_shade <- df_plot[df_plot$x >= x_val, ]
    }
    
    ggplot() +
      geom_line(data = df_plot, aes(x = x, y = y), 
                color = "black", linewidth = 1.2) +
      geom_area(data = df_shade, aes(x = x, y = y), 
                fill = "steelblue", alpha = 0.6) +
      geom_vline(xintercept = x_val, color = "red", 
                 linetype = "dashed", linewidth = 1) +
      labs(title = paste0("F-Distribution (df1 = ", df1_val, ", df2 = ", df2_val, ")"),
           x = "F-value",
           y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", linewidth = 16))
  })
  
  # Generate result text for F-distribution
  output$f_result <- renderText({
    
    df1_val <- input$f_df1
    df2_val <- input$f_df2
    x_val <- input$f_x
    tail_type <- input$f_tail
    
    if (tail_type == "left") {
      prob <- pf(x_val, df1_val, df2_val)
      paste0("P(F ≤ ", round(x_val, 4), ") = ", round(prob, 6))
    } else if (tail_type == "right") {
      prob <- 1 - pf(x_val, df1_val, df2_val)
      paste0("P(F > ", round(x_val, 4), ") = ", round(prob, 6),
             "\n\nThis is your p-value for the F-test (ANOVA).")
    }
  })
  
  # =====================================
  # === BINOMIAL DISTRIBUTION ===========
  # =====================================
  
  # Generate plot for Binomial distribution
  output$binom_plot <- renderPlot({
    
    n_val <- input$binom_n
    p_val <- input$binom_p
    k_val <- input$binom_k
    type <- input$binom_type
    
    # Create all possible values
    k_seq <- 0:n_val
    prob_seq <- dbinom(k_seq, n_val, p_val)
    
    df_plot <- data.frame(k = k_seq, prob = prob_seq)
    
    # Determine which bars to highlight
    if (type == "exact") {
      df_plot$highlight <- df_plot$k == k_val
    } else if (type == "left") {
      df_plot$highlight <- df_plot$k <= k_val
    } else if (type == "right") {
      df_plot$highlight <- df_plot$k >= k_val
    }
    
    ggplot(df_plot, aes(x = k, y = prob, fill = highlight)) +
      geom_bar(stat = "identity", width = 0.7) +
      scale_fill_manual(values = c("gray70", "steelblue")) +
      labs(title = paste0("Binomial Distribution (n = ", n_val, ", p = ", p_val, ")"),
           x = "Number of successes (k)",
           y = "Probability") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", linewidth = 16),
            legend.position = "none")
  })
  
  # Generate result text for Binomial distribution
  output$binom_result <- renderText({
    
    n_val <- input$binom_n
    p_val <- input$binom_p
    k_val <- input$binom_k
    type <- input$binom_type
    
    if (type == "exact") {
      prob <- dbinom(k_val, n_val, p_val)
      paste0("P(X = ", k_val, ") = ", round(prob, 6))
    } else if (type == "left") {
      prob <- pbinom(k_val, n_val, p_val)
      paste0("P(X ≤ ", k_val, ") = ", round(prob, 6))
    } else if (type == "right") {
      prob <- 1 - pbinom(k_val - 1, n_val, p_val)
      paste0("P(X ≥ ", k_val, ") = ", round(prob, 6))
    }
  })
  
  # Generate statistics for Binomial distribution
  output$binom_stats <- renderText({
    
    n_val <- input$binom_n
    p_val <- input$binom_p
    
    mean_val <- n_val * p_val
    sd_val <- sqrt(n_val * p_val * (1 - p_val))
    
    paste0("Distribution Properties:\n",
           "Mean (μ) = np = ", round(mean_val, 2), "\n",
           "Std Dev (σ) = √(np(1-p)) = ", round(sd_val, 2))
  })
  
  # =====================================
  # === POISSON DISTRIBUTION ============
  # =====================================
  
  # Generate plot for Poisson distribution
  output$pois_plot <- renderPlot({
    
    lambda_val <- input$pois_lambda
    k_val <- input$pois_k
    type <- input$pois_type
    
    # Create x-axis range: 0 to lambda + 4*sqrt(lambda)
    k_max <- max(k_val + 5, ceiling(lambda_val + 4*sqrt(lambda_val)))
    k_seq <- 0:k_max
    prob_seq <- dpois(k_seq, lambda_val)
    
    df_plot <- data.frame(k = k_seq, prob = prob_seq)
    
    # Determine which bars to highlight
    if (type == "exact") {
      df_plot$highlight <- df_plot$k == k_val
    } else if (type == "left") {
      df_plot$highlight <- df_plot$k <= k_val
    } else if (type == "right") {
      df_plot$highlight <- df_plot$k >= k_val
    }
    
    ggplot(df_plot, aes(x = k, y = prob, fill = highlight)) +
      geom_bar(stat = "identity", width = 0.7) +
      scale_fill_manual(values = c("gray70", "steelblue")) +
      labs(title = paste0("Poisson Distribution (λ = ", lambda_val, ")"),
           x = "Number of events (k)",
           y = "Probability") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", linewidth = 16),
            legend.position = "none")
  })
  
  # Generate result text for Poisson distribution
  output$pois_result <- renderText({
    
    lambda_val <- input$pois_lambda
    k_val <- input$pois_k
    type <- input$pois_type
    
    if (type == "exact") {
      prob <- dpois(k_val, lambda_val)
      paste0("P(X = ", k_val, ") = ", round(prob, 6))
    } else if (type == "left") {
      prob <- ppois(k_val, lambda_val)
      paste0("P(X ≤ ", k_val, ") = ", round(prob, 6))
    } else if (type == "right") {
      prob <- 1 - ppois(k_val - 1, lambda_val)
      paste0("P(X ≥ ", k_val, ") = ", round(prob, 6))
    }
  })
  
  # Generate statistics for Poisson distribution
  output$pois_stats <- renderText({
    
    lambda_val <- input$pois_lambda
    
    paste0("Distribution Properties:\n",
           "Mean (μ) = λ = ", round(lambda_val, 2), "\n",
           "Std Dev (σ) = √λ = ", round(sqrt(lambda_val), 2))
  })
  
}

# ============================================
# === RUN APPLICATION ========================
# ============================================

shinyApp(ui = ui, server = server)