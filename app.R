library(shiny)
library(bslib)
library(ggplot2)

ui <- bslib::page_fluid(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  # Banner/header
  # Sticky full-width banner
  shiny::tags$div(
    class = "position-sticky top-0 bg-primary text-white p-3 shadow-sm mb-3",
    style = "z-index: 1020; width: 100vw; margin-left: calc(-50vw + 50%);",
    shiny::h1("Task Prioritization App", class = "mb-0")
  ),
  bslib::layout_columns(
    col_widths = 12,
    
    shiny::uiOutput("task_input_ui"),

    bslib::card(
      shiny::h2("Task List"),
      shiny::uiOutput("task_list_ui")
    ),
    
    shiny::uiOutput("comparison_section"),  
    shiny::uiOutput("results_section"),
    shiny::tags$script(shiny::HTML("
  $(document).on('keydown', function(e) {
    if (e.key === 'Enter' && $('#task_input').is(':focus')) {
      e.preventDefault();
      $('#add_task').click();
    }
  });
")),
    shiny::tags$script(shiny::HTML("
  Shiny.addCustomMessageHandler('focusTaskInput', function(message) {
    $('#task_input').focus();
  });
"))
  )
)

server <- function(input, output, session) {
  tasks <- shiny::reactiveVal(character())
  edit_index <- shiny::reactiveVal(NULL)
  votes <- shiny::reactiveVal(list())
  current_index <- shiny::reactiveVal(1)
  mode <- shiny::reactiveVal("entry")
  
  
  output$task_input_ui <- shiny::renderUI({
    
    if (mode() != "entry") return(NULL)
    
    bslib::card(
      title = "Enter Tasks",
      bslib::card_body(
        shiny::textInput("task_input", NULL, 
                  placeholder = "Type a task and click Add", width = "100%"),
        bslib::layout_columns(
          width = 6,
          shiny::actionButton("add_task", "Add / Update", class = "btn-primary"),
          shiny::actionButton("start_comparison", "Start Comparison", class = "btn-success")
        )
      ))
    
    
  })
  
  # Add or update task
  shiny::observeEvent(input$add_task, {
    new_task <- trimws(input$task_input)
    if (new_task == "")  return()
    if(new_task %in% tasks()){
      shiny::showNotification("This task has already been added.", 
                              type = "warning", duration = 3)
      shiny::updateTextInput(session, "task_input", value = "")
      return()
    }
    t <- tasks()
    idx <- edit_index()
    if (!is.null(idx)) {
      t[idx] <- new_task
      edit_index(NULL)
    } else {
      t <- c(t, new_task)
    }
    tasks(t)
    shiny::updateTextInput(session, "task_input", value = "")
    session$sendCustomMessage("focusTaskInput", TRUE)
  })
  
  # Edit or delete task
  shiny::observeEvent(input$edit_task, {
    idx <- as.numeric(input$edit_task)
    shiny::updateTextInput(session, "task_input", value = tasks()[idx])
    edit_index(idx)
  })
  
  shiny::observeEvent(input$delete_task, {
    idx <- as.numeric(input$delete_task)
    t <- tasks()
    tasks(t[-idx])
  })
  
  output$task_list_ui <- shiny::renderUI({
    t <- tasks()
    if (length(t) == 0) return(shiny::tags$em("No tasks added yet."))
    shiny::tagList(
      lapply(seq_along(t), function(i) {
        shiny::div(class = "d-flex justify-content-between align-items-center mb-2",
                   shiny::span(t[i]),
            if(mode() == "entry"){
              shiny::div(
                shiny::actionButton(paste0("edit_", i), "Edit", 
                                    class = "btn btn-sm btn-warning"),
                shiny::actionButton(paste0("delete_", i), "Delete", 
                                    class = "btn btn-sm btn-danger")
              )
            },
            shiny::tags$script(shiny::HTML(sprintf(
              "$('#edit_%d').on('click', function() { Shiny.setInputValue('edit_task', %d, {priority: 'event'}); });", i, i
            ))),
            shiny::tags$script(shiny::HTML(sprintf(
              "$('#delete_%d').on('click', function() { Shiny.setInputValue('delete_task', %d, {priority: 'event'}); });", i, i
            )))
        )
      })
    )
  })
  
  # Start comparison
  shiny::observeEvent(input$start_comparison, {
    t <- tasks()
    if (length(t) < 2) return(warning("<2 tasks selected. Not proceeding."))
    mode("compare")
    com <- combn(t, 2, simplify = TRUE) %>% 
      t() %>% 
      as.data.frame() %>% 
      dplyr::mutate(Result = NA_real_) %>% 
      dplyr::rename(Left = V1, Right = V2)
    votes(com)
    current_index(1)
  })
  
  # Voting
  shiny::observeEvent(input$vote_left, {
    shiny::req(mode() == "compare")
    pair <- c(votes()[current_index(),]$Left, votes()[current_index(),]$Right)
    key <- paste(pair, collapse = " vs ")
    v <- votes()
    v$Result[current_index()] <- pair[1]
    votes(v)
  })
  
  shiny::observeEvent(input$vote_right, {
    shiny::req(mode() == "compare")
    pair <- c(votes()[current_index(),]$Left, votes()[current_index(),]$Right)
    key <- paste(pair, collapse = " vs ")
    v <- votes()
    v$Result[current_index()] <- pair[2]
    votes(v)
  })
  
  shiny::observeEvent(input$prev_button, {
    idx <- current_index()
    if (idx > 1) current_index(idx - 1)
  })

  shiny::observeEvent(input$next_button, {
    idx <- current_index()
    if (idx < nrow(votes())) current_index(idx + 1)
  })
  
  shiny::observeEvent(input$finish_button, {
    mode("result")
  })
  
  output$comparison_section <- shiny::renderUI({
    
    if (mode() != "compare") return(NULL)
    
    shiny::req(current_index(), votes())
    
    
    
    pair <- c(votes()[current_index(),]$Left, votes()[current_index(),]$Right)
    key <- paste(pair, collapse = " vs ")
    selected <- votes()$Result[current_index()]
    

    nav_buttons <- bslib::layout_columns(
      width = c(6, 6),
      if (current_index() > 1) {
        shiny::actionButton("prev_button", "Back", class = "btn btn-light")
      } else {
        shiny::div()  # placeholder column for spacing
      },
      if (!is.null(selected)) {
        if (current_index() < nrow(votes())) {
          shiny::actionButton("next_button", "Next", class = "btn btn-primary")
        } else {
          shiny::actionButton("finish_button", "Finish", class = "btn btn-success")
        }
      } else {
        shiny::span()  # placeholder so layout stays balanced
      }
    )
    
    
    bslib::card(
      title = "Pairwise Comparison",
      bslib::card_body(
        shiny::h5("Which task has higher priority?"),
        shiny::div(class = "d-flex justify-content-center align-items-center gap-3",
                   shiny::actionButton("vote_left", pair[1],
                         class = if (isTRUE(selected == pair[1])) "btn btn-success" else "btn btn-outline-secondary"),
                   shiny::strong("vs."),
                   shiny::actionButton("vote_right", pair[2],
                         class = if (isTRUE(selected == pair[2])) "btn btn-success" else "btn btn-outline-secondary"
            )
        ),
        shiny::br(),
        nav_buttons
      )
    )
  })
  
  output$results_section <- shiny::renderUI({
    shiny::req(mode() == "result")
    bslib::card(
      title = "Results",
      bslib::card_body(
        shiny::h2("Pairwise Comparison Plot"),
        shiny::plotOutput("comparison_plot"),
        shiny::br(),
        shiny::h2("Task Prioritization"),
        shiny::tableOutput("ranking_table")
      )
    )
  })
  
  output$comparison_plot <- shiny::renderPlot({
    df <- votes()
    shiny::req(length(df) > 0)
    
    
    # Get all unique values
    all_items <- unique(c(df$Left, df$Right))
    
    # Create aliases (A-Z, AA, AB, etc. if needed)
    make_aliases <- function(n) {
      if (n <= 26) return(LETTERS[1:n])
      base <- expand.grid(LETTERS, LETTERS)
      c(LETTERS, apply(base, 1, paste0, collapse = ""))[1:n]
    }
    aliases <- make_aliases(length(all_items))
    
    # Create alias map
    alias_map <- tibble::tibble(
      label = all_items,
      alias = aliases
    )
    
    # Replace Left, Right, Result with aliases
    df_alias <- df %>%
      dplyr::left_join(alias_map, by = c("Left" = "label")) %>% dplyr::rename(Left_alias = alias) %>%
      dplyr::left_join(alias_map, by = c("Right" = "label")) %>% dplyr::rename(Right_alias = alias) %>%
      dplyr::left_join(alias_map, by = c("Result" = "label")) %>% dplyr::rename(Result_alias = alias)
    
    
    alias_index <- tibble::tibble(alias = aliases, id = seq_along(aliases))
    
    df_plot <- df_alias %>%
      dplyr::left_join(alias_index, by = c("Left_alias" = "alias")) %>% dplyr::rename(x = id) %>%
      dplyr::left_join(alias_index, by = c("Right_alias" = "alias")) %>% dplyr::rename(y = id)
    
    alias_key <- dplyr::left_join(alias_map, alias_index)
    
    summary_tbl <- df_alias %>%
      dplyr::mutate(Result_alias = factor(Result_alias, levels = alias_map$alias)) %>% 
      dplyr::count(Result_alias, name = "Count", .drop = FALSE) %>%
      dplyr::left_join(alias_map, by = c("Result_alias" = "alias")) %>% 
      dplyr::left_join(alias_index, by = c("Result_alias" = "alias"))
    
    p_matrix <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_tile(fill = "white", color = "black") +
      ggplot2::geom_text(ggplot2::aes(label = Result_alias), size = 5) +
      # diagonal
      ggplot2::geom_tile(data = alias_index, ggplot2::aes(x=id, y=id),
                fill = "gray", color = "black") +
      ggplot2::geom_text(data = alias_index, ggplot2::aes(x=id, y=id, label = alias),
                color = "red",
                size = 5)+
      # labels
      ggplot2::geom_text(data = alias_key, ggplot2::aes(x=id+1, y=id, label = label),
                color = "black",
                size = 5)+
      # Bottom row
      ggplot2::geom_tile(data = alias_index, ggplot2::aes(x=id, y=max(id+1)),
                fill = "gray", color = "black") +
      ggplot2::geom_text(data = alias_index, ggplot2::aes(x=id, y=max(id+1), label = alias),
                color = "red",
                size = 5)+
      # sum
      ggplot2::geom_tile(data = alias_index, ggplot2::aes(x=id, y=max(id)+2),
                fill = "white", color = "black") +
      ggplot2::geom_text(data = summary_tbl, ggplot2::aes(x=id, y=max(id+2), label = Count),
                color = "black",
                size = 5)+
      # Sum title
      ggplot2::annotate("tile", 
               x=max(alias_index$id)+1, 
               y=max(alias_index$id)+2,
               fill = "gray", color = "black")+
      ggplot2::annotate("text", 
               x=max(alias_index$id)+1, 
               y=max(alias_index$id)+2,
               label = "SUM",
               fontface = "bold",
               color = "black",
               size = 5)+
      # appearance
      ggplot2::scale_x_continuous(breaks = alias_index$id, 
                         labels = alias_index$alias,
                         # limits = c(min(alias_index$id)-0.5, max(alias_index$id)+0.5),
                         expand = c(0, 1)) +
      ggplot2::scale_y_reverse(breaks = c(max(alias_index$id): (max(alias_index$id)+2)), 
                      # labels = alias_index$alias,
                      # limits = c(max(alias_index$id)+0.5, min(alias_index$id)-0.5),
                      expand = c(0, 0)
      ) +
      ggplot2::coord_fixed(xlim = c(min(alias_index$id)-0.5, max(alias_index$id)+0.5),
                  ylim = c(max(alias_index$id)+2.5, min(alias_index$id)-0.5),
                  clip = "off") +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank())
    p_matrix
    

  })
  
  output$ranking_table <- shiny::renderTable({
    df <- votes()
    if (length(df) == 0) return(NULL)
    df %>% 
      dplyr::count(Result) %>% 
      dplyr::mutate(Result = factor(Result, levels = tasks())) %>% 
      tidyr::complete(Result, fill = list(n = 0)) %>% 
      dplyr::arrange(dplyr::desc(n)) %>% 
      dplyr::mutate(Rank = dplyr::row_number()) %>% 
      dplyr::rename(Count = n, Task = Result)
      
  })
  
  
  
}

shinyApp(ui, server)
