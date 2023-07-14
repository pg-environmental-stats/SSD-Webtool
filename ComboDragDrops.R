DD.3 <- {fluidPage(
  fluidRow(
    class = "panel panel-heading",
    #      div(
    #        class = "panel-heading",
    #        h3("Drag variables to roles")
    #      ),
    fluidRow(
      class = "panel-body",
      column(
        width = 12,
        tags$div(
          class = "panel panel-default",
          tags$div(class = "panel-heading", "Drag variables to their proper roles below.  Each field must be filled."),
          tags$div(
            class = "panel-body",
            id = "sort1",
            tags_from_names(names(testData))
          )
        ),
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Exposure Conc"
          ),
          tags$div(
            class = "panel-body",
            id = "sort2"
          )
        ),
        # analyse as y
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Response Counts"
          ),
          tags$div(
            class = "panel-body",
            id = "sort3"
          )
        ),
        # analyse as z
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Group Sizes"
          ),
          tags$div(
            class = "panel-body",
            id = "sort4"
          )
        )

      )
    )
  ),
  sortable_js(
    "sort1",
    options = sortable_options(
      group = list(
        name = "sortGroup1",
        put = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      sort = FALSE,
      onSort = sortable_js_capture_input("sort_vars")
    )
  ),
  sortable_js(
    "sort2",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_x")
    )
  ),
  sortable_js(
    "sort3",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_y")
    )
  ),
  sortable_js(
    "sort4",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_z")
    )
  )
)
}
DD.2 <- {fluidPage(
  fluidRow(
    class = "panel panel-heading",
    div(
      class = "panel-heading",
      h3("Drag variables to roles")
    ),
    fluidRow(
      class = "panel-body",
      column(
        width = 12,
        tags$div(
          class = "panel panel-default",
          tags$div(class = "panel-heading", "Drag Variables to Roles.  Each field on right must be filled."),
          tags$div(
            class = "panel-body",
            id = "sort1",
            tags_from_names(names(testData))
          )
        ),
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Exposure Conc"
          ),
          tags$div(
            class = "panel-body",
            id = "sort2"
          )
        ),
        # analyse as y
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Responses"
          ),
          tags$div(
            class = "panel-body",
            id = "sort3"
          )
        )
      )      )
  ),
  sortable_js(
    "sort1",
    options = sortable_options(
      group = list(
        name = "sortGroup1",
        put = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      sort = FALSE,
      onSort = sortable_js_capture_input("sort_vars")
    )
  ),
  sortable_js(
    "sort2",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_x")
    )
  ),
  sortable_js(
    "sort3",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_y")
    )
  )
)
}
DD.SSD <- {fluidPage(
  fluidRow(
    class = "panel panel-heading",
    div(
      class = "panel-heading",
      h3("Drag variables to roles")
    ),
    fluidRow(
      class = "panel-body",
      column(
        width = 12,
        tags$div(
          class = "panel panel-default",
          tags$div(class = "panel-heading", "Each field below must be filled."),
          tags$div(
            class = "panel-body",
            id = "sort1",
            tags_from_names(namesInFrame)
          )
        ),
        # analyse as x
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Species Labels"
          ),
          tags$div(
            class = "panel-body",
            id = "sort2"
          )
        ),
        # analyse as y
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Responses/NOECs"
          ),
          tags$div(
            class = "panel-body",
            id = "sort3"
          )
        )
      )
    )
  ),
  sortable_js(
    "sort1",
    options = sortable_options(
      group = list(
        name = "sortGroup1",
        put = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      sort = FALSE,
      onSort = sortable_js_capture_input("sort_vars")
    )
  ),
  sortable_js(
    "sort2",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_x")
    )
  ),
  sortable_js(
    "sort3",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_y")
    )
  )
)
}
DD.SSD3 <- {fluidPage(
  fluidRow(
    class = "panel panel-heading",
    #      div(
    #        class = "panel-heading",
    #        h3("Drag variables to roles")
    #      ),
    fluidRow(
      class = "panel-body",
      column(
        width = 12,
        tags$div(
          class = "panel panel-default",
          tags$div(class = "panel-heading", "Drag variables to their proper roles below.  Each field must be filled."),
          tags$div(
            class = "panel-body",
            id = "sort1",
            tags_from_names(names(testData))
          )
        ),
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Species"
          ),
          tags$div(
            class = "panel-body",
            id = "sort2"
          )
        ),
        # analyse as y
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Response/NOEC"
          ),
          tags$div(
            class = "panel-body",
            id = "sort3"
          )
        ),
        # analyse as z
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$span(class = "glyphicon glyphicon-stats"),
            "Grouping Var"
          ),
          tags$div(
            class = "panel-body",
            id = "sort4"
          )
        )

      )
    )
  ),
  sortable_js(
    "sort1",
    options = sortable_options(
      group = list(
        name = "sortGroup1",
        put = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      sort = FALSE,
      onSort = sortable_js_capture_input("sort_vars")
    )
  ),
  sortable_js(
    "sort2",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_x")
    )
  ),
  sortable_js(
    "sort3",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_y")
    )
  ),
  sortable_js(
    "sort4",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      swap = TRUE,
      swapClass = "sortable-swap-highlight",
      onSort = sortable_js_capture_input("sort_z")
    )
  )
)
}
