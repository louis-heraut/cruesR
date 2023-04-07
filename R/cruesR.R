# Copyright 2023 Louis Héraut (louis.heraut@inrae.fr)*1
#                     
# *1   INRAE, France
#
# This file is part of cruesR R package.
#
# cruesR R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# cruesR R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with cruesR R package.
# If not, see <https://www.gnu.org/licenses/>.


library(shiny)

# Personnal colors
grey99COL = "#fcfcfc"
grey98COL = "#fafafa"
grey97COL = "#f7f7f7"
grey94COL = "#f0f0f0"
grey90COL = "#e5e5e5"
grey85COL = "#d9d9d9"
grey75COL = "#bfbfbf"
grey70COL = "#b3b3b3"
grey65COL = "#a6a6a6"
grey60COL = "#999999"
grey50COL = "#808080"
grey40COL = "#666666"
grey30COL = "#4d4d4d"
grey20COL = "#333333"
grey18COL = "#2e2e2e"
grey15COL = "#262626"
grey9COL = "#171717"

yellowCOL = "#fddc5c"
orangeCOL = "#ffa62b"
redCOL = "#dc343b"

lightCyanCOL = "#66c1bf"
midCyanCOL = "#008c8e"
darkCyanCOL = "#275662"
INRAECyanCOL = "#00a3a6"


# Define UI
ui = fluidPage(
    
    tags$head(tags$script('
	var dimension = [0, 0];
	$(document).on("shiny:connected", function(e) {
		dimension[0] = window.innerWidth;
		dimension[1] = window.innerHeight;
		Shiny.onInputChange("dimension", dimension);
	});
	$(window).resize(function(e) {
		dimension[0] = window.innerWidth;
		dimension[1] = window.innerHeight;
		Shiny.onInputChange("dimension", dimension);
	});
    ')),

    fileInput("upload", ""),
    fluidRow(column(3, htmlOutput("lim")),
             column(1, offset=1,
                    actionButton("store_button", HTML("<b>+</b>"))),
             column(1, actionButton("remove_button", HTML("<b>-</b>"))),
             column(1, actionButton("reset_button", HTML("<b>X</b>")))),
    plotly::plotlyOutput("plot", width="auto", height="auto"),
    tags$br(),
    fluidRow(column(10, textOutput("info")),
             column(1, downloadButton("downloadData", "")))
)

# Define server
#' @title server
#' @description ...
#' @param input ...
#' @param output ...
#' @param session ...
#' @return ...
#' @examples
#' ...
#' @export
server = function(input, output, session) {
    library(shiny)
    session$onSessionEnded(stopApp)
    
    rv = reactiveValues(lim=NULL,
                        serie=0,
                        data=NULL,
                        idDate=NA,
                        idValue=NA,
                        zoom=NULL,
                        store=dplyr::tibble(),
                        store_full=dplyr::tibble())

    observeEvent({
        input$upload
    }, {
        if (!is.null(input$upload)) {
            rv$data = dplyr::as_tibble(read.table(
                                 file=input$upload$name,
                                 header=TRUE,
                                 sep=";",
                                 quote='"'))

            for (j in 1:ncol(rv$data)) {
                if (is.factor(rv$data[[j]])) {
                    d = try(as.Date(rv$data[[1, j]],
                                    tryFormats=c("%Y-%m-%d",
                                                 "%Y/%m/%d",
                                                 "%Y%m%d")))
                    test = nchar(as.character(rv$data[[1, j]])) > 10
                    if("try-error" %in% class(d) || is.na(d) | test) {
                        rv$data[j] = as.character(rv$data[[j]])
                    } else {
                        rv$data[j] = as.Date(rv$data[[j]])
                    }
                }
            }
            
            rv$idDate = 1
            rv$idValue = 2
            rv$data[[rv$idDate]] =
                as.POSIXct(as.character(rv$data[[rv$idDate]]),
                           tryFormats=c("%Y-%m-%d %H:%M:%OS",
                                        "%Y/%m/%d %H:%M:%OS",
                                        "%Y-%m-%d %H:%M",
                                        "%Y/%m/%d %H:%M",
                                        "%Y-%m-%d",
                                        "%Y/%m/%d",
                                        "%Y%m%d"))
        } else {
            rv$data = NULL
        }
    })
    
    # React to changes in date range input
    observeEvent(rv$data, {
        if (!is.null(rv$data)) {
            output$plot = plotly::renderPlotly({
                
                shiny::validate(need(!is.null(input$upload),
                                     message=FALSE))
                
                p = plotly::plot_ly()

                x = rv$data[[rv$idDate]]
                y = rv$data[[rv$idValue]]
                x_label = names(rv$data)[rv$idDate]
                y_label = names(rv$data)[rv$idValue]
                
                p = plotly::add_trace(
                                p,
                                type="scatter",
                                mode="lines",
                                x=x,
                                y=y,
                                line=list(color=grey20COL, width=0.85),
                                xhoverformat="%d/%m/%Y",
                                hovertemplate = paste0(
                                    x_label, " ", "%{x}<br>",
                                    "<b>", y_label, "</b> %{y}",
                                    "<extra></extra>"),
                                hoverlabel=list(
                                    bgcolor=INRAECyanCOL,
                                    font=list(size=12,
                                              color="white"),
                                    bordercolor="white"))

                p = plotly::layout(
                                p,
                                
                                autosize=FALSE,
                                separators='. ',
                                width=input$dimension[1]-50,
                                height=400,
                                margin=list(l=0,
                                            r=0,
                                            b=0,
                                            t=0,
                                            pad=0),
                                
                                xaxis=list(showgrid=FALSE,
                                           ticks="outside",
                                           tickcolor=grey75COL,
                                           tickfont=
                                               list(color=grey40COL),
                                           showline=TRUE,
                                           linewidth=2,
                                           linecolor=grey85COL,
                                           showticklabels=TRUE,
                                           mirror="all"),
                                
                                yaxis=list(
                                    range=c(0, max(y)*1.05),
                                    title=list(
                                        text=paste0(
                                            "<b>",
                                            y_label,
                                            "</b>"),
                                        font=
                                            list(color=grey20COL)),
                                    gridcolor=grey85COL,
                                    gridwidth=0.6,
                                    ticks="outside",
                                    tickcolor=grey75COL,
                                    tickfont=list(color=grey40COL),
                                    showline=TRUE,
                                    linewidth=2,
                                    linecolor=grey85COL,
                                    zerolinecolor=grey85COL,
                                    zerolinewidth=0.6,
                                    mirror=TRUE,
                                    fixedrange=TRUE),

                                autosize=FALSE,
                                plot_bgcolor=grey97COL,
                                paper_bgcolor='transparent',
                                showlegend=FALSE)

                p = plotly::config(
                                p,
                                locale="fr",
                                displaylogo=FALSE,
                                toImageButtonOptions =
                                    list(format="svg"),
                                modeBarButtonsToRemove =
                                    list("lasso2d",
                                         "select2d",
                                         "drawline",
                                         # "zoom2d",
                                         "drawrect",
                                         "autoScale2d",
                                         "hoverCompareCartesian",
                                         "hoverClosestCartesian")
                            )
                p = plotly::event_register(p, 'plotly_relayout')
            })
        } else {
            output$plot = plotly::renderPlotly({
                p = plotly::plot_ly()
                p = plotly::event_register(p, 'plotly_relayout')
            })
        }
    })

    observe({
        rv$zoom = plotly::event_data("plotly_relayout")
    })

    observeEvent({
        rv$zoom
        rv$data
    }, {        
        if(is.null(rv$zoom) ||
           names(rv$zoom[1]) %in% c("xaxis.autorange", "width")) {
            xmin = min(rv$data[[rv$idDate]])
            xmax = max(rv$data[[rv$idDate]])
        } else {
            xmin = gsub("[.][[:digit:]]+$", "", rv$zoom$`xaxis.range[0]`)
            xmax = gsub("[.][[:digit:]]+$", "", rv$zoom$`xaxis.range[1]`)
            xmin = as.POSIXct(xmin,
                              tryFormats=c("%Y-%m-%d %H:%M:%OS",
                                           "%Y/%m/%d %H:%M:%OS",
                                           "%Y-%m-%d %H:%M",
                                           "%Y/%m/%d %H:%M",
                                           "%Y-%m-%d",
                                           "%Y/%m/%d",
                                           "%Y%m%d"))
            xmax = as.POSIXct(xmax,
                              tryFormats=c("%Y-%m-%d %H:%M:%OS",
                                           "%Y/%m/%d %H:%M:%OS",
                                           "%Y-%m-%d %H:%M",
                                           "%Y/%m/%d %H:%M",
                                           "%Y-%m-%d",
                                           "%Y/%m/%d",
                                           "%Y%m%d"))

        }
        rv$lim = c(xmin, xmax)
    })
    
    observeEvent(input$store_button, {
        rv$serie = rv$serie + 1
        data = dplyr::filter(rv$data,
                             rv$lim[1] <= rv$data[[rv$idDate]] &
                             rv$data[[rv$idDate]] <= rv$lim[2])
        rv$store_full = dplyr::bind_rows(
                                   rv$store_full,
                                   dplyr::tibble(serie=rv$serie,
                                                 data))
        rv$store = dplyr::bind_rows(
                             rv$store,
                             dplyr::tibble(start=rv$lim[1],
                                           end=rv$lim[2]))
    })

    observeEvent(input$remove_button, {
        rv$store = rv$store[1:(nrow(rv$store)-1),]
        rv$store_full = dplyr::filter(rv$store_full, serie != rv$serie)
        rv$serie = rv$serie - 1
    })

    observeEvent(input$reset_button, {
        rv$serie = 0
        rv$store = dplyr::tibble()
        rv$store_full = dplyr::tibble()
    })

    lim = reactive({
        if (is.null(rv$lim)) {
            " "
        } else {
            paste0("from ", "<b>", rv$lim[1], "</b>",
                   " to ", "<b>", rv$lim[2], "</b>")
        }
    })
    output$lim = renderUI({
        HTML(lim())
    })

    info = reactive({
        if (nrow(rv$store) == 0) {
            ""
        } else {
            paste0(paste0("[", rv$store$start, " / ",
                          rv$store$end, "]"),
                   collapse=" ")
        }
    })
    output$info = renderText({
        info()
    })

    output$downloadData = downloadHandler(
        filename = function () {
            paste0("period_selection_for_", input$filename, ".txt")
        },
        content = function (file) {
            write.table(rv$store_full,
                        file,
                        sep=";",
                        row.names=FALSE)
        }
    )
}

#' @title float
#' @description ...
#' @param input ...
#' @param output ...
#' @param session ...
#' @return ...
#' @examples
#' ...
#' @export
float = function () {
    library(shiny)
    shinyApp(ui, server)
}
