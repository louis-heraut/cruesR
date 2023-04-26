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
    session$onSessionEnded(stopApp)
    
    rv = reactiveValues(filename=NULL,
                        lim=NULL,
                        serie=0,
                        data=NULL,
                        idDate=NA,
                        idValue=NA,
                        zoom=NULL,
                        store=dplyr::tibble(),
                        store_full=dplyr::tibble())

    
    observeEvent(input$upload, {
        if (!is.null(input$upload)) {
            rv$filename = gsub("[.].*$", "",
                               basename(input$upload$name))
            rv$data = dplyr::as_tibble(read.table(
                                 file=input$upload$datapath,
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
                maxValue_win = max(y, na.rm=TRUE)*1.05
                x_label = names(rv$data)[rv$idDate]
                y_label = names(rv$data)[rv$idValue]
                
                p = plotly::add_trace(
                                p,
                                type="scatter",
                                mode="lines",
                                x=x,
                                y=y,
                                line=list(color="#f0f0f0", width=1.2),
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

                if (nrow(rv$store) > 0) {
                    for (i in 1:nrow(rv$store)) {
                        p = plotly::add_trace(
                                        p,
                                        type="scatter",
                                        mode="lines",
                                        x=c(rv$store[[1]][i],
                                            rv$store[[1]][i],
                                            rv$store[[2]][i],
                                            rv$store[[2]][i],
                                            rv$store[[1]][i]),
                                        y=c(0, maxValue_win,
                                            maxValue_win, 0, 0),
                                        fill="toself",
                                        opacity=0.4,
                                        fillcolor=lightCyanCOL,
                                        line=list(width=0),
                                        text=paste0(
                                            "from <b>",
                                            rv$store[[1]][i], "</b>",
                                            " to <b>", rv$store[[2]][i],
                                            "</b>"),
                                        hoverinfo="text",
                                        hoveron="fills",
                                        hoverlabel=
                                            list(bgcolor=lightCyanCOL,
                                                 font=list(color="white",
                                                           size=12),
                                                 bordercolor="white"))
                    }
                }

                p = plotly::layout(
                                p,
                                
                                autosize=FALSE,
                                separators='. ',
                                width=input$dimension[1]-45,
                                height=340,
                                margin=list(l=0,
                                            r=0,
                                            b=0,
                                            t=0,
                                            pad=0),
                                
                                xaxis=list(showgrid=FALSE,
                                           ticks="outside",
                                           tickcolor="#516b90",
                                           tickfont=
                                               list(color="#b3c0d4"),
                                           showticklabels=TRUE),
                                
                                yaxis=list(
                                    range=c(0, maxValue_win),
                                    title=list(
                                        text=paste0(
                                            "<b>",
                                            y_label,
                                            "</b>"),
                                        font=
                                            list(color="#405472")),
                                    gridcolor="#1e2735",
                                    gridwidth=0.6,
                                    ticks="outside",
                                    tickcolor="#516b90",
                                    tickfont=list(color="#b3c0d4"),
                                    zerolinecolor="#1e2735",
                                    zerolinewidth=1.4,
                                    fixedrange=TRUE),

                                autosize=FALSE,
                                plot_bgcolor='transparent',
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
            xmin = min(rv$data[[rv$idDate]], na.rm=TRUE)
            xmax = max(rv$data[[rv$idDate]], na.rm=TRUE)
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
            "<font size='3' color='#00a3a6'>Select a period on the graph and click on the </font><font size='3' color='#66c1bf'><b>Store</b></font><font size='3' color='#00a3a6'> button</font>"
        } else {
            date_start = format(rv$store$start, "%d %b %Y")
            time_start = format(rv$store$start, "%H:%M:%S")
            date_end = format(rv$store$end, "%d %b %Y")
            time_end = format(rv$store$end, "%H:%M:%S")

            tmp = paste0(
                "<b><font size='3' color='#00a3a6'> ",
                date_start, "</font></b>",
                "<font size='2' color='#00a3a6'> ",
                time_start, "</font>",
                " / ",
                "<b><font size='3' color='#00a3a6'> ",
                date_end, "</font></b>",
                "<font size='2' color='#00a3a6'> ",
                time_end, "</font>")
            tmp[length(tmp)] = gsub("00a3a6", "66c1bf", tmp[length(tmp)])
            tmp = tail(tmp, n=4)
            
            paste0(tmp, collapse="&emsp;")
        }
    })

    output$info = renderUI({
        HTML(info())
    })

    observeEvent(input$download_button, {
        output$downloadData = downloadHandler(
            filename = function () {
                paste0("period_selection_for_",
                       rv$filename,
                       ".txt")
            },
            content = function (file) {
                write.table(rv$store_full,
                            file,
                            sep=";",
                            row.names=FALSE)
            }
        )
        jsinject = "setTimeout(function()
                        {window.open($('#downloadData')
                        .attr('href'))}, 100);"
        session$sendCustomMessage(type='jsCode',
                                  list(value=jsinject))
    })
    


}
