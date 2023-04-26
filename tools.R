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

file.choose = function(...) {
  pathname <- NULL;
  tryCatch({
    pathname <- file.choose();
  }, error = function(ex) {
  })
  pathname;
}

create_iconLib = function (icon_dir, resources_path) {
    iconLib = icons::icon_set(file.path(resources_path, icon_dir))
    return (iconLib)
}

Button = function (inputId, label=NULL, icon_name=NULL,
                   width=NULL, tooltip=NULL, ...){

    if (is.null(tooltip)) {
        actionButton(inputId=inputId,
                     label=div(label,
                               style="float:right;
                                          padding-left:0.2rem;"),
                     icon=NULL,
                     width=width,
                     img(icon_name, align="right",
                         style="text-align: center;
                                display: flex; align-items: center;"),
                     ...)
    } else {
        div(class="Tooltip bunch",
            HTML(paste0(
                actionButton(inputId=inputId,
                             label=div(label,
                                       style="float:right;
                                          padding-left:0.2rem;"),
                             icon=NULL,
                             width=width,
                             img(icon_name, align="right",
                                 style="text-align: center;
                                        display: flex; align-items: center;"),
                             ...),
                '<span class="Tooltiptext">', tooltip, '</span>')))
    }
}

selectButton = function (inputId, label=NULL, icon_name=NULL,
                         class='', selected=FALSE, tooltip=NULL, ...){

    if (is.null(tooltip)) {
        div(class="Tooltip bunch",
            HTML(paste0(
                checkboxGroupButtons(
                    status=class,
                    inputId=inputId,
                    label=NULL,
                    choiceNames=
                        paste0(img(icon_name,
                                   align="right",
                                   style="text-align: center;
                                          display: flex; align-items: center;"),
                               label),
                    choiceValues=TRUE,
                    selected=selected,
                    ...),
                '<span class="Tooltiptext">', tooltip, '</span>')))
    } else {
        checkboxGroupButtons(
            status=class,
            inputId=inputId,
            label=NULL,
            choiceNames=
                paste0(img(icon_name,
                           align="right",
                           style="text-align: center;
                                  display: flex; align-items: center;"),
                       label),
            choiceValues=TRUE,
            selected=selected,
            ...)
    }
}

updateSelectButton = function (session, inputId, label=NULL,
                               icon_name=NULL, class='',
                               selected=FALSE, ...){

    if (is.null(icon_name) & is.null(label)) {
        choiceNames = NULL
        choiceValues = NULL
    } else {
        choiceNames = paste0(img(icon_name,
                                 align="right",
                                 style="text-align: center;
                                        display: flex; align-items: center;"),
                             label)
        choiceValues = TRUE
    }
    
    updateCheckboxGroupButtons(
        session=session,
        status=class,
        inputId=inputId,
        label=NULL,
        choiceNames=choiceNames,
        choiceValues=choiceValues,
        selected=selected,
        ...)
}

radioButton = function (class='', choiceIcons=NULL, choiceNames=NULL,
                        choiceValues=NULL, choiceTooltips=NULL, ...) {

    if (!is.null(choiceNames)) {
        choiceItems = choiceNames

        if (is.null(choiceValues)) {
            choiceValues = choiceNames
        }
        
    } else {
        choiceItems = replicate(max(length(choiceTooltips),
                                    length(choiceIcons)),
                                "", simplify=FALSE)
    }
    if (!is.null(choiceIcons)) {
        inter = lapply(choiceIcons, img, align="right",
                       style="text-align: center;
                              display: flex; align-items: center;")
        choiceItems = mapply(paste0, inter, choiceItems)
    }
    if (!is.null(choiceTooltips)) {
        choiceItems = lapply(paste0(choiceItems,
                                    '<span class="Tooltiptext">',
                                    choiceTooltips,
                                    '</span>'), HTML)
        choiceItems = lapply(choiceItems,
                             div, class="Tooltip bunch")
    }

    radioGroupButtons(
        status=class,
        label=NULL,
        choiceNames=choiceItems,
        choiceValues=choiceValues,
        ...)
}


updateRadioButton = function (session, class='', choiceIcons=NULL,
                              choiceNames=NULL, choiceValues=NULL,
                              choiceTooltips=NULL, ...) {

    if (!is.null(choiceNames)) {
        choiceItems = choiceNames

        if (is.null(choiceValues)) {
            choiceValues = choiceNames
        }
        
    } else {
        choiceItems = replicate(max(length(choiceTooltips),
                                    length(choiceIcons)),
                                "", simplify=FALSE)
    }
    if (!is.null(choiceIcons)) {
        inter = lapply(choiceIcons, img, align="right",
                       style="text-align: center;
                              display: flex; align-items: center;")
        choiceItems = mapply(paste0, inter, choiceItems)
    }
    if (!is.null(choiceTooltips)) {
        choiceItems = lapply(paste0(choiceItems,
                                    '<span class="Tooltiptext">',
                                    choiceTooltips,
                                    '</span>'), HTML)
        choiceItems = lapply(choiceItems,
                             div, class="Tooltip bunch")
    }
    
    updateRadioGroupButtons(
        session=session,
        status=class,
        label=NULL,
        choiceNames=choiceItems,
        choiceValues=choiceValues,
        ...)
}


Slider = function (class, modeText=FALSE, ...) {
    if (modeText) {
        div(class=class,
            sliderTextInput(...))
    } else {
        div(class=class,
            sliderInput(...))
    }
}
updateSlider = function (class, modeText=FALSE, ...) {
    if (modeText) {
        div(class=class,
            updateSliderTextInput(...))
    } else {
        div(class=class,
            updateSliderInput(...))
    }
}


