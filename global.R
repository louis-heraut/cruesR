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
library(shinyjs)
library(shinyWidgets)
library(icons)
library(dplyr)


# Sourcing R files
source('tools.R', encoding='UTF-8')
source('settings.R', encoding='UTF-8')

# Sourcing app
source('server.R', encoding='UTF-8')
source('ui.R', encoding='UTF-8')

# Running app localy
shinyApp(ui=ui, server=server)
