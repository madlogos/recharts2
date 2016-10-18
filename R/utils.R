# make sure htmlwidgets:::toJSON() turns list() to {} in JSON, instead of []
.emptyList = setNames(list(), character())
emptyList = function() .emptyList

# evaluate a formula using `data` as the environment, e.g. evalFormula(~ z + 1,
# data = data.frame(z = 1:10))
evalFormula = function(x, data) {
  if (!inherits(x, 'formula')) return(x)
  if (length(x) != 2) stop('The formula must be one-sided: ', deparse(x))
  eval(x[[2]], data, environment(x))
}

evalVar <- function(var, data){
    stopifnot(inherits(var, 'formula'))
    if (var != ~NULL){
        evalFormula(var, data=data)
    }
}

evalVarArg <- function(x, data, simplify=FALSE, eval=TRUE){
    # eval var list to a data.frame
    # E.g.
    ## evalVarArg(Species, iris)
    ## evalVarArg(~Species, iris)
    ## evalVarArg("Species", iris)
    ## evalVarArg(as.numeric(Species), iris)
    ## evalVarArg(~as.numeric(Species), iris)
    ## evalVarArg("as.numeric(Species)", iris)
    ## evalVarArg(c(Species, Sepal.Width), iris)
    ## evalVarArg(c(as.numeric(Species), Sepal.Width), iris)
    ## evalVarArg(c(as.numeric(Species)+1, Sepal.Width), iris)

    # character, don't coerce; otherwise, coerce to formula

    # coerce x to formula if is symbol
    if (length(substitute(x)) > 1){
        if (as.character(substitute(x)[[1]]) %in% c(
            'c', 'list', 'data.frame', 'vector', 'matrix')){
            x <- arg1 <- as.list(substitute(x))
        }else if (as.character(substitute(x)[[1]] == '~')){
            if (grepl('^c|list|data\\.frame|vector|matrix\\(',
                      deparse(substitute(x)[[2]]))){
                x <- arg1 <- as.list(substitute(x)[[2]])
                x <- arg1 <- x[2:length(x)]
            }else{
                x <- arg1 <- as.list(substitute(x))
            }
        }else{
            x <- arg1 <- list(substitute(x))
        }
    }else{
        x <- arg1 <- list(substitute(x))
    }

    for (i in seq_along(x)) {
        if (is.character(x[[i]]))
            x[[i]] <- sub('^\\"(.*)\\"$' , '\\1', x[[i]])
        else {
            if (is.symbol(x[[i]]) || is.language(x[[i]])){
                if (as.character(x[[i]] != '~'))
                    x[[i]] <- as.formula(paste('~', deparse(x[[i]])))
            }
        }
    }
    arg1 <- arg1[! (x %in% c(~c, ~list, ~data.frame) | as.character(x) == '`~`')]
    x <- x[! (x %in% c(~c, ~list, ~data.frame) | as.character(x) == '`~`')]

    # loop evalVar() and filter valid data.frame
    out <- sapply(x, evalVar, data=data, simplify=FALSE)
    nrows <- sapply(out, length)
    out <- out[nrows==nrow(data)]
    names <- as.character(arg1)
    names <- names[nrows==nrow(data)]
    names <- gsub("^ *~ *(.*)$|^c\\((.*)\\)$", "\\1", names)
    out <- as.data.frame(out, stringsAsFactors=FALSE)
    names(out) <- names
    if (length(out) == 0){
        warning(paste("You yielded nothing by requiring", deparse(substitute(arg1)),
                      "out of", deparse(substitute(data))))
        return(NULL)
    }else{
        if (eval){
            if (simplify) if (ncol(out) == 1) return(out[,1])
            return(out)
        }else{
            return(names)
        }
    }
}

# merge two lists by names,
# e.g. x = list(a = 1, b = 2), mergeList(x, list(b = 3)) => list(a = 1, b = 3)
# mergeList(list(a=1, b=2), list(a=NULL, b=3), keep.null=TRUE) ==>
# list(a=NULL, b=3)
mergeList = function(x, y, merge.exclude=NULL, skip.merge.na=FALSE,
                     skip.merge.null=FALSE, keep.null=FALSE) {
    if (!is.list(y) || length(y) == 0) return(x)
    yn = names(y)
    if (length(yn) == 0 || any(yn == '')) {
      warning('The second list to be merged into the first must be named')
      return(x)
    }
    for (i in yn) {
      xi = if (length(x[[i]]) == 0) NULL else x[[i]]
      yi = if (length(y[[i]]) == 0) NULL else y[[i]]
      if (is.list(xi)) {
          if (is.list(yi)) x[[i]] = mergeList(xi, yi)
      } else {
          if (all(is.null(yi))){
              if (! skip.merge.null){
                  if (keep.null) x[i] = list(NULL)
                  else x[[i]] = NULL
              }
          }else if (all(is.na(yi))){
              if (! skip.merge.na) x[[i]] = yi
          }else{
              yiMerge = sapply(merge.exclude, function(s) {
                  identical(yi, s) })
              if (!any(yiMerge)) x[[i]] = yi
          }
      }
    }
    x
}

# merge two lists by names, e.g. x = list(a = 1, b = 2), mergeList(x, list(b =
# 3)) => list(a = 1, b = 3)
legacyMergeList = function(x, y) {
    if (!is.list(y) || length(y) == 0) return(x)
    yn = names(y)
    if (length(yn) == 0 || any(yn == '')) {
        warning('The second list to be merged into the first must be named')
        return(x)
    }
    for (i in yn) {
        xi = x[[i]]
        yi = y[[i]]
        if (is.list(xi)) {
            if (is.list(yi)) x[[i]] = mergeList(xi, yi)
        } else x[[i]] = yi
    }
    x
}

# automatic labels from function arguments
autoArgLabel = function(arg, auto) {
    if (inherits(try(arg, TRUE), 'try-error')) arg <- deparse(substitute(arg))
    if (! inherits(arg, 'formula') && ! is.null(arg)) {
        if (! grepl("^~", arg, ''))  arg <- as.formula(paste('~', arg))
    }
    if (is.null(arg)) return('')
    if (inherits(arg, 'formula')) return(deparse(arg[[2]]))
    auto
}


getSeriesPart <- function(chart, element=c(
    'name', 'category', 'type', 'data', 'large', 'mapType'),
    drop=TRUE, fetch.all=FALSE,
...){
    ## get all the element names vector from an echarts object's series
    ## 'category' is special, it returns data series based on different chart types
    stopifnot(inherits(chart, 'echarts'))
    element <- match.arg(element)
    hasZ <- 'timeline' %in% names(chart$x)
    if (hasZ){
        data <- try(sapply(seq_len(length(chart$x$options)), function(i) {
            sapply(chart$x$options[[i]]$series, function(lst) {
                if (fetch.all) ifnull(lst[['data']], NA)
                else lst[['data']]})
        }, simplify=!drop), TRUE)

        ## natural elements
        if (element %in% c('name', 'type', 'data', 'large', 'mapType')){
            ## generic situations
            obj <- try(sapply(seq_len(length(chart$x$options)), function(i) {
                sapply(chart$x$options[[i]]$series, function(lst) {
                    if (fetch.all) ifnull(lst[[element]], NA)
                    else lst[[element]]})
            }, simplify=!drop), TRUE)

            ## special situations
            if (chart$x$options[[1]]$series[[1]]$type %in%
                c('funnel', 'pie', 'radar')){
                if (element == 'data')
                    obj <- unlist(data)[names(unlist(data))=='value']
            }else if (chart$x$options[[1]]$series[[1]]$type %in%
                      c('force', 'chord')){
                if (element == 'data')
                    obj <- data[names(data) %in% c('nodes', 'links')]
            }
        ## special elements
        }else if (element %in% c('category')){
            if (chart$x$options[[1]]$series[[1]]$type %in%
                c('funnel', 'pie', 'radar')){
                obj <- unlist(data)[names(unlist(data))=='name']
            }else if (chart$x$options[[1]]$series[[1]]$type %in%
                      c('force', 'chord')){
                if ('categories' %in% names(chart$x$options[[1]]$series[[1]])){
                    obj <- unlist(chart$x$options[[1]]$series[[1]]$categories)
                }else if ('data' %in% names(chart$x$options[[1]]$series[[1]])){
                    obj <- unlist(chart$x$options[[1]]$series[[1]]$data)
                }else{
                    obj <- unlist(lapply(
                        1:length(chart$x$options),function(opt) {
                        z <- chart$x$options[[opt]]
                        o <- lapply(1:length(z$series), function(s){
                            source <- unlist(z$series[[s]]$links)
                            return(source[names(source)=='source'])
                        })
                        return(unlist(o))
                    }))
                }
            }else{
                obj <- try(sapply(seq_len(length(chart$x$options)), function(i) {
                    sapply(chart$x$options[[i]]$series, function(lst) {
                        if (fetch.all) ifnull(lst[['name']], NA)
                        else lst[['name']]})
                }, simplify=!drop), TRUE)
            }
        }

    }else{
        data <- try(sapply(chart$x$series, function(lst) {
            if (fetch.all) ifnull(lst[['data']], NA)
            else lst[['data']]
        }, simplify=!drop), TRUE)

        ## natural elements
        if (element %in% c('name', 'type', 'data', 'large', 'mapType')){
            ## generic situation
            obj <- try(sapply(chart$x$series, function(lst) {
                if (fetch.all) ifnull(lst[[element]], NA)
                else lst[[element]]
                }, simplify=!drop), TRUE)
            ## special situation
            if (chart$x$series[[1]]$type %in% c('funnel', 'pie', 'radar')){
                if (element == 'data')
                    obj <- unlist(data)[names(unlist(data))=='value']
            }else if (chart$x$series[[1]]$type %in% c('force', 'chord')){
                if (element == 'data')
                    obj <- data[names(data) %in% c('nodes', 'links')]
            }
        ## special elements
        }else if (element %in% c('category')){
            if (chart$x$series[[1]]$type %in% c('funnel', 'pie', 'radar')){
                obj <- unlist(data)[names(unlist(data))=='name']
            }else if (chart$x$series[[1]]$type %in% c('force', 'chord')){
                if ('categories' %in% names(chart$x$series[[1]])){
                    obj <- unlist(chart$x$series[[1]]$categories)
                }else if ('data' %in% names(chart$x$series[[1]])){
                    obj <- unlist(chart$x$series[[1]]$data)
                }else{
                    obj <- unlist(lapply(1:length(chart$x$series),function(s) {
                        o <- unlist(chart$x$series[[s]]$links)
                        return(o[names(o)=='source'])
                    }))
                }

            }else{
                obj <- try(sapply(chart$x$series, function(lst) {
                    if (fetch.all) ifnull(lst[['name']], NA)
                    else lst[['name']]
                }, simplify=!drop), TRUE)
            }
        }
    }
    if (hasZ && !drop && element %in% c(
        'name', 'type', 'category', 'large', 'mapType'))
        obj <- matrix(obj, ncol=length(chart$x$options))
    if (drop) obj <- unlist(obj)
    return(obj)
}


analyzeSeries <- function(chart, series){
    stopifnot(inherits(chart, 'echarts'))
    hasZ <- 'timeline' %in% names(chart$x)
    newSeries <- NULL
    # note: do not extract 'category'
    lvlSeries <- getSeriesPart(chart, 'name', drop=FALSE, fetch.all=TRUE)
    allSeries <- if (hasZ) apply(lvlSeries, 2, function(col) {
        seq_len(length(col))}) else seq_along(lvlSeries)  # index series
    dim(allSeries) <- dim(lvlSeries)
    lvlSeries <- if (hasZ) lapply(1:ncol(lvlSeries), function(c) lvlSeries[,c]) else
        list(lvlSeries)
    allSeries <- if (hasZ) lapply(1:ncol(allSeries), function(c) allSeries[,c]) else
        list(allSeries)

    if (is.null(series)){  # null, then apply to all series
        lvlseries <- lvlSeries
        series <- allSeries
    }else{
        if (is.numeric(series)){
            if (hasZ){
                series <- lapply(allSeries, function(col) {
                    intersect(series, col)
                })
                lvlseries <- lapply(seq_len(length(series)), function(j) {
                    lvlSeries[[j]][series[[j]]]
                })
            }else{
                series <- intersect(series, allSeries[[1]])
                lvlseries <- lvlSeries[[1]][series]
            }
        }else{
            newSeries <- unlist(series)[! unlist(series) %in% getSeriesPart(
                chart, 'name', fetch.all=TRUE)]
            if (hasZ){
                lvlseries <- lapply(lvlSeries, function(col) {
                    intersect(series, col)
                })
                series <- lapply(seq_len(length(lvlseries)), function(j){
                    allSeries[[j]][which(lvlSeries[[j]] %in% lvlseries[[j]])]
                })
            }else{
                lvlseries <- intersect(series, lvlSeries[[1]])
                series <- allSeries[[1]][which(lvlSeries[[1]] %in% lvlseries)]
            }
        }
    }
    return(list(numSeries=series, strSeries=lvlseries, allNumSeries=allSeries,
                allStrSeries=lvlSeries, strNewSeries=newSeries))
}


filterSeriesParts <- function(lst, type){
    stopifnot(type %in% c('line', 'bar', 'scatter', 'pie', 'radar', 'chord',
                          'force', 'map', 'gauge', 'funnel',
                          'treemap', 'wordCloud', 'heatmap'))
    fixedParts <- c('type', 'name', 'tooltip', 'data', 'itemStyle', 'markPoint',
                    'markLine', 'clickable', 'z', 'zlevel')
    validParts <- switch(
        type,
        line=c('stack', 'xAxisIndex', 'yAxisIndex', 'symbol', 'symbolSize',
               'symbolRotate', 'showAllSymbol', 'smooth', 'dataFilter',
               'legendHoverLink'),
        bar=c('stack', 'xAxisIndex', 'yAxisIndex', 'barGap', 'barCategoryGap',
              'barMinHeight', 'barWidth', 'barMaxWidth', 'legendHoverLink'),
        scatter=c('xAxisIndex', 'yAxisInde, x', 'symbol', 'symbolSize',
                  'symbolRotate', 'large', 'largeThreshold', 'legendHoverLink'),
        pie=c('legendHoverLink', 'center', 'radius', 'startAngle', 'minAngle',
              'clockWise', 'roseType', 'selectedOffset', 'selectedMode'),
        radar=c('symbol', 'symbolSize', 'symbolRotate', 'legendHoverLink',
                'polarIndex'),
        chord=c('symbol', 'symbolSize', 'clockWise', 'categories', 'links',
                'matrix', 'minRadius', 'maxRadius', 'ribbonType', 'showScale',
                'showScaleText', 'padding', 'sort', 'sortSub', 'nodes'),
        force=c('symbol', 'symbolSize', 'large', 'center', 'roam', 'categories',
                'links', 'matrix', 'size', 'minRadius', 'maxRadius', 'linkSymbol',
                'linkSymbolSize', 'scaling', 'gravity', 'draggable', 'useWorker',
                'steps', 'nodes'),
        map=c('selectedMode', 'mapType', 'hoverable', 'dataRangeHoverLink',
              'mapLocation', 'mapValueCalculation', 'mapValuePrecision',
              'showLegendSymbol', 'roam', 'scaleLimit', 'nameMap', 'textFixed',
              'geoCoord', 'heatmap'),
        gauge=c('legendHoverLink', 'center', 'radius', 'startAngle', 'endAngle',
                'min', 'max', 'splitNumber', 'axisLine', 'axisTick', 'axisLabel',
                'splitLine', 'pointer', 'title', 'detail'),
        funnel=c('legendHoverLink', 'sort', 'min', 'max', 'x', 'y', 'x2', 'y2',
                 'width', 'height', 'funnelAlign', 'minSize', 'maxSize', 'gap'),
        eventRiver=c('xAxisIndex', 'legendHoverLink', 'weight'),
        treemap=c('center', 'size', 'root'),
        tree=c('symbol', 'symbolSize', 'roam', 'rootLocation', 'layerPadding',
               'nodePadding', 'orient', 'direction'),
        wordCloud=c('center', 'size', 'textRotation', 'autoSize'),
        heatmap=c('blurSize', 'gradientColors', 'minAlpha', 'valueScale',
                  'opacity')
        )
    validParts <- c(fixedParts, validParts)
    lst <- lst[intersect(names(lst), validParts)]
    return(lst)
}


getYFromEChart <- function(chart, ...){
    ## get y series data and extract the unique values vector
    stopifnot(inherits(chart, 'echarts'))
    hasZ <- 'timeline' %in% names(chart$x)
    .getY <- function(seriesData){
        if (! is.null(dim(seriesData))){
            if (dim(seriesData)[2] > 1){
                return(seriesData[,2])
            }else{
                return(seriesData[,1])
            }
        }else{
            return(seriesData)
        }
    }
    if (hasZ){
        y <- sapply(chart$x$options, function(lst){
            Ys <- sapply(lst$series, function(l) {
                return(.getY(l$data))
            })
            return(Ys)
        })
    }else{
        y <- sapply(chart$x$series, function(lst) {
            return(.getY(lst$data))
        })
    }
    uniY <- suppressWarnings(as.numeric(unique(unlist(y))))
    return(uniY[!is.na(uniY)])
}

#' @export
#' @exportMethod + echarts
"+.echarts" <- function(e1, e2){
    stopifnot(inherits(e1, 'echarts'))
    browser()
    e1 <- deparse(substitute(e1))
    e2 <- deparse(substitute(e2))
    return(paste(e1,'%>%',e2))
}

#-----Palettes and others---------
#' Get The Colors Vector From A Named Palette
#'
#' Get hex color vector of a named palette from \code{\link{RColorBrewer}}, \code{\link{ggthemes}}
#' or \code{\link{grDevices}}. You can \code{\link{show_col}} the vector to
#' see the effects.
#' @param palname name of the palette. Default NULL to get echarts default. Could be:
#' \itemize{
#'  \item \link{RColorBrewer} palettes: Including \code{'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu',
#'  'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent', 'Dark2', 'Paired', 'Pastel1',
#'  'Pastel2', 'Set1', 'Set2', 'Set3', 'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens',
#'  'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
#'  'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'} \cr
#'  \item \link{ggthemes} palettes: \code{'calc', 'economist', 'economist_white', 'economist_stata',
#'  'excel', 'exel_fill', 'excel_line', 'excel_new', 'few', 'fivethirtyeight', '538', 'manyeyes',
#'  'gdocs', 'pander', 'tableau', 'stata', 'stata1', 'stata1r', 'statamono', 'ptol',
#'  'tableau20', 'tableau10medium', 'tableaugray', 'tableauprgy', 'tableaublrd',
#'  'tableaugnor', 'tableaucyclic', 'tableau10light', 'tableaublrd12', 'tableauprgy12',
#'  'tableaugnor12', 'hc', 'darkunica', 'solarized', 'solarized_red', 'solarized_yellow',
#'  'solarized_orange', 'solarized_magenta', 'solarized_violet', 'solarized_blue',
#'  'solarized_cyan', 'solarized_green', 'wsj', 'wsj_rgby', 'wsj_red_green',
#'  'wsj_black_green', 'wsj_dem_rep', 'colorblind', 'trafficlight'} \cr
#'  \item Aetna official palettes: Including \code{'aetnagreen', 'aetnablue', 'aetnaviolet',
#'  'aetnaorange', 'aetnateal', 'aetnacranberry'} \cr
#'  \item Other palettes: \code{'rainbow', 'terrain', 'topo', 'heat', 'cm'}
#' }
#' @param n length of the color vector when the palette is continuous (\code{rain, cm,
#' terrain, topo, heat, ...}). Default 6.
#' @import RColorBrewer scales ggthemes
#' @export
#' @return color vectors
#'
#' @seealso \code{\link{RColorBrewer}}, \code{\link{scales}}, \code{\link{ggthemes}},
#' \code{\link{show_col}}
#' @examples
#' \dontrun{
#' library(scales)
#' show_col(getColFromPal('tableau20'))
#' show_col(getColFromPal('hc'))
#' }
getColFromPal <- function(palname=NULL, n=6){
    brewer <- c(
        'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn',
        'Spectral', 'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1',
        'Set2', 'Set3', 'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys',
        'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
        'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')
    themePal <- list(
        default=c(
            '#ff7f50', '#87cefa', '#da70d6', '#32cd32', '#6495ed', '#ff69b4',
            '#ba55d3', '#cd5c5c', '#ffa500', '#40e0d0', '#1e90ff', '#ff6347',
            '#7b68ee', '#00fa9a', '#ffd700', '#6b8e23', '#ff00ff', '#3cb371',
            '#b8860b', '#30e0e0' ),
        macarons=c(
            "#2ec7c9", "#b6a2de", "#5ab1ef", "#ffb980", "#d87a80", "#8d98b3",
            "#e5cf0d", "#97b552", "#95706d", "#dc69aa", "#07a2a4", "#9a7fd1",
            "#588dd5", "#f5994e", "#c05050", "#59678c", "#c9ab00", "#7eb00a",
            "#6f5553", "#c14089"),
        infographic=c(
            "#C1232B", "#B5C334", "#FCCE10", "#E87C25", "#27727B", "#FE8463",
            "#9BCA63", "#FAD860", "#F3A43B", "#60C0DD", "#D7504B", "#C6E579",
            "#F4E001", "#F0805A", "#26C0C0"))
    echartJS <- paste(readLines(
        system.file('htmlwidgets/echarts.js', package='recharts'),
        encoding='UTF-8'), collapse='')
    lapply(c("blue", "dark", "gray", "green", "helianthus", "macarons2", "mint",
             "red", "roma", "sakura", "shine", "vintage"), function(theme) {
                 themePal[[theme]] <<- eval(parse(text=paste0('c(', gsub(paste0(
                     "^.*var ", theme, "Theme =.+?color:\\[(.+?)\\].*$"),
                     "\\1", echartJS), ')')))
             })
    tableau <- data.frame(
        nick=c('tableau20', 'tableau10medium', 'tableaugray', 'tableauprgy',
               'tableaublrd', 'tableaugnor', 'tableaucyclic', 'tableau10light',
               'tableaublrd12', 'tableauprgy12', 'tableaugnor12', 'tableau',
               'tableaucolorblind', 'trafficlight'),
        pal=c('tableau20', 'tableau10medium', 'gray5', 'purplegray6',
              'bluered6', 'greenorange6', 'cyclic', 'tableau10light',
              'bluered12', 'purplegray12', 'greenorange12', 'tableau10',
              'colorblind10', 'trafficlight'))
    ## echarts default
    colObj <- themePal$default
    if (! is.null(palname)) palname <- tolower(palname)
    if (! is.null(palname)){
        if (palname %in% paste0(
            "aetna", c('green','blue','teal','cranberry','orange','violet'))){
            colObj <- switch(
                palname,
                aetnagreen=c("#7AC143", "#7D3F98", "#F47721", "#D20962",
                             "#00A78E", "#00BCE4", "#B8D936", "#EE3D94",
                             "#FDB933", "#F58F9F", "#60C3AE", "#5F78BB",
                             "#5E9732", "#CEA979", "#EF4135", "#7090A5"),
                aetnablue=c("#00BCE4", "#D20962", "#7AC143", "#F47721",
                            "#7D3F98", "#00A78E", "#F58F9F", "#B8D936",
                            "#60C3AE", "#FDB933", "#EE3D94", "#5E9732",
                            "#5F78BB", "#CEA979", "#EF4135", "#7090A5"),
                aetnateal=c("#00A78E", "#F47721", "#7AC143", "#00BCE4",
                            "#D20962", "#7D3F98", "#60C3AE", "#FDB933",
                            "#B8D936", "#5F78BB", "#F58F9F", "#EE3D94",
                            "#5E9732", "#CEA979", "#EF4135", "#7090A5"),
                aetnacranberry=c("#D20962", "#00BCE4", "#7D3F98", "#7AC143",
                                 "#F47721", "#00A78E", "#F58F9F", "#60C3AE",
                                 "#EE3D94", "#B8D936", "#FDB933", "#5E9732",
                                 "#5F78BB", "#CEA979", "#EF4135", "#7090A5"),
                aetnaorange=c("#F47721", "#7AC143", "#00A78E", "#D20962",
                              "#00BCE4", "#7D3F98", "#FDB933", "#B8D936",
                              "#60C3AE", "#F58F9F", "#5F78BB", "#EE3D94",
                              "#5E9732", "#CEA979", "#EF4135", "#7090A5"),
                aetnaviolet=c("#7D3F98", "#7AC143", "#F47721", "#00A78E",
                              "#00BCE4", "#D20962", "#F58F9F", "#B8D936",
                              "#FDB933", "#60C3AE", "#5F78BB", "#EE3D94",
                              "#5E9732", "#CEA979", "#EF4135", "#7090A5")
            )
        }else if (palname %in% tolower(brewer)){
            Palname <- brewer[which(tolower(brewer)==palname)]
            maxcolors <- brewer.pal.info[row.names(brewer.pal.info)==Palname,
                                         "maxcolors"]
            colObj <- brewer.pal(ifelse((maxcolors>n && n>2), n, maxcolors),
                                 Palname)
        }else if (palname %in% tolower(paste0('_', c(
            "blue", "dark", "gray", "green", "helianthus", "macarons2", "mint",
            "red", "roma", "sakura", "shine", "vintage", "default", "macarons",
            "infographic")))){
            colObj <- themePal[[sub("^_(.+)$", "\\1", palname)]]
        }else{
            if (palname %in% c('rainbow', 'terrain', 'topo', 'heat', 'cm')){
                colObj <- switch(palname,
                                 rainbow=substr(rainbow(n), 1, 7),
                                 terrain=substr(terrain.colors(n), 1, 7),
                                 heat=substr(heat.colors(n), 1, 7),
                                 topo=substr(topo.colors(n), 1, 7),
                                 cm=substr(cm.colors(n), 1, 7)
                )
            }else{
                if (palname %in% c('pander')){
                    colObj <- palette_pander(n)
                }else if (palname %in% c('calc')){
                    colObj <- ggthemes:::ggthemes_data$calc$colors
                }else if (palname %in% c('ptol')) {
                    colObj <- ptol_pal()(ifelse(n > 12, 12, n))
                }else if (palname %in% c('excel', "excel_fill", "excel_line",
                                         "excel_new")){
                    palname <- unlist(strsplit(palname, "excel_"))[2]
                    if (is.na(palname)) palname <- 'new'
                    colObj <- ggthemes:::ggthemes_data$excel[[palname]]
                }else if (palname %in% c('economist', 'economist_white',
                                         'economist_stata')){
                    palname <- unlist(strsplit(palname,"economist_"))[2]
                    if (is.na(palname) || palname=='white') {
                        colObj <- ggthemes:::ggthemes_data$economist$fg
                    } else {
                        colObj <- ggthemes:::ggthemes_data$economist$stata$fg
                    }
                }else if (palname %in% c('darkunica', 'hc')){
                    palname <- ifelse(palname == 'hc', 'default', palname)
                    colObj <- ggthemes:::ggthemes_data$hc$palettes[[palname]]
                }else if (palname %in% c('wsj', 'wsj_rgby', 'wsj_red_green',
                                         'wsj_black_green', 'wsj_dem_rep')){
                    palname <- unlist(strsplit(palname,"wsj_"))[2]
                    if (is.na(palname)) palname <- 'colors6'
                    colObj <- ggthemes:::ggthemes_data$wsj$palettes[[palname]]
                }else if (palname %in% c('stata', 'stata1', 'stata1r', 'statamono')){
                    palname <- switch(palname, stata='stata', stata1='s1color',
                                      stata1r='s1rcolor', statamono='mono')
                    if (palname == 'stata'){
                        colObj <- ggthemes:::ggthemes_data$stata$colors
                    }else{
                        colObj <- try(eval(parse(text=paste0(
                            "stata_pal('", palname, "')(15)"))), TRUE)
                    }
                }else if (palname %in% c('few', 'few_dark', 'few_light')){
                    palname <- unlist(strsplit(palname,"few_"))[2]
                    if (is.na(palname)) palname <- "medium"
                    colObj <- ggthemes:::ggthemes_data$few[[palname]]
                }else if (palname %in%
                          c('fivethirtyeight','gdocs', 'colorblind', 'manyeyes',
                            '538')){
                    if (palname == '538') palname <- 'fivethirtyeight'
                    colObj <- ggthemes:::ggthemes_data[[palname]]
                }else if (palname %in%
                          c('tableau20', 'tableau10medium', 'tableaugray', 'tableauprgy',
                            'tableaublrd', 'tableaugnor', 'tableaucyclic', 'tableau10light',
                            'tableaublrd12', 'tableauprgy12', 'tableaugnor12', 'tableau',
                            'tableaucolorblind', 'trafficlight')){
                    palname <- tableau[tableau$nick==palname,"pal"]
                    colObj <- try(eval(parse(text=paste0("tableau_color_pal(palette='",
                                                         palname,"')(20)"))), TRUE)
                }else if (palname %in%
                          c('solarized', 'solarized_red', 'solarized_yellow',
                            'solarized_orange', 'solarized_magenta', 'solarized_violet',
                            'solarized_blue', 'solarized_cyan', 'solarized_green')){
                    palname <- unlist(strsplit(palname,"solarized_"))[2]
                    colObj <- try(eval(parse(text=paste0(
                        "solarized_pal('", ifnull(palname, 'blue'), "')(20)"))), TRUE)
                }
            }
        }
    }
    return(as.vector(colObj))
}

#' Get Hex Color Vector (Not Exported)
#'
#' Get color vector from a palette/color name. It is wider than \code{\link{getColFromPal}}.
#' @param palette Palette, default NULL. Could be
#' \itemize{
#'  \item palette name, e.g, "Blues". The palette will be proceeded by \code{\link{getColFromPal}}.
#'  \item a hex color, e.g., "#FFFFFF", "0xFFFFFFFF"
#'  \item a vector or color names, hex colors
#'  \item NULL
#' }
#' @param ... Elipsis
#'
#' @return A vector of hex colors
#'
#' @seealso \code{\link{getColFromPal}} \code{\link{RColorBrewer}} \code{\link{ggthemes}}
#' @examples
#' \dontrun{
#' library(scales)
#' show_col(getColors(NULL))
#' show_col(getColors("terrain"))
#' show_col(getColors(c('red', 'gold', 'skyblue')))
#' }
getColors <- function(palette, ...){
    # build a function to extract palette info
    # used for echartR
    if ("n" %in% names(list(...))) n <- list(...)[['n']] else n <- 6
    if (length(palette)==1) {
        if (substr(palette, 1, 1)=="#"){
            if (nchar(palette) == 7 || nchar(palette) == 4) {
                return(palette)
            }else{
                palette <- paste0('0x', substring(palette, seq(2,8,2), seq(3,9,2)))
                palette <- strtoi(palette)
                return(rgba(palette))
            }
        }else if (palette %in% colors()){
            return(substr(col2hcl(palette), 1, 7))
        }else if (grepl('^rgba\\(', palette)){
            return(palette)
        }else{
            palettes <- unlist(strsplit(palette, "[\\(\\)]", perl=TRUE))
            if (length(palettes)==1){
                return(getColFromPal(palettes[1], n))
            }else{
                aetPal <- getColFromPal(palettes[1], as.numeric(palettes[2]))
                if (as.numeric(palettes[2]) < length(aetPal)){
                    return(sample(aetPal, as.numeric(palettes[2])))
                }else{
                    return(aetPal)
                }
            }
        }
    }else if(length(palette)>1){
        .convCol <- function(iPal){
            if (!is(try(col2rgb(iPal), TRUE), "try-error")){
                if (substr(iPal, 1, 1) == "#"){
                    return(toupper(iPal))
                }else{
                    vecCol <- as.vector(col2rgb(iPal))
                    return(rgba(vecCol))
                }
            }
        }
        aetPal <- unlist(lapply(palette, .convCol))
        return(aetPal)
    }else{
        return(getColFromPal(NULL))
    }
}


# -------------Lazy functions to judge class-------------------
isDate <- function(x, format=NULL){
    if (!is.null(format)){
        if (!is(try(as.Date(x),TRUE),"try-error")) TRUE else FALSE
    }else{
        if (!is(try(as.Date(x,format=format),TRUE),"try-error")) TRUE else FALSE
    }
}
isTime <- function(x, origin=NULL, tz='CST'){
    if (is.null(origin)){
        return(FALSE)
    }else{
        if (!is(try(as.POSIXct,T),"try-error")) TRUE else FALSE
    }
}

isLatin <- function(x){
    if (is.factor(x)) x <- as.character(x)
    return(all(grepl("^[[:alnum:][:space:][:punct:]]+$", x, perl=TRUE)))
}
isFormula <- function(x){
    return(inherits(x, 'formula'))
}

#' If-else Replacement Function For Vectors
#'
#' @param x A vector/list to replace
#' @param y The vector to be replaced with
#' @param cond Condition string, could be 'is.null', 'is.na', 'is.nan', 'is.blank',
#' 'is.zero', or 'missing'
#' @export
#' @examples
#' \dontrun{
#' ifna(c(NA, 1, 4, NA), 0)
#' # get c(0, 1, 4, 0)
#' }
#'
iif <- function(x, y, cond=c(
    'is.null', 'is.na', 'is.nan', 'is.blank', 'is.zero', 'missing')){
    is.blank <- function(x)  length(x) == 0
    is.zero <- function(x) x == 0
    cond <- match.arg(cond)
    o <- x
    fun <- eval(parse(text=cond))
    if (is.list(o)){
        o <- lapply(o, function(l) {
            l[fun(l)] <- y
            return(l)
        })
    }else{
        if (length(o) == 0) o <- y
        else if (length(o[fun(o)]) > 0) o[fun(o)] <- y
    }

    return(o)
}

#' @export
#' @rdname iif
ifnull <- function(x, y) iif(x, y, 'is.null')

#' @export
#' @rdname iif
ifna <- function(x, y) iif(x, y, 'is.na')

#' @export
#' @rdname iif
ifnan <- function(x, y) iif(x, y, 'is.nan')

#' @export
#' @rdname iif
ifblank <- function(x, y) iif(x, y, 'is.blank')

#' @export
#' @rdname iif
ifempty <- ifblank

#' @export
#' @rdname iif
ifzero <- function(x, y) iif(x, y, 'is.zero')

#' @export
#' @rdname iif
ifmissing <- function(x, y) iif(x, y, 'missing')

#--------------------data struc changes---------------------------
asEchartData <- function(x, na.string='-'){
    # convert matrix/data.frame or vector to JSON-list lists
    # and convert NA to '-'
    if (!is.null(dim(x))){
        o = apply(x, 1, function(row){
            row = as.list(unname(row))
            row = lapply(row, function(e) e = if (is.na(e)) na.string else e)
            return(row)
        })
        # if (nrow(x) == 1 && ncol(x) > 1)
        #     o = list(unname(o))
    }else{
        o = as.list(unname(x))
        o = lapply(x, function(e) e = if (is.na(e)) na.string else e)
    }

    return(unname(o))
}

#' @importFrom digest sha1
reElementId <- function(chart, seed=NULL){
    stopifnot(inherits(chart, 'echarts'))
    if (!is.null(seed)) if (is.numeric(seed)) set.seed(seed)
    elementId = paste0('echarts-', sha1(
        paste0(convTimestamp(Sys.time()), Sys.info()[['nodename']],
               sample(10000000000, 1))))
    txt <- paste(deparse(chart, backtick=TRUE, control='all'), collapse='')
    txt <- gsub("(document\\.getElementById\\()([^\\)]+?)\\)",
                paste0("\\1'", elementId, "'\\)"), txt)
    chart <- eval(parse(text=txt))
    chart$elementId <- elementId
    class(chart) <- c('echarts','htmlwidget')
    return(chart)
}


convTimestamp <- function(time, from='R', to='JS'){
    stopifnot(inherits(time, c("numeric", "Date", "POSIXct", "POSIXlt")))
    if (from=='R' && to=='JS'){
        time <- as.POSIXlt(time, orig="1970-01-01")
        gmtoff <- ifnull(as.POSIXlt(Sys.time(), orig='1970-01-01')$gmtoff, 0)
        time <- as.numeric(time) - gmtoff
        return(time * 1000)
    }
    if (from=='JS' && to=='R')
        return(as.POSIXct(time/1000, orig="1970-01-01"))
}
#--------Other functions for position, color, HTML table conversion------------

#' Get A String Containing 'rgba' Function
#'
#' Echarts uses rgba function heavily. You can convert color vectors into rgba function
#' in string form to pass to an echarts object.
#' @param vecrgb A vector of RGB elements, or simply red int.
#' @param ... If vecrgb is simply red int, you can pass green, blue, alpha int here.
#'
#' @return A character string. E.g, 'rgba(125, 125, 125, 0.6)' or '#FFFFFF'
#' @export
#'
#' @examples
#' \dontrun{
#' rgba(c(123, 123, 124, 125))  # return 'rgba(123,123,124,0.490196078431373)'
#' rgba(123, 123, 124, 0.5) # return 'rgba(123,123,124,0.5)'
#' rgba(123, 123, 124)  # return '#7B7B7C'
#' }
rgba <- function(vecrgb, ...){
    if (is.matrix(vecrgb) && dim(vecrgb) == c(3,1)) vecrgb <- vecrgb[,1]
    ## vecrgb is yielded from col2rgb()

    if (is.list(vecrgb)) rgb <- as.vector(unlist(vecrgb))
    if (length(vecrgb) == 1) vecrgb <- c(vecrgb, unlist(list(...)))
    if (min(vecrgb, na.rm=TRUE)<0 || max(vecrgb, na.rm=TRUE)>255) {
        stop("All elements should be numeric 0-255!")
    }
    if (length(vecrgb[!is.na(vecrgb)]) == 3){
        return(rgb(red=vecrgb[1], green=vecrgb[2], blue=vecrgb[3], max=255))
    }else if (length(vecrgb[!is.na(vecrgb)])==4){
        #return(rgb(red=vecrgb[1],green=vecrgb[2],blue=vecrgb[3],alpha=vecrgb[4],
        #           max=255))
        return(paste0('rgba(',vecrgb[1],',',vecrgb[2],',',vecrgb[3],',',
                      as.numeric(ifelse(vecrgb[4]<=1, vecrgb[4],
                                        round(vecrgb[4]/255, 4))),
                      ')'))
    }else{
        stop("Must be of length 3 or 4!")
    }
}

checkColorDiff <- function(col1, col2, ...){
    stopifnot((col1 %in% colors() || grepl("#[[:xdigit:]]{6}", col1) ||
                   grepl("^rgba\\(", col1)) &&
              (col2 %in% colors() || grepl("#[[:xdigit:]]{6}", col2) ||
                   grepl("^rgba\\(", col2)))
    if (grepl("^rgba\\(", col1)){
        col1 <- as.numeric(unlist(strsplit(col1, "[\\(,\\)]")[[1]][2:5]))
        col1 <- rgb(col1[1], col1[2], col1[3], col1[4]*255, max=255)
    }else{
        col1 <- getColors(col1)
    }
    if (grepl("^rgba\\(", col2)){
        col2 <- as.numeric(unlist(strsplit(col2, "[\\(,\\)]")[[1]][2:5]))
        col2 <- rgb(col2[1], col2[2], col2[3], col2[4]*255, max=255)
    }else{
        col2 <- getColors(col2)
    }

    bright1 <- sum(c(299, 587, 114) * col2rgb(col1))/1000
    bright2 <- sum(c(299, 587, 114) * col2rgb(col2))/1000
    brightDiff <- abs(bright1 - bright2)
    hueDiff <- sum(abs(col2rgb(col1, TRUE) - col2rgb(col2, TRUE)))
    return(data.frame('Diff' = c(brightDiff, hueDiff),
                      'Suffiecient'=c(brightDiff >= 125, hueDiff >= 500),
                      row.names=c('Bright', 'Hue')))
}

#' Invert A Color to Its Conplementary Color
#'
#' @param color A hex or named color, or color in 'rgba(R, G, B, A)' string.
#' @param mode One or a vector of modes combined. You can only input the first letter.
#' Default 'bw', which is most useful in textStyles.
#' \describe{
#'  \item{\code{bw}}{black and white invertion}
#'  \item{\code{opposite}}{complete invertion to get an opposite color}
#'  \item{\code{hue}}{only invert hue in terms of \code{\link{hsv}}}
#'  \item{\code{saturation}}{only invert saturation in terms of \code{\link{hsv}}}
#'  \item{\code{lumination}}{only invert lumination in terms of \code{\link{hsv}}}
#' }
#' @param ... Elipsis
#'
#' @return Inverted hex color
#' @export
#'
#' @seealso \code{\link{hsv}}, \code{\link{rgb2hsv}}, \code{\link{rgb}},
#' @examples
#' col <- sapply(list('o', 'h', 'l', 's', 'b', c('h', 'l'), c('h', 's'),
#'               c('l', 's'), c('h', 's', 'l')), function(mode) {
#'               return(invertColor('darkred', mode))
#'         })
#' library(scales)
#' show_col(c('darkred', unlist(col)))
#'
invertColor <- function(color, mode=c('bw', 'opposite', 'hue', 'saturation',
                                      'lumination', ''),
                        ...){
    if (! grepl("^rgba\\(", color)) col <- color <- getColors(color)
    if (grepl("^rgba\\(", color)){
        col <- as.numeric(unlist(strsplit(col, "[\\(,\\)]")[[1]][2:5]))
        col <- rgb(col[1], col[2], col[3], col[4]*255, max=255)
    }
    modeAbbrev <- tolower(substr(mode, 1, 1))
    rgb <- col2rgb(col)
    hsv <- rgb2hsv(rgb)

    if ('b' %in% modeAbbrev){  # black and white invert
        bright <- sum(c(299, 587, 114) * rgb) / 1000
        if (bright >= 128) return("#000000")
        else return("#FFFFFF")
    }else if ('o' %in% modeAbbrev) {
        rgb_neg <- rep(255, 3) - rgb
        return(rgb(rgb_neg[1], rgb_neg[2], rgb_neg[3], max=255))
    }else{
        if ('h' %in% modeAbbrev)
            hsv[1] <- ifelse(hsv[1] > 0.5, hsv[1] - 0.5, hsv[1] + 0.5)
        if ('s' %in% modeAbbrev)
            hsv[2] <- 1 - hsv[2]
        if ('l' %in% modeAbbrev)
            hsv[3] <- 1 - hsv[3]
        return(hsv(hsv[1], hsv[2], hsv[3]))
    }
}

autoMultiPolarChartLayout <- function(n, col.max=5, gap=5, top=5, bottom=5,
                                      left=5, right=5){
    layouts <- data.frame(row=ceiling(n/(1:col.max)), col=1:col.max)
    layouts$empty <- abs(layouts$row * layouts$col - n)
    layouts$diff <- abs(layouts$row - layouts$col)
    layouts$defects <- layouts$empty + layouts$diff
    layouts <- layouts[order(layouts$defects, layouts$diff, layouts$empty,
                             layouts$row), ]
    rows <- layouts[1, 'row']
    cols <- layouts[1, 'col']

    ## calculate the sizing params
    centers <- expand.grid(left + ((1:cols)*2 - 1) * ((100-left-right)/2) /cols,
                           top + ((1:rows)*2 - 1) * ((100-top-bottom)/2) /rows)
    centers <- centers[1:n,]
    radius <- (min(100-left-right, 100-top-bottom) -
                   gap * (max(rows, cols) -1)) / max(rows, cols)
    return(list(rows=rows, cols=cols, centers=centers, radius=radius))
}

parseTreeNodes <- function(data, name='name', parent='parent'){
    name <- as.character(substitute(name))
    parent <- as.character(substitute(parent))
    name <- name[length(name)]
    parent <- parent[length(parent)]

    names(data)[which(names(data) == name)] <- 'name'
    names(data)[which(names(data) == parent)] <- 'parent'
    validColNames <- c('name', 'value', 'itemStyle', 'symbol', 'symbolSize')
    if (!all(names(data) %in% c('parent', validColNames)))
        stop("treeNode data only accepts column names of ",
             paste(c('parent', validColNames), ', '),
             " ('name' and 'parent' could be named differently) .")

    if (!any(is.na(data$parent)))
        stop('parent columns must contain at least one NA to be the root.')

    colnames <- names(data)
    data <- data.frame(lapply(names(data), function(col){
        col = if (grepl('value|Size', col)) as.numeric(data[,col]) else
            as.character(data[,col])
        return(col)
    }), stringsAsFactors=FALSE)
    names(data) <- colnames

    orderBase <- data[which(data$name == data$parent),]

    .getRecursiveNodes <- function(nodeName){
        if (is.na(nodeName)) dt <- data[which(is.na(data$parent)),]
        else dt <- data[which(data$parent %in% nodeName),]

        children <- unique(as.character(dt$name))

        out <- unname(apply(dt, 1, function(row){
            if (nrow(dt) > 0){
                o <- lapply(intersect(names(dt), validColNames), function(col){
                    if (grepl('value|Size', col))
                        as.numeric(unname(row[col]))
                    else
                        as.character(unname(row[col]))
                })
                names(o) <- intersect(names(dt), validColNames)
                if (nrow(data[which(data$parent %in% row['name']),]) > 0)
                    o$children <- .getRecursiveNodes(as.character(row['name']))
                return(o)
            }
        }))
        return(out)
    }

    return(.getRecursiveNodes(NA))
}

matchSubtype <- function(subtype, lstSubtype, mode=c('any', 'all', 'detail',
                                                     'which')){
    stopifnot(length(subtype)==1)
    mode <- match.arg(mode)
    if (mode=='any'){
        any(sapply(lstSubtype, function(x) subtype %in% x))
    }else if (mode == 'all'){
        all(sapply(lstSubtype, function(x) subtype %in% x))
    }else if (mode=='detail'){
        sapply(lstSubtype, function(x) subtype %in% x)
    }else if (mode=='which'){
        which(sapply(lstSubtype, function(x) subtype %in% x))
    }
}

getJSElementSize <- function(chart, element=c('width', 'height')){
    stopifnot(inherits(chart, 'echarts'))
    element <- match.arg(element)
    if (is.null(chart$elementId))
        stop("The echarts object has not been assigned a fixed elementId!")
    return(paste0("document.getElementById('", chart$elementId,"').",
                  switch(element, width='offsetWidth', height='offsetHeight')))
}

#' Text Position and Direction
#'
#' Converts text postion from clock digits to c(x, y, direction) vector, or vice versa.
#' @param pos 1-12, clock digits.
#'
#' @return A vector of x-alignment, y-alignment and direction.
#' @export
#'
#' @examples
#' \dontrun{
#' vecPos(2) ## returns c("right", "top", "vertical")
#' }
#' @note
#' # Postion of Clock Numbers 1-12 \cr
#' \tabular{lllll}{
#'  10(l, t, v) \tab 11(l, t, h) \tab 12(c, t, h) \tab 1(r, t, h) \tab 2(r, t, v) \cr
#'  9(l, c, v) \tab \tab \tab \tab 3(r, c, v) \cr
#'  8(l, b, v) \tab 7(l, b, h) \tab 6(c, b, h) \tab 5(r, b, h) \tab 4(r, b, v)
#' }
#' @rdname position.orient
vecPos <- function(pos){
    TblPos=as.data.frame(rbind(c("right",  "top",    "horizontal"),
                               c("right",  "top",    "vertical"),
                               c("right",  "middle", "vertical"),
                               c("right",  "bottom", "vertical"),
                               c("right",  "bottom", "horizontal"),
                               c("center", "bottom", "horizontal"),
                               c("left",   "bottom", "horizontal"),
                               c("left",   "bottom", "vertical"),
                               c("left",   "middle", "vertical"),
                               c("left",   "top",    "vertical"),
                               c("left",   "top",    "horizontal"),
                               c("center", "top",    "horizontal")
                               ),
                         stringsAsFactors=FALSE)
    names(TblPos) <- c("x","y","z")
    return(as.vector(unlist(TblPos[pos,])))
}


#' @param x String, 'left', 'right' or 'center'
#' @param y String, 'top', 'middle' or 'vertical'
#' @param orient String, 'horizontal' or 'vertical'
#'
#' @return A clock digit number
#' @export
#'
#' @examples
#' \dontrun{
#' clockPos("right", "top", "vertical") ## returns 2
#' }
#' @rdname position.orient
clockPos <- function(x, y, orient){
    TblPos=as.data.frame(rbind(c("right",  "top",    "horizontal"),
                               c("right",  "top",    "vertical"),
                               c("right",  "middle", "vertical"),
                               c("right",  "bottom", "vertical"),
                               c("right",  "bottom", "horizontal"),
                               c("center", "bottom", "horizontal"),
                               c("left",   "bottom", "horizontal"),
                               c("left",   "bottom", "vertical"),
                               c("left",   "middle", "vertical"),
                               c("left",   "top",    "vertical"),
                               c("left",   "top",    "horizontal"),
                               c("center", "top",    "horizontal")
    ),
    stringsAsFactors=FALSE)
    names(TblPos) <- c("x","y","z")
    return(which(TblPos$x==x & TblPos$y==y & TblPos$z==orient))
}

exchange <- function(x, y){
    a <- x
    x <- y
    y <- a
    return(list(x, y))
}
