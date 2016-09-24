#' \code{Javascript}-style Operator Assignment
#'
#' If you are familiar with \code{Javascript}, you may miss +=, -=, *=, /= very much.
#' The operator can be \code{+, -, *, /, ^, **, \\, mod, root}.
#' @param lhs Left hand side argument.
#' @param rhs Right hand side argument.
#' @param envir Environment for operation to take place.
#'
#' @return Modified lhs
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## update an object entirely
#' a <- 3
#' a %+=% 2     # 3  ==> (3 + 2)     ==> 5
#' a %-=% 2     # 5  ==> (5 - 2)     ==> 3
#' a %*=% 2     # 3  ==> (3 * 2)     ==> 6
#' a %/=% 2     # 6  ==> (6 / 2)     ==> 3
#' a %^=% 2     # 3  ==> (3 ^ 2)     ==> 9
#' a %**=% 2    # 9  ==> (3 ** 2)    ==> 81
#' a %\=% 30    # 81 ==> (81 %% 30)  ==> 21
#' a %mod=% 8   # 21 ==> (21 %% 8)   ==> 5
#' a %root=% 2  # 5  ==> (5 ^ (1/2)) ==> 2.236
#'
#' ## object and object
#' a <- 1:4
#' b <- 4:1
#' a %+=% b     # a ==> c(5,5,5,5)
#' a %*=% b     # a ==> c(20, 15, 10, 5)   ## (c(5,5,5,5) * c(4,3,2,1))
#'
#' ## update an object partially
#' b <- 1:4
#' b[1] %+=% 1     # b ==> c(2,2,3,4)
#'
#' c <- list(list(A=1, B=2:3), list(C=4))
#' c[[1]]$A %-=% 1 # c[[1]]$A ==> 0 ## (rest elements are not changed)
#'
#' 1 %+=% b[2]     # simply print 3 (1 + b[2]), but no variable is changed
#' 1 %+=% 1        # simply print 2 (1 + 1), but no variable is changed
#' }
#' @rdname JS.Operator.Assignment
#'
`%+=%` <- function(lhs, rhs, envir=parent.frame()){
    opr <- gsub("^%(.+)=%$", "\\1", deparse(match.call()[[1]]))
    if (opr %in% c('\\', '%%', '%', 'mod')) opr <- '%%'
    if (opr %in% c('root')) transformRHS <- "1/" else transformRHS <- ""
    if (opr %in% c('^', '**', 'root')) opr <- '^'

    oLHS <- deparse(substitute(lhs))
    oRHS <- deparse(substitute(rhs))

    nameLHS <- strsplit(oLHS,split="[$\\[\\]]", perl=TRUE)[[1]][1]
    nameRHS <- strsplit(oRHS,split="[$\\[\\]]", perl=TRUE)[[1]][1]
    fullLHS <- deparse(substitute(lhs))
    fullRHS <- deparse(substitute(rhs))

    # mirror the object in function envir
    if (!identical(lhs, substitute(lhs)))
        assign(nameLHS, get(nameLHS, envir), envir=environment())
    if (!identical(rhs, substitute(rhs)))
        assign(nameRHS, get(nameRHS, envir), envir=environment())

    # assign value to the object within function envir
    if (identical(lhs, substitute(lhs)))  {
        out <- eval(parse(text=paste0("`", opr, "`(", lhs, ",", transformRHS,
                                      fullRHS, "[[1]])")))
        return(out)
    }else{
        eval(parse(text=paste0(fullLHS, " <- `", opr, "`(", fullLHS, ", ",
                               transformRHS, fullRHS, ")")))
        assign(nameLHS, eval(parse(text=nameLHS)), envir=envir)
    }
}

#' @export
#' @rdname JS.Operator.Assignment
`%-=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%*=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%/=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%^=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%**=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%\\=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%mod=%` <- `%+=%`

#' @export
#' @rdname JS.Operator.Assignment
`%root=%` <- `%+=%`

# not useful at all.
# #' Pipe Binary Operation
# #'
# #' \code{\link{\%>\%}} is a great tool to chain a series of operations in a concise format.
# #' Here provides binary operators combined with pipeline.
# #' @param lhs Left hand side object
# #' @param rhs Right hand side object
# #'
# #' @return A new object with the value \code{rhs opr rhs}
# #'
# #' @export
# #' @seealso \code{\link{magrittr}}
# #' @examples
# #' ## simple object
# #' a <- 3
# #' a %>+% 1 %>-% 2 %>*% 3 %>/% 4
# #'
# #' ## vector
# #' a <- 1:4
# #' a %>+% 1 %>-% 2 %>*% 3 %>/% 4
# #'
# #' ## object operates object
# #' a <- 1:4
# #' b <- 1:2
# #' c <- 3:4
# #' a %>-% b %>+% c %>*% b %>/% c %>^% b %>root% c
# #'
# #' @rdname Pipe.Binary.Operators
#
# `%>+%` <- function(lhs, rhs){
#     opr <- deparse(match.call()[[1]])
#     #browser()
#     lhs <- deparse(substitute(lhs))
#     rhs <- deparse(substitute(rhs))
#     # combine into one string
#     call <- paste(lhs, opr, rhs)
#
#     # match correct operator
#     call <- gsub("%>\\*\\*%", "%>\\^%", call)
#     call <- gsub("%>mod%", "%>%%%", call)
#     call <- gsub("%>root%", "%>\\^1/%", call)
#
#     # format call
#     call <- gsub("%>(\\S+)%\\s+(\\S+)", "%>% `\\1`\\(\\2\\) ", call)
#     call <- gsub("%>\\^%\\s+(\\S+)", "%>% `\\^`\\(\\2\\)", call)
#     call <- gsub("%>\\*%\\s+(\\S+)", "%>% `\\*`\\(\\2\\)", call)
#     call <- gsub("`\\^1/`\\((\\S+)\\)", "`\\^`\\(1/\\1\\)", call) # root x^(1/y)
#
#     # evaluate
#     eval(parse(text=call), envir=parent.frame())
# }
#
# #' @export
# #' @rdname Pipe.Binary.Operators
# `%>-%` <- `%>+%`
#
# #' @export
# #' @rdname Pipe.Binary.Operators
# `%>*%` <- `%>+%`
#
# #' @export
# #' @rdname Pipe.Binary.Operators
# `%>/%` <- `%>+%`
#
# #' @export
# #' @rdname Pipe.Binary.Operators
# `%>^%` <- `%>+%`
#
# #' @export
# #' @rdname Pipe.Binary.Operators
# `%>**%` <- `%>+%`
#
# #' @export
# #' @rdname Pipe.Binary.Operators
# `%>root%` <- `%>+%`
#
# #' @export
# #' @rdname Pipe.Binary.Operators
# `%>mod%` <- `%>+%`

###-------table format-----------
#' Reformat HTML Table
#'
#' Convert a data frame to an HTML table object and reformat it.
#' @param dataset The dataset to draw table.
#' @param heading The heading you want to input.
#'        '|' indicates colspan, '=' indicates rowspan.
#' @param footRows The last several rows as <tfoot>.
#' @param align Alignment of columns.
#' @param concatCol Index of columns to concatenate,
#'        to make the table look hierachical.
#' @param caption Table caption.
#' @param tableWidth Width of the table.
#'
#' @return A table in HTML.
#' @importFrom knitr kable
#' @seealso \code{\link{knitr::kable}}
#'
#' @examples
#' \dontrun{
#' ## A reformatted table with colspan/colrow=2
#' heading <- matrix(c("Sepal", NA, "Petal", NA, "Species", "Length", "Width",
#'                     "Length", "Width", NA), byrow=TRUE, nrow=2)
#' reheadHTMLTable(head(iris), heading)
#' }
reheadHTMLTable <- function(dataset, heading, footRows=0,
                            align=c('left', rep('center', ncol-1)),
                            concatCol=NULL, caption=NULL,
                            tableWidth='100%'){
    if ((!is.null(dataset) & !is.data.frame(dataset)) |
        !(is.data.frame(heading) | is.matrix(heading) | is.vector(heading))){
        stop(paste0('`dataset` must be a data.frame, while you gave a ',
                    class(dataset),
                    '\n`heading` must be a vector/matrix/data.frame, ',
                    'while you gave a ', class(heading),"."))
    }else{
        if (is.vector(heading)) heading <- t(matrix(heading))
        if (!is.null(dataset)){
            ncol <- ncol(dataset)
            if (ncol!=ncol(heading))
                stop(paste("Not equal counts of columns! Dataset has",
                           ncol, "cols, while heading has",ncol(heading), '.'))
        }else{
            ncol <- sub('(^[dhr]+?)[^dhr].+$','\\1',gsub('.+?<t([dhr]).+?',
                                                         '\\1',htmltable))
            ncol <- table(strsplit(ncol,"")[[1]])
            ncol <- floor((ncol[['h']]+ncol[['d']])/ncol[['r']])
        }
        align_simp <- substr(tolower(align),1,1)
        if (!all(align_simp %in% c('l','c','r'))){
            stop('`align` only accepts values of "l(eft)", "c(enter)" and "r(ight)".')
        }else{
            align[align_simp=="l"] <- "left"
            align[align_simp=="c"] <- "center"
            align[align_simp=="r"] <- "right"
        }
        if (length(align) > ncol(heading)){
            align <- align[1:ncol(heading)]
        }else if (length(align)<ncol(heading)){
            align <- c(align[1:length(align)],
                       rep(align[length(align)],ncol(heading)-length(align)))
        }
        align_simp <- substr(tolower(align),1,1)
        # loadPkg('knitr')

        dataset <- as.data.frame(dataset)
        if (!is.null(concatCol)){
            for (icol in concatCol){
                col <- as.character(dataset[,icol])
                lag <- c(NA,as.character(dataset[1:(nrow(dataset)-1),icol]))
                col[col==lag] <- ""
                dataset[,icol] <- col
            }
        }

        if (!(is.null(footRows) | footRows==0)){
            if (footRows>=nrow(dataset))
                stop("footRows cannot be >= number of datatable rows!")
            htmlBody <- knitr::kable(dataset[1:(nrow(dataset)-footRows),],
                                     format='html',align=align_simp,row.names=FALSE)
            htmlFoot <- knitr::kable(dataset[(nrow(dataset)-footRows+1):
                                                 nrow(dataset),],
                                     format='html',align=align_simp,row.names=FALSE)
            htmlBody <- gsub("(^.+</tbody>).+$","\\1",htmlBody)
            htmlFoot <- gsub("^.+<tbody>(.+)</tbody>.+$","<tfoot>\\1</tfoot>",htmlFoot)
            htmltable <- paste0(htmlBody,"\n",htmlFoot,"\n</table>")
        }else{
            htmltable <- knitr::kable(dataset,format='html',
                                      align=align_simp, row.names=FALSE)
        }

        if (!is.null(caption)){
            htmltable <- gsub("<table>",paste0("<table>\n<caption>",caption,"</caption>"),
                              htmltable)
        }
        class(htmltable) <- 'knitr_kable'
        attributes(htmltable) <- list(format='html',class='knitr_kable')
        rehead <- '<thead>'
        for (j in 1:ncol(heading)){
            if (all(is.na(heading[,j]))){
                heading[1,j] <- '$'
                if (nrow(heading)>1) heading[2:nrow(heading),j] <- "|"
            }
        }
        heading[1,][is.na(heading[1,])] <- "="
        if (nrow(heading)>1){
            heading[2:nrow(heading),][is.na(heading[2:nrow(heading),])] <- "|"
        }
        dthead <- heading
        for (i in 1:nrow(heading)){
            for (j in 1:ncol(heading)){
                dthead[i,j] <- ifelse(heading[i,j] %in% c('|','='),"",
                                      paste0('   <th style="text-align:',align[j],';"> ',
                                             heading[i,j],' </th>\n'))
                if (! heading[i,j] %in% c("|","=")){
                    if (i==1 & heading[i,j]=="$"){
                        dthead[i,j] <- paste0('   <th rowspan="',nrow(heading),
                                              '" style="text-align:',align[j],
                                              ';">&nbsp;&nbsp;&nbsp;</th>\n')
                    }
                    if (j<ncol(heading)) {
                        if (heading[i,j+1] == "="){
                            colspan <- paste0(heading[i,(j+1):ncol(heading)],
                                              collapse="")
                            ncolspan <- nchar(sub("^(=+).*$","\\1",colspan))+1
                            dthead[i,j] <- sub('<th ',paste0('<th colspan="',
                                                             ncolspan,'" '),
                                               dthead[i,j])
                            dthead[i,j] <- sub('align: *?(left|right)',
                                               paste0('align:center'),
                                               dthead[i,j])
                        }
                    }
                    if (i<nrow(heading)){
                        if (heading[i+1,j] == "|"){
                            rowspan <- paste0(heading[(i+1):nrow(heading),j],
                                              collapse="")
                            nrowspan <- nchar(sub("^(\\|+).*$","\\1",rowspan))+1
                            if (grepl("colspan",dthead[i,j])){
                                if (sum(!heading[i:(i+nrowspan-1),j:(j+ncolspan-1)]
                                        %in% c('=','|'))==1){
                                    dthead[i,j] <- sub('<th ',paste0('<th rowspan="',
                                                                     nrowspan,'" '),
                                                       dthead[i,j])
                                }else{
                                    dthead[i,j] <- sub('colspan.+?style',
                                                       paste0('rowspan="',
                                                              nrowspan,'" style'),
                                                       dthead[i,j])
                                }
                            }else{
                                dthead[i,j] <- sub('<th ',paste0('<th rowspan="',
                                                                 nrowspan,'" '),
                                                   dthead[i,j])
                            }
                        }
                    }
                }
            }
        }
        for (i in 1:nrow(heading)){
            rehead <- paste0(rehead,'\n  <tr>\n', paste0(dthead[i,],collapse=""),
                             '  </tr>\n', collapse="")
        }
        rehead <- paste0(rehead,'</thead>')
        rehead <- gsub('<thead>.+</thead>', rehead, htmltable)
        class(rehead) <- class(htmltable)
        attributes(rehead) <- attributes(htmltable)
        return(sub('<table', paste0('<table width=', as.character(tableWidth)),
                   rehead))
    }
}
#------------percent format---------------
convNum2Pct <- function(vector,digits=0){
    if (is.na(digits)) digits=0
    if (is.numeric(vector)){
        vec <- vector
        vec[which(!vec %in% c(NaN,Inf))] <-
            sprintf(paste0("%.",digits,"f%%"),100*vector[which(!vec %in% c(NaN,Inf))])
        return(vec)
    }else{
        return(vector)
    }
}
convPct2Num <- function(vector){
    if (any(grepl("[[:space:]]*((^\\d+[\\d\\.]\\d+)|\\d+)%$",vector))){
        vec <- vector
        which <- which(grepl("[[:space:]]*((^\\d+[\\d\\.]\\d+)|\\d+)%$",vector))
        vec[which] <- as.numeric(gsub("[[:space:]]*(.+)%$","\\1",vector[which]))/100
        vec[!seq_len(length(vec)) %in% which] <- NA
        return(as.numeric(vec))
    }else{
        return(rep(NA,length(vector)))
    }
}
