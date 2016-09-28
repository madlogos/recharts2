validChartTypes <- read.csv(
        system.file('validChartTypes.csv', package = 'recharts2'), header=TRUE,
        stringsAsFactors=FALSE)

mapShapeJS <- read.csv(system.file('mapNameJS.csv', package='recharts2'),
                       header=TRUE, stringsAsFactors=FALSE)
mapShapeJS[,1] <- sapply(mapShapeJS[,1], function(char) eval(parse(text=char)))

validSymbols <- c(
    'circle', 'rect', 'roundRect', 'triangle', 'diamond', 'pin', 'arrow',
    'emptyCircle', 'emptyRect', 'emptyRoundRect', 'emptyTriangle', 'emptyDiamond',
    'emptyPin', 'emptyArrow', 'none')

.onLoad <- function(libname, pkgname='recharts2'){
    if (Sys.info()[['sysname']] == 'Windows'){
        Sys.setlocale('LC_CTYPE', 'Chs')
    }else{
        Sys.setlocale('LC_CTYPE', 'zh_CN.utf-8')
    }
    options(encoding="native.enc")
}
