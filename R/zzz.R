validChartTypes <- read.csv(
        system.file('validChartTypes.csv', package = 'recharts'), header=TRUE,
        stringsAsFactors=FALSE)

validSymbols <- c('circle', 'rectangle', 'triangle', 'diamond', 'emptyCircle',
                  'emptyRectangle', 'emptyTriangle', 'emptyDiamond', 'heart',
                  'droplet', 'pin', 'arrow', 'star3', 'star4', 'star5', 'star6',
                  'star7', 'star8', 'star9', 'star10', 'star', 'none')

mapShapeJS <- matrix(c(
    "\u65B0\u7586","\u897F\u85CF","\u5185\u8499\u53E4","\u9752\u6D77","\u56DB\u5DDD",
    "\u9ED1\u9F99\u6C5F","\u7518\u8083","\u4E91\u5357","\u5E7F\u897F","\u6E56\u5357",
    "\u9655\u897F","\u5E7F\u4E1C","\u5409\u6797","\u6CB3\u5317","\u6E56\u5317",
    "\u8D35\u5DDE","\u5C71\u4E1C","\u6C5F\u897F","\u6CB3\u5357","\u8FBD\u5B81",
    "\u5C71\u897F","\u5B89\u5FBD","\u798F\u5EFA","\u6D59\u6C5F","\u6C5F\u82CF",
    "\u91CD\u5E86","\u5B81\u590F","\u6D77\u5357","\u53F0\u6E7E","\u5317\u4EAC",
    "\u5929\u6D25","\u4E0A\u6D77","\u9999\u6E2F","\u6FB3\u95E8","xinjiang","xizang",
    "neimenggu","qinghai","sichuan","heilongjiang","gansu","yunnan","guangxi",
    "hunan","shanxi1","guangdong","jilin","hebei","hubei","guizhou","shandong",
    "jiangxi","henan","liaoning","shanxi","anhui","fujian","zhejiang","jiangsu",
    "chongqing","ningxia","hainan","taiwan","beijing","tianjin","shanghai","xianggang",
    "aomen"), ncol=2)

.onLoad <- function(libname, pkgname='recharts'){
    if (Sys.info()[['sysname']] == 'Windows'){
        Sys.setlocale('LC_CTYPE', 'Chs')
    }else{
        Sys.setlocale('LC_CTYPE', 'zh_CN.utf-8')
    }
}
