## Section: User-defined Function
######################################################################################################################################################

library(grDevices)

#' @title display.cols: Display colors
#'
#' @description This function displays colors.
#'
#' @param col (required) color(s) to be displayed
#'
#' @return no value will be returned but a plot will be generated
#'
#' @export
#'
#' @examples display.cols(c("#009999", "#0000FF"))
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
display.cols <- function(col) {

  ## get no. of colors in col
  tmp_len = length(col)

  ## generate plot
  image(1:tmp_len, 1, as.matrix(1:tmp_len), col=col, axes=FALSE, xlab='', ylab='')

  ## assign names of colors to the plot
  mtext(text=names(col), side=1, line=0.3, at=1:tmp_len, las=2, cex=0.8)

  ## do not return anything
  return(invisible(NULL))
}
## End of Function;




#' @title create.col.palette: Create color palette
#'
#' @description This function creates color palette colors.
#'
#' @param cols (required) colors to interpolate; must be a valid argument to `grDevices::col2rgb()`.
#' @param dimension (required) number of colors to be generated; must be an integer greater than 1.
#' @param ... (optional) arguments to pass to `grDevices::colorRampPalette`
#'
#' @return a serious of colors interpolated from input colors
#'
#' @export
#'
#' @examples ## create monochromatic color palette
#' create.col.palette(c("#0000FF")) \cr
#' ## create color palette
#' create.col.palette(c("#00FF00", "#FFFF00", "#FF0000"))
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
create.col.palette <- function(cols, dimension, ...) {

  ## get the length of cols
  tmp.len = length(cols)

  ## if only one color is given, add white color to generate a monochrome color palettes
  if (tmp.len == 1) {
    tmp.source.col = c('#FFFFFF', cols)

    tmp.start = 2 ## exclude white (the first elemenet) from output
    tmp.end = dimension + 1 ## dimension increased by 1 after adding white
  } else {
    ## otherwise, no change is needed
    tmp.source.col = cols
    tmp.start = 1
    tmp.end = dimension
  }

  ## create palette generating function
  tmp.col.fun <- grDevices::colorRampPalette(colors = tmp.source.col, ...) ## #FFFFFF - white color

  ## Generate a series of color.
  return(tmp.col.fun(tmp.end)[tmp.start: tmp.end])

}
#End of Function;




######################################################################################################################################################
## End of Section;





## Section: Color Setting - MISC
######################################################################################################################################################


## Ancient China
Ancient_China_1 = rgb(red = c(  158, 226, 207, 184,  79,  64,  65, 154, 218, 194, 142, 16, 140, 107,  70), 
                      green = c(46,  175, 182, 206, 164, 125, 138, 180, 227, 196,  41, 20,  80,  51, 146), 
                      blue = c( 36,  66,   74, 142, 133,  82, 180, 205, 230, 195,  97, 32,  44,  26, 185), 
                      max = 255,
                      names = c('FuPenZiHong', 'HuPiHuang', 'CaoHuang', 'GanLanShiLv', 'ZhuLv', 'BoHeLv', 
                                'YuanWeiLan', 'XingLan', 'YunFengBai', 'YueYingBai', 
                                'XianCaiZi', 'GangLan', 'YanShiZong', 'SunPiZong', 'GuLan'))
## reorder the color
Ancient_China_1 = Ancient_China_1[c('FuPenZiHong', 'HuPiHuang', 'CaoHuang', 'GanLanShiLv', 'ZhuLv', 'BoHeLv', 
                                    'XingLan', 'GuLan', 'YuanWeiLan', 'GangLan', 'XianCaiZi', 
                                    'YanShiZong', 'SunPiZong', 'YunFengBai', 'YueYingBai')]
display.cols(Ancient_China_1)



## Zang Lan
ZangLan = c("#382C77", "#9EACCB", "#F3F9F8")
display.cols(ZangLan)

## Dai Lan
DaiLan = c("#3F4F65", "#5683C3", "#B1D1EE")
display.cols(DaiLan)

## Shi Qing
ShiQing = c("#115069", "#1E7FA0", "#E2F2F6")
display.cols(ShiQing)

## Tian Qing
TianQiang = c("#44818B", "#C3E0E7", "#EBF6FF")
display.cols(TianQiang)

## Zhu Qing
ZhuQing = c("#2F4527", "#355730", "#758E61", "#F2F8EB")
display.cols(ZhuQing)

## Liu Se
LiuSe = c("#2E5450", "#A8CD34", "#FFF687")
display.cols(LiuSe)

## Fei Cui
FeiCui = c("#366545", "#62BE9D", "#FAF8D0")
display.cols(FeiCui)

## Zhu Mu Lv
ZhuMuLv = c("#2E402C", "#176F58", "#49704D")
display.cols(ZhuMuLv)

## Qing Bai
QingBai = c("#3E8463", "#BEE0D0", "#E2F2F6")
display.cols(QingBai)

## Zhi Zi Se
ZhiZi = c("#D7A220", "#F5D553", "#F7F6F5")
display.cols(ZhiZi)

## Liu Li Huang
LiuLiHuang = c("#A03C2D", "#F0C800", "#F4F2CE")
display.cols(LiuLiHuang)

## Tuo Huang
TuoHuang = c("#CE391E", "#F4A135", "#FFF9D0")
display.cols(TuoHuang)

## Mo Se
MoSe = c("#29262E", "#4E606C", "#7993A2", "#CDE2E9", "#F5FBFE")
display.cols(MoSe)


## Xue Qing
XueQing = c("#534B6B", "#7E6F9D", "#AAA1CE", "#BBBCDE", "#8591C8")
display.cols(XueQing)

## Qing Lian
QingLian = c("#6E60A8", "#9C64A7", "#8187C2", "#C7D1EA")
display.cols(QingLian)

## Tan
Col.Palette.Tan = c('#373737', '#C0B283', '#DCD0C0', '#F4F4F4')
display.cols(Col.Palette.Tan)

## GrayBlue
Col.Palette.GrayBlue = c('#A9B7C0', '#C7D8C6', '#EFD9C1', '#CCCBC6')
display.cols(Col.Palette.GrayBlue)

## Evening
Col.Palette.Evening = c('#192231', '#985E6D', '#98878F', '#494E6B')
display.cols(Col.Palette.Evening)


## Vermillion
Col.Palette.Vermillion = c('#333A56', '#52658F', '#E8E8E8', '#F7F5E6')
display.cols(Col.Palette.Vermillion)

# ## Gunmetal
# Col.Palette.Gunmetal = c('#32435F', '#A67F78', '#8F8681', '#E1DCD9')
# display.cols(Col.Palette.Gunmetal)


## Misty Greens
Col.Palette.MistyGreens = c('#003E19', '#028C6A', '#7BC5AE', '#D1EDE1')
display.cols(Col.Palette.MistyGreens)


## Succulent
Col.Palette.Succulent = c('#283B42', '#1D6A96', '#85B8CB', '#D1DDD8')
display.cols(Col.Palette.Succulent)


## Celestial Green
Col.Palette.CelestialGreen = c('#002C2F', '#1E646E', '#023459', '#B2A59F')
display.cols(Col.Palette.CelestialGreen)


## Icy Stone
Col.Palette.IcyStone = c('#6B799E', '#EBC57C', '#9C8F96', '#A6C2CE')
display.cols(Col.Palette.IcyStone)


## Sea Green
Col.Palette.SeaGreen = c('#232941', '#295651', '#499360', '#97BAA4')
display.cols(Col.Palette.SeaGreen)


## Peace
Col.Palette.Peace = c('#3D313F', '#847072', '#869DAB', '#DCC1B0')
display.cols(Col.Palette.Peace)

## Cool Chameleon
Col.Palette.CoolCham = c('#50697D', '#ACC66D', '#A8BCBA', '#D9CFE7')
display.cols(Col.Palette.CoolCham)

## Mountain
Col.Palette.Mountain = c('#514644', '#535D55', '#A5B7C1', '#DBDBE5')
display.cols(Col.Palette.Mountain)

## Foggy
Col.Palette.Foggy = c('#030923', '#3A5A69', '#D5C5E9', '#FDDCEF')
display.cols(Col.Palette.Foggy)

## Birch
Col.Palette.Birch = c('#030305', '#EFB730', '#C2E3F4', '#F2F4EF')
display.cols(Col.Palette.Birch)

## Calming
Col.Palette.Calm = c('#E5D67B', '#A1B872', '#E8CAA4', '#BED3D4')
display.cols(Col.Palette.Calm)

## Bloom
Col.Palette.Bloom = c('#65C4D8', '#9EDAE3', '#DAB2D3', '#EAC9C0')
display.cols(Col.Palette.Bloom)

## Shadow
Col.Palette.Shadow = c('#2E1E11', '#655C57', '#F3B749', '#FEF0BF')
display.cols(Col.Palette.Shadow)

## Bakery
Col.Palette.Bakery = c('#01273C', '#353C42', '#607178', '#FBD1A7')
display.cols(Col.Palette.Bakery)

## Sunshine
Col.Palette.Sunshine = c('#414B6F', '#876363', '#FFB21A', '#FFEFA1')
display.cols(Col.Palette.Sunshine)

## Fruit
Col.Palette.Fruit = c('#9D6AB9', '#FECE00', '#5EAA5F', '#EE3239')
display.cols(Col.Palette.Fruit)

## Marine
Col.Palette.Marine = c('#000E2B', '#013B63', '#55C9EA', '#FEB75D')
display.cols(Col.Palette.Marine)

######################################################################################################################################################
## End of Section;





## Section: Color Setting - Chinese Colors
######################################################################################################################################################

## Red
ChineseColors.Reds = c("#FFB3A7", "#ED5736", "#F00056", "#F47983", "#DB5A6B", "#F20C00",
                       "#C93756", "#F05654", "#FF2121", "#8C4356", "#C83C23", "#9D2933",
                       "#FF4C00", "#FF4E20", "#F35336", "#CB3A56", "#FF2D51", "#C91F37",
                       "#EF7A82", "#FF0097", "#FF3300", "#C3272B", "#A98175", "#C32136",
                       "#B36D61", "#BE002F", "#DC3023", "#F9906F")
names(ChineseColors.Reds) = c("FenHong", "FeiSe", "PinHong", "TaoHong", "HaiTangHong", "ShiLiuHong",
                              "YingTaoSe", "YinHong", "DaHong", "JiangZi", "FeiHong", "YanZhi",
                              "ZhuHong", "Dan", "Tong", "QianSe", "HuoHong", "HeChi",
                              "YanHong", "YangHong", "Yan", "Chi", "Wan", "ZaoHong",
                              "Tan", "YanHong", "TuoHong", "TuoYan")
display.cols(ChineseColors.Reds)



## Yellow
ChineseColors.Yellow = c("#FFF143", "#FAFF72", "#EAFF56", "#FFA631", "#FF8C31", "#FF8936",
                         "#FFA400", "#FF7500", "#FFC773", "#F0C239", "#FA8C35", "#B35C44",
                         "#A88462", "#C89B40", "#60281E", "#B25D25", "#827100", "#7C4B00",
                         "#9B4400", "#AE7000", "#9C5333", "#955539", "#CA6924", "#6E511E",
                         "#D3B17D", "#E29C45", "#896C39", "#D9B611")
names(ChineseColors.Yellow) = c("EHuang", "TaHuang", "YingCaoSe", "XingHuang", "XingHong", "JuHuang",
                                "ChengHuang", "JuHong", "JiangHuang", "XiangSe", "ChengSe", "ChaiSe",
                                "TuoSe", "HunHuang", "LiSe", "ZongSe", "ZongLv", "ZongHei",
                                "ZongHong", "ZongHuang", "Zhe", "ZheSe", "HuPo", "HeSe",
                                "KuHuang", "HuangLu", "QiuSe", "QiuXiangSe")
display.cols(ChineseColors.Yellow)



## Green
ChineseColors.Green = c("#BDDD22", "#C9DD22", "#AFDD22", "#789262", "#A3D900", "#9ED900",
                        "#0EB83A", "#0EB840", "#0AA344", "#00BC12", "#0C8918", "#1BD1A5",
                        "#2ADD9C", "#48C0A3", "#3DE1AD", "#40DE5A", "#00E09E", "#00E079",
                        "#C0EBD7", "#E0EEE8", "#BBCDC5", "#424C50", "#00E500", "#9ED048",
                        "#96CE54", "#7BCFA6", "#2EDFA3", "#7FECAD", "#A4E2C6", "#21A675",
                        "#057748", "#BCE672")
names(ChineseColors.Green) = c("NenLv", "LiuHuang", "LiuLv", "ZhuQing", "CongHuang", "CongLv",
                               "CongQing", "CongQian", "QingCong", "YouLv", "LvShen", "BiSe",
                               "BiLv", "QingBi", "FeiCuiSe", "CaoLv", "QingSe", "QingCui",
                               "QingBai", "ELuanQing", "XieKeQing", "YaQing", "LvSe", "DouLv",
                               "DouQing", "ShiGao", "YuSe", "Piao", "AiLv", "SongBoLv",
                               "SongHuaLv", "SongHuaSe")
display.cols(ChineseColors.Green)


## Blue
ChineseColors.Blue = c("#44CEF6", "#177CB0", "#065279", "#3EEDE7", "#70F3FF", "#4B5CC4",
                       "#A1AFC9", "#2E4E7E", "#3B2E7E", "#4A4266", "#426666", "#425066",
                       "#574266", "#8D4BBB", "#815463", "#815476", "#4C221B", "#003371",
                       "#56004F", "#801DAE", "#4C8DAE", "#B0A4E3", "#CCA4E3", "#EDD1D8",
                       "#E4C6D0")
names(ChineseColors.Blue) = c("Lan", "DianQing", "DianLan", "BiLan", "WeiLan", "BaoLan",
                              "LanHuiSe", "ZangQing", "ZangLan", "Dai", "DaiLv", "DaiLan",
                              "DaiZi", "ZiSe", "ZiJiang", "JiangZi", "ZiTan", "GanQingGanZi",
                              "ZiTang", "QingLian", "QunQing", "XueQing", "DingXiangSe", "OuSe",
                              "OuHeSe")
display.cols(ChineseColors.Blue)


## horizon
ChineseColors.Horizon = c("#75878A", "#519A73", "#A29B7C", "#7397AB", "#395260", "#D1D9E0")
names(ChineseColors.Horizon) = c("CangSe", "CangLv", "CangHuang", "CangQing", "CangHei", "CangBai")
display.cols(ChineseColors.Horizon)


## water
ChineseColors.Water = c("#88ADA6", "#F3D3E7", "#D4F2E7", "#D2F0F4", "#D3E0F3", "#30DFF3", 
                        "#25F8CB")
names(ChineseColors.Water) = c("ShuiSe", "ShuiHong", "ShuiLv", "ShuiLan", "TanQing", "HuLan",
                               "HuLv")
display.cols(ChineseColors.Water)


## Gray
ChineseColors.Gray = c("#FFFFFF", "#FFFBF0", "#F2FDFF", "#D6ECF0", "#F2ECDE", "#E0F0E9", 
                       "#F3F9F1", "#E9F1F6", "#C2CCD0", "#FCEFE8", "#E3F9FD", "#808080", 
                       "#EEDEB0", "#F0F0F4")
names(ChineseColors.Gray) = c("JingBai", "XiangYaBai", "XueBai", "YueBai", "Gao", "Su",
                              "TuBai", "ShuangSe", "HuaBai", "YuDuBai", "YingBai", "HuiSe",
                              "YaBai", "QianBai")
display.cols(ChineseColors.Gray)


## Black
ChineseColors.Black = c("#622A1D", "#3D3B4F", "#725E82", "#392F41", "#161823", "#50616D", 
                       "#758A99", "#000000", "#493131", "#312520", "#5D513C", "#75664D", 
                       "#6B6882", "#665757", "#41555D")
names(ChineseColors.Black) = c("XuanSe", "XuanQing", "WuSe", "WuHei", "QiHei", "MoSe",
                              "HeiHui", "HeiSe", "ZiSe", "MeiHei", "LiHei", "Li",
                              "You", "YouHei", "An")
display.cols(ChineseColors.Black)


## Gold
ChineseColors.Gold = c("#F2BE45", "#EACD76", "#E9E7EF", "#BACAC6", "#A78E44", "#549688")
names(ChineseColors.Gold) = c("ChiJin", "JinSe", "YinBai", "LaoYin", "WuJin", "TongLv")
display.cols(ChineseColors.Gold)

######################################################################################################################################################
## End of Section;