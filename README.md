# drawMap

Easily Draw China Maps

![](https://img.shields.io/badge/R-package-success)
![](https://img.shields.io/badge/Version-0.3.0-success)
![](https://img.shields.io/github/license/psychbruce/drawMap?label=License&color=success)
[![](https://img.shields.io/github/stars/psychbruce/drawMap?style=social)](https://github.com/psychbruce/drawMap/stargazers)

<a href="https://en.wikipedia.org/wiki/Creative_Commons_license"><img src="https://s1.ax1x.com/2020/07/28/aAjUJg.jpg" width="120px" height="42px"></a>

- 复制、修改、使用、分享本代码库，必须遵守<b>「创作共用许可协议 CC BY-NC-SA」（原作者署名-非商业用途使用-相同方式共享）</b>


## Author

[包寒吴霜 \| Bao H.-W.-S.](https://psychbruce.github.io)

E-mail: [baohws@foxmail.com](mailto:baohws@foxmail.com)

Website: [psychbruce.github.io](https://psychbruce.github.io)

[ResearchGate](https://www.researchgate.net/profile/Han_Wu_Shuang_Bao) |
[GitHub](https://github.com/psychbruce) |
[知乎](https://www.zhihu.com/people/psychbruce)


## Install from GitHub
```r
install.packages("devtools")
devtools::install_github("psychbruce/drawMap")
```


## How to Use

**For details, run `?drawChinaMap` or `help(drawChinaMap)` to read the help page.**

```r
## Template
View(provdata_temp)  # a template province-level dataset
drawChinaMap()  # draw a template of China map (no variables)
drawChinaMap(provdata_temp, var="geoE", nsmall=1, filename="ChinaMap1.png")
drawChinaMap(provdata_temp, var="geoN", nsmall=1, colors="Reds", direc=-1, addlabel=FALSE, filename="ChinaMap2.png")

## How to use it with a real dataset?
View(provdata_demo)  # a demo dataset (per capita GDP for 31 mainland provinces)

# Method 1: Use the 'var.prov' parameter
drawChinaMap(provdata_demo, var.prov="Province", var="GDPpc", nsmall=0, filename="ChinaMap_GDPpc.png")

# Method 2: Use dplyr::left_join() or dplyr::right_join() to merge datasets
provdata=dplyr::right_join(provdata_temp, provdata_demo, by=c("prov"="Province"))
drawChinaMap(provdata, var="GDPpc", nsmall=0, title="GDP per capita", filename="ChinaMap_GDPpc.png")
```


## Release Notes
### Current version: `0.3.0`
### Major changes:
+ `0.3.0` - 2020.09
  + Easier to use
  + More examples in help page
+ `0.2.0` - 2020.07
  + General bug-fixes and improvements
+ `0.1.0` - 2019.08
  + Initial commit
